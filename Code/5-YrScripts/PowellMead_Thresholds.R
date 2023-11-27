# ============================================================================
# Compare CRMMS-ESP runs 
#   Calculate PE thresholds at Mead and Powell
#
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "fig_dir_nm")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
scenarios = names(scenario_dir)
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

## Max Date
max_date = '2028-12' #'2024-12'

slots = c("Mead.Pool Elevation", "Powell.Pool Elevation")
file_nm_end <- paste0('_thru', format(ym(max_date), "%Y"))

## -- Read in CRMMS results

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rep("res.rdf", length(slots)),
  slot = slots, 
  period = rep("asis", length(slots)),
  summary = rep(NA, length(slots)),
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = slots,
  stringsAsFactors = FALSE
))

# read/process RDFs
df <- NULL
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i],
    keep_cols = 'Unit'
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = sort(unique(scen_res$TraceNumber))
  trces = trces[trces >= 0]
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df, scen_res)
}

df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  filter(Date <= as.yearmon(format(ym(max_date), "%Y-%m"))) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios)) %>%
  # bring units into line with crss
  mutate(Value = ifelse(Variable %in% c('Mead.Inflow', 'Mead.Storage', "Powell.Outflow",
                                        "Powell.Inflow", "Powell.Storage"),
                        Value * 10^3,
                        Value))

## -- Calculate thresholds (as long as full ESP)
yrmax = year(ym(max_date))
if (length(scenarios) == 2) {
  custom_Tr_col <- c('#f1c40f', '#8077ab')
} else {
  custom_Tr_col <- scales::hue_pal()(length(scenarios))
}

## Threshold calcs
pwl_elevs = c(3525,3490,3375)
df_powl_thresholds <- NULL
for (i in 1:length(pwl_elevs)) {
  df_i = df_scens %>%
    filter(Variable == 'Powell.Pool Elevation') %>%
    mutate(Yeari = ifelse(month(Date) > 9,
                          year(Date)+1,
                          year(Date))) %>%
    group_by(Scenario, Yeari, Trace) %>%
    summarise(min3490 = min(Value)) %>%
    filter(min3490 < pwl_elevs[i]) %>%
    ungroup() %>%
    group_by(Yeari, Scenario) %>%
    summarise(cnt = n()) %>%
    mutate(prob = round(cnt/30 *100, digits = 0),
           thresh = pwl_elevs[i]) 
  
  df_powl_thresholds = rbind.data.frame(df_powl_thresholds, df_i)
}

summ_pwl = df_powl_thresholds %>% ungroup() %>% select(-cnt) %>%
  filter(Yeari <= yrmax) %>%
  pivot_wider(names_from = Yeari, values_from = prob) %>%
  replace (is.na(.), 0) 

# Powell plot thresholds
summ_pwl %>%
  pivot_longer(cols = colnames(summ_pwl)[-c(1,2)]) %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'), 
                          levels = paste0(pwl_elevs, ' ft')),
         name = as.numeric(name)) %>%
  ggplot(aes(name, value, color = factor(Scenario), linetype = factor(thresh2))) +
  geom_line(size = 1.5) +
  scale_color_manual(values = custom_Tr_col) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, 
    linetype = "Powell Elevations", #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake Powell Elevations in any Month of the Water Year"
  ) 
ggsave(file.path(fig_dir, paste0("Thresholds_Powell",  file_nm_end, ".png")), 
       width = 7, height = 5)

## Powell threshold by month
df_powell_thresholdsDATE <- NULL
for (i in 1:length(pwl_elevs)) {
  df_i <- df_scens %>%
    filter(Variable == 'Powell.Pool Elevation') %>%
    mutate(val = ifelse(Value < pwl_elevs[i], 1, 0)) %>%
    group_by(Scenario, Date) %>%
    summarise(cnt = sum(val)) %>%
    mutate(prob = round(cnt/30 * 100, digits = 0),
           thresh = pwl_elevs[i]) 
  
  df_powell_thresholdsDATE = rbind.data.frame(df_powell_thresholdsDATE, df_i)
}

df_powell_thresholdsDATE %>% ungroup() %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'), 
                          levels = paste0(pwl_elevs, ' ft'))) %>%
  ggplot(aes(Date, prob, color = Scenario)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = custom_Tr_col) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake Powell Elevations in each Month"
  ) +
  facet_grid(thresh2 ~ .)
ggsave(file.path(fig_dir, paste0("Thresholds_PowellMonthly",  file_nm_end, ".png")), 
       width = 8.5, height = 8)

## Mead thresholds
mead_elevs = c(1020,1000,950,900)
df_mead_thresholds <- NULL
for (i in 1:length(mead_elevs)) {
  df_i <- df_scens %>%
    filter(Variable == 'Mead.Pool Elevation') %>%
    mutate(Year = year(Date)) %>%
    group_by(Scenario, Year, Trace) %>%
    summarise(min3490 = min(Value)) %>%
    filter(min3490 < mead_elevs[i]) %>%
    ungroup() %>%
    group_by(Year, Scenario) %>%
    summarise(cnt = n()) %>%
    mutate(prob = round(cnt/30 *100, digits = 0),
           thresh = mead_elevs[i]) 
  
  df_mead_thresholds = rbind.data.frame(df_mead_thresholds, df_i)
}

summ_mead = df_mead_thresholds %>% ungroup() %>% select(-cnt) %>%
  filter(Year <= yrmax) %>%
  pivot_wider(names_from = Year, values_from = prob) %>%
  replace (is.na(.), 0) 


# MEAD plot thresholds
summ_mead %>%
  pivot_longer(cols = colnames(summ_mead)[-c(1,2)]) %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'), 
                          levels = paste0(mead_elevs, ' ft')),
         name = as.numeric(name)) %>%
  ggplot(aes(name, value, color = factor(Scenario), linetype = factor(thresh2))) +
  geom_line(size = 1.5) +
  scale_color_manual(values = custom_Tr_col) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, 
    linetype = "Mead Elevations", #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake Mead Elevations in any Month of the Year"
  ) 
ggsave(file.path(fig_dir, paste0("Thresholds_Mead",  file_nm_end, ".png")), 
       width = 7, height = 5)

# MEAD when falling below thresholds in any month
df_mead_thresholdsDATE <- NULL
for (i in 1:length(mead_elevs)) {
  df_i <- df_scens %>%
    filter(Variable == 'Mead.Pool Elevation') %>%
    mutate(val = ifelse(Value < mead_elevs[i], 1, 0)) %>%
    group_by(Scenario, Date) %>%
    summarise(cnt = sum(val)) %>%
    mutate(prob = round(cnt/30 * 100, digits = 0),
           thresh = mead_elevs[i]) 
  
  df_mead_thresholdsDATE = rbind.data.frame(df_mead_thresholdsDATE, df_i)
}

df_mead_thresholdsDATE %>% ungroup() %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'), 
                          levels = paste0(mead_elevs, ' ft'))) %>%
  ggplot(aes(Date, prob, color = Scenario)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = custom_Tr_col) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake Mead Elevations in each Month"
  ) +
  facet_grid(thresh2 ~ .)

ggsave(file.path(fig_dir, paste0("Thresholds_MeadMonthly",  file_nm_end, ".png")), 
       width = 8.5, height = 8)

## summarize PE quantiles
pe_summary = df_scens %>%
  group_by(Variable, Scenario, Date) %>% 
  summarise(q = list(quantile(Value, probs = seq(0,1, by = 0.1)))) %>% 
  unnest_wider(q)

## write data to excel doc
wb1 <- openxlsx::createWorkbook("Thresholds_PEs")
openxlsx::addWorksheet(wb1, "Powell_Thresholds")
openxlsx::writeData(wb1, "Powell_Thresholds", summ_pwl)
openxlsx::addWorksheet(wb1, "Mead_Thresholds")
openxlsx::writeData(wb1, "Mead_Thresholds", summ_mead)
openxlsx::addWorksheet(wb1, 'PE_quantiles')
openxlsx::writeData(wb1, 'PE_quantiles', pe_summary)

# save workbook
openxlsx::saveWorkbook(wb1, file.path(fig_dir, paste0('Thresholds_PEs', file_nm_end, '.xlsx')), overwrite = T)