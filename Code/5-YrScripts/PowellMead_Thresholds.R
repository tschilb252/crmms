# ============================================================================
# Compare CRMMS-ESP for different CRMMS scenarios 
#   Calculate PE thresholds at Mead and Powell
#
# ============================================================================
rm(list=ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

## -- Inputs
scenario_dir <- c(
  'Apr2022_v3',
  # 'Apr2022_v2',
  'Apr2022_v1',
  'Mar2022'
  # 'Feb2022',
  # 'Jan2022_OG',
  # 'Jan2022_updatedRegression'
)
scenarios <- c(
  'Apr. 2022 v3',
  # 'Apr. 2022 v2',
  'Apr. 2022 v1',  
  'Mar. 2022'
  # 'Feb. 2022',
  # 'Jan. 2022',
  # 'Jan. 2022 Updated Regres.'
)
fig_dir_nm <- 'Aprv1,3_Mar_Compare'
# ^ script will create directory with this name if it doesn't exist

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Output Data', fig_dir_nm)
data_dir <- file.path('rdfOutput', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))

## Max Date
max_date = '2026-12' #'2024-12'

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
  trces = unique(scen_res$TraceNumber)
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
openxlsx::write.xlsx(summ_pwl, file = file.path(fig_dir, paste0('Thresholds_Powell', file_nm_end, '.xlsx')))

# Powell plot thresholds
summ_pwl %>%
  pivot_longer(cols = colnames(summ_pwl)[-c(1,2)]) %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'), 
                          levels = paste0(pwl_elevs, ' ft')),
         name = as.numeric(name)) %>%
  ggplot(aes(name, value, color = factor(Scenario), linetype = factor(thresh2))) +
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, 
    linetype = "Powell Elevations", #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake Powell Elevations in any month of a year"
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
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake powell Elevations in each month"
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
openxlsx::write.xlsx(summ_mead, file = file.path(fig_dir, paste0('Thresholds_Mead', file_nm_end, '.xlsx')))


# MEAD plot thresholds
summ_mead %>%
  pivot_longer(cols = colnames(summ_mead)[-c(1,2)]) %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'), 
                          levels = paste0(mead_elevs, ' ft')),
         name = as.numeric(name)) %>%
  ggplot(aes(name, value, color = factor(Scenario), linetype = factor(thresh2))) +
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, 
    linetype = "Mead Elevations", #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake Mead Elevations in any month of a year"
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
  scale_y_continuous(limits = c(0,100), expand = c(0,0), 
                     breaks = seq(0,100, by =20)) +
  bor_theme() +
  labs(
    y = 'Percent of Traces (%)', x = NULL, 
    color = NULL, #size = NULL, fill = NULL,
    title = "Percent of Traces Below Lake Mead Elevations in each month"
  ) +
  facet_grid(thresh2 ~ .)

ggsave(file.path(fig_dir, paste0("Thresholds_MeadMonthly",  file_nm_end, ".png")), 
       width = 8.5, height = 8)