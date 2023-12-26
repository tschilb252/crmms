# ============================================================================
# Compare CRMMS-ESP runs 
#   Calculate PE thresholds at Mead and Powell
#
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "fig_dir_nm", "custom_Tr_col")))

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

# set up
slots <- rep(c("Powell.Pool Elevation", "Mead.Pool Elevation"), times = 2)
rdfs = c(rep('res.rdf', length(slots)))
per = c("wy","cy", "asis", "asis")
summry = c("min","min", NA, NA)
varble = c("Powell.Pool Elevation Min WY", "Mead.Pool Elevation Min CY",
           "Powell.Pool Elevation", "Mead.Pool Elevation")

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rdfs,
  slot = slots,
  period = per,
  summary = summry,
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = varble,
  stringsAsFactors = FALSE
))

# read/process RDFs
df <- NULL
for (i in 1:length(scenarios)) {

  # check that directory exists
  if (!dir.exists(data_dir[i])) { stop(paste("Data directory does not exist:", data_dir[i]))}

  scen_res <- rdf_aggregate(
    agg = rwa1,
    rdf_dir = data_dir[i]
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
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y"))  %>%
  select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Trace = 1991 + Trace - min(df$TraceNumber, na.rm = T))

# # mead < 1020, 1000, 950, 895 ---------------------------------------
ss <- 'Mead.Pool Elevation Min CY'
mead_elevs <- c(1020, 1000, 950, 896)

df_i = df_scens %>%
  filter(Variable == ss) %>%
  mutate(year = year(Date))
  
for (i in 1:length(mead_elevs)){ 

  df_iplot = df_i %>%
    mutate(Value = Value < mead_elevs[i]) %>%
    group_by(year, Scenario) %>%
    summarise(Value = mean(Value)) 
  
  lims_max = ifelse(max(df_iplot$Value)>0.5, 1, 0.5)
  
  ggplot(df_iplot, aes(year, Value, fill = Scenario)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    bor_theme() +
    labs(
      title = 'Lake Mead Minimum Annual Elevation',
      subtitle = paste0('Percent of Traces Less Than Elevation ',mead_elevs[i],' feet msl'),
      x = 'Year', y = 'Percent of Traces', fill = NULL
    ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, lims_max),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = 2023:2050, expand = c(0.02,0.02)) +
    scale_fill_manual(values = custom_Tr_col)
  
  ggsave(filename = file.path(fig_dir, paste0('Thresholds_Mead_lt_', mead_elevs[i], '.png')),
         width = 6.5, height = 5)
}

test = df_i %>%
  mutate(Value_1025 = Value < 1025,
         Value_1020 = Value < 1020,
         Value_1000 = Value < 1000,
         Value_950 = Value < 950,
         Value_896 = Value < 896) %>%
  group_by(year, Scenario) %>%
  summarise(Value_1025 = mean(Value_1025),
            Value_1020 = mean(Value_1020),
            Value_1000 = mean(Value_1000),
            Value_950 = mean(Value_950),
            Value_896 = mean(Value_896))
write.csv(test, file.path(fig_dir, 'Thresholds_Mead.csv'))


# powell < pool elevations ------------------------------------------
ss <- 'Powell.Pool Elevation Min WY'
pwl_elevs = c(3525, 3490, 3400)

df_i = df_scens %>%
  filter(Variable == ss) %>%
  mutate(year = year(Date))

for (i in 1:length(custom_Tr_col)){ 
  
  df_iplot = df_i %>%
    mutate(Value = Value < custom_Tr_col[i]) %>%
    group_by(year, Scenario) %>%
    summarise(Value = mean(Value)) 
  
  lims_max = ifelse(max(df_iplot$Value)>0.5, 1, 0.5)
  
  ggplot(df_iplot, aes(year, Value, fill = Scenario)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    bor_theme() +
    labs(
      title = 'Lake Powell Minimum Water Year Elevation',
      subtitle = paste0('Percent of Traces Less Than Elevation ',custom_Tr_col[i],' feet msl'),
      x = 'Year', y = 'Percent of Traces', fill = NULL
    ) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, lims_max),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = 2023:2050, expand = c(0.02,0.02)) +
    scale_fill_manual(values = custom_Tr_col)
  
  ggsave(filename = file.path(fig_dir, paste0('Thresholds_Powell_lt_', custom_Tr_col[i], '.png')),
         width = 6.5, height = 5)
}

test = df_i %>%
  mutate(Value_3525 = Value < 3525,
         Value_3490 = Value < 3490,
         Value_3400 = Value < 3400) %>%
  group_by(year, Scenario) %>%
  summarise(Value_3525 = mean(Value_3525),
            Value_3490 = mean(Value_3490),
            Value_3400 = mean(Value_3400))
write.csv(test, file.path(fig_dir, 'Thresholds_Powell.csv'))


## --- Powell threshold by month
df_powell_thresholdsDATE <- NULL
for (i in 1:length(pwl_elevs)) {
  df_i <- df_scens %>%
    filter(Variable == 'Powell.Pool Elevation') %>%
    mutate(Value = Value < pwl_elevs[i]) %>%
    group_by(Date, Scenario) %>%
    summarise(Value = mean(Value)) %>%
    mutate(thresh = pwl_elevs[i]) 
  
  df_powell_thresholdsDATE = rbind.data.frame(df_powell_thresholdsDATE, df_i)
}

df_powell_thresholdsDATE %>% 
  ungroup() %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'),
                          levels = paste0(pwl_elevs, ' ft'))) %>%
  ggplot(aes(Date, Value, color = Scenario)) +
  # geom_bar(position = 'dodge', stat = 'identity') +
  geom_line(size = 1.5) +
  bor_theme() +
  labs(
    title = 'Lake Powell Minimum Water Year Elevation',
    subtitle = paste0('Percent of Traces Less Than Elevation '),
    x = 'Year', y = 'Percent of Traces', fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, lims_max),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = 2023:2050, expand = c(0.02,0.02)) +
  scale_color_manual(values = custom_Tr_col) +
  facet_grid(thresh2 ~ .)

ggsave(file.path(fig_dir, paste0("Thresholds_PowellMonthly.png")), 
       width = 8.5, height = 8)


## --- Mead threshold by month
df_mead_thresholdsDATE <- NULL
for (i in 1:length(mead_elevs)) {
  df_i <- df_scens %>%
    filter(Variable == 'Mead.Pool Elevation') %>%
    mutate(Value = Value < mead_elevs[i]) %>%
    group_by(Date, Scenario) %>%
    summarise(Value = mean(Value)) %>%
    mutate(thresh = mead_elevs[i]) 
  
  df_mead_thresholdsDATE = rbind.data.frame(df_mead_thresholdsDATE, df_i)
}

df_mead_thresholdsDATE %>% 
  ungroup() %>%
  mutate(thresh2 = factor(paste0(as.character(thresh), ' ft'),
                          levels = paste0(mead_elevs, ' ft'))) %>%
  ggplot(aes(Date, Value, color = Scenario)) +
  # geom_bar(position = 'dodge', stat = 'identity') +
  geom_line(size = 1.5) +
  bor_theme() +
  labs(
    title = 'Lake Mead Minimum Water Year Elevation',
    subtitle = paste0('Percent of Traces Less Than Elevation '),
    x = 'Year', y = 'Percent of Traces', fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, lims_max),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = 2023:2050, expand = c(0.02,0.02)) +
  scale_color_manual(values = custom_Tr_col) +
  facet_grid(thresh2 ~ .)

ggsave(file.path(fig_dir, paste0("Thresholds_MeadMonthly.png")), 
       width = 8.5, height = 8)
