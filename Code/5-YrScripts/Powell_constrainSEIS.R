# ============================================================================
# Compare CRMMS-ESP run 
#   Powell Constrained due to SEIS protection of 3500 or bypass capacity 
#   
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "fig_dir_nm", "custom_Tr_col")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)
library(patchwork)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
scenarios = names(scenario_dir)
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

## Max Date
max_yr = 2027
end_file_nm = paste0('_', max_yr)

##### ------ Tiers | Powell Release ------ #####

# set up 
slots = c(
  'PowellData.ReleaseTier',
  'PowellData.TargetAnnualReleaseVolume',
  'PowellData.ActualAnnualReleaseVolume',
  'PowellData.ReleaseConstrained_3500Bypass',
  "Powell.Storage", "Powell.Pool Elevation",
  "PowellInflow.Unregulated"
)

rdfs = c(rep('flags.rdf', length(slots)-3),
         rep('res.rdf', 2),
         rep('streamflow.rdf', 1))

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rdfs,
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
  
  # check that directory exists
  if (!dir.exists(data_dir[i])) { stop(paste("Data directory does not exist:", data_dir[i]))}
  
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i], 
    find_all_slots = FALSE
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
  mutate(Scenario = factor(Scenario, levels = scenarios))

PowellTierLab = c('Equalization Tier' = '#a6cee3', 
                  'Upper Elevation\nBalancing Tier' = '#1f78b4',
                  'Mid Elevation\nRelease Tier' = '#b2df8a',
                  'Lower Elevation\nBalancing Tier' = '#33a02c')
## process df
decSlots = c('PowellData.ReleaseTier',
             'PowellData.TargetAnnualReleaseVolume',
             'PowellData.ActualAnnualReleaseVolume',
             'PowellData.ReleaseConstrained_3500Bypass')
df_i = df_scens %>% 
  filter(Variable %in% decSlots) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(year(Date) <= max_yr) %>%
  mutate(Year = year(Date),
         PowellData.ReleaseConstrained_3500Bypass = 
           ifelse(is.na(PowellData.ReleaseConstrained_3500Bypass),
                  0, PowellData.ReleaseConstrained_3500Bypass), 
         act_TARV = PowellData.ActualAnnualReleaseVolume/10^3,
         tarv = PowellData.TargetAnnualReleaseVolume/10^3,
         `Powell Tiers` = factor(PowellData.ReleaseTier, 
                                 levels = c(0,1,2,3),
                                 labels = names(PowellTierLab)),
         `ConstRel` = factor(PowellData.ReleaseConstrained_3500Bypass, 
                              levels = c(0, 1),
                              labels = c('Not Constrained', 'Constrained Release'))) %>%
  select(-all_of(decSlots), -Date) 

## agg streamflow
df_flow = df_scens %>% 
  filter(Variable %in% c("PowellInflow.Unregulated")) %>%
  select(-Variable) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year > min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(ann = sum(Value))

## agg storage deficit
store_3490 = CRSSIO::elevation_to_storage(3490, 'powell')/1000
df_st = df_scens %>% 
  filter(Variable %in% c("Powell.Storage")) %>%
  select(-Variable) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year >= min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(MinPPDeficit = store_3490 - min(Value)) 

## end of WY PE
df_eowy = df_scens %>% 
  filter(Variable %in% c("Powell.Pool Elevation")) %>%
  select(-Variable) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year >= min(year(Date)) & month(Date) == 9) %>%
  select(-Date) %>%
  rename(eowyPE = Value)

## combine data
df_agg = left_join(df_i, df_flow, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_st, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_eowy, by = c('Scenario', 'Trace', 'Year'))

df_agg %>% filter(ConstRel != 'Not Constrained')


## --- plot data
for (plot_yr in 2024:2027) {
  df_plot = df_agg %>%
    filter(Year == plot_yr)
  
  ## -- Powell Unreg Inflow vs. TARV
  ggplot(df_plot, aes(ann, act_TARV, color = Scenario, shape = ConstRel, 
                      alpha = 0.5, size = 1.5)) +
    geom_point() +
    scale_color_manual(values = custom_Tr_col) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 40000, by = 1000)) +
    theme_bw()+
    labs(x = 'WY Annual Unregulated Inflow (kaf)',
         y = 'WY Annual Release Volume (kaf)',
         title = paste('Powell Unreg. Inflow vs. TARV for WY', plot_yr))+
    guides(alpha = 'none', size = 'none') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_PowellUnreg_Const_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell TARV vs. EOWY PE
  ggplot(df_plot, aes(act_TARV, eowyPE, color = Scenario, shape = ConstRel, 
                      alpha = 0.5, size = 1.5)) +
    geom_point() +
    scale_color_manual(values = custom_Tr_col) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma, breaks = seq(3000, 3700, by = 5)) +
    theme_bw()+
    labs(y = 'EOWY Pool Elevation (ft)',
         x = 'WY Annual Release Volume (kaf)',
         title = paste('Powell Comparison', plot_yr))+
    guides(alpha = 'none', size = 'none')
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_PowellPE_Const_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell TARV vs. Deficit Vol
  ggplot(df_plot, aes(act_TARV, MinPPDeficit, color = Scenario, shape = ConstRel, 
                      alpha = 0.5, size = 1.5)) +
    geom_point() +
    scale_color_manual(values = custom_Tr_col) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 15000, by = 1000)) +
    scale_y_continuous(labels = scales::comma) +
    theme_bw()+
    labs(y = 'Max Deficit Volume Below 3,490 ft (kaf)',
         x = 'WY Annual Release Volume (kaf)',
         title = paste('Powell Comparison', plot_yr))+
    guides(alpha = 'none', size = 'none')
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_DeficitVol_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell Projected vs Actual TARV
  ggplot(df_plot, aes(tarv, act_TARV, color = Scenario, shape = ConstRel, 
                      alpha = 0.5, size = 1.5)) +
    geom_point() +
    scale_color_manual(values = custom_Tr_col) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 15000, by = 1000)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 15000, by = 1000)) +
    theme_bw()+
    labs(x = 'Projected WY Annual Release Volume (kaf)',
         y = 'Actual WY Annual Release Volume (kaf)',
         title = paste('Powell Comparison', plot_yr))+
    guides(alpha = 'none', size = 'none')
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_ActvsProj_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell Projected vs Actual TARV
  df_plot %>% mutate(tarvdif = tarv - act_TARV) %>%
  ggplot(aes(tarv, tarvdif, color = Scenario, shape = ConstRel, 
                      alpha = 0.5, size = 1.5)) +
    geom_point() +
    scale_color_manual(values = custom_Tr_col) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 15000, by = 1000)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 15000, by = 1000)) +
    theme_bw()+
    labs(x = 'Projected WY Annual Release Volume (kaf)',
         y = 'Actual WY Annual Release Volume (kaf)',
         title = paste('Powell Comparison', plot_yr))+
    guides(alpha = 'none', size = 'none')
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_ActvsProj_",plot_yr, ".png")), 
         width = 8, height = 7)
}
