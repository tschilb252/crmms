# ============================================================================
# Compare CRMMS-ESP run 
#   Powell Tiers / Powell TARV 
#   
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "scenarios", "fig_dir_nm")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)
library(patchwork)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
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
  'PowellData.ConstrainedLEBT_LowerElevBalancingBranch',
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
             'PowellData.ConstrainedLEBT_LowerElevBalancingBranch')
df_i = df_scens %>% 
  filter(Variable %in% decSlots) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(year(Date) <= max_yr) %>%
  mutate(Year = year(Date),
         PowellData.ConstrainedLEBT_LowerElevBalancingBranch = 
           ifelse(is.na(PowellData.ConstrainedLEBT_LowerElevBalancingBranch),
                  5, PowellData.ConstrainedLEBT_LowerElevBalancingBranch), 
         act_TARV = PowellData.ActualAnnualReleaseVolume/10^3,
         tarv = PowellData.TargetAnnualReleaseVolume/10^3,
         `Powell Tiers` = factor(PowellData.ReleaseTier, 
                                 levels = c(0,1,2,3),
                                 labels = names(PowellTierLab)),
         `ConstLEBT` = factor(PowellData.ConstrainedLEBT_LowerElevBalancingBranch, 
                              levels = c(0, 1,2,3,5),
                              labels = c('Other Tier', 'Standard Balancing', 
                                         'Constrained Bal', 'Constrained to 7.0', 'No Data')),
         `IsConstLEBT` = factor(ifelse(PowellData.ConstrainedLEBT_LowerElevBalancingBranch %in% c(2,3), 
                                       'Constrained', 'Not Constrained'),
                                levels = c('Constrained', 'Not Constrained'))) %>%
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
store_3490 = 3742.714 #CRSSIO::elevation_to_storage(3490, 'powell')/1000
df_st = df_scens %>% 
  filter(Variable %in% c("Powell.Storage")) %>%
  select(-Variable) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year > min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(MinPPDeficit = store_3490 - min(Value)) 

## end of WY PE
df_eowy = df_scens %>% 
  filter(Variable %in% c("Powell.Pool Elevation")) %>%
  select(-Variable) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year > min(year(Date)) & month(Date) == 9) %>%
  select(-Date) %>%
  rename(eowyPE = Value)

## combine data
df_agg = left_join(df_i, df_flow, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_st, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_eowy, by = c('Scenario', 'Trace', 'Year'))


## ---plot data
if (length(scenarios) == 2) {
  custom_Tr_col <- c('#f1c40f', '#8077ab')
} else {
  custom_Tr_col <- scales::hue_pal()(length(scenarios))
}
for (plot_yr in 2023:2024) {
  df_plot = df_agg %>%
    filter(Year == plot_yr)
  
  ## -- Powell Unreg Inflow vs. TARV
  ggplot(df_plot, aes(ann, act_TARV, color = Scenario, shape = IsConstLEBT, 
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
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_PowellUnreg_ConstLEBT_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell TARV vs. EOWY PE
  ggplot(df_plot, aes(act_TARV, eowyPE, color = Scenario, shape = IsConstLEBT, 
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
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_PowellPE_ConstLEBT_",plot_yr, ".png")), 
         width = 8, height = 7)
}
