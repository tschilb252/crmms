# ============================================================================
# Compare CRMMS-ESP run 
#   Scatter plots to compare Powell Fcst, Release, EOY Powell/Mead PE 
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
source(file.path('Code', 'add_MeadPowell_tiers.R'))
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

## Max Date
max_yr = 2028
end_file_nm = paste0('_', max_yr)

##### ------ Tiers | Powell Release ------ #####

# set up 
slots = c(
  'PowellData.ReleaseTier',
  'PowellData.TargetAnnualReleaseVolume',
  'PowellData.ActualAnnualReleaseVolume',
  "Powell.Storage", "Powell.Pool Elevation", 
  "Mead.Pool Elevation", "Mead.Outflow", "Mead.Storage",
  "PowellInflow.Unregulated"
)
rdfs = c(rep('flags.rdf', length(slots)-6),
         rep('res.rdf', 5),
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

## -- process and combine hydrologies
df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Trace = 1991 + Trace - min(df$TraceNumber, na.rm = T),
         Year = year(Date),
         WY = ifelse(month(Date)>=10, year(Date) + 1,
                     year(Date))) %>%
  filter(Year <= max_yr) 

PowellTierLab = c('Equalization Tier' = '#a6cee3', 
                  'Upper Elevation\nBalancing Tier' = '#1f78b4',
                  'Mid Elevation\nRelease Tier' = '#b2df8a',
                  'Lower Elevation\nBalancing Tier' = '#33a02c')
## process df
decSlots = c('PowellData.ReleaseTier',
             'PowellData.TargetAnnualReleaseVolume',
             'PowellData.ActualAnnualReleaseVolume')
df_i = df_scens %>% 
  filter(Variable %in% decSlots) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(year(Date) <= max_yr) %>%
  mutate(Year = year(Date),
         act_TARV = PowellData.ActualAnnualReleaseVolume/10^3,
         tarv = PowellData.TargetAnnualReleaseVolume/10^3,
         `Powell Tiers` = factor(PowellData.ReleaseTier, 
                                 levels = c(0,1,2,3),
                                 labels = names(PowellTierLab))) %>%
  select(-all_of(decSlots), -Date) 

## --- agg streamflow with current year of historical data

## Read historical data from hdb
sdis <- c("PowellInflow.Unregulated" = 1856)
start_date = format(ym("1990-10"), "%Y-%m")
dt_fcst = df_scens %>% group_by(Scenario) %>% summarise(dt = min(Date))
end_date = format(max(dt_fcst$dt) - 1/12, "%Y-%m") 

df_hdb <- bind_rows(
  hdb_query(sdis["PowellInflow.Unregulated"], "uc", "m", start_date, end_date)
)

df_hdb <- df_hdb %>%
  mutate(Variable = names(sdis)[match(sdi, sdis)],
         Date = as.yearmon(parse_date_time(time_step, "m/d/y H:M:S")),
         WY = ifelse(month(Date) >= 10,
                     year(Date) + 1, year(Date)),
         value = value/1000)

# Add historical data to model projections
df_flow <- df_scens %>%
  filter(Variable == "PowellInflow.Unregulated") %>%
  select("Scenario", "Variable", "Date", "Trace", "Value", "WY" )
for (i in 1:length(scenarios)) {
  df_scensI = df_scens %>% filter(Scenario == scenarios[i]) 
  df_histAdd <- df_hdb %>% 
    filter(WY %in% min(df_scensI$WY) & 
             Date < min(df_scensI$Date)) %>%
    select(Variable, Date, WY, Value = value)
  
  df_add <- df_scensI %>% 
    filter(WY %in% min(df_scensI$WY)) %>% 
    select(Scenario, Variable, Trace, WY) %>% distinct()
  
  addHist = left_join(df_histAdd, df_add, by = c("WY", "Variable")) %>%
    select(colnames(df_flow))
  
  df_flow = rbind.data.frame(df_flow, addHist)
}

df_flow2 = df_flow %>%
  group_by(Scenario, Variable, Trace, WY) %>%
  summarise(ann = sum(Value), 
            n = n()) %>%
  filter(n == 12) %>%
  select(Year = WY, Trace, Scenario, ann)

## agg Mead outflow
df_cyOutflow = df_scens %>% 
  filter(Variable %in% "Mead.Outflow") %>%
  select(-Variable, -WY) %>%
  mutate(Year = year(Date)) %>%
  # filter(Year > min(year(Date), na.rm = T)) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(MeadOut = sum(Value)/1000, 
            n = n()) %>%
  filter(n == 12) %>% select(-n)

## agg storage deficit
store_3490 = CRSSIO::elevation_to_storage(3490, 'powell')/1000
df_st = df_scens %>% 
  filter(Variable %in% c("Powell.Storage")) %>%
  select(-Variable) %>%
  filter(Year > min(year(Date))) %>%
  group_by(WY, Trace, Scenario) %>%
  summarise(MinPPDeficit = store_3490 - min(Value)) %>%
  rename(Year = WY)

## agg storage deficit
store_950 = CRSSIO::elevation_to_storage(950, 'mead')/1000
df_st_mead = df_scens %>% 
  filter(Variable %in% c("Mead.Storage")) %>%
  select(-Variable, -WY) %>%
  mutate(Year = year(Date)) %>%
  filter(Year > min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(MinPPDeficitMead = store_950 - min(Value)) 

## end of WY PE
df_eowy = df_scens %>% 
  filter(Variable %in% c("Powell.Pool Elevation")) %>%
  select(-Variable, -Year) %>%
  filter(month(Date) == 9) %>%
  select(-Date) %>%
  rename(eowyPowellPE = Value) %>%
  rename(Year = WY)

df_eocy = df_scens %>% 
  filter(Variable %in% c("Mead.Pool Elevation", "Powell.Pool Elevation")) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(Year = year(Date)) %>%
  filter(month(Date) == 12) %>%
  select(-Date, -WY) %>%
  rename(eocyMeadPE = `Mead.Pool Elevation`,
         eocyPowellPE = `Powell.Pool Elevation`)

## combine data
df_agg = left_join(df_i, df_flow2, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_st, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_eowy, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_eocy, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_cyOutflow, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_st_mead, by = c('Scenario', 'Trace', 'Year'))


minYr = min(df_agg$Year)
for (i in 0:1) {
  plot_yr = minYr + i
  df_plot = df_agg %>%
    filter(Year == plot_yr)
  
  ## -- Powell Unreg Inflow vs. TARV
  breaks_x = seq(0, 40000, by = 1000)
  breaks_x2 = seq(0, 40000, by = 500)
  ggplot(df_plot, aes(ann, act_TARV, color = Scenario, shape = Scenario, 
                      alpha = 0.5, size = 1.5)) +
    # geom_point(data = df_plot_avg, aes(ann, act_TARV, color = Scenario), 
    #            shape = 4, alpha = 1, size = 4, stroke = 3) +
    geom_point() +
    scale_color_manual(values = custom_Tr_col) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma, 
                       breaks = breaks_x, minor_breaks = breaks_x2,
                       sec.axis = sec_axis(
                         trans = ~unregkaf_to_poa(.),
                         breaks = unregkaf_to_poa(breaks_x),
                         labels = scales::label_percent(),
                         name = '% of Avg. Unregulated Inflow'
                       )) +
    theme_bw() +
    labs(x = paste('WY', plot_yr, 'Powell Unregulated Inflow (kaf)'),
         y = paste('WY', plot_yr, 'Powell Release (kaf)'),
         title = paste('Powell Unreg. Inflow vs. Powell Relese for WY', plot_yr)) +
    guides(alpha = 'none', size = 'none')  +
    theme(axis.text.x.bottom  = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text.x.top  = element_text(angle = 45, vjust = 0, hjust = 0))
  ggsave(file.path(fig_dir, paste0("Scatter_PowellUnreg_PowellRel_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell Unreg Inflow vs. TARV -- In metric
  breaks_y = seq(1000, 11000, by = 500)
  breaks_y2 = seq(1000, 11000, by = 250)
  breaks_x = seq(0, 200000, by = 1000)
  breaks_x2 = seq(0, 200000, by = 500)
  ggplot(df_plot, aes(ann, act_TARV, color = Scenario, shape = Scenario, 
                      alpha = 0.5, size = 1.5)) +
    geom_point() +
    scale_color_manual(values = custom_Tr_col) +
    scale_y_continuous(
      labels = scales::comma, breaks = breaks_y, minor_breaks = breaks_y2,
      sec.axis = sec_axis(
        trans = ~kaf_to_mcm(.)/1000,
        breaks = kaf_to_mcm(breaks_y)/1000,
        labels = scales::comma,
        name = 'WY Annual Release Volume (bcm)'
      )) +
    scale_x_continuous(labels = scales::comma, 
                       breaks = breaks_x, minor_breaks = breaks_x2,
                       sec.axis = sec_axis(
                         trans = ~kaf_to_mcm(.)/1000,
                         breaks = kaf_to_mcm(breaks_x)/1000,
                         labels = scales::comma,
                         name = 'WY Annual Unregulated Inflow (bcm)'
                       )) +
    theme_bw() +
    labs(x = 'WY Annual Unregulated Inflow (kaf)',
         y = 'WY Annual Release Volume (kaf)',
         title = paste('Powell Unreg. Inflow vs. TARV for WY', plot_yr)) +
    guides(alpha = 'none', size = 'none') +
    theme(axis.text.x.bottom  = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text.x.top  = element_text(angle = 45, vjust = 0, hjust = 0))
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellUnreg_PowellRel_",plot_yr, "_MX.png")), 
         width = 8, height = 7)
  
  ## -- Powell TARV vs. EOWY PE
  ggplot(df_plot, aes(act_TARV, eowyPowellPE, color = Scenario, shape = Scenario, 
                      alpha = 0.5, size = 1.5)) +
    geom_hline(yintercept = 3490, color = "grey", linetype = 'dashed') +
    geom_point() +
    
    scale_color_manual(values = custom_Tr_col) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma, breaks = seq(3000, 3700, by = 10)) +
    theme_bw()+
    labs(y = 'EOWY Pool Elevation (ft)',
         x = 'WY Annual Release Volume (kaf)',
         title = paste('Powell Comparison', plot_yr))+
    guides(alpha = 'none', size = 'none') +
    theme(axis.text.x.bottom  = element_text(angle = 45, vjust = 1, hjust = 1))
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_eowyPowellPE_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell unreg inflow vs. min power pool deficit
  ggplot(df_plot, aes(ann, MinPPDeficit, color = Scenario, shape = Scenario, 
                      alpha = 0.5,
                      size = 1.5)) +
    geom_hline(yintercept = 0, color = 'gray') +
    geom_point() +
    
    scale_color_manual(values = custom_Tr_col) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma, 
                       breaks = breaks_x, minor_breaks = breaks_x2,
                       sec.axis = sec_axis(
                         trans = ~unregkaf_to_poa(.),
                         breaks = unregkaf_to_poa(breaks_x),
                         labels = scales::label_percent(),
                         name = '% of Avg. Unregulated Inflow'
                       )) +
    theme_bw() +
    labs(x = 'WY Annual Unregulated Inflow (kaf)',
         y = 'Minimum Power Pool Deficit (kaf)',
         title = paste('Powell Unreg. Inflow vs. Maximum Volume Below Min Power Pool in WY', plot_yr)) +
    guides(alpha = 'none', size = 'none')+
    theme(axis.text.x.bottom  = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text.x.top  = element_text(angle = 45, vjust = 0, hjust = 0))
  
  ggsave(file.path(fig_dir, paste0("Scatter_PowellUnreg_PowellMinPP_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell Release vs. Powell Elevation
  ggplot(df_plot, aes(act_TARV, eowyPowellPE, color = Scenario, shape = Scenario, 
                      alpha = 0.5,
                      size = 1.5)) +
    geom_point() +
    
    scale_color_manual(values = custom_Tr_col) +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(0,5000, by =10)) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 40000, by = 1000)) +
    theme_bw() +
    labs(y = paste0("End-of-WY ", plot_yr, ' Powell Pool Elevation (ft)'),
         x = paste0("WY ", plot_yr, ' Powell Release (kaf)'),
         title = paste('Powell Release vs. Powell Pool Elevation in', plot_yr)) +
    guides(alpha = 'none', size = 'none') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_PowellEOWYPE_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell Release vs. Mead Elevation
  ggplot(df_plot, aes(act_TARV, eocyMeadPE, color = Scenario, shape = Scenario, 
                      alpha = 0.5,
                      size = 1.5)) +
    geom_point() +
    
    scale_color_manual(values = custom_Tr_col) +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(0,2000, by =10)) +
    scale_x_continuous(labels = scales::comma, breaks = seq(0, 40000, by = 1000)) +
    theme_bw() +
    labs(y = paste0("End-of-CY ", plot_yr, ' Mead Pool Elevation (ft)'),
         x = paste0("WY ", plot_yr, ' Powell Release (kaf)'),
         title = paste('Powell Release vs. Mead Pool Elevation in', plot_yr)) +
    guides(alpha = 'none', size = 'none') + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  ggsave(file.path(fig_dir, paste0("Scatter_PowellRel_MeadEOCYPE_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell Unreg Inflow vs. Mead Elevation
  ggplot(df_plot, aes(ann, eocyMeadPE, color = Scenario, shape = Scenario, 
                      alpha = 0.5,
                      size = 1.5)) +
    geom_point() +
    
    scale_color_manual(values = custom_Tr_col) +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(0,2000, by =10)) +
    scale_x_continuous(labels = scales::comma, 
                       breaks = breaks_x, minor_breaks = breaks_x2,
                       sec.axis = sec_axis(
                         trans = ~unregkaf_to_poa(.),
                         breaks = unregkaf_to_poa(breaks_x),
                         labels = scales::label_percent(),
                         name = '% of Avg. Unregulated Inflow'
                       )) +
    theme_bw() +
    labs(y = paste0("End-of-CY ", plot_yr, ' Mead Pool Elevation (ft)'),
         x = paste0("WY ", plot_yr, ' Powell Unregulated Inflow (kaf)'),
         title = paste('Powell Release vs. Mead Pool Elevation in', plot_yr)) +
    guides(alpha = 'none', size = 'none') + 
    theme(axis.text.x.bottom  = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text.x.top  = element_text(angle = 45, vjust = 0, hjust = 0))
  ggsave(file.path(fig_dir, paste0("Scatter_PowellUnreg_MeadEOCYPE_",plot_yr, ".png")), 
         width = 8, height = 7)
  
  ## -- Powell PE vs. Mead Elevation
  pe_breaks = seq(0, 4000, by =10)
  pe_breaks_2 = seq(0, 4000, by = 5)
  ggplot(df_plot, aes(eocyPowellPE, eocyMeadPE, color = Scenario, shape = Scenario, 
                      alpha = 0.5,
                      size = 1.5)) +
    geom_point() +
  
    scale_color_manual(values = custom_Tr_col) +
    geom_vline(xintercept = 3490, color = 'gray', linetype = 'dashed') +
    annotate(
      "text", x = 3492, y = 1075, 
      label = "Powell\nMin. Power Pool", 
      hjust = 0, color = '#505050'
    ) +
    geom_hline(yintercept = 950, color = 'gray', linetype = 'dashed') +
    annotate(
      "text", x = 3570, y = 957, 
      label = "Mead\nMin. Power Pool", 
      hjust = 0, color = '#505050'
    ) +
    scale_y_continuous(labels = scales::comma, 
                       breaks = pe_breaks, minor_breaks = pe_breaks_2,
                       sec.axis = sec_axis(
                         trans = ~elevation_to_storage(., reservoir = 'mead'),
                         breaks = elevation_to_storage(pe_breaks, reservoir = 'mead'),
                         labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
                         name = paste0("End-of-CY ", plot_yr, ' Mead Storage (maf)')
                       )) +
    scale_x_continuous(labels = scales::comma, 
                       breaks = pe_breaks, minor_breaks = pe_breaks_2,
                       sec.axis = sec_axis(
                         trans = ~elevation_to_storage(., reservoir = 'powell'),
                         breaks = elevation_to_storage(pe_breaks, reservoir = 'powell'),
                         labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
                         name = paste0("End-of-CY ", plot_yr, ' Powell Storage (maf)')
                       )) +
    theme_bw() +
    labs(y = paste0("End-of-CY ", plot_yr, ' Mead Pool Elevation (ft)'),
         x = paste0("End-of-CY ", plot_yr, ' Powell Pool Elevation (ft)'),
         title = paste('Powell vs. Mead Pool Elevation in', plot_yr)) +
    guides(alpha = 'none', size = 'none') + 
    theme(axis.text.x.bottom  = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text.x.top  = element_text(angle = 45, vjust = 0, hjust = 0))
  ggsave(file.path(fig_dir, paste0("Scatter_Powell_Mead_EOCYPE_",plot_yr, ".png")), 
         width = 8, height = 7)
}
