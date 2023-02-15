# ============================================================================
# Compare CRMMS-ESP run - Single Trace Data
#   Powell Tiers / Powell TARV / LB Condition
#   
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "scenarios", "fig_dir_nm")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)
library(patchwork)
library(rhdb)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)

## Max Date
max_yr = 2027
end_file_nm = paste0('_', max_yr)

##### ------ Tiers | Powell Release ------ #####

# set up 
slots = c(
  'Shortage.Shortage Flag', 'PowellData.ReleaseTier',
  'PowellData.TargetAnnualReleaseVolume',
  'PowellData.ActualAnnualReleaseVolume', 'DCP BWSCP Flags.LB DCP BWSCP',
  "Powell.Storage", "Powell.Pool Elevation", "Mead.Storage", "Mead.Pool Elevation",
  "Powell.Outflow", "Mead.Outflow", "MeadData.OriginalOutflow",
  "Mohave.Evaporation", "Havasu.Evaporation",
  "PowellInflow.Unregulated", "CoRivMeadToMohave:GainsAboveDavis.Local Inflow", 
  "PowellToMead:LeesFerryGage.Local Inflow",
  "MWDAndCAPReach:GainsAboveParker.Local Inflow", "HavasuToImperial:GainsPkrToImp.Local Inflow",
  "BelowImperialDam:GainsImpToNIB.Local Inflow",
  "PumpingFromLakeMead.Total Diversion"
  
)
rdfs = c(rep('flags.rdf', length(slots)-16),
         rep('res.rdf', 9),
         rep('streamflow.rdf', 6),
         rep('diversion.rdf', 1))

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
  trces = unique(scen_res$TraceNumber)
  trces = trces[trces >= 0]
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df, scen_res)
}

df<- df %>% na.omit()

df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y"))  %>%
  select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Trace = 1991 + Trace - min(df$TraceNumber, na.rm = T))

## Colors and Labels
DCPlab = c('Mead > 1,090 ft'= '#a6cee3', 
           'Mead 1,075-1,090 ft [0]' = '#1f78b4',
           'Mead 1,050-1,075 ft [1]' = '#b2df8a',
           'Mead 1,045-1,050 ft [2a]' = '#fb9a99',
           'Mead 1,040-1,045 ft [2b]' = '#e31a1c',
           'Mead 1,035-1,040 ft [2c]' = '#fdbf6f',
           'Mead 1,030-1,035 ft [2d]' = '#ff7f00',
           'Mead 1,025-1,030 ft[2e]' = '#ffff99',
           'Mead 895-1,025 ft [3]' = '#cab2d6')
PowellTierLab = c('Equalization Tier' = '#a6cee3', 
                  'Upper Elevation Balancing Tier' = '#1f78b4',
                  'Mid Elevation Release Tier' = '#b2df8a',
                  'Lower Elevation Balancing Tier' = '#33a02c')
ShortLabs = c('Normal Condition' = '#a6cee3', 
              'Shortage - Level 1' = '#1f78b4',
              'Shortage - Level 2' = '#b2df8a',
              'Shortage - Level 3' = '#cab2d6')
decSlots = c('PowellData.ReleaseTier',
             'PowellData.TargetAnnualReleaseVolume',
             'PowellData.ActualAnnualReleaseVolume',
             "DCP BWSCP Flags.LB DCP BWSCP",
             'Shortage.Shortage Flag')

## process df
df_i = df_scens %>% 
  filter(Variable %in% decSlots) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(year(Date) <= max_yr) %>%
  mutate(Year = year(Date),
         `Powell Release` = PowellData.ActualAnnualReleaseVolume/10^3,
         TARV = PowellData.TargetAnnualReleaseVolume/10^3,
         `Powell Tiers` = factor(PowellData.ReleaseTier, 
                                 levels = c(0,1,2,3),
                                 labels = names(PowellTierLab)),
         `Lower Basin Shortage`= factor(`Shortage.Shortage Flag`, 
                                        levels = c(0,1,2,3),
                                        labels = names(ShortLabs)),
         `DCP Contribution` = factor(`DCP BWSCP Flags.LB DCP BWSCP`, 
                                     levels = 8:0, 
                                     labels = names(DCPlab))) %>%
  select(-all_of(decSlots), -Date) 


## agg streamflow
df_flow = df_scens %>% 
  filter(Variable %in% c("PowellInflow.Unregulated")) %>%
  select(-Variable) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year > min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(`Powell UnregInflow` = sum(Value)) %>%
  mutate(`Percent of Avg.` = `Powell UnregInflow`/9603.392)

## Mead Outflow
df_flowMead = df_scens %>%
  filter(Variable %in% c("Mead.Outflow")) %>%
  select(-Variable) %>%
  mutate(Year = year(Date)) %>%
  filter(Year > min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(`Mead Outflow` = sum(Value)/1000)

## agg Mead outflow
# df_cyOutflow1 = df_scens %>% 
#   filter(Variable %in% "Mead.Outflow") %>%
#   select(-Variable) %>%
#   rename(MeadOut = Value)
# df_cyOutflow2 = df_scens %>% 
#   filter(Variable %in% "MeadData.OriginalOutflow") %>%
#   select(-Variable) %>%
#   rename(MeadOrigOut = Value)
# df_flowMead = left_join(df_cyOutflow1, df_cyOutflow2, 
#                          by = c("Scenario", "Trace", "Date")) %>%
#   mutate(MeadOrigOut = ifelse(is.na(MeadOrigOut) | MeadOrigOut == 0, 
#                               MeadOut, MeadOrigOut),
#          Year = year(Date)) %>%
#   filter(Year > min(year(Date), na.rm = T)) %>%
#   group_by(Year, Trace, Scenario) %>%
#   summarise(`Mead Orig CY Outflow` = sum(MeadOrigOut)/1000, 
#             `Mead CY Outflow` = sum(MeadOut)/1000) %>%
#   mutate(`Change in Mead CY Outflow` = `Mead Orig CY Outflow` - `Mead CY Outflow`)

# Estimate LB use = MeadOUut - (gains + evap) + DivFromMead
df_lb_use = df_scens %>%
  filter(Variable %in% 
           c("Mead.Outflow", "Mohave.Evaporation", "Havasu.Evaporation",
             "CoRivMeadToMohave:GainsAboveDavis.Local Inflow", "MWDAndCAPReach:GainsAboveParker.Local Inflow", 
             "HavasuToImperial:GainsPkrToImp.Local Inflow", "BelowImperialDam:GainsImpToNIB.Local Inflow",
             "PumpingFromLakeMead.Total Diversion")) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(`LB Use Est.` = Mead.Outflow/1000 - 
           (Mohave.Evaporation  + Havasu.Evaporation + 
              `CoRivMeadToMohave:GainsAboveDavis.Local Inflow` + `MWDAndCAPReach:GainsAboveParker.Local Inflow` + 
              `HavasuToImperial:GainsPkrToImp.Local Inflow` + `BelowImperialDam:GainsImpToNIB.Local Inflow`) +
           `PumpingFromLakeMead.Total Diversion`/1000
         - (25.039 + 116.633)/12) %>% # projected MX Bypass + Excess flows
  mutate(Year = year(Date)) %>%
  filter(Year > min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(`LB Use Est.` = sum(`LB Use Est.`)) %>%
  select(Scenario, Year, Trace, `LB Use Est.`)

## agg storage deficit
store_3490 = CRSSIO::elevation_to_storage(3490, 'powell')/1000
df_st = df_scens %>% 
  filter(Variable %in% c("Powell.Storage")) %>%
  select(-Variable) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year > min(year(Date))) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(`MinPP Deficit` = store_3490 - min(Value)) 

## end of WY PE
df_eowy = df_scens %>% 
  filter(Variable %in% c("Powell.Storage", "Powell.Pool Elevation", 
                         "Mead.Storage", "Mead.Pool Elevation")) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date))) %>%
  filter(Year > min(year(Date)) & month(Date) == 9) %>%
  select(-Date) %>%
  pivot_wider(names_from = Variable, names_prefix = 'EOWY_', values_from = Value) %>%
  rename(`EOWY Powell Storage` = "EOWY_Powell.Storage", `EOWY Powell Elevation` = "EOWY_Powell.Pool Elevation", 
         `EOWY Mead Storage` = "EOWY_Mead.Storage",  `EOWY Mead Elevation` = "EOWY_Mead.Pool Elevation")

## end of CY PE
df_eocy = df_scens %>% 
  filter(Variable %in% c("Powell.Storage", "Powell.Pool Elevation", 
                         "Mead.Storage", "Mead.Pool Elevation")) %>%
  mutate(Year = year(Date)) %>%
  filter(month(Date) == 12) %>%
  select(-Date) %>%
  pivot_wider(names_from = Variable, names_prefix = 'EOCY_', values_from = Value)%>%
  rename(`EOCY Powell Storage` = "EOCY_Powell.Storage", `EOCY Powell Elevation` = "EOCY_Powell.Pool Elevation", 
         `EOCY Mead Storage` = "EOCY_Mead.Storage",  `EOCY Mead Elevation` = "EOCY_Mead.Pool Elevation")

## Compact Pt
sdis <- c("Paria" = 1579, "LeesFerry" = 1578)
start_date = format(ym("2000-10"), "%Y-%m")
end_date = format(max(df_scens$Date) - 1/12, "%Y-%m") # assumes all same start month
df_hdb <- hdb_query(sdis, "uc", "m", start_date, end_date) %>%
  mutate(Variable = names(sdis)[match(sdi, sdis)],
         Date = as.yearmon(parse_date_time(time_step, "m/d/y H:M:S")),
         Year = ifelse(month(Date)>=10, year(Date) + 1,
                       year(Date)),
         value = value/1000)


df_avgPar = df_hdb %>% mutate(mon = month(Date)) %>%
  filter(Variable == "Paria" & Year %in% 2005:2021) %>%
  group_by(mon) %>%
  summarise(PariamonAvg = mean(value))

df_compt = df_scens %>%  mutate(mon = month(Date)) %>%
  filter(Variable %in% c("PowellToMead:LeesFerryGage.Local Inflow", "Powell.Outflow")) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  left_join(df_avgPar, by = "mon") %>%
  mutate(Compt = `PowellToMead:LeesFerryGage.Local Inflow` + Powell.Outflow + PariamonAvg)  %>%
  select(Scenario, Date, Trace, Compt) 

df_histPt = df_hdb %>% select(Date, Year, Variable, value) %>%
  pivot_wider(names_from = Variable, values_from = value) %>%
  mutate(Compt = Paria + LeesFerry) %>%
  select(Year, Date, Compt) %>%
  na.omit()

df_histPt = data.frame(Trace = rep(sort(unique(df_compt$Trace)), each = length(unique(df_hdb$Year))),
                       Year = rep(sort(unique(df_hdb$Year)), times = length(unique(df_compt$Trace)))) %>% 
  right_join(df_histPt, by = "Year")

# Add historical data to model projections
df_new <- NULL
for (i in 1:length(scenarios)) {
  df_scensI = df_compt %>% 
    filter(Scenario == scenarios[i]) %>% #na.omit() %>%
    mutate(Year = ifelse(month(Date)>=10, year(Date) + 1,
                         year(Date)))
  df_histAdd <- df_histPt %>% 
    filter(Date < min(df_scensI$Date)) %>%
    select(Trace, Date, Year, Compt)
  
  df_add <- df_scensI %>% 
    filter(Year < max(df_scensI$Year)) %>%
    select(Trace, Date, Year, Compt)
  
  df_comb = rbind.data.frame(df_histAdd, df_add) 
  df_new = rbind.data.frame(df_new, df_comb %>% mutate(Scenario = scenarios[i]))
}

df_comptNew = df_new %>% group_by(Scenario, Trace, Year) %>%
  summarise(tot = sum(Compt),
            n = n())

# check that there are 12 months for each year
if (any(df_comptNew$n != 12)) {
  stop('Some scenarios/WYs do not have 12 entries')
  df_comptCheck = df_comptNew %>% filter(n != 12)
}

df_CP = df_comptNew %>%
  mutate(`Compact Point WY` = rollsum(tot, k=10, fill=NA, align='right')) %>%
  na.omit() %>%
  select(Scenario, Trace, Year, `Compact Point WY`) %>%
  filter(Year >= max(df_hdb$Year))

# ggplot(df_CP, aes(x = factor(Year), y = `Compact Point WY`, fill = Scenario)) +
#   geom_boxplot()

## combine data
df_agg = left_join(df_i, df_flow, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_st, by = c('Scenario', 'Trace', 'Year')) %>% 
  left_join(df_flowMead, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_eowy, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_eocy, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_lb_use, by = c('Scenario', 'Trace', 'Year')) %>%
  left_join(df_CP, by = c('Scenario', 'Trace', 'Year'))

write.csv(df_agg, file.path(fig_dir, "TraceData.csv"))

# test = df_agg %>%
#   filter(Year %in% 2023:2024) %>%
#   group_by(Scenario, Trace) %>%
#   summarise(sumRel = sum(act_TARV)) %>% ungroup() %>%
#   filter(sumRel < 14000)
# group_by(Scenario) %>% 
#   summarise(minRel = min(sumRel))
# filter(Trace == 2011)

min(test$`EOWY_Mead.Pool Elevation`)

## p+M storage
test2 = df_agg  %>% 
  mutate(eowy_comb = EOWY_Powell.Storage + EOWY_Mead.Storage,
         eocy_comb = EOCY_Powell.Storage + EOCY_Mead.Storage)
test2 %>% filter(Year == 2024) %>% 
  filter(eowy_comb < 6500)
