# ============================================================================
# Compare CRMMS-ESP run 
#   LB uses / ICS bank / MSCP 
#   ***Currently not set up to read in historical data, therefore year 1 will 
#     be wrong when comparing different months
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

## Max Date
max_yr = 2028
end_file_nm = paste0('_', max_yr)

##### ------ ICS LB use ------ #####
slots = c(
  "AnnualWaterUse.AZ_BasicApportionment", "AnnualWaterUse.CA_BasicApportionment",
  "AnnualWaterUse.AzTotalAnnual", "AnnualWaterUse.CaTotalAnnual",
  "AnnualWaterUse.NvTotalAnnual",
  "Mexico Shortage and Surplus.MexicoAdjustedSched", "Mexico_CU_Forecast1.MexicoTJ",
  
  'ICS Credits.Bank_CA', 'ICS Credits.Bank_AZ', 'ICS Credits.Bank_NV',
  'ICS Credits.AnnualCreationEC_AZ', 'ICS Credits.AnnualDeliveryEC_AZ', 
  'ICS Credits.AnnualCreationSysEff_AZ', 'ICS Credits.AnnualDeliverySysEff_AZ',
  'ICS Credits.AnnualCreationDCP_AZ', 'ICS Credits.AnnualDeliveryBiNat_CAWCD',
  'ICS Credits.AnnualCreationEC_CA', 'ICS Credits.AnnualDeliveryEC_CA',
  'ICS Credits.AnnualCreationSysEff_CA', 'ICS Credits.AnnualDeliverySysEff_CA',
  'ICS Credits.AnnualCreationEC_NV', 'ICS Credits.AnnualDeliveryEC_NV', 
  'ICS Credits.AnnualCreationSysEff_NV', 'ICS Credits.AnnualDeliverySysEff_NV',
  'ICS Credits.AnnualCreationImp_NV', 'ICS Credits.AnnualDeliveryImp_NV',
  'ICS Credits.AnnualCreationTrib_NV',  'ICS Credits.AnnualDeliveryCurrentYearTribConserv_NV',
  'ICS Credits.AnnualDeliveryBiNat_CA', 'ICS Credits.AnnualDeliveryBiNat_NV',
  "ICS Credits.VacatedBankSpace_AZ", "ICS Credits.VacatedBankSpace_CA",
  "ICS Credits.VacatedBankSpace_NV", "ICS Credits.IntendedECICSCreationToBeDelivered_MWD",
  

  ## new slots
  "AnnualWaterUse.ComputedDepletion_Ann_LBMX", "AnnualWaterUse.ComputedDepletion_Ann_LB",
  "AnnualWaterUse.ComputedDepletion_Ann_MX", "AnnualWaterUse.ComputedDepletion_Ann_LBMX_wBypassExcess",
  "Mead.Pool Elevation"
)
rdfs = c(rep('LB_UseICS.rdf', length(slots) - 5),
         rep("flags.rdf", 4), 'res.rdf')

# custom_colors <- c("#e94849", "#5f98c6", "#71bf6e", "#fdb462", "#B79F00", "#00BFC4")

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
    find_all_slots = F
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = sort(unique(scen_res$TraceNumber))
  trces = trces[trces >= 0]
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df,
              scen_res %>%
                mutate(TraceNumber = 1991 + TraceNumber - 
                         min(scen_res$TraceNumber, na.rm = T)))
}

# df_scens <- data.table::as.data.table(df)  %>% 
#   mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
#   dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
#   mutate(Scenario = factor(Scenario, levels = scenarios))

## -- process and combine hydrologies
df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         # Trace = 1991 + Trace - min(df2$TraceNumber, na.rm = T),
         Year = year(Date)) %>%
  filter(Year <= max_yr) 



## LB ICS bank 
df_i = df_scens %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(year(Date) <= max_yr) %>%
  mutate(Year = factor(year(Date)),
         MSCPflowRed = (AnnualWaterUse.AZ_BasicApportionment + 
                          AnnualWaterUse.CA_BasicApportionment -
                          (AnnualWaterUse.AzTotalAnnual + AnnualWaterUse.CaTotalAnnual))/10^3,
         totalICSBank = (`ICS Credits.Bank_CA` + `ICS Credits.Bank_AZ` + 
                           `ICS Credits.Bank_NV`)/10^3) %>%
  select(-any_of(slots), -Date) 

i_breaks = seq(0,3000, by = 100)
g <- ggplot(df_i, aes(Year, totalICSBank, fill = Scenario)) +
  bor_theme() +
  geom_hline(yintercept = 2700, color = 'grey', linetype = 'dashed') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
  labs(x = 'End of Year', y = 'Total ICS Bank Balance (kaf)') 
print(g)
ggsave(file.path(fig_dir, paste0("LB_ICSBank", end_file_nm , ".png")), 
               width = 8, height = 7)

## Bank by state
df_i2 = df_scens %>% 
  filter(Variable %in% c('ICS Credits.Bank_CA', 'ICS Credits.Bank_AZ', 
                         'ICS Credits.Bank_NV')) %>%
  # pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(Year = factor(year(Date)),
         State = case_when(
           endsWith(Variable, "AZ") | endsWith(Variable, "AZ") ~ "Arizona",
           endsWith(Variable, "CA") ~ "California",
           endsWith(Variable, "NV") ~ "Nevada"),
         Value = Value / 10^3) %>%
  select(-Variable, -Date)
df_statCap = data.frame(State = c("Arizona", "California", "Nevada"),
                        Value = c(600, 1650, 450))

df_inew = df_i %>% 
  mutate(State = 'Total') %>%
  select(Scenario, Trace, Value = totalICSBank, Year, State) 


i_breaks = seq(0,3000, by = 25)
g1 = ggplot(df_i2, aes(Year, Value, fill = Scenario, shape = Scenario)) +
  bor_theme() +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma) +#, breaks = i_breaks) +
  geom_hline(data = df_statCap, aes(yintercept = Value), linetype = 'dashed') +
  labs(x = 'End of Year', y = 'ICS Bank Balance (kaf)') +
  facet_grid(State ~ ., scales = 'free_y')

i_breaks = seq(0,3000, by = 150)
g2 = ggplot(df_inew, aes(Year, Value, fill = Scenario, shape = Scenario)) +
  bor_theme() +
  geom_hline(yintercept = 2700, color = 'grey', linetype = 'dashed') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
  labs(x = 'End of Year', y = 'ICS Bank Balance (kaf)') +
  facet_grid(State ~ ., scales = 'free_y')

g3 <- g2 / g1 + plot_layout(guides = "collect", heights = c(1, 3))
print(g3)
ggsave(file.path(fig_dir, paste0("LB_ICSBank_byState", end_file_nm , ".png")), 
       width = 7, height = 9)

## MSCP compliance
# i_breaks = seq(-2000,5000, by = 200)
# g <- ggplot(df_i, aes(Year, MSCPflowRed, fill = Scenario)) +
#   bor_theme() +
#   geom_hline(yintercept = 845, color = 'grey', linetype = 'dashed') +
#   geom_hline(yintercept = 0, color = 'grey') +
#   CRSSIO::stat_boxplot_custom(position = "dodge") +
#   scale_fill_manual(values = custom_Tr_col) +
#   scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
#   labs(x = 'End of Year', y = 'MSCP Flow Reductions (kaf)') 
# print(g)
# ggsave(file.path(fig_dir, paste0("LB_MSCP", end_file_nm , ".png")), 
#        width = 7, height = 6)

## State Use - estimate from model - no hydrologic shortage
slots = c("AnnualWaterUse.AzTotalAnnual", "AnnualWaterUse.CaTotalAnnual",
          "AnnualWaterUse.NvTotalAnnual", "Mexico Shortage and Surplus.MexicoAdjustedSched", 
          "Mexico_CU_Forecast1.MexicoTJ")
slot_nms = c('Arizona', 'California', 'Nevada', "Mexico-TJ", "TJ")
df_i = df_scens %>% 
  filter(year(Date) <= max_yr) %>%
  filter(Variable %in% slots) %>%
  mutate(Variable = factor(Variable, levels = slots, labels = slot_nms),
         Value = Value/10^3, 
         Year = factor(year(Date)))
slots_all <- c('Arizona', 'California', 'Nevada', 'Mexico', "Total LB States", "Total LB States + MX")
df_i2 = df_i %>% 
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(Mexico = `Mexico-TJ` + TJ,
         'Total LB States' = Arizona + California + Nevada,
         'Total LB States + MX' = Arizona + California + Nevada + Mexico) %>% 
  pivot_longer(cols = all_of(slots_all),
               names_to = 'Variable', values_to = 'Value') %>%
  select(all_of(colnames(df_i))) %>%
  mutate(Variable = factor(Variable, levels = slots_all, labels = slots_all))
# df_i = rbind.data.frame(df_i, df_i2)

# Edit scenario that didn't update states apportionment
#   ie, scenarios that protect Mead elevations absolutely
# scen_issue = c("Protect 3490',950'")
# scen_good = which(!(scenarios_2 %in% scen_issue))
# df_i2 = df_i2 %>%
#   filter(Scenario %in% scenarios_2[scen_good])
# custom_col_i <- custom_Tr_col[scen_good]

## LB as per CRMMS accounting
var_plot = list(c('Arizona', 'California', 'Nevada', "Mexico", 'Total LB States + MX'),
                c('Arizona', 'California', 'Nevada', 'Total LB States'))
plt_nm = c("LB_Use_byState_MX", "LB_Use_byState")
df_useConst = data.frame(Variable = c('Arizona', 'California', 'Nevada', "Mexico", 
                                      'Total LB States + MX', 'Total LB States'),
                         Value = c(2800, 4400,300,1500,9000,7500))

for (i in 1:length(plt_nm)) {
  g1 <- ggplot(df_i2 %>%
                 filter(Variable %in% var_plot[[i]] & Year %in% 2024:2026), 
               aes(Year, Value, fill = Scenario)) +
    bor_theme()+
    CRSSIO::stat_boxplot_custom(position = "dodge") +
    geom_point(position = position_dodge(0.75), size = 0.75) +
    scale_fill_manual(values = custom_Tr_col) +
    scale_y_continuous(labels = scales::comma) +
    geom_hline(data = df_useConst %>%
                 filter(Variable %in% var_plot[[i]]), 
               aes(yintercept = Value), linetype = 'dashed') +
    labs(x = NULL, y = 'Annual Use (kaf)', title = "Use when not hydrologically shorted") +
    facet_grid(Variable ~ ., scales = 'free_y')
  print(g1)
  ggsave(file.path(fig_dir, paste0(plt_nm[i], end_file_nm , "_est.png")), 
         width = 8, height = 6+length(var_plot[[i]])*0.5)
}

# # New use calculation - not available for No Action!
# UseNew_slots = c("AnnualWaterUse.ComputedDepletion_Ann_LB", "AnnualWaterUse.ComputedDepletion_Ann_MX", 
#                  "AnnualWaterUse.ComputedDepletion_Ann_LBMX", "AnnualWaterUse.ComputedDepletion_Ann_LBMX_wBypassExcess")
# names(UseNew_slots) <- c("Total LB States", "Mexico", 'Total LB States + MX', "LB+MX with BypassExcess")
# df_useNew = df_scens %>% 
#   filter(Variable %in% UseNew_slots) %>%
#   na.omit() %>% filter(year(Date) <= max_yr) %>%
#   mutate(Year = factor(year(Date)),
#          Variable = factor(Variable, levels = UseNew_slots, labels = names(UseNew_slots)),
#          Value = Value / 10^3)
# 
# g1 <- ggplot(df_useNew, aes(Year, Value, fill = Scenario)) +
#   bor_theme()+
#   CRSSIO::stat_boxplot_custom(position = "dodge") +
#   scale_fill_manual(values = custom_Tr_col) +
#   geom_point(position = position_dodge(0.75), size = 0.75) +
#   
#   scale_y_continuous(labels = scales::comma) +
#   labs(x = NULL, y = 'Annual Use (kaf)') +
#   facet_grid(Variable ~ ., scales = 'free_y')
# print(g1)
# ggsave(file.path(fig_dir, paste0("LB_MX_", end_file_nm , "_CUActual.png")), 
#        width = 8, height = 6+4*0.5)

# ## LB Reductions
# lb_minus_nv = 4400+2800
# nv_incr = c(245, 250, 255, 260, 265)
# df_norm = cbind.data.frame(Year = 2022:2026, NormUse = c(nv_incr + lb_minus_nv))
# df_red = df_useNew %>% 
#   filter(Variable == "Total LB States") %>%
#   mutate(Year = as.numeric(as.character(Year))) %>%
#   left_join(df_norm) %>%
#   mutate(`LB Reductions` = NormUse - Value) %>%
#   mutate(Year = factor(Year))
# g1 <- ggplot(df_red, aes(Year, `LB Reductions`, fill = Scenario)) +
#   bor_theme()+
#   CRSSIO::stat_boxplot_custom(position = "dodge") +
#   scale_fill_manual(values = custom_Tr_col) +
#   geom_point(position = position_dodge(0.75), size = 0.75) +
#   # scale_fill_manual(values = custom_col_i) +
#   scale_y_continuous(labels = scales::comma, 
#                      limits = c(0, max(df_red$`LB Reductions`)+200), expand = c(0,0)) +
#   labs(x = NULL, y = 'LB Reductions (kaf)') 
# print(g1)
# ggsave(file.path(fig_dir, paste0("LB_Reductions_", end_file_nm , "_Actual.png")), 
#        width = 8, height = 6)

# ## Mead EOCY range
# df_pe = df_scens %>% 
#   filter(Variable %in% "Mead.Pool Elevation") %>%
#   na.omit() %>% filter(year(Date) <= max_yr) %>%
#   mutate(Year = factor(year(Date))) %>%
#   filter(month(Date) == 12)
# hlines = c(895, 950, 975, 1000, 1025, 1030, 1035, 1040, 1045, 1050, 1075)
# 
# ggplot(df_pe, aes(Year, Value, fill = Scenario)) +
#   bor_theme()+
#   geom_hline(yintercept = hlines, color = 'grey', linetype = 'dashed') +
#   CRSSIO::stat_boxplot_custom(position = "dodge") +
#   scale_fill_manual(values = custom_Tr_col) +
#   geom_point(position = position_dodge(0.75), size = 0.75) +
#   # scale_fill_manual(values = custom_col_i) +
#   scale_y_continuous(labels = scales::comma, breaks = seq(0,2000, by = 20)) +
#   labs(x = NULL, y = 'Mead EOCY Pool Elevation (kaf)') 
# ggsave(file.path(fig_dir, paste0("LB_MeadEOCYPE_", end_file_nm , "_Actual.png")), 
#        width = 8, height = 6)


## ICS
df_ICS_init = df_scens %>% 
  filter(year(Date) <= max_yr) %>%
  mutate(
    State = case_when(
      endsWith(Variable, "AZ") | endsWith(Variable, "AZ") ~ "Arizona",
      endsWith(Variable, "CA") ~ "California",
      endsWith(Variable, "NV") ~ "Nevada"),
    ICS = case_when(
      grepl("Creation", Variable) ~ "Creation",
      grepl("Delivery", Variable) ~ "Delivery"),
    type = case_when(
      grepl("EC_", Variable) ~ "EC",
      grepl("SysEff_", Variable) ~ "SysEff",
      grepl("Imp_", Variable) ~ "Imp",
      grepl("Trib", Variable) ~ "Trib",
      grepl("DCP_", Variable) ~ "DCP",
      grepl("BiNat_", Variable) ~ "BiNat",
      grepl("VacatedBankSpace_", Variable) ~ "Vacated"),
    Year = factor(year(Date))) %>%
  mutate(ICS = ifelse(type == 'Vacated', 'Delivery', ICS)) %>%
  na.omit() %>%
  select(-Variable)
df_cafix = df_scens %>% filter(Variable == "ICS Credits.IntendedECICSCreationToBeDelivered_MWD") %>%
  filter(year(Date) <= max_yr) %>%
  mutate(type = "Vacated",
         ICS = "Delivery",
         State = "California") %>%
  select(-Variable) %>%
  mutate(Year = factor(year(Date))) %>%
  rename(ValueMinus = Value)
df_ICS_init = left_join(df_ICS_init, df_cafix, 
                        by = c("Scenario", "Date", "Trace", "Year", "State", "ICS", "type")) %>%
  mutate(Value = ifelse(is.na(ValueMinus), Value, Value - ValueMinus)) %>%
  select(-ValueMinus)

df_ICS_1 = df_ICS_init %>%
  filter(type != "Vacated")

## !! fix NV Creation and delivery of EC ICS that would be happening in the same year
df_ICS_NVECfix = df_ICS_1 %>% filter(State == 'Nevada' & type == "EC") %>%
  pivot_wider(names_from = ICS, values_from = Value) %>%
  mutate(EC_dif = Creation - Delivery,
         Creation = ifelse(EC_dif > 0, EC_dif, 0),
         Delivery = ifelse(EC_dif < 0, -EC_dif, 0)) %>%
  select(-EC_dif) %>%
  pivot_longer(cols = c(Creation, Delivery), names_to = "ICS", 
               values_to = "Value") %>%
  select(colnames(df_ICS_1))
df_ICS_1 = rbind(df_ICS_1 %>% filter(!(State == 'Nevada' & type == "EC")),
                 df_ICS_NVECfix)


df_ICS = df_ICS_1 %>%
  pivot_wider(names_from = type, values_from = Value) %>%
  replace(is.na(.), 0) %>%
  mutate(total = (EC + SysEff + Imp + Trib + DCP + BiNat)/10^3) %>%
  select(-c(EC, SysEff, Imp, Trib, DCP, BiNat)) %>%
  pivot_wider(names_from = ICS, values_from = total) %>%
  mutate(ICS = Creation - Delivery)



g1 <- ggplot(df_ICS, aes(Year, ICS, fill = Scenario))+
  bor_theme()+
  geom_hline(yintercept = 0, color = 'gray') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
  theme(legend.position="top") +
  facet_grid(State ~ ., scales = 'free_y')
print(g1)
ggsave(file.path(fig_dir, paste0("LB_ICS_putVtake", end_file_nm , ".png")), 
       width = 7, height = 6)

df_ICS_plot = df_ICS_1 %>%
  pivot_wider(names_from = ICS, values_from = Value) %>%
  replace(is.na(.), 0) %>%
  mutate(Value = (Creation - Delivery)/1000)

library(colorspace)
drk_cols = darken(custom_Tr_col, 0.3)
lgt_cols = lighten(custom_Tr_col, 0.3)

g2 <- ggplot(df_ICS_plot, aes(factor(type), Value, fill = Scenario, shape = Scenario, color = Scenario))+
  bor_theme()+
  geom_hline(yintercept = 0, color = 'gray') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = lgt_cols) +
  scale_color_manual(values = drk_cols) +
  # geom_point(size = 2, position=position_dodge(0.8)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_grid(State ~ Year, scales = 'free_y')
print(g2)
ggsave(file.path(fig_dir, paste0("LB_ICS_type_byState", end_file_nm , ".png")), 
       width = 9, height = 6)

## ICS summary with vacated bank space - not in model output rn!
# df_ICS_plot2 = df_ICS_init %>%
#   pivot_wider(names_from = ICS, values_from = Value) %>%
#   mutate(Creation = if_else(is.na(Creation), 0, Creation),
#          Delivery = if_else(is.na(Delivery), 0, Delivery)) %>%
#   mutate(Value = (Creation - Delivery)/1000)

# g2 <- ggplot(df_ICS_plot2, aes(factor(type), Value, fill = Scenario, shape = Scenario, color = Scenario))+
#   bor_theme()+
#   geom_hline(yintercept = 0, color = 'gray') +
#   CRSSIO::stat_boxplot_custom(position = "dodge") +
#   scale_fill_manual(values = lgt_cols) +
#   scale_color_manual(values = drk_cols) +
#   # geom_point(size = 2, position=position_dodge(0.8)) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
#   theme(legend.position="top",
#         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
#   facet_grid(State ~ Year, scales = 'free_y')
# print(g2)
# ggsave(file.path(fig_dir, paste0("LB_ICS_type_byState", end_file_nm , "_vacated.png")), 
#        width = 9, height = 6)

## === Read vacated space to system water slots - 
#   cant read in BiNatICS but those wont be vacated to system water in these runs!
slots = c("ICS Credits.VacatedDCPICSToSysWater_CAWCD", "ICS Credits.VacatedBiNationalICSToSysWater_CAWCD",
          "ICS Credits.VacatedDCPICSToSysWater_MWD", "ICS Credits.VacatedECICSToSysWater_MWD",
          "ICS Credits.VacatedBiNationalICSToSysWater_MWD", "ICS Credits.VacatedDCPICSToSysWater_SNWA",
          "ICS Credits.VacatedECICSToSysWater_SNWA", "ICS Credits.VacatedImpICSToSysWater_SNWA",
          "ICS Credits.VacatedTribICSToSysWater_SNWA", "ICS Credits.VacatedBiNationalICSToSysWater_SNWA",
          "ICS Credits.VacatedCurrentYearTribICSToSysWater_SNWA")
rdfs = rep('LB_UseICS.rdf', length(slots))

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
df2 <- NULL
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i],
    find_all_slots = F
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = sort(unique(scen_res$TraceNumber))
  trces = trces[trces >= 0]
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df2 <- rbind(df2,
              scen_res %>%
                mutate(TraceNumber = 1991 + TraceNumber - 
                         min(scen_res$TraceNumber, na.rm = T)))
}

df2 = df2 %>% filter(!(TraceNumber == -99)) 
df_scens2 <- data.table::as.data.table(df2)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Trace = 1991 + Trace - min(df2$TraceNumber, na.rm = T),
         Year = year(Date)) %>%
  filter(Year <= max_yr) 

df_vac = df_scens2 %>% filter(year(Date) <= max_yr) %>%
  mutate(
    State = case_when(
      endsWith(Variable, "CAWCD") ~ "Arizona",
      endsWith(Variable, "MWD") ~ "California",
      endsWith(Variable, "SNWA") ~ "Nevada"),
    ICS = "System Water",
    type = case_when(
      grepl("EC", Variable) ~ "EC",
      grepl("SysEff", Variable) ~ "SysEff",
      grepl("Imp", Variable) ~ "Imp",
      grepl("Trib", Variable) ~ "Trib",
      grepl("DCP", Variable) ~ "DCP",
      grepl("BiNat", Variable) ~ "BiNat"),
    Year = factor(year(Date))) %>%
  # mutate(ICS = ifelse(type == 'Vacated', 'Delivery', ICS)) %>%
  na.omit() %>%
  select(-Variable)

df_vac2 = df_vac %>%
  group_by(Scenario, Year, Trace, State, ICS, type) %>%
  summarise(Value = sum(Value))

g2 <- ggplot(df_vac2, aes(factor(type), Value, fill = Scenario, shape = Scenario, color = Scenario))+
  bor_theme()+
  geom_hline(yintercept = 0, color = 'gray') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = lgt_cols) +
  scale_color_manual(values = drk_cols) +
  # geom_point(size = 2, position=position_dodge(0.8)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual ICS Vacated to System Water\n(kaf)') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_grid(State ~ Year, scales = 'free_y')
print(g2)
ggsave(file.path(fig_dir, paste0("LB_ICS_VacatedSysWater_byState", end_file_nm , ".png")), 
       width = 9, height = 6) 


# replace(is.na(.), 0) %>%
## summary years
yrs_tot = 2024:2026
df_vac2 = df_vac %>%
  filter(Year %in% yrs_tot) %>%
  group_by(Scenario, Trace, State) %>%
  summarise(SystemWater = sum(Value)/1000)

## three year aggregated ICS Creation/Delivery
df_ICStest = df_ICS %>%
  filter(Year %in% yrs_tot) %>%
  group_by(Trace, Scenario, State) %>%
  summarise(Creation = sum(Creation),
            Delivery = sum(Delivery))# %>%
# mutate(ICS_net = Creation - Delivery) %>%
# pivot_longer(cols = c(ICS_net, Creation, Delivery))

df_ICSVac = left_join(df_vac2, df_ICStest, by = c("Scenario", "Trace", "State")) %>%
  pivot_longer(cols = c(SystemWater, Creation, Delivery))

g1 <- ggplot(df_ICSVac, aes(factor(name), value, fill = Scenario))+
  bor_theme()+
  geom_hline(yintercept = 0, color = 'gray') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
  theme(legend.position="top") +
  facet_grid(State ~ ., scales = 'free_y')
print(g1)
ggsave(file.path(fig_dir, paste0("LB_ICS_SysWater_byState", end_file_nm , ".png")), 
       width = 9, height = 6) 


# stats for df
xx2_med = df_ICSVac %>%
  group_by(Scenario, State, name) %>%
  summarise(med = median(value),
            ymin = min(value),
            ymax = max(value))

testout = xx2_med %>% ungroup() %>%
  mutate(StateAlt = paste0(State, '-', Scenario)) %>%
  select(StateAlt, name, med) %>%
  pivot_wider(names_from = StateAlt, values_from = med) %>%
  select(name,sort(names(.)))
write.csv(testout, file.path(fig_dir, "TraceDataSum_LBICS_NVfix.csv"))


## ====  LB Users -set up 
slots = c(
  "PumpingFromLakeMead:SNWP.Diversion Requested", 
  "MWDDiversion.Total Diversion", # includes some of MX, should use "MWDDiversion:MWD.Diversion Requested"
  "CAPDiversion.Total Diversion"
)
slot_nms = c("SNWA", "MWD (+some TJ-MX)", "CAP")
rdfs = rep('diversion.rdf', length(slots))

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
df<- NULL
for (i in 1:length(scenarios_2)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i]
  )
  scen_res$Scenario <- scenarios_2[i]
  
  # keep only last 30 traces (ESP)
  trces = sort(unique(scen_res$TraceNumber))
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df, scen_res)
}

df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios_2),
         Variable = factor(Variable, levels = slots, labels = slot_nms)) 


df_ann = df_scens %>% 
  mutate(Year = factor(year(Date))) %>% 
  group_by(Year, Variable, Scenario, Trace) %>%
  summarise(Value = sum(Value)/1000)

## -- Major water user's use
g1 <- ggplot(df_ann, aes(Year, Value, fill = Scenario)) +
  bor_theme()+
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual Use (kaf)',
       subtitle = '*first year will not include use prior to the run start date') +
  facet_grid(Variable ~ ., scales = 'free_y')
print(g1)
ggsave(file.path(fig_dir, paste0("LB_SpecificUsers", end_file_nm , ".png")), 
       width = 8, height = 7)

