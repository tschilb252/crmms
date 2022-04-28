# ============================================================================
# Compare Last official CRMMS-ESP run 
#   LB uses / ICS bank / MSCP 
# ============================================================================
rm(list=ls())
library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)
library(patchwork)

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
max_yr = 2026
end_file_nm = paste0('_', max_yr)

##### ------ ICS LB use ------ #####
slots = c(
  "AnnualWaterUse.AZ_BasicApportionment", "AnnualWaterUse.CA_BasicApportionment",
  "AnnualWaterUse.AzTotalAnnual", "AnnualWaterUse.CaTotalAnnual",
  "AnnualWaterUse.NvTotalAnnual",
  'ICS Credits.Bank_CA', 'ICS Credits.Bank_AZ', 'ICS Credits.Bank_NV',
  'ICS Credits.AnnualCreationEC_AZ', 'ICS Credits.AnnualDeliveryEC_AZ', 
  'ICS Credits.AnnualCreationSysEff_AZ', 'ICS Credits.AnnualDeliverySysEff_AZ',
  'ICS Credits.AnnualCreationDCP_AZ', 'ICS Credits.AnnualDeliveryBiNat_CAWCD',
  'ICS Credits.AnnualCreationEC_CA', 'ICS Credits.AnnualDeliveryEC_CA',
  'ICS Credits.AnnualCreationSysEff_CA', 'ICS Credits.AnnualDeliverySysEff_CA',
  'ICS Credits.AnnualCreationEC_NV', 'ICS Credits.AnnualDeliveryEC_NV', 
  'ICS Credits.AnnualCreationSysEff_NV', 'ICS Credits.AnnualDeliverySysEff_NV',
  'ICS Credits.AnnualCreationImp_NV', 'ICS Credits.AnnualDeliveryImp_NV',
  'ICS Credits.AnnualCreationTrib_NV', 'ICS Credits.AnnualDeliveryTrib_NV',
  'ICS Credits.AnnualDeliveryBiNat_CA', 'ICS Credits.AnnualDeliveryBiNat_NV'
)
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
df<- NULL
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i]
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
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios))

# open pdf to save figs
pdf(file.path(fig_dir, paste0('LBAnalysis_compare', end_file_nm, '.pdf')), width=8, height=8)

## LB ICS bank 
df_i = df_scens %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(year(Date) <= max_yr) %>%
  mutate(Year = factor(year(Date)),
         MSCPflowRed = (AnnualWaterUse.AZ_BasicApportionment + 
                          AnnualWaterUse.CA_BasicApportionment -
                          (AnnualWaterUse.AzTotalAnnual + AnnualWaterUse.CaTotalAnnual))/10^3,
         totalICSBank = (`ICS Credits.Bank_CA` + `ICS Credits.Bank_AZ` + 
                           `ICS Credits.Bank_NV`)/10^3) %>%
  select(-all_of(slots), -Date) 

i_breaks = seq(0,3000, by = 200)
g <- ggplot(df_i, aes(Year, totalICSBank, fill = Scenario)) +
  bor_theme() +
  geom_hline(yintercept = 2700, color = 'grey', linetype = 'dashed') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
  labs(x = 'End of Year', y = 'Total ICS Bank Balance (kaf)') 
print(g)

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

df_inew = df_i %>% 
  mutate(State = 'Total') %>%
  select(Scenario, Trace, Value = totalICSBank, Year, State) 

i_breaks = seq(0,3000, by = 25)
g1 = ggplot(df_i2, aes(Year, Value, fill = Scenario, shape = Scenario)) +
  bor_theme() +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +#, breaks = i_breaks) +
  labs(x = 'End of Year', y = 'ICS Bank Balance (kaf)') +
  facet_grid(State ~ ., scales = 'free_y')

i_breaks = seq(0,3000, by = 150)
g2 = ggplot(df_inew, aes(Year, Value, fill = Scenario, shape = Scenario)) +
  bor_theme() +
  geom_hline(yintercept = 2700, color = 'grey', linetype = 'dashed') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
  labs(x = 'End of Year', y = 'ICS Bank Balance (kaf)') +
  facet_grid(State ~ ., scales = 'free_y')

g3 <- g2 / g1 + plot_layout(guides = "collect", heights = c(1, 3))
print(g3)

## MSCP compliance
i_breaks = seq(-2000,3000, by = 200)
g <- ggplot(df_i, aes(Year, MSCPflowRed, fill = Scenario)) +
  bor_theme() +
  geom_hline(yintercept = 845, color = 'grey', linetype = 'dashed') +
  geom_hline(yintercept = 0, color = 'grey') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
  labs(x = 'End of Year', y = 'MSCP Flow Reductions (kaf)') 
print(g)

## State Use
slots = c("AnnualWaterUse.AzTotalAnnual", "AnnualWaterUse.CaTotalAnnual",
          "AnnualWaterUse.NvTotalAnnual")
slot_nms = c('Arizona', 'California', 'Nevada')
df_i = df_scens %>% 
  filter(year(Date) <= max_yr) %>%
  filter(Variable %in% slots) %>%
  mutate(Variable = factor(Variable, levels = slots, labels = slot_nms),
         Value = Value/10^3, 
         Year = factor(year(Date)))
df_i2 = df_i %>% 
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(Value = Arizona + California + Nevada,
         Variable = 'Total') %>% 
  select(all_of(colnames(df_i))) 
df_i = rbind.data.frame(df_i, df_i2)
g1 <- ggplot(df_i, aes(Year, Value, fill = Scenario)) +
  bor_theme()+
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual Use (kaf)') +
  facet_grid(Variable ~ ., scales = 'free_y')
print(g1)

## ICS
df_ICS_1 = df_scens %>% 
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
      grepl("Trib_", Variable) ~ "Trib",
      grepl("DCP_", Variable) ~ "DCP",
      grepl("BiNat_", Variable) ~ "BiNat"),
    Year = factor(year(Date))) %>%
  na.omit() %>% 
  select(-Variable) 
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
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
  theme(legend.position="top") +
  facet_grid(State ~ ., scales = 'free_y')
print(g1)

df_ICS_1 = df_ICS_1 %>%
  pivot_wider(names_from = ICS, values_from = Value) %>%
  mutate(Value = Creation - Delivery)

g2 <- ggplot(df_ICS_1, aes(factor(type), Value, fill = Scenario, shape = Scenario))+
  bor_theme()+
  geom_hline(yintercept = 0, color = 'gray') +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  geom_point(size = 2, position=position_dodge(0.8)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
  theme(legend.position="top",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_grid(State ~ Year, scales = 'free_y')
print(g2)


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
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i]
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
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Variable = factor(Variable, levels = slots, labels = slot_nms)) 


df_ann = df_scens %>% 
  mutate(Year = factor(year(Date))) %>% 
  group_by(Year, Variable, Scenario, Trace) %>%
  summarise(Value = sum(Value)/1000)

g1 <- ggplot(df_ann, aes(Year, Value, fill = Scenario)) +
  bor_theme()+
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual Use (kaf)',
       subtitle = '*first year will not include use prior to the run start date') +
  facet_grid(Variable ~ ., scales = 'free_y')
print(g1)

g1 <- ggplot(df_ann, aes(Year, Value, fill = Scenario)) +
  bor_theme()+
  geom_violin(position = "dodge") +
  geom_dotplot(binaxis='y', stackdir='center', 
               position=position_dodge(0.8)) +  
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'Annual Use (kaf)',
       subtitle = '*first year will not include use prior to the run start date') +
  facet_grid(Variable ~ ., scales = 'free_y')
print(g1)

dev.off()