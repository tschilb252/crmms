# ============================================================================
# Compare Last official CRMMS-ESP run 
#   LB Analysis of ICS and Use 
#
# ============================================================================
library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)
library(patchwork)

LBuseICS_compare <- function(scenarios, 
                           scen_names = NA, 
                           scenario_dir,
                           output_dir) {
  
  # set up 
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
  
  if (is.na(scen_names[1])) {
    names(scenarios) = scenarios
  } else if (length(scen_names) != length(scenarios)) {
    stop('number of scenarios and scenario names must be equal')
  } else {
    names(scenarios) = scen_names
  }
  
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
  scen_res <- rw_scen_aggregate(
    scenarios,
    agg = rwa1,
    scen_dir = scenario_dir
  )
  
  df_scens <- data.table::as.data.table(scen_res)  %>% 
    mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
    dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
    filter(Trace > 3) %>% # remove min/most/max 
    mutate(Scenario = factor(Scenario, levels = names(scenarios)))

  # set to figure dir
  setwd(output_dir)
  pdf('LBAnalysis_compare.pdf', width=8, height=8)
    
  ## LB ICS bank 
  df_i = df_scens %>% pivot_wider(names_from = Variable, values_from = Value) %>%
    mutate(Year = factor(year(Date)),
           MSCPflowRed = (AnnualWaterUse.AZ_BasicApportionment + 
                            AnnualWaterUse.CA_BasicApportionment -
                            (AnnualWaterUse.AzTotalAnnual + AnnualWaterUse.CaTotalAnnual))/10^3,
           totalICSBank = (`ICS Credits.Bank_CA` + `ICS Credits.Bank_AZ` + 
                             `ICS Credits.Bank_NV`)/10^3) %>%
    select(-all_of(slots), -Date) 
  
  i_breaks = seq(0,3000, by = 200)
  g <- ggplot(df_i, aes(Year, totalICSBank, fill = Scenario)) +
    theme_bw() +
    geom_hline(yintercept = 2700, color = 'grey', linetype = 'dashed') +
    stat_boxplot_custom(position = "dodge") +
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
    theme_bw() +
    stat_boxplot_custom(position = "dodge") +
    scale_y_continuous(labels = scales::comma) +#, breaks = i_breaks) +
    labs(x = 'End of Year', y = 'ICS Bank Balance (kaf)') +
    facet_grid(State ~ ., scales = 'free_y')

  i_breaks = seq(0,3000, by = 150)
  g2 = ggplot(df_inew, aes(Year, Value, fill = Scenario, shape = Scenario)) +
    theme_bw() +
    geom_hline(yintercept = 2700, color = 'grey', linetype = 'dashed') +
    stat_boxplot_custom(position = "dodge") +
    scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
    labs(x = 'End of Year', y = 'ICS Bank Balance (kaf)') +
    facet_grid(State ~ ., scales = 'free_y')
  
  g3 <- g2 / g1 + plot_layout(guides = "collect", heights = c(1, 3))
  print(g3)
  
  ## MSCP compliance
  i_breaks = seq(-2000,3000, by = 200)
  g <- ggplot(df_i, aes(Year, MSCPflowRed, fill = Scenario)) +
    theme_bw() +
    geom_hline(yintercept = 845, color = 'grey', linetype = 'dashed') +
    geom_hline(yintercept = 0, color = 'grey') +
    stat_boxplot_custom(position = "dodge") +
    scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
    labs(x = 'End of Year', y = 'MSCP Flow Reductions (kaf)') 
  print(g)
  
  ## State Use
  slots = c("AnnualWaterUse.AzTotalAnnual", "AnnualWaterUse.CaTotalAnnual",
            "AnnualWaterUse.NvTotalAnnual")
  slot_nms = c('Arizona', 'California', 'Nevada')
  df_i = df_scens %>% 
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
    theme_bw()+
    stat_boxplot_custom(position = "dodge") +
    scale_y_continuous(labels = scales::comma) +
    labs(x = NULL, y = 'Annual Use (kaf)') +
    facet_grid(Variable ~ ., scales = 'free_y')
  print(g1)

  ## ICS
  df_ICS_1 = df_scens %>% 
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
    theme_bw()+
    geom_hline(yintercept = 0, color = 'gray') +
    stat_boxplot_custom(position = "dodge") +
    scale_y_continuous(labels = scales::comma) +
    labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
    theme(legend.position="top") +
    facet_grid(State ~ ., scales = 'free_y')
  print(g1)

  df_ICS_1 = df_ICS_1 %>%
    pivot_wider(names_from = ICS, values_from = Value) %>%
    mutate(Value = Creation - Delivery)
  
  g2 <- ggplot(df_ICS_1, aes(factor(type), Value, fill = Scenario, shape = Scenario))+
    theme_bw()+
    geom_hline(yintercept = 0, color = 'gray') +
    stat_boxplot_custom(position = "dodge") +
    geom_point(size = 2, position=position_dodge(0.8)) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = NULL, y = 'Annual ICS Amount\n[Creation - Delivery]\n(kaf)') +
    theme(legend.position="top",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    facet_grid(State ~ Year, scales = 'free_y')
  print(g2)
  
  dev.off()
}