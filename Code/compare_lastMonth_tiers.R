# ============================================================================
# Compare Last official CRMMS-ESP run 
#   Powell and Mead Tiers 
#
# ============================================================================
library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)

tiers_compare <- function(scenarios, 
                           scen_names = NA, 
                           # slots,
                           # rdf,
                           scenario_dir,
                           output_dir) {
  
  # set up 
  slots = c(
    'Shortage.Shortage Flag', 'PowellData.ReleaseTier',
    'PowellData.ActualAnnualReleaseVolume', 'DCP BWSCP Flags.LB DCP BWSCP'
  )
  rdfs = rep('flags.rdf', length(slots))
  
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
    select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
    filter(Trace > 3) %>% # remove min/most/max 
    mutate(Scenario = factor(Scenario, levels = names(scenarios)))
  
  ## Colors and Labels
  DCPlab = c('Mead > 1,090 ft'= '#a6cee3', 
             'Mead 1,075-1,090 ft' = '#1f78b4',
             'Mead 1,050-1,075 ft' = '#b2df8a',
             'Mead 1,045-1,050 ft' = '#fb9a99',
             'Mead 1,040-1,045 ft' = '#e31a1c',
             'Mead 1,035-1,040 ft' = '#fdbf6f',
             'Mead 1,030-1,035 ft' = '#ff7f00',
             'Mead 1,025-1,030 ft' = '#ffff99',
             'Mead 895-1,025 ft' = '#cab2d6')
  PowellTierLab = c('Equalization Tier' = '#a6cee3', 
                    'Upper Elevation\nBalancing Tier' = '#1f78b4',
                    'Mid Elevation\nRelease Tier' = '#b2df8a',
                    'Lower Elevation\nBalancing Tier' = '#33a02c')
  ShortLabs = c('Normal Condition' = '#a6cee3', 
                'Shortage - Level 1' = '#1f78b4',
                'Shortage - Level 2' = '#b2df8a',
                'Shortage - Level 3' = '#cab2d6')
  
  ## process df
  df_i = df_scens %>% pivot_wider(names_from = Variable, values_from = Value) %>%
    mutate(Year = factor(year(Date)),
           act_TARV = PowellData.ActualAnnualReleaseVolume/10^3,
           `Powell Tiers` = factor(PowellData.ReleaseTier, 
                                   levels = c(0,1,2,3),
                                   labels = names(PowellTierLab)),
           `Lower Basin Shortage`= factor(`Shortage.Shortage Flag`, 
                                          levels = c(0,1,2,3),
                                          labels = names(ShortLabs)),
           `DCP Contribution` = factor(`DCP BWSCP Flags.LB DCP BWSCP`, 
                                       levels = 8:0, 
                                       labels = names(DCPlab))) %>%
             select(-all_of(slots), -Date) 
  
  
  
  # set to figure dir
  setwd(output_dir)
  pdf('Tiers_compare.pdf', width=8, height=7)
  
  # barplot of Powell tiers
  g <- ggplot(df_i, aes(Scenario, fill = `Powell Tiers`)) +
    theme_bw() +
    scale_fill_manual(values = PowellTierLab) +
    geom_bar(stat = "count") +
    scale_y_continuous(breaks = seq(0,35, by =5), expand = c(0,0)) +
    labs(x = '', y = 'Number of Traces') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    facet_grid(~ Year)
  print(g)
  
  # LB Shortage
  g <- ggplot(df_i, aes(Scenario, fill = `Lower Basin Shortage`)) +
    theme_bw() +
    scale_fill_manual(values = ShortLabs) +
    geom_bar(stat = "count") +
    scale_y_continuous(breaks = seq(0,35, by =5), expand = c(0,0)) +
    labs(x = '', y = 'Number of Traces') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    facet_grid(~ Year)
  print(g)
  
  # LB Shortage/DCP
  g <- ggplot(df_i, aes(Scenario, fill = `DCP Contribution`)) +
    theme_bw() +
    scale_fill_manual(values = DCPlab)+
    geom_bar(stat = "count") +
    scale_y_continuous(breaks = seq(0,35, by =5), expand = c(0,0)) +
    labs(x = '', y = 'Number of Traces') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    facet_grid(~ Year)
  print(g)
  
  # TARV boxplot doplot
  i_breaks = c(seq(1000,7000, by = 500), 7480, seq(8000,20000, by = 500)) 
  g <- ggplot(df_i, aes(Year, act_TARV, fill = Scenario)) +
    theme_bw() +
    # stat_boxplot_custom(position = "dodge") +
    geom_violin(position = "dodge") +
    geom_dotplot(binaxis='y', stackdir='center', 
                 position=position_dodge(0.8)) +
    scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
    labs(x = 'Year', y = 'Annual Powell Release (kaf)') +
    theme(legend.position="top") +
    facet_grid(`Powell Tiers` ~ ., scales = 'free_y')
  print(g)
  
  # Total TARV; no facet
  g <- ggplot(df_i, aes(Year, act_TARV, fill = Scenario)) +
    theme_bw() +
    stat_boxplot_custom(position = "dodge") +
    scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
    labs(x = 'Year', y = 'Annual Powell Release (kaf)') +
    theme(legend.position="top")
  print(g)
  
  dev.off()
}