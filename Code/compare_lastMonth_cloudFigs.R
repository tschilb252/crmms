# ============================================================================
# Compare Last official CRMMS-ESP run 
#   Powell and Mead Pool Elevation Analysis 
#
# ============================================================================
library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

# source tier functions
source(file.path(Sys.getenv('CRMMS_DIR'), 'Code', 'add_MeadPowell_tiers.R'))

cloudFig_slots <- function(scenarios, 
                           scen_names = NA, 
                           slots,
                           rdf,
                           scenario_dir,
                           output_dir) {
  
  # set up 
  rdfs = rep(rdf, length(slots))
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
    scen_dir = scenario_dir, 
    keep_cols = 'Unit'
  )
  
  df_scens <- data.table::as.data.table(scen_res)  %>% 
    mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
    select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
    filter(Trace > 3) %>% # remove min/most/max 
    mutate(Scenario = factor(Scenario, levels = names(scenarios)))
  
  df_units = data.table::as.data.table(scen_res) %>%
    select(Variable, Unit) %>% distinct()
  
 
  ## Pool Elevation Monthly Cloud plot
  scen_plot = names(scenarios)
  df_stat = df_scens %>%
    filter(Scenario %in% scen_plot) %>%
    group_by(Scenario, Variable, Date) %>%
    summarise(CRMMSESP.max = max(Value),
              CRMMSESP.90 = quantile(Value, .9), 
              CRMMSESP.50 = quantile(Value, .5), 
              CRMMSESP.10 = quantile(Value, .1), 
              CRMMSESP.min = min(Value)) 
  df_crmmsCloud = df_stat %>% 
    select(-c("CRMMSESP.90", "CRMMSESP.50", "CRMMSESP.10")) %>%
    mutate(Cloud = factor(paste(Scenario, 'Range'), levels = paste(scen_plot, 'Range')))
  df_crmmsStats = df_stat %>% 
    select(-c("CRMMSESP.max", "CRMMSESP.min"))%>%
    pivot_longer(cols = c("CRMMSESP.90", "CRMMSESP.50", "CRMMSESP.10"), names_to = 'Trace') %>%
    mutate(Cloud = factor('CRMMS-ESP Range'),
           Trace = factor(Trace, 
                          levels = c("CRMMSESP.10", "CRMMSESP.50", "CRMMSESP.90"),
                          labels = c("CRMMS-ESP: 10%", "CRMMS-ESP: 50%", "CRMMS-ESP: 90%")))
  
  ## plot inputs
  custom_size <- rep(c(1,1.15,1))
  custom_lt <- rep(c(2,1,4))
  xrange =  range(df_crmmsCloud$Date)
  xbreaks = df_stat %>% ungroup() %>% 
    filter(month(Date) %in% c(4,8,12)) %>% select(Date) %>% c()
  
  # set to figure dir
  setwd(output_dir)
  pdf('Res_compare.pdf', width=11, height=7)
  
  ## loop through slots
  for (i in 1:length(slots)) {
    slot_i = slots[i]
    
    if (grepl('Pool Elevation', slot_i)) {
      y_breaks <- seq(0, 10025, 25)
      y_breaks2 <- seq(0, 10025, 5)
      ylabPE = 'Pool Elevation (ft)'
    } else {
      y_breaks <- NULL
      y_breaks2 <- NULL
      unit_i = df_units %>% filter(Variable == slot_i)
      ylabPE = paste0(sapply(strsplit(slot_i, split= ".", fixed = TRUE), tail, 1L),
                      ' (', unit_i$Unit, ')')
    }
    
    ## Plot slot
    gg <- ggplot() + 
      geom_line(data = filter(df_crmmsStats, Variable == slot_i),
                aes(x = Date, y = value, color = Scenario, linetype = Trace, size = Trace)) +
      scale_linetype_manual(values = custom_lt) +
      scale_size_manual(values = custom_size) +
      geom_ribbon(data = filter(df_crmmsCloud, Variable == slot_i), 
                  aes(x = Date, ymin = CRMMSESP.min, ymax = CRMMSESP.max, fill = Cloud), 
                  alpha = 0.3) +
      scale_x_yearmon(expand = c(0,0), breaks = as.vector(xbreaks$Date),
                      minor_breaks = unique(df_crmmsCloud$Date),
                      limits = c(min(df_crmmsCloud$Date), max(df_crmmsCloud$Date))) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        y = ylabPE, x = NULL, 
        color = NULL, linetype = NULL, size = NULL, fill = NULL,
        title = paste(slot_i, 'Comparison: Offical vs. Dev')
      ) +
      geom_vline(
        xintercept = as.yearmon(c("Dec 2021", "Dec 2022", "Dec 2023",  
                                  "Dec 2024",  "Dec 2025", "Dec 2026", "Dec 2027")), 
        size = 0.75, color = "#ffdc70"
      ) +
      theme_bw(base_size = 14) +
      guides(alpha = "none",
             color = guide_legend(nrow = 3, order = 1),
             linetype = guide_legend(nrow = 3, order = 1),
             size = guide_legend(nrow = 3, order = 1),
             fill = guide_legend(nrow = 2, order = 2)
      ) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5), 
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm")
      )
    
    # add specific breaks for pool elevation figs - results in a warning
    if (grepl('Pool Elevation', slot_i)) {
      gg <- gg +
        scale_y_continuous(
          labels = scales::comma, breaks = y_breaks, minor_breaks = y_breaks2)
      
    }
    
    # add tiers to Powell and Mead PE
    if (slot_i == 'Powell.Pool Elevation') {
      g <- gg %>%
        add_powell_tiers(xrange)
      print(g)
    } else if (slot_i == 'Mead.Pool Elevation') {
      g <- gg %>%
        add_mead_tiers(xrange)
      print(g)
    } else {
      print(gg)
    }
  }
  
  dev.off()
}