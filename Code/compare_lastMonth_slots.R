# compare last month's official run and development run
library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

# slots = c("TaylorPark.Storage",'Powell.Outflow', 'Powell.Pool Elevation', 'Powell.Storage',
#           'Mead.Outflow', 'Mead.Pool Elevation', 'Mead.Storage')
# rdf = rep('res.rdf', length(slots))
# output_dir = getwd()

summarize_slot_difs <- function(scenarios,
                                scenario_dir,
                                output_dir,
                                slots,
                                rdf) {
  
  names(scenarios) = c("Official", "Dev")
  
  # slots/agg to read
  rwa1 <- rwd_agg(data.frame(
    file = rdf,
    slot = slots, 
    period = rep("asis", length(slots)),
    summary = rep(NA, length(slots)),
    eval = rep(NA, length(slots)),
    t_s = rep(NA, length(slots)),
    variable = slots,
    stringsAsFactors = FALSE
  ))
  
  # read/process rdf
  scen_res <- rw_scen_aggregate(
    scenarios,
    agg = rwa1,
    scen_dir = scenario_dir
  )
  
  df_i <- data.table::as.data.table(scen_res)  %>% 
    mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
    select(Scenario, Variable, Date, Trace = TraceNumber, Value)
  
  check_difs = df_i %>% 
    pivot_wider(names_from = Scenario, values_from = Value) %>%
    mutate(diff = .data[[names(scenarios)[1]]] - .data[[names(scenarios)[2]]])
  
  # differences in traces that show change between official and dev
  df_difs = check_difs %>% 
    filter(diff != 0) %>%
    group_by(Variable) %>%
    summarise(Avg_dif = mean(diff))
  
  difs_pr = check_difs %>% 
    filter(diff != 0) %>%
    select(Variable, Trace) %>%
    distinct() 
  
  # open logfile to write
  log_fl <- file(paste0(output_dir, "\\diff_summary.txt"), open = 'w')
  cat(c("Differences in slots for any timestep and trace", ""), file = log_fl, sep = '\n')
  rw_nms = rownames(difs_pr)
  for (i in 1:length(slots)) {
    cat(paste(slots[i], ' ---'), file = log_fl, sep = '\n')
    df_i = difs_pr %>% filter(Variable == slots[i])
    df_difs_i = df_difs %>% filter(Variable == slots[i])
    
    if (nrow(df_i) == 33) {
      cat("   ... All traces have differences", file = log_fl, sep = '\n')
      cat(paste("   ...", round(df_difs_i$Avg_dif, 2), "= average difference over traces and all timesteps with differences"), 
          file = log_fl, sep = '\n')
    } else if (nrow(df_i) == 0) {
      cat("   ... No differences", file = log_fl, sep = '\n')
    } else {
      cat(paste0(c("   ... In traces:", df_i$Trace), collapse = " "), file = log_fl, sep = '\n')
      cat(paste("   ...", round(df_difs_i$Avg_dif, 2), "= average difference over traces and all timesteps with differences"), 
          file = log_fl, sep = '\n')
    }
  }
  close(log_fl)
  
  write.csv(check_difs, paste0(output_dir, "\\slot_diffs.csv"))
}

