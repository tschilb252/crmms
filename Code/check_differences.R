# Libraries
library(RWDataPlyr)
library(tidyverse)
library(lubridate)

# source helper functions
source(paste0(Sys.getenv("CRMMS_DIR"), "\\Code\\compare_plots_helperFuncs.R"))

# TEST - function inputs - how to do this with RiverSMART ?
scenario_dir <- paste0(Sys.getenv("CRMMS_DIR"), "\\Scenario")
base_scen_nm <- c("ModelBase,RulesBase", "Base")
dev_scen_nm <- c("ModelDev,RulesDev", "Dev")
slots_default <- c(
  apply(expand.grid(
    c('Powell', 'Mead', 'FlamingGorge', 'BlueMesa','Navajo', 'Fontenelle', 
      'Crystal','MorrowPoint','TaylorPark', 'Mohave', 'Havasu'),
    c('Outflow', 'Inflow','Storage','Pool Elevation')),   # slots
    1, paste, collapse = "."),
  "PowellData.ReleaseTier", "Shortage.Shortage Flag")
data_files <- c("OpsUse.csv", "ReservoirOutput.csv")
out_fl_nm = "test"


compare_difs <- function(scenario_dir,
                             output_dir,
                             base_scen_nm, #= c("ModelBase,RulesBase", "Base"),
                             dev_scen_nm, #= c("ModelDev,RulesDev", "Dev"),
                             slotNames = NA,
                             data_files = "ReservoirOutput.csv",
                             out_fl_nm = "outDifs") {
  hydroIC <- c("Wet", "Avg", "Dry")
  print(paste('Processing Data in:', data_files))
  
  if (slotNames =="NA") {
    slotNames <- slots_default
  }
  
  # get scenarios for desired groups
  scenarios <- list.files(scenario_dir)
  base_scenarios <- scenarios[grep(base_scen_nm[1], scenarios)]
  dev_scenarios <- scenarios[grep(dev_scen_nm[1], scenarios)]
  all_scenarios <- c(base_scenarios, dev_scenarios)

  # get data and add scenario group
  df <- getScenarios(slotNames, all_scenarios, data_files, scenario_dir)
  df$ScenarioGroup <- NA
  df$ScenarioGroup[grep(base_scen_nm[1], df$scenario_i)] <- base_scen_nm[2]
  df$ScenarioGroup[grep(dev_scen_nm[1], df$scenario_i)] <- dev_scen_nm[2]
  
  # get run date
  n=6
  df$run_date = substr(x=df$scenario_i, start=nchar(df$scenario_i)-n, stop=nchar(df$scenario_i))
  
  # add hydrology IC column
  df$hydroGroup <- NA
  for (i in hydroIC) {
    df$hydroGroup[grep(i, df$scenario_i)] <- i
  }
  
  # reformat df
  df <- df %>% 
    select(-RunNumber, -scenario_i) %>%
    mutate(
      # year = year(Timestep),
      # month = month(Timestep),
      ScenarioGroup = factor(ScenarioGroup),
      hydroGroup = factor(hydroGroup, levels = hydroIC),
      Timestep = as.Date(Timestep)
    ) 
  
  ## check for differences
  df_dif = df %>%
    group_by(ObjectSlot, hydroGroup, run_date, Timestep) %>%
    pivot_wider(names_from = ScenarioGroup, values_from = Value) %>%
    mutate(dif = Base - Dev) 
  df_test = df_dif %>% 
    filter(abs(dif) >= 0.001)
  
  # open log file
  log_fl <- file(paste0(output_dir, "\\log_file.txt"), open = 'w')
  if (nrow(df_test) > 0) {
    df_i = df_test %>% ungroup() %>%
      select(-c(Timestep, Unit, Base, Dev, dif)) %>%
      distinct()
    
    cat(c("Differences in ...", ""), file = log_fl, sep = '\n')
    cat(paste0(colnames(df_i), collapse = ','), 
        file = log_fl, append = T, sep = '\n')
    cat(apply(df_i, 1, paste0, collapse=','), 
        file = log_fl, append = T, sep = '\n')
  } else {
    cat(paste("No differences > 0.001"), file = log_fl, sep="\n")
  }
  close(log_fl)
  
  # write output to text file
  write.table(df_test, paste0(output_dir, "\\", out_fl_nm, ".txt"),
              sep = "\t", row.names = FALSE)
}