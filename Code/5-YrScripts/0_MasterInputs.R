# ============================================================================
# Master Inputs for processing 5-year CRMMS projections   
#
# ============================================================================
rm(list=ls())

## -- Inputs
# Directory name - all directories stored in Scenario/ folder 
scenario_dir <- c(
  "2023-01_ESP",
  "2022-08_ESP"
)
# Name when plotting
scenarios <- c(
  "January 2023",
  "August 2022"
)
fig_dir_nm <- '2023-01_ComparetoAug'
# ^ script will create directory in Results/ with this name if it doesn't exist

## Run Scripts
source(file.path('Code', '5-YrScripts','ESP_compare.R'))
# source(file.path('Code', '5-YrScripts','LB_ICSUse.R'))
source(file.path('Code', '5-YrScripts','Compare_Slots_Cloud+Trace.R'))
source(file.path('Code', '5-YrScripts','PowellTiers_LBCondition.R'))
source(file.path('Code', '5-YrScripts','PowellMead_Thresholds.R'))
source(file.path('Code', '5-YrScripts','5yr_table.R'))
