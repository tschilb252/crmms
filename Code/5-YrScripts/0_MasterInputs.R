# ============================================================================
# Master Inputs for processing 5-year CRMMS projections   
#
# ============================================================================
rm(list=ls())

## -- Inputs
# Scenarios: "Name of Scenario" = "Directory name" (all data in Scenario folder)
scenario_dir <- c(
  "Aug" = "2023-08_ESP",
  "Apr" = "2023-04_ESP"
)

fig_dir_nm <- '2023-08_5YrCompare'
# ^ script will create directory in Results/ with this name if it doesn't exist

## Run Scripts
source(file.path('Code', '5-YrScripts','ESP_compare.R'))
source(file.path('Code', '5-YrScripts','LB_ICSUse.R'))
source(file.path('Code', '5-YrScripts','Compare_Slots_Cloud+Trace.R'))
source(file.path('Code', '5-YrScripts','PowellTiers_LBCondition.R'))
source(file.path('Code', '5-YrScripts','PowellMead_Thresholds.R'))
source(file.path('Code', '5-YrScripts','5yr_table.R'))
source(file.path('Code', '5-YrScripts','Trace_Data.R'))
source(file.path('Code', '5-YrScripts','PowellUnreg_vs.tarv,etc.R'))
source(file.path('Code', '5-YrScripts','Energy_Compare.R'))


