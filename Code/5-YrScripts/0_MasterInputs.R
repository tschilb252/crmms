# ============================================================================
# Master Inputs for processing 5-year CRMMS projections   
#
# ============================================================================
rm(list=ls())

## -- Inputs
# Scenarios: "Name of Scenario" = "Directory name" 
#   (data are in sub-directories of the Scenario folder)
scenario_dir <- c("Feb" = "2024-02_ESP",
                  "Jan" = "2024-01_ESP")


fig_dir_nm <- '2024-02_2yr_Compare'
# ^ script will create directory in Results/ with this name if it doesn't exist

## Scenario colors
if (length(scenario_dir) == 2) {
  custom_Tr_col <- c('#f1c40f', '#8077ab')
} else {
  custom_Tr_col <- scales::hue_pal()(length(scenarios))
}

## Run Scripts
source(file.path('Code', '5-YrScripts','ESP_compare.R'))
source(file.path('Code', '5-YrScripts','LB_ICSUse.R'))
source(file.path('Code', '5-YrScripts','Compare_Slots_Cloud+Trace.R'))
source(file.path('Code', '5-YrScripts','PowellTiers_LBCondition.R'))
source(file.path('Code', '5-YrScripts','PowellMead_Thresholds.R'))
source(file.path('Code', '5-YrScripts','PowellMead_RelPE_summ.R'))
source(file.path('Code', '5-YrScripts','5yr_table.R'))
source(file.path('Code', '5-YrScripts','Trace_Data.R'))
source(file.path('Code', '5-YrScripts','PowellUnreg_vs.tarv,etc.R'))
source(file.path('Code', '5-YrScripts','Energy_Compare.R'))
