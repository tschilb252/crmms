# ============================================================================
# Master Inputs for processing 5-year CRMMS projections   
#
# ============================================================================
rm(list=ls())

## -- Inputs
# Directory name - all directories stored in rdfOutput folder 
scenario_dir <- c(
  'May2022',
  # 'Apr2022_wy22Fix'
  # 'Apr2022_ClimoISM',
  # 'Apr2022_v3',
  # 'Apr2022_v2',
  # 'Apr2022_v1'
  # 'Mar2022'
  # 'Feb2022_wy22Fix',
  'Feb2022'
  # 'Feb2022_ClimoISM'
  # 'Jan2022_OG',
  # 'Jan2022_updatedRegression'
)
# Name when plotting
scenarios <- c(
  'May 2022',
  # 'Apr. 2022 - wyfix'
  # "Apr. 2022 - ClimoISM",
  # 'Apr. 2022 - Official',
  # 'Apr. 2022 v2',
  # 'Apr. 2022 v1'
  # 'Mar. 2022'
  # 'Feb. 2022 - wyfix',
  'February 2022'
  # 'Feb 2022 - ClimoISM'
  # 'Jan. 2022',
  # 'Jan. 2022 Updated Regres.'
)
fig_dir_nm <- 'May_official'
# ^ script will create directory with this name if it doesn't exist

## Run Scripts
source(file.path('Code', '5-YrScripts','ESP_compare.R'))
source(file.path('Code', '5-YrScripts','LB_ICSUse.R'))
source(file.path('Code', '5-YrScripts','cloud+trace_slots.R'))
source(file.path('Code', '5-YrScripts','PowellTiers_LBCondition.R'))
source(file.path('Code', '5-YrScripts','PowellMead_Thresholds.R'))
source(file.path('Code', '5-YrScripts','5yr_table.R'))
