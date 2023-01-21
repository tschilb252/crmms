# ============================================================================
# Create and Compare CRMMS-ESP 5-year table
#   
#
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "scenarios", "fig_dir_nm")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', '5-YrScripts', '5yr_table_naming.R'))

## Max Year
trace_n = 30
slots = c(coorOps_Powell_slots, coorOps_Mead_slots)
file_nm_end <- ''

## -- Read in CRMMS results

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rep("flags.rdf", length(slots)),
  slot = slots, 
  period = rep("asis", length(slots)),
  summary = rep(NA, length(slots)),
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = slots,
  stringsAsFactors = FALSE
))

# read/process RDFs
df <- NULL
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i],
    keep_cols = 'Unit'
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = sort(unique(scen_res$TraceNumber))
  trces = trces[trces >= 0]
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df, scen_res)
}

df_scens <- data.table::as.data.table(df)  %>% 
  # mutate(Date = as.yearmon(paste0(Month, Year), "%Y")) %>%
  select(Scenario, Variable, Year, Trace = TraceNumber, Value) %>%
  # filter(Date <= as.yearmon(format(ym(max_date), "%Y-%m"))) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Variable = factor(Variable, levels = slots,
                           labels = names(slots)))

df_pwl_summ = df_scens %>% 
  filter(Variable %in% names(coorOps_Powell_slots)) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  # group_by(Scenario, Year, Trace) %>
  mutate(Equalization = EqualizationAbove823 + EqualizationAt823,
         UpperBalancing = UpperBalancingAbove823 + UpperBalancingAt823 +
           UpperBalancingBelow823,
         MidElevationRelease = MidElevationReleaseAt823 + MidElevationReleaseAt748,
         LowerBalancing = LowerBalancingAbove823 + LowerBalancingAt823 +
           LowerBalancingBelow823) %>%
  select(Scenario, Year, Trace, Equalization, UpperBalancing, 
         MidElevationRelease, LowerBalancing)

# check to make sure powell summary slots are correct
check_pwl_summ = df_pwl_summ %>% mutate(test = Equalization + UpperBalancing + 
                         MidElevationRelease + LowerBalancing) %>%
  filter(test != 1 & Year > min(df_scens$Year))
if (nrow(check_pwl_summ) >  0) {
  stop('Powell summary slot error')
}

# combine 
df_scens = rbind(df_scens, 
                 df_pwl_summ %>% 
                   pivot_longer(cols = c(Equalization, UpperBalancing, 
                                         MidElevationRelease, LowerBalancing), 
                                names_to = "Variable", values_to = "Value"))

# doesnt work always
# if (length(unique(df_scens$Trace)) != trace_n) {
#   stop('number of traces incorrect')
# }


tbl_base = df_scens %>%
  group_by(Scenario, Year, Variable) %>%
  summarise(PerTrace = sum(Value)/trace_n*100) %>%
  ungroup() %>%
  pivot_wider(names_from = Year, values_from = PerTrace) %>%
  mutate(Variable = factor(Variable, 
                           levels = names(c(coorOps_Powell_out, coorOps_Mead_out)))) %>%
  arrange(Scenario, Variable)


# Table for excel
tbl_out = tbl_base %>%
  mutate(across(3:ncol(tbl_base), round, 0),
         Variable = factor(Variable, 
                           labels = c(coorOps_Powell_out, coorOps_Mead_out)))
openxlsx::write.xlsx(tbl_out, file = file.path(fig_dir, paste0('5yr_Table', file_nm_end, '.xlsx')))


