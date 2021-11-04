# compare last month's official run and development run
library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)


## Scenario Names
my_scens = c(
  'MRM_RFCfcst,OctOff_mdl,Rules1.6',
  'MRM_RFCfcst,OctOff_mdl,Rules1.6_dev'
)
names(my_scens) = c("Oct Official", 
                    "Oct Dev"
)

# setwd(Sys.getenv('CRMMS_DIR'))
# setwd('C:/Users/sabaker/Projects/Models/CRMMS')

slots = c('Powell.Outflow', 'Powell.Pool Elevation', 'Powell.Storage',
          'Mead.Outflow', 'Mead.Pool Elevation', 'Mead.Storage')
rdfs = rep('res.rdf', length(slots))


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
  my_scens,
  agg = rwa1,
  scen_dir = 'Scenario'
)

df_i <- data.table::as.data.table(scen_res)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  select(Scenario, Variable, Date, Trace = TraceNumber, Value)

check_difs = df_i %>% 
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  mutate(diff = .data[[names(my_scens)[1]]] - .data[[names(my_scens)[2]]])

test = check_difs %>% filter(diff != 0)
unique(test$Trace)


## Powell TARV check
rwa2 <- rwd_agg(data.frame(
  file = 'res.rdf',
  slot = c('Powell.Outflow'), 
  period = "wy",
  summary = 'sum',
  eval = NA,
  t_s = NA,
  variable = 'tarv',
  stringsAsFactors = FALSE
))


# read/process RDFs
scen_res <- rw_scen_aggregate(
  my_scens,
  agg = rwa2,
  scen_dir = 'Scenario'
)

df_i <- data.table::as.data.table(scen_res)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  select(Scenario, Variable, Date, Trace = TraceNumber, Value)

check_difs = df_i %>% 
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  mutate(diff = .data[[names(my_scens)[1]]] - .data[[names(my_scens)[2]]])

test = check_difs %>% filter(abs(diff) > 0.05)
unique(test$Trace)
