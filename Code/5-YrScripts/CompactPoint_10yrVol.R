# LF 10yr flows
rm(list=setdiff(ls(), c("scenario_dir", "fig_dir_nm", "custom_Tr_col")))

library(tidyverse)
library(CRSSIO)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(rhdb)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
scenarios = names(scenario_dir)
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

# Max Year
max_yr = 2028

## -- CRMMS results
slots = c("Powell.Outflow", 
          'PowellToMead:LeesFerryGage.Local Inflow') 
rdfs = c(rep('res.rdf', length(slots)-1), rep('streamflow.rdf', times = 1))

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
df <- NULL
for (i in 1:length(scenarios)) {
  
  # check that directory exists
  if (!dir.exists(data_dir[i])) { stop(paste("Data directory does not exist:", data_dir[i]))}
  
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i],
    find_all_slots = F
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = sort(unique(scen_res$TraceNumber))
  trces = trces[trces >= 0]
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df,
              scen_res %>%
                mutate(TraceNumber = 1991 + TraceNumber - 
                         min(scen_res$TraceNumber, na.rm = T)))
}
df <- df %>% na.omit()

## -- process and combine hydrologies
df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Year = year(Date)) 

# calc crmms compact pt volume
df_lf <- df_scens %>% group_by(Scenario, Trace, Date) %>%
  summarise(Value = sum(Value)) %>%
  mutate(wy = ifelse(month(Date) >= 10,
                     year(Date) + 1, year(Date)))


## Read historical data from hdb, sum for compact pt
dates_min = df_scens %>% group_by(Scenario) %>% summarise(mindate = min(Date))
sdis <- c("Lees Ferry" = 1578, # actually Lees Ferry!
          "Paria" = 1579) 
start_date = format(ym("2005-10"), "%Y-%m")
end_date = format(max(dates_min$mindate) - 1/12, "%Y-%m") 
df_hdb <- bind_rows(
  hdb_query(sdis["Lees Ferry"], "uc", "m", start_date, end_date),
  hdb_query(sdis["Paria"], "uc", "m", start_date, end_date)
)

df_hdb <- df_hdb %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         Date = as.yearmon(parse_date_time(time_step, "m/d/y H:M:S")),
         
         value = value/1000) %>%
  group_by(Date) %>%
  # remove Paria (make this Lees Ferry gage and not Lee Ferry compact)
  filter(slot != 'Paria') %>%
  summarise(Value = sum(value)) %>%
  mutate(wy = ifelse(month(Date) >= 10,
                     year(Date) + 1, year(Date)))

min_FullHist <- dates_min %>%
  mutate(wy = ifelse(month(mindate) >= 10,
                     year(mindate) + 1, year(mindate)))

# add in historical data
df_new <- NULL
for (i in 1:length(scenarios)) {
  df_scensI = df_lf %>% 
    filter(Scenario == scenarios[i]) 
  
  df_histAdd <- df_hdb %>%
    filter(Date < min(df_scensI$Date)) #& wy >= min(min_FullHist$wy)) #%>%
  
  df_trhist = df_scensI %>% select(Scenario, Trace) %>% distinct()
  
  df_histAddTr = merge(df_histAdd, df_trhist) %>%
    select(colnames(df_scensI))
  
  df_new = rbind.data.frame(df_new, 
                            rbind.data.frame(df_histAddTr, df_scensI))  
}

df_crmmsLF = df_new %>%
  group_by(wy, Trace, Scenario) %>%
  summarise(Value = sum(Value)/1000, 
            n = n()) %>%
  filter(n == 12)

df_crmms2 = df_crmmsLF %>%
  arrange(Scenario, Trace, wy) %>%
  group_by(Scenario, Trace) %>%
  mutate(roll_sum = RcppRoll::roll_sum(Value, 10, align = "right", fill = NA))  %>%
  na.omit() 

df_hist10 <- df_crmms2 %>% ungroup() %>%
  filter(min(min_FullHist$wy) > wy) %>%
  select(wy, roll_sum) %>% 
  distinct() %>%
  mutate(Scenario = factor('Historical'))

df_crmmsPlot <- df_crmms2 %>%
  filter(min(min_FullHist$wy) <= wy & wy <= max_yr)

## plot
ggplot() +
  CRSSIO::stat_boxplot_custom(data = df_crmmsPlot,
                              aes(factor(wy), roll_sum, fill = Scenario),
                              position = "dodge") +
  scale_fill_manual(values = custom_Tr_col) +
  geom_line(data = df_hist10, aes(factor(wy), roll_sum, group = 1), color = 'black') +
  theme_bw() +
  labs(x = 'Water Year', y = '10-year Volume (maf)',
       fill = NULL) +
  theme(
    # panel.background = element_rect(fill='transparent'),
    # plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent')
  )

ggsave(file.path(fig_dir, "lees_ferry_10yr.png"),width = 6.5, height = 5)
