# CRMMS vs CRSS for Feb 2022 CRMMS run
rm(list=ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(rhdb)

## -- Inputs
scenario_dir <- c(
  'Apr2022'  
  # 'Mar2022'
  # 'Feb2022'
)
scenarios <- c(
  'April CRMMS-ESP'   
  # 'March CRMMS-ESP'
  # 'February CRMMS-ESP'
)
# fig_dir_nm <- 'AprvMarvFeb2022_5yr' 
# fig_dir_nm <- 'AprvMar2022_5yr' 
# fig_dir_nm <- 'AprvFeb2022_5yr' 
fig_dir_nm <- 'Apr2022_5yr' 
# ^ script will create directory with this name if it doesn't exist

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Output Data', fig_dir_nm)
data_dir <- file.path('rdfOutput', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))


base_dir = 'C:/Users/sabaker/Projects/Models/Projection Analyses'
crmms_dir_jan = 'C:/Users/sabaker/Projects/Models/CRMMS Info/CRMMS-January2022/rdfOutput'
# crmms_dir = 'C:/Users/sabaker/Projects/Models/CRMMS Info/CRMMS-February2022/rdfOutput'
crss_dir = file.path(base_dir, "data", "Jan2022_modelCompare")
# fig_dir = file.path(base_dir, 'figures', '2022-02_FebCRMMS_crssCloud')
# source(file.path(base_dir, 'add_MeadPowell_tiers.R'))

slots = c("Mead.Pool Elevation", "Powell.Pool Elevation") 
plot_title = c('Lake Mead End-of-Month Elevations', 
               'Lake Powell End-of-Month Elevations')
sub_title = 'April 2022 CRMMS-ESP Projection with Range of Uncertainty from January 2022 CRSS'
crss_scenName = 'Jan 2022 CRSS'
crmms_nm = 'Apr 2022 CRMMS-ESP'

## Read historical data from hdb
sdis <- c("Mead.Pool Elevation" = 1930, "Powell.Pool Elevation" = 1928)
run_date = "2022-04"
hist_nMons = 7 # keep 7 months before start date
# end_date = format(ym(run_date) + months(23), "%Y-%m")
histStart_date = format(ym(run_date) - months(hist_nMons), "%Y-%m") 

df_hist <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", histStart_date, run_date),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", histStart_date, run_date)
)

## Reorg histrical data
df_hist <- df_hist %>%
  mutate(Variable = names(sdis)[match(sdi, sdis)],
         Year = year(time_step),
         Scenario = 'Historical',
         Trace = 1) %>%
  rename(Date = time_step, Value = value) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(Scenario, Variable, Date, Year, Trace, Value) %>% na.omit()


## -- CRMMS results

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = rep("res.rdf", length(slots)),
  slot = slots, 
  period = rep("asis", length(slots)),
  summary = rep(NA, length(slots)),
  eval = rep(NA, length(slots)),
  t_s = rep(NA, length(slots)),
  variable = slots,
  stringsAsFactors = FALSE
))

# read in data
scen_res <- rdf_aggregate(  
  agg = rwa1, 
  rdf_dir = data_dir,
  keep_cols = 'Unit')

df_crmms <- data.table::as.data.table(scen_res)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  mutate(Scenario = crmms_nm) %>%
  dplyr::select(Scenario, Variable, Date, Year, Trace = TraceNumber, Value) %>%
  # bring units into line with crss
  mutate(Value = ifelse(Variable %in% c('Mead.Inflow', 'Mead.Storage', "Powell.Outflow",
                                        "Powell.Inflow", "Powell.Storage"),
                        Value * 10^3,
                        Value)) %>%
  filter(Trace %in% 4:33)

# add historical data from run date to connect
df_crmms_con = df_hist %>% 
  filter(Date == format(ym(run_date) - months(1), "%Y-%m")) %>%
  mutate(Scenario = crmms_nm)
df_crmms = rbind(df_crmms, df_crmms_con)

df_units = data.table::as.data.table(scen_res) %>%
  select(Variable, Unit) %>% distinct()

## --- Jan 22 CRMSS-ESP for CRSS Jan projection - read in data
scen_res <- rdf_aggregate(
  agg = rwa1,
  rdf_dir = crmms_dir_jan,
  keep_cols = 'Unit')

df_crmms_22 <- data.table::as.data.table(scen_res)  %>%
  filter(Year == 2022) %>%
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  mutate(Scenario = crss_scenName) %>%
  dplyr::select(Scenario, Variable, Date, Year, Trace = TraceNumber, Value) %>%
  # bring units into line with crss
  mutate(Value = ifelse(Variable %in% c('Mead.Inflow', 'Mead.Storage', "Powell.Outflow",
                                        "Powell.Inflow", "Powell.Storage"),
                        Value * 10^3,
                        Value))

## -- CRSS results
df_crss <- readRDS(file.path(crss_dir, "CRSS_Jan2022_CRMMStraces4to33_lots.rds")) %>%
  mutate(Trace = paste0(Scenario,'-', Trace),
         Scenario = crss_scenName)

df_crss <- rbind(df_crmms_22, df_crss)

## connect CRSS cloud to historical
df_crss_con = df_hist %>% 
  filter(Date == min(df_crss$Date) - 1/12) %>%
  mutate(Scenario = crss_scenName)
df_crss = rbind(df_crss, df_crss_con)


## -- Combine and process
df_all <- rbind(df_hist, df_crmms, df_crss) %>%
  filter(year(Date) <= 2026)

df_all2 <- df_all %>%
  group_by(Date, Scenario, Variable) %>%
  summarise(mdl.max = max(Value),
            mdl.90 = quantile(Value, .9), 
            mdl.50 = quantile(Value, .5), 
            mdl.10 = quantile(Value, .1), 
            mdl.min = min(Value)) %>%
  ungroup() %>%
  select(Scenario, Variable, Date, mdl.max, mdl.90, mdl.50, mdl.10, mdl.min) #%>%

df_Cloud = df_all2 %>%
  filter(Scenario != 'Historical') %>%
  select(-c("mdl.90", "mdl.50", "mdl.10")) %>%
  mutate(Cloud = factor(paste(Scenario, 'Range')))

df_Stat = df_all2 %>%
  pivot_longer(cols = c("mdl.90", "mdl.50", "mdl.10"), names_to = 'Trace') %>%
  mutate(Cloud = factor(paste(Scenario, 'Range')),
         Trace = factor(Trace, 
                        levels = c("mdl.10", "mdl.50", "mdl.90"),
                        labels = c("10%", "50%", "90%"))) %>% 
  filter(Scenario != crss_scenName)

## -- Setup plot
nms = c('Historical', crmms_nm, crss_scenName)
# custom_Tr_col <- c(rep('#f1c40f', 3), rep('#8077ab', 3))
custom_col = c("grey30", "#f2ca27", "gray80")   #c('#cbbedd', '#fdcd9e')
names(custom_col) <- nms
custom_line <- custom_col[-3]
custom_cloud <- custom_col[-1] 
names(custom_cloud) <- paste(names(custom_cloud), "Range")

custom_size <- rep(c(1,1.15,1), 2)
custom_lt <- rep(c(2,1,4), 2)
xrange =  range(df_Stat$Date)
breaks_x = df_Stat %>%
  filter(month(Date) %in% c(4,8,12)) %>%
  select(Date) %>% distinct()



## loop through slots
for (i in 1:2) {
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
  gg <-
    ggplot() + 
    geom_line(data = filter(df_Stat, Variable == slot_i),
              aes(x = Date, y = value, color = Scenario, linetype = Trace, size = Trace)) +
    scale_color_manual(values = custom_line) +
    scale_linetype_manual(values = custom_lt) +
    scale_size_manual(values = custom_size) +
    geom_ribbon(data = filter(df_Cloud, Variable == slot_i), 
                aes(x = Date, ymin = mdl.min, ymax = mdl.max, fill = Cloud), 
                alpha = 0.35) +
    scale_fill_manual(values = custom_cloud)  +
    scale_x_yearmon(expand = c(0,0), breaks = breaks_x$Date,
                    minor_breaks = unique(df_Stat$Date),
                    limits = c(min(df_Stat$Date), max(df_Stat$Date))) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      y = ylabPE, x = NULL, 
      color = NULL, linetype = NULL, size = NULL, fill = NULL,
      title = plot_title[i],
      subtitle = sub_title
    ) +
    geom_vline(
      xintercept = as.yearmon(c("Dec 2021", "Dec 2022", "Dec 2023",  
                                "Dec 2024",  "Dec 2025", "Dec 2026", "Dec 2027")), 
      size = 0.75, color = "gray70"
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
      legend.key.width = unit(1.2, "cm"),
      plot.title = element_text(face="bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 13)
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
    # print(g)
  } else if (slot_i == 'Mead.Pool Elevation') {
    g <- gg %>%
      add_mead_tiers(xrange)
    # print(g)
  } else {
    g <- gg
    # print(gg)
  }
  
  crssplot:::add_logo_vertical(g, .87, .01, .97, .12) # add usbr logo
  
  ggsave(file.path(fig_dir, paste0(slot_i, "_mdlComp_CRSS.v.CRMMS.png")), 
         width = 11, height = 9)
}
