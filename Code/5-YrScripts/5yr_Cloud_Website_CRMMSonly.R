# ============================================================================
# CRMMS-ESP 5-Year projections for Website
#    CRMMS current projections only   
#
# ============================================================================
rm(list=ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(rhdb)

## -- Inputs
# Directory name - all directories stored in Scenario/ folder 
scenario_dir <- c(
  "2023-04_ESP"
  # "2023-01_ESP"
)
# Name when plotting
scenarios <- c(
  "April 2023"
  # "January 2023"
)
fig_dir_nm <- '2023-04_5yrCompare_v2'
# ^ script will create directory in Results/ with this name if it doesn't exist

## Plotting Inputs
sub_title = 'April 2023 CRMMS-ESP Projection'
# crmms_nm = 'August 2022 CRMMS-ESP'
run_date = "2023-04"
max_yr = 2027

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))

## plot info
slots = c("Mead.Pool Elevation", "Powell.Pool Elevation") 
plot_title = c('Lake Mead End-of-Month Elevations', 
               'Lake Powell End-of-Month Elevations')

## Read historical data from hdb
sdis <- c("Mead.Pool Elevation" = 1930, "Powell.Pool Elevation" = 1928)
hist_nMons = 7 # keep 7 months before start date
histStart_date = format(ym(run_date) - months(hist_nMons), "%Y-%m") 

df_hist <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", histStart_date, run_date),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", histStart_date, run_date)
)

## Reorg historical data
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
slots = names(sdis)
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


df_crmms <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios)) %>%
  dplyr::select(Scenario, Variable, Date, Year, Trace = TraceNumber, Value) 

# add historical data from run date to connect
df_crmms_con = df_hist %>% 
  filter(Date == format(ym(run_date) - months(1), "%Y-%m")) %>%
  mutate(Scenario = scenarios[1]) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios)) 
df_crmms = rbind(df_crmms, df_crmms_con)

df_units = data.table::as.data.table(df) %>%
  select(Variable, Unit) %>% distinct()


## -- Combine and process
df_all <- rbind(df_hist, df_crmms) %>%
  filter(year(Date) <= max_yr)

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
                        labels = c("10%", "50%", "90%"))) #%>% 


## -- Setup plot
nms = c('Historical', scenarios)
if (length(scenarios) <= 2) {
  custom_col = c("grey30", "#f2ca27", '#8077ab')[1:(length(scenarios)+1)]
} else {
  custom_col = c("grey30", "#f2ca27",  scales::hue_pal()(length(scenarios)-1)) 
}
names(custom_col) <- nms
custom_line <- custom_col#[-3]
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
  
  ## data
  df_plot = filter(df_Stat, Variable == slot_i)
  df_cloud_plot = filter(df_Cloud, Variable == slot_i)
  ylimmin = min(df_cloud_plot$mdl.min)*0.97
  ylimmax = max(df_cloud_plot$mdl.max)*1.01
  ylimmin = ifelse(slot_i == 'Powell.Pool Elevation' & ylimmin < 3370, 3370, 
                   ifelse(slot_i == 'Mead.Pool Elevation' & ylimmin < 950, 895,
                          ylimmin))
  ylimmax = ifelse(slot_i == 'Powell.Pool Elevation' & ylimmax > 3712, 3712, 
                   ifelse(slot_i == 'Mead.Pool Elevation' & ylimmax > 1250, 1025,
                          ylimmax))
  
  ## Plot slot
  gg <-
    ggplot() + 
    geom_line(data = df_plot,
              aes(x = Date, y = value, color = Scenario, linetype = Trace, size = Trace)) +
    scale_color_manual(values = custom_line) +
    scale_linetype_manual(values = custom_lt) +
    scale_size_manual(values = custom_size) +
    geom_ribbon(data = df_cloud_plot,
                aes(x = Date, ymin = mdl.min, ymax = mdl.max, fill = Cloud),
                alpha = 0.35) +
    scale_fill_manual(values = custom_cloud)  +
    geom_rect(aes( 
      xmin=as.yearmon("Dec 2026"),
      xmax=max(df_plot$Date),
      ymin=ylimmin, ymax=ylimmax), fill = "gray", alpha = 0.5) +
    scale_x_yearmon(expand = c(0,0), breaks = breaks_x$Date,
                    minor_breaks = unique(df_Stat$Date),
                    limits = c(min(df_Stat$Date), max(df_Stat$Date))) +
    labs(
      y = ylabPE, x = NULL,
      color = NULL, linetype = NULL, size = NULL, fill = NULL,
      title = plot_title[i],
      subtitle = sub_title,
      caption = "1 - For modeling purposes, simulated years beyond 2026 (shaded region) assume a continuation of the 2007 Interim Guidelines, the 2019 Colorado River Basin Drought\nContingency Plans, and Minute 323, including the Binational Water Scarcity Contingency Plan. Except for certain provisions related to ICS recovery and Upper Basin\ndemand management, operations under these agreements are in effect through 2026. Reclamation anticipates beginning a process in 2023 to develop operations\nfor post-2026, and the modeling assumptions described here are subject to change for the analysis to be used in that process."
    ) +
    geom_vline(
      xintercept = as.yearmon(c("Dec 2021", "Dec 2022", "Dec 2023",
                                "Dec 2024",  "Dec 2025", "Dec 2027")),
      size = 0.75, color = "gray70"
    ) +
    geom_vline(
      xintercept = as.yearmon("Dec 2026"), size = 0.75, color = "gray40"
    ) +
    annotate("text", x = as.yearmon("Nov 2026"), y = (ylimmax - ylimmin)/15 + ylimmin,
             label = bquote('Current policies expire in '~2026^1), hjust = 1) +
    geom_segment(aes(x = as.yearmon("Aug 2025"), y = (ylimmax - ylimmin)/26 + ylimmin, 
                     xend = as.yearmon("Nov 2026"), yend = (ylimmax - ylimmin)/26 + ylimmin),
                 arrow = arrow(length = unit(0.25, "cm"))) +
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
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      plot.caption = element_text(hjust = 0, size = 8)
    )
  
  # add tiers to Powell and Mead PE
  if (slot_i == 'Powell.Pool Elevation') {
    g <- gg %>%
      add_powell_tiers(xrange) +
      scale_y_continuous(
        labels = scales::comma, breaks = y_breaks, minor_breaks = y_breaks2,
        limits = c(ylimmin, ylimmax), expand = c(0,0), 
        sec.axis = sec_axis(
          ~CRSSIO::elevation_to_storage(., "powell"),
          breaks = CRSSIO::elevation_to_storage(y_breaks, "powell"),
          labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
          name = "Storage (maf)"
        )
      )
  } else if (slot_i == 'Mead.Pool Elevation') {
    g <- gg %>%
      add_mead_tiers(xrange) +
      scale_y_continuous(
        labels = scales::comma, breaks = y_breaks, minor_breaks = y_breaks2,
        expand = c(0,0), limits = c(ylimmin, ylimmax),
        sec.axis = sec_axis(
          ~CRSSIO::elevation_to_storage(., "mead"),
          breaks = CRSSIO::elevation_to_storage(y_breaks, "mead"),
          labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
          name = "Storage (maf)"
        ))
  } else {
    g <- gg
  }
  
  crssplot:::add_logo_vertical(g, .87, .01, .97, .12) # add usbr logo
  
  ggsave(file.path(fig_dir, paste0(slot_i, "_mdlComp_CRSS.v.CRMMS.png")), 
         width = 11, height = 9.5)
}
# Warnings expected 