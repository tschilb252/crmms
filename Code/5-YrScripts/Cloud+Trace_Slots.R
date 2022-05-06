# ============================================================================
# Compare CRMMS-ESP slots for different CRMMS scenarios 
#   
#
# ============================================================================
rm(list=ls())

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

## -- Inputs
scenario_dir <- c(
  'Apr2022_v3',
  # 'Apr2022_v2',
  'Apr2022_v1',
  'Mar2022'
  # 'Feb2022',
  # 'Jan2022_OG',
  # 'Jan2022_updatedRegression'
)
scenarios <- c(
  'Apr. 2022 v3',
  # 'Apr. 2022 v2',
  'Apr. 2022 v1',  
  'Mar. 2022'
  # 'Feb. 2022',
  # 'Jan. 2022',
  # 'Jan. 2022 Updated Regres.'
)
fig_dir_nm <- 'Aprv1,3_Mar_Compare'
# ^ script will create directory with this name if it doesn't exist

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Output Data', fig_dir_nm)
data_dir <- file.path('rdfOutput', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))

## Max Date
max_date = '2026-12' #'2024-12'

## Trace Select?
single_Trace <- FALSE

# single_Trace <- TRUE
# sel_trace <- c(6)
# trace_yr = 1991 + sel_trace - 4

slots = c("Mead.Pool Elevation", "Powell.Pool Elevation",
          "Mead.Inflow", "Powell.Inflow",
          "Mead.Outflow", "Powell.Outflow",
          "Mead.Storage", "Powell.Storage",
          # "Mohave.Outflow", "Havasu.Outflow",
          "FlamingGorge.Outflow", "FlamingGorge.Storage",
          "BlueMesa.Outflow", "BlueMesa.Storage",
          "Navajo.Outflow", "Navajo.Storage",
          "PowellInflow.Unregulated")
plot_title = paste('CRMMS-ESP Run Comparison')
file_nm_end <- ifelse(single_Trace, 
                      paste0('_thru', format(ym(max_date), "%Y"), '_', trace_yr),
                      paste0('_thru', format(ym(max_date), "%Y")))

## -- Read in CRMMS results

# slots/agg to read
rwa1 <- rwd_agg(data.frame(
  file = c(rep("res.rdf", length(slots)-1), 'streamflow.rdf'),
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
  trces = unique(scen_res$TraceNumber)
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df, scen_res)
}

df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  filter(Date <= as.yearmon(format(ym(max_date), "%Y-%m"))) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios)) %>%
  # bring units into line with crss
  mutate(Value = ifelse(Variable %in% c('Mead.Inflow', 'Mead.Storage', "Powell.Outflow",
                                        "Powell.Inflow", "Powell.Storage"),
                        Value * 10^3,
                        Value))

df_units = data.table::as.data.table(df) %>%
  select(Variable, Unit) %>% distinct()

## -- add total P+M storage as new slot
slot_add = 'Powell + Mead Storage'
slot_units = 'maf'

df_store = df_scens %>%
  filter(Variable %in% c("Powell.Storage", "Mead.Storage")) %>%
  group_by(Scenario, Date, Trace) %>% 
  summarise(Value = sum(Value)/ 10^6) %>%
  mutate(Variable = slot_add) %>%
  select(colnames(df_scens))

# add to df
df_scens = rbind.data.frame(df_scens, df_store)
slots = c(slots, slot_add)
df_units = rbind.data.frame(df_units,
                            cbind(Variable = slot_add, Unit = slot_units))


## -- Combine and process
if (single_Trace) {
  df_scens <- df_scens %>%
    filter(Trace == sel_trace)
}

df_all <- df_scens %>%
  group_by(Date, Scenario, Variable) %>%
  summarise(mdl.max = max(Value),
            mdl.90 = quantile(Value, .9), 
            mdl.50 = quantile(Value, .5), 
            mdl.10 = quantile(Value, .1), 
            mdl.min = min(Value)) %>%
  ungroup() %>%
  select(Scenario, Variable, Date, mdl.max, mdl.90, mdl.50, mdl.10, mdl.min) #%>%

df_Cloud = df_all %>%
  select(-c("mdl.90", "mdl.50", "mdl.10")) %>%
  mutate(Cloud = factor(paste(Scenario, 'Range')))

df_Stat = df_all %>%
  pivot_longer(cols = c("mdl.90", "mdl.50", "mdl.10"), names_to = 'Trace') %>%
  mutate(Cloud = factor(paste(Scenario, 'Range')),
         Trace = factor(Trace, 
                        levels = c("mdl.10", "mdl.50", "mdl.90"),
                        labels = c("10%", "50%", "90%")))

## -- Setup plot
custom_Tr_col <- scales::hue_pal()(length(scenarios))
custom_cloud <- custom_Tr_col 
custom_size <- rep(c(1,1.15,1), length(scenarios))
custom_lt <- rep(c(2,1,4), length(scenarios))
names(custom_Tr_col) <- scenarios
names(custom_cloud) <- paste(scenarios, 'Range')
xrange =  range(df_Cloud$Date)
xbreaks = df_Cloud %>% filter(month(Date) %in% c(4,8,12)) %>%
  select(Date) %>% distinct()

df_Stat = df_Stat %>% 
  mutate(Scenario = factor(Scenario, levels = scenarios))
df_Cloud = df_Cloud %>% 
  mutate(Scenario = factor(Scenario, levels = scenarios))

## loop through slots
# pdf(file.path(fig_dir, paste0("Compare_monthlyCloud", file_nm_end, ".png")),
#     width = 11, height = 9)
for (i in 1:length(slots)) {
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
  gg <- ggplot() + 
    geom_line(data = filter(df_Stat, Variable == slot_i), 
              aes(x = Date, y = value, color = Scenario, linetype = Trace, size = Trace)) +
    scale_color_manual(values = custom_Tr_col) +
    scale_linetype_manual(values = custom_lt) +
    scale_size_manual(values = custom_size) +
    geom_ribbon(data = filter(df_Cloud, Variable == slot_i), 
                aes(x = Date, ymin = mdl.min, ymax = mdl.max, fill = Cloud), 
                alpha = 0.3) +
    scale_fill_manual(values = custom_cloud)  +
    scale_x_yearmon(expand = c(0,0), breaks = xbreaks$Date,
                    minor_breaks = unique(df_Cloud$Date),
                    limits = c(min(df_Cloud$Date), max(df_Cloud$Date))) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      y = ylabPE, x = NULL, 
      color = NULL, linetype = NULL, size = NULL, fill = NULL,
      title = plot_title,
      subtitle = slot_i
    ) +
    geom_vline(
      xintercept = as.yearmon(c("Dec 2021", "Dec 2022", "Dec 2023",  
                                "Dec 2024",  "Dec 2025", "Dec 2026", "Dec 2027")), 
      size = 0.75, color = "#ffdc70"
    ) +
    bor_theme() +
    guides(alpha = "none",
           color = guide_legend(nrow = length(scenarios), order = 1),
           linetype = "none",
           size = "none",
           fill = "none"
    ) +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 90, vjust = 0.5), 
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm")
    )
  
  # add specific breaks for pool elevation figs - results in a warning
  if (!single_Trace) {
    gg <- gg + 
      guides(alpha = "none",
             color = guide_legend(nrow = length(scenarios), order = 1),
             linetype = guide_legend(nrow = 3, order = 1),
             size = guide_legend(nrow = length(scenarios), order = 1),
             fill = guide_legend(nrow = length(scenarios), order = 2))
  }
  
  # add tiers to Powell and Mead PE
  if (slot_i == 'Powell.Pool Elevation') {
    yrange = filter(df_Stat, Variable == slot_i) %>% 
      mutate(min = min(mdl.min), max = max(mdl.max)) %>% select(min, max) %>% distinct()
    ymin = ifelse(yrange$min < 3500, yrange$min-15, 3490)
    ymax = ifelse(yrange$max > 3575, yrange$max+15, 3575)
    
    g <- gg %>%
      add_powell_tiers(xrange) +
      scale_y_continuous(
        labels = scales::comma, breaks = y_breaks, minor_breaks = y_breaks2,
        limits = c(ymin, ymax), expand = c(0,0))
    print(g)
  } else if (slot_i == 'Mead.Pool Elevation') {
    yrange = filter(df_Stat, Variable == slot_i) %>% 
      mutate(min = min(mdl.min), max = max(mdl.max)) %>% select(min, max) %>% distinct()
    ymin = ifelse(yrange$min < 1000, yrange$min*.99, 1000)
    ymax = ifelse(yrange$max > 1100, yrange$max*1.01, 1100)
    
    g <- gg %>%
      add_mead_tiers(xrange) +
      scale_y_continuous(
        labels = scales::comma, breaks = y_breaks, minor_breaks = y_breaks2,
        limits = c(ymin, ymax), expand = c(0,0))
    print(g)
  } else {
    print(gg)
  }
  
  ggsave(file.path(fig_dir, paste0(slot_i, "_mdlComp_monthlyCloud", file_nm_end, ".png")), 
         width = 8.5, height = 7)
}
# dev.off()