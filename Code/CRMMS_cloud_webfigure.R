# ============================================================================
# Cloud plot for 24-MS and CRMMS-ESP Results
#   Figures for 2-Year Probabilistic Projections webpage 
#   
#   Steps: Open CRMMS.Rproj; Read messages (run renv::restore() if prompted to);
#          Update Inputs (run_date, MRIDs for min/most/max); Run script
# ==============================================================================
rm(list=ls())

# libraries (when opening Rproj)
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(rhdb)
library(crssplot)

### --- Inputs

## 24-MS MRIDs & Date - UPDATE!
run_date = c('2022-01')
most_mrid <- 3173 
min_mrid <- 3174
max_mrid <- 3175

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
data_dir = 'Output Data'  # path from current CRMMS directory
data_fl = 'CRMMS_EnsembleOutput.xlsm'


### --- Process Data

## SDIs / slots / 24MS trace info
sdis <- c("Mead.Pool Elevation" = 1930, "Powell.Pool Elevation" = 1928)
slots <- names(sdis)
mrid_to_trace <- c("24MS Min", "24MS Max", "24MS Most")
names(mrid_to_trace) <- c(min_mrid, max_mrid, most_mrid)

## CRMMS-ESP - Read in slots
setwd(data_dir)
dfi <- NULL
for (j in 1:length(slots)) {
  dfj = read_excel(data_fl, sheet = slots[j], skip = 2)[,1:34] %>% na.omit()
  dfi = rbind.data.frame(dfi, 
                         cbind.data.frame(run = run_date, slot = slots[j], 
                                          dfj))
}

## Reorg ESP data
esp_traces = paste('ESP', 1991:2020)
colnames(dfi)[7:36] <- esp_traces
colnames(dfi)[3] <- "Date"
df_full = dfi %>% 
  mutate(Date = ceiling_date(Date + month(1), "month") - days(1)) %>%
  # keep first 24 months of data
  filter(Date < min(Date) + months(24)) %>%
  mutate(Date = as.yearmon(Date)) %>%
  # drop the min/most/max columns from CRMMS-Ensemble Mode
  dplyr::select(-Trace1, -Trace2, -Trace3)

## Historical data info / end date
hist_nMons = 7 # keep 7 months before start date
end_date = format(ym(run_date) + months(23), "%Y-%m")
histStart_date = format(ym(run_date) - months(hist_nMons), "%Y-%m") 

## Read 24-MS data from hdb - no vpn needed
df_hdb <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", run_date, end_date, most_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", run_date, end_date, most_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", run_date, end_date, min_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", run_date, end_date, min_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", run_date, end_date, max_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", run_date, end_date, max_mrid)
)

## Reorg 24-MS data
df_hdb <- df_hdb %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = run_date,
         Trace = mrid_to_trace[as.character(mrid)]) %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid)

## Read historical data from hdb
df_hist <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", histStart_date, run_date),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", histStart_date, run_date)
)

## Reorg histrical data
df_hist <- df_hist %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = run_date,
         Trace = 'Historical') %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid) %>% na.omit()

## Add historical data to 24MS df
df_hdb = rbind(df_hdb, df_hist)

# ## Save HDB data if needed
# fl = file.path(paste0(run_date, '_24MS.rds'))
# if (!file.exists(fl)) {
#   saveRDS(df_hdb, fl)
# }

## Cloud inputs - naming
cloud_name = 'CRMMS-ESP Projections Range'
cloud_model = 'CRMMS' 

## Cloud stats from CRMMS-ESP
df_stat = df_full %>%
  pivot_longer(cols = esp_traces, names_to = 'Trace') %>%
  group_by(run, slot, Date) %>%
  summarise(cloud.max = max(value),
            cloud.min = min(value)) %>%
  mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date)) 

## ESP traces for figs
df_24MS = df_full %>% 
  select(c("slot", "Date", all_of(esp_traces))) %>%
  pivot_longer(cols = all_of(esp_traces), names_to = 'Trace') %>%
  # add in the 24MS data
  bind_rows(df_hdb) %>%
  mutate(Cloud = factor(cloud_name)) 

## Connect historical data and initial conditions
df_init = df_24MS %>% 
  filter(Date == run_date) %>% 
  select(-value, -Date) %>% distinct()
df_hist2 = df_hist %>% 
  filter(Date == max(Date)) %>%
  select(slot, Date, value)
df_initAdd = left_join(df_init, df_hist2, by = 'slot')
df_24MS <- rbind(df_24MS, df_initAdd)

# add historical to the fill data too
df_stat <- bind_rows(
  df_stat,
  df_hist2 %>% 
    mutate('cloud.min' = value) %>% 
    rename('cloud.max' = value) %>% 
    mutate(run = run_date, Cloud = cloud_name)
)


### ----------------- PLOTTING

## naming for figures
esp_label <- "CRMMS-ESP Projections \n(30 projections)"
lab_names <- c("24-Month Study Minimum Probable", 
               "24-Month Study Maximum Probable", 
               "24-Month Study Most Probable",
               "Historical",
               rep(esp_label, 35))

names(lab_names) <- c("24MS Min", "24MS Max", "24MS Most", "Historical", 
                      esp_traces)
nn <- lab_names[1:5]

## Min, Max, Most, ESP is order of these colors, size, linetype
custom_colors <- c('#DA3139', '#104E8B', '#26AE44', 'grey20', 'grey43')
custom_size <- c(rep(1.2, 3), 1, 0.5)
custom_lt <- c(rep(2, 3), 1, 1)
custom_alpha <- c(rep(1, 4), 0.35)
names(custom_colors) <- names(custom_size) <- names(custom_lt) <- 
  names(custom_alpha) <- nn
cloud_color <- 'grey85'

## Filter data for res plots
df_stat_p = df_stat %>% filter(slot == 'Powell.Pool Elevation')
df_stat_m = df_stat %>% filter(slot == 'Mead.Pool Elevation')
df_24MS_p = df_24MS %>% filter(slot == 'Powell.Pool Elevation') %>%
  mutate(trace_labels = lab_names[Trace])
df_24MS_m = df_24MS %>% filter(slot == 'Mead.Pool Elevation') %>%
  mutate(trace_labels = lab_names[Trace])

## Powell tier for figures
Timestep = matrix(seq.Date(as.Date('2007-10-1'), 
                           as.Date('2026-09-1'), 'months') - 1,
                  byrow = T, ncol = 12)
Timestep = as.Date(as.vector(t(cbind(Timestep[,1], Timestep))), 
                   origin = '1970-01-01')[2:length(Timestep)]
powell_line = data.frame(
  Timestep = as.POSIXct(Timestep),
  Eq_elev = rep(c(3636, 3639, 3642, 3643, 3645, 3646, 3648, 3649, 3651, 3652, 
                  3654, 3655, 3657, 3659, 3660, 3662, 3663, 3664, 3666),
                each=13)[1:227],
  UEB = 3575,
  `Run Date` = as.Date('2008-01-31'), check.names = F,
  stringsAsFactors = FALSE) %>%
  mutate(Cloud = factor(cloud_name), Timestep = as.yearmon(Timestep)) 

## Powell --------------------
p_breaks <- seq(3350, 3725, 25)
p_breaks2 <- seq(3350, 3725, 5)
yy <- NULL #c(3400, 3675) # NULL for default ylim
gg <- 
  ggplot(df_stat_p, aes(x = Date, fill = Cloud)) +
  scale_fill_discrete(type = cloud_color) +
  geom_ribbon(aes(ymin = cloud.min, ymax = cloud.max), alpha = 0.3) +
  geom_line(data = filter(df_24MS_p, trace_labels == esp_label), 
            aes(x = Date, y = value, color = trace_labels, 
                alpha = trace_labels, group = Trace,
                linetype = trace_labels, size = trace_labels)) +
  geom_line(data = filter(df_24MS_p, trace_labels != esp_label), 
            aes(x = Date, y = value, color = trace_labels, 
                alpha = trace_labels, group = Trace,
                linetype = trace_labels, size = trace_labels)) +
  
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_lt) +
  scale_size_manual(values = custom_size) +
  scale_alpha_manual(values = custom_alpha) +
  scale_x_yearmon(expand = c(0,0), breaks = unique(df_24MS$Date),
                  minor_breaks = unique(df_24MS$Date),
                  limits = c(min(df_24MS$Date), max(df_24MS$Date))) +
  scale_y_continuous(
    labels = scales::comma, breaks = p_breaks, minor_breaks = p_breaks2,
    limits = yy,
    sec.axis = sec_axis(
      ~CRSSIO::elevation_to_storage(., "powell"),
      breaks = CRSSIO::elevation_to_storage(p_breaks, "powell"),
      labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
      name = "Storage (maf)"
    )
  ) +
  labs(
    y = "Pool Elevation (ft)", x = NULL, 
    color = NULL, linetype = NULL, size = NULL, fill = NULL,
    title = 'Lake Powell End-of-Month Elevations',
    subtitle = paste(cloud_model, 'Projections from', format(ym(run_date), "%B %Y"))
  ) +
  # tier stuff
  geom_hline(
    yintercept = c(3575, 3525), 
    color = 'black', linetype = 'solid'
  ) +
  geom_hline(yintercept = 3490, color = 'grey20', linetype = 2) +
  #geom_hline(yintercept = 3490, colour = '#800000', size = 1) +
  geom_vline(
    xintercept = as.yearmon(c("Dec 2021", "Dec 2022", "Dec 2023")), 
    size = 1, color = "#ffdc70",  #"#ffdc70" or "grey45"
    alpha = 0.8
  ) +
  geom_line(
    data = powell_line, 
    aes(x = Timestep, y=Eq_elev), 
    colour = "black", linetype = 1
  ) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=3670, label="Equalization Tier (ET)", angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=3620, label="Upper Elevation Balancing Tier\n(3,575' to ET)", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=3544, label="Mid-Elevation Release Tier\n(3,525' to 3,575')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=3510, label="Lower Elevation Balancing Tier\n(<3,525')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=3477, label="Minimum Power Pool\n(3,490')", 
           angle=00, size=3, hjust = 0) +
  theme_bw(base_size = 14) +
  guides(alpha = 'none',
         color = guide_legend(nrow = 3, order = 1),
         linetype = guide_legend(nrow = 3, order = 1),
         size = guide_legend(nrow = 3, order = 1),
         fill = guide_legend(order = 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    plot.margin = unit(c(0.1,0.1,1,0.1), "cm"),
    plot.title = element_text(face="bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 13)
  )

crssplot:::add_logo_vertical(gg, .87, .01, .97, .12) # add usbr logo

ggsave("crmmsCloud_powell.png", 
       width = 11, height = 8)

## Mead -------------------------
m_breaks <- seq(900, 1250, 25)
m_breaks2 <- seq(900, 1250, 5)
yy <- c(1000, 1150) # NULL for default ylimit
gg <-
  ggplot(df_stat_m, aes(x = Date, fill = Cloud)) +
  scale_fill_discrete(type = cloud_color) +
  geom_ribbon(aes(ymin = cloud.min, ymax = cloud.max), alpha = 0.3) +
  geom_line(data = filter(df_24MS_m, trace_labels == esp_label), 
            aes(x = Date, y = value, color = trace_labels, 
                alpha = trace_labels, group = Trace,
                linetype = trace_labels, size = trace_labels)) +
  geom_line(data = filter(df_24MS_m, trace_labels != esp_label), 
            aes(x = Date, y = value, color = trace_labels, 
                alpha = trace_labels, group = Trace,
                linetype = trace_labels, size = trace_labels)) +
  
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_lt) +
  scale_size_manual(values = custom_size) +
  scale_alpha_manual(values = custom_alpha) +
  scale_x_yearmon(expand = c(0,0), breaks = unique(df_24MS$Date),
                  minor_breaks = unique(df_24MS$Date),
                  limits = c(min(df_24MS$Date), max(df_24MS$Date))) +
  scale_y_continuous(
    labels = scales::comma, breaks = m_breaks, minor_breaks = m_breaks2,
    limits = yy, expand = c(0,0),
    sec.axis = sec_axis(
      ~CRSSIO::elevation_to_storage(., "mead"),
      breaks = CRSSIO::elevation_to_storage(m_breaks, "mead"),
      labels = scales::comma_format(scale = 1/1000000, accuracy = 0.01),
      name = "Storage (maf)"
    )
  ) +
  labs(y = "Pool Elevation (ft)", x = NULL, color = NULL, linetype = NULL,
       size = NULL,
       fill = NULL,
       title = 'Lake Mead End-of-Month Elevations',
       subtitle = paste(cloud_model, 'Projections from', format(ym(run_date), "%B %Y"))) +
  # tier stuff
  geom_hline(
    yintercept = c(1110, 1090, 1045), 
    colour = 'black', linetype = 'dashed'
  ) +
  geom_hline(yintercept = c(1145, 1075, 1050, 1025), color = "black", linetype = 1) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1147.5, label="Surplus Condition (>1,145')", angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1125, label="Normal Condition\n(1,075' to 1,145')", 
           angle=00, size=3, hjust = 0) +
  # annotate("text", x = as.yearmon(ym(run_date) - months(5)),
  #          y=1083, label="Drought Contingency Plan Contributions\n(<1,090')", 
  #          angle=00, size=3, hjust = 0) +
  # annotate("text", x = as.yearmon(ym(run_date) - months(6)),
  #          y=1063, label="Shortage Condition\n(<1,075')", 
  #          angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1063, label="Level 1 Shortage Condition\n(1,050' to 1,075')",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1037, label="Level 2 Shortage Condition\n(1,025' to 1,050')",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1015, label="Level 3 Shortage Condition\n(<1,025')",
           angle=00, size=3, hjust = 0) +
  geom_vline(
    xintercept = as.yearmon(c("Dec 2021", "Dec 2022")),
    size = 1, color = "#ffdc70",  #"#ffdc70" or "grey45"
    alpha = 0.8
  ) +
  theme_bw(base_size = 14) +
  guides(alpha = 'none',
         color = guide_legend(nrow = 3, order = 1),
         linetype = guide_legend(nrow = 3, order = 1),
         size = guide_legend(nrow = 3, order = 1),
         fill = guide_legend(order = 3)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    legend.position = "bottom",
    legend.key.width = unit(1.2, "cm"),
    plot.margin = unit(c(0.1,0.1,1,0.1), "cm"),
    plot.title = element_text(face="bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 13)
  )

crssplot:::add_logo_vertical(gg, .87, .01, .97, .12) # add usbr logo

ggsave("crmmsCloud_mead.png", 
       width = 11, height = 8)
