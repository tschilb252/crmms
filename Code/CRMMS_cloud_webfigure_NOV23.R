# ============================================================================
# Cloud plot for 24-MS and CRMMS-ESP Results
#   Figures for 2-Year Probabilistic Projections webpage 
#   
#   Steps: Open CRMMS.Rproj; Read messages (run renv::restore() if prompted to);
#          Update Inputs (most_run_date, MRIDs for min/most/max); Run script
# ==============================================================================
rm(list=ls())

# libraries (when opening Rproj)
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(rhdb)
library(crssplot)
library(magick)
source(file.path('Code', 'add_MeadPowell_tiers.R'))

### --- Inputs

## 24-MS MRIDs & Date - UPDATE!
most_mrid <- 3239 
min_mrid <- 3240
max_mrid <- 3238

## 24MS Run Date - UPDATE!
most_run_date = c('2023-11')
min_run_date = c('2023-11')
max_run_date = c('2023-10') 

## Directories & Data
# * opening CRMMS.Rproj will set your working directory to Rproj location
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
data_dir = file.path('Output Data') # path from current CRMMS directory
data_fl = 'CRMMS_EnsembleOutput.xlsm'


### --- Process Data

## SDIs / slots / 24MS trace info
sdis <- c("Mead.Pool Elevation" = 1930, "Powell.Pool Elevation" = 1928)
slots <- names(sdis)
mrid_to_trace <- c("24MS Min", "24MS Max", "24MS Most")
mrid_to_runDate <- c(min_run_date, max_run_date, most_run_date)
names(mrid_to_trace) <- names(mrid_to_runDate) <- 
  c(min_mrid, max_mrid, most_mrid)

## CRMMS-ESP - Read in slots
data_fl_in = file.path(data_dir, data_fl)
dfi <- NULL
for (j in 1:length(slots)) {
  dfj = read_excel(data_fl_in, sheet = slots[j], skip = 2)#[,c(1,5:34)] %>% na.omit()
  col_keep = colnames(dfj)[c(1,(ncol(dfj)-29):ncol(dfj))] # keep only ESP (last 30 traces)
  dfi = rbind.data.frame(dfi, 
                         cbind.data.frame(run = most_run_date, slot = slots[j], 
                                          dfj[col_keep]))
}

## Reorg ESP data
esp_traces = paste('ESP', 1991:2020)
colnames(dfi)[4:33] <- esp_traces
colnames(dfi)[3] <- "Date"
df_full = dfi %>% na.omit() %>%
  mutate(Date = ceiling_date(Date + month(1), "month") - days(1)) %>%
  # keep first 24 months of data
  filter(Date < min(Date) + months(24)) %>%
  mutate(Date = as.yearmon(Date)) 

## Historical data info / end date
hist_nMons = 7 # keep 7 months before start date
end_date = format(ym(most_run_date) + months(23), "%Y-%m")
histStart_date = format(ym(most_run_date) - months(hist_nMons), "%Y-%m") 

## Read 24-MS data from hdb - no vpn needed
df_hdb <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", most_run_date, end_date, most_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", most_run_date, end_date, most_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", min_run_date, end_date, min_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", min_run_date, end_date, min_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", most_run_date, end_date, max_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", most_run_date, end_date, max_mrid)
)

## Reorg 24-MS data
df_hdb <- df_hdb %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = mrid_to_runDate[as.character(mrid)],
         Trace = mrid_to_trace[as.character(mrid)]) %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid)

## Read historical data from hdb
df_hist <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", histStart_date, most_run_date),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", histStart_date, most_run_date)
)

## Reorg histrical data
df_hist <- df_hist %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = most_run_date,
         Trace = 'Historical') %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid) %>% na.omit()

## Add historical data to 24MS df
df_hdb = rbind(df_hdb, df_hist)

# ## Save HDB data if needed
# fl = file.path(paste0(most_run_date, '_24MS.rds'))
# if (!file.exists(fl)) {
#   saveRDS(df_hdb, fl)
# }

## Cloud inputs - naming
cloud_name = 'CRMMS-ESP Projections Range'
cloud_model = 'CRMMS' 

## Cloud stats from CRMMS-ESP
# This table could be better summarized then output to .csv for excel for River Ops purposes

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
  mutate(run = most_run_date) %>%
  # add in the 24MS data
  bind_rows(df_hdb) %>%
  mutate(Cloud = factor(cloud_name)) 

## Connect historical data and initial conditions
df_init = df_24MS %>% 
  # not connecting the max probable to start date for now & run == most_run_date
  filter(Date == most_run_date) %>% 
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
    mutate(run = most_run_date, Cloud = cloud_name)
)

### ----------------- PLOTTING

# labeling 24-MS lines
maxLab_droa = T #ifelse(month(ym(max_run_date)) %in% c(1,4,8), T, F)
minLab_droa = F #ifelse(month(ym(min_run_date)) %in% c(1,4,8), T, F)

## naming for figures
esp_label <- "CRMMS-ESP Projection \n(30 traces)"
lab_names <- c(paste(format(ym(max_run_date), "%B %Y"),
                     ifelse(maxLab_droa, 'Probable Maximum ', "DROA Probable Maximum"), 
                     "24-Month Study"), 
               paste(format(ym(most_run_date), "%B %Y"),"Most Probable 24-Month Study"),
               paste(format(ym(min_run_date), "%B %Y"), 
                     ifelse(minLab_droa, 'Probable Minimum', "DROA Probable Minimum"),
                     "24-Month Study"), 
               "Historical",
               rep(esp_label, 30))

names(lab_names) <- c("24MS Max", "24MS Most", "24MS Min","Historical", 
                      esp_traces)
nn <- lab_names[1:5]

# plot subtitle
mons_run = unique(c(format(ym(max_run_date), "%B"), 
                    format(ym(min_run_date), "%B"),
                    format(ym(most_run_date), "%B")))
subtitle_in = paste(cloud_model, 'Projections from', paste(mons_run, collapse = ' and '),  
                    format(ym(min_run_date), "%Y"))


## Min, Max, Most, ESP is order of these colors, size, linetype
custom_colors <- c( '#104E8B', '#26AE44','#DA3139', 'grey20', 'grey43')
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
powell_line = eq_tier_df() %>%
  mutate(Cloud = factor(cloud_name))

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
    title = bquote('Lake Powell End-of-Month'~Elevations),
    subtitle = subtitle_in ) + #paste(cloud_model, 'Projections from', 'April and May 2023'),
    #caption = bquote(' '^1~'Projected Lake Powell end-of-month physical elevations from the latest CRMMS-ESP and 24-Month Study inflow scenarios.                  ')
 
  # tier stuff
  geom_hline(
    yintercept = c(3575, 3525), 
    color = 'black', linetype = 'solid'
  ) +
  geom_hline(yintercept = 3490, color = 'grey20', linetype = 2) +
  geom_vline(
    xintercept = as.yearmon(c("Dec 2022", "Dec 2023", "Dec 2024")), 
    size = 1, color = "#ffdc70",  #"#ffdc70" or "grey45"
    alpha = 0.8
  ) +
  geom_line(
    data = powell_line,
    aes(x = Date, y=Eq_elev),
    colour = "black", linetype = 1
  ) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3670, label="Equalization Tier (ET)", angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3620, label="Upper Elevation Balancing Tier\n(3,575' to ET)", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3544, label="Mid-Elevation Release Tier\n(3,525' to 3,575')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3510, label="Lower Elevation Balancing Tier\n(<3,525')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=3477, label="Minimum Power Pool\n(3,490')", 
           angle=00, size=3, hjust = 0) +
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
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

#crssplot:::add_logo_vertical(gg, .87, .01, .97, .12) # add usbr logo

ggsave(file.path(data_dir, "crmmsCloud_powell_wologo.png"),
       width = 11, height = 8)

# ggsave("C:/Temp/crmmsCloud_powell.png", 
#        width = 11, height = 8)

crmms_p <- image_read(file.path(data_dir, "crmmsCloud_powell_wologo.png"))
logo_raw <- image_read("https://www.usbr.gov/lc/region/g4000/BofR-vert.png")
test_plot <- image_composite(crmms_p,image_resize(logo_raw,"250"),offset = "+2910+2110")
image_write(test_plot, file.path(data_dir, "crmmsCloud_powell.png"))

## Mead -------------------------
m_breaks <- seq(900, 1250, 25)
m_breaks2 <- seq(900, 1250, 5)
yy <- c(1000, 1125) # NULL for default ylimit
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
       title = bquote('Lake Mead End-of-Month'~Elevations),
       subtitle = subtitle_in) + #paste(cloud_model, 'Projections from', 'April 2023'),
       #caption = bquote(' '^1~'Projected Lake Mead end-of-month physical elevations from the latest CRMMS-ESP and 24-Month Study inflow scenarios.                  ')
  # tier stuff
  geom_hline(
    yintercept = c(1110, 1090, 1045), 
    colour = 'black', linetype = 'dashed'
  ) +
  geom_hline(yintercept = c(1145, 1075, 1050, 1025), color = "black", linetype = 1) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=1147.5, label="Surplus Condition (>1,145')", angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=1100, label="Normal Condition\n(1,075' to 1,145')", 
           angle=00, size=3, hjust = 0) +
  # annotate("text", x = as.yearmon(ym(most_run_date) - months(5)),
  #          y=1083, label="Drought Contingency Plan Contributions\n(<1,090')", 
  #          angle=00, size=3, hjust = 0) +
  # annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
  #          y=1063, label="Shortage Condition\n(<1,075')", 
  #          angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=1060, label="Level 1 Shortage Condition\n(1,050' to 1,075')",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=1037, label="Level 2 Shortage Condition\n(1,025' to 1,050')",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(most_run_date) - months(6)),
           y=1015, label="Level 3 Shortage Condition\n(<1,025')",
           angle=00, size=3, hjust = 0) +
  geom_vline(
    xintercept = as.yearmon(c("Dec 2022", "Dec 2023", "Dec 2024")),
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
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

#crssplot:::add_logo_vertical(gg, .87, .01, .97, .12) # add usbr logo

ggsave(file.path(data_dir, "crmmsCloud_mead_wologo.png"),
       width = 11, height = 8)

# ggsave("C:/Temp/crmmsCloud_powell.png", 
#        width = 11, height = 8)

crmms_p <- image_read(file.path(data_dir, "crmmsCloud_mead_wologo.png"))
logo_raw <- image_read("https://www.usbr.gov/lc/region/g4000/BofR-vert.png")
test_plot <- image_composite(crmms_p,image_resize(logo_raw,"250"),offset = "+2910+2110")
image_write(test_plot, file.path(data_dir, "crmmsCloud_mead.png"))