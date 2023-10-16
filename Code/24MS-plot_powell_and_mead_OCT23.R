#######################################################################################################
# packageVersion('ggplot2')

rm(list=ls())
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)
library(rhdb)
library(crssplot)
library(magick)
source(file.path('Code', 'add_MeadPowell_tiers.R'))

## 24-MS MRIDs & Date - UPDATE!
run_date = c('2023-10')

most_mrid <- 3236 
min_mrid <- 3237
max_mrid <- 3238

## 24MS Run Date - UPDATE!
most_run_date = c('2023-10')
min_run_date = c('2023-10')
max_run_date = c('2023-10') 

## UPDATE this to add "DROA" to legend
maxLab_droa = F
minLab_droa = F

cloud_model = 'CRMMS'

# nmonths
wy = ifelse(month(ym(run_date))>9, year(ym(run_date)), year(ym(run_date))-1)
wy_start = c(paste0(wy,"-10"))
wy_end = c(paste0(wy+2,"-09"))


hist_nMons = 7 # keep 7 months before start date
end_date = format(ym(run_date) + months(23), "%Y-%m")
histStart_date = format(ym(run_date) - months(hist_nMons), "%Y-%m")
sdis <- c("Mead.Pool Elevation" = 1930, "Powell.Pool Elevation" = 1928)
slots <- names(sdis)
mrid_to_trace <- c("24MS Min", "24MS Max", "24MS Most")
names(mrid_to_trace) <- c(min_mrid, max_mrid, most_mrid)



df_hdb <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", most_run_date, end_date, most_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", most_run_date, end_date, most_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", min_run_date, end_date, min_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", min_run_date, end_date, min_mrid),
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", most_run_date, end_date, max_mrid),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", most_run_date, end_date, max_mrid)
)

df_hdb <- df_hdb %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = run_date,
         Trace = mrid_to_trace[as.character(mrid)]) %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid)

## Read historical data from hdb
df_hist <- bind_rows(
  hdb_query(sdis["Powell.Pool Elevation"], "uc", "m", histStart_date, most_run_date),
  hdb_query(sdis["Mead.Pool Elevation"], "lc", "m", histStart_date, most_run_date)
)

df_hist <- df_hist %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         run = run_date,
         Trace = 'Historical') %>%
  rename(Date = time_step) %>%
  mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
  select(-sdi, -mrid) %>% na.omit()




## Connect historical data and initial conditions
df_init = df_hdb %>% 
  filter(Date == run_date) %>% 
  select(-value, -Date) %>% distinct()
df_hist2 = df_hist %>% 
  filter(Date == max(Date)) %>%
  select(slot, Date, value)
df_initAdd = left_join(df_init, df_hist2, by = 'slot')
df_24MS <- rbind(df_hdb, df_initAdd)

## Add historical data to 24MS df
df_24MS = rbind(df_24MS, df_hist)
df_24MS = arrange(df_24MS,Trace)



# # ########## Get Powell WY Releases ###########################################################################
# historical <- hdb_query(1920, "uc", "m", wy_start, format(ym(most_run_date) - months(1), "%Y-%m"))
# 
# powRel <- bind_rows(historical,
#                     hdb_query(1920, "uc", "m", most_run_date, wy_end, most_mrid))
# 
# powRelmin <- bind_rows(historical,
#                     hdb_query(1920, "uc", "m", min_run_date, wy_end, min_mrid))
# powRelmax <- bind_rows(hdb_query(1920, "uc", "m", wy_start, format(ym(max_run_date) - months(1), "%Y-%m")),
#                        hdb_query(1920, "uc", "m", max_run_date, wy_end, max_mrid))
# 
# # powRelWY <- powRel %>%
# #   rename(Date = time_step) %>%
# #   mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
# #   mutate(WY = ifelse(month(Date)>9,year(Date)+1,year(Date))) %>%
# #   group_by(WY) %>%
# #   summarise(WYRelease = sum(value))
# #
# # powRelWY[1,2]
# 
# powRelmost <- powRel %>%
#   rename(Date = time_step, Most = value) %>%
#   mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
#   select(Date, Most)
# 
# powRelmin <- powRelmin %>%
#   rename(Date = time_step, Min = value) %>%
#   mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
#   select(Date, Min)
# 
# powRelmax <- powRelmax %>%
#   rename(Date = time_step, Max = value) %>%
#   mutate(Date = as.yearmon(parse_date_time(Date, "m/d/y H:M:S"))) %>%
#   select(Date, Max)
# 
# powReleaseWY <- inner_join(inner_join(powRelmost,powRelmin, by="Date"),powRelmax, by="Date") %>%
#     mutate(WY = ifelse(month(Date)>9,year(Date)+1,year(Date))) %>%
#     group_by(WY) %>%
#     summarise(MostRel = round(sum(Most)/1000000,2),
#               MinRel = round(sum(Min)/1000000,2),
#               MaxRel = round(sum(Max)/1000000,2))

mostrd <- ym(most_run_date)
minrd <- ym(min_run_date)
maxrd <- ym(max_run_date)

mostlab <- paste(month.name[month(mostrd)],year(mostrd), "Most Probable Inflow with a Lake Powell release of",
                "7.48", "maf in WY", "2024", "and", 
                "9.00", "maf in WY", "2025")

minlab <- paste(month.name[month(minrd)],year(minrd), "Probable Minimum Inflow with a Lake Powell release of",
                "7.48", "maf in WY", "2024", "and", 
                "7.48", "maf in WY", "2025")

maxlab <- paste(month.name[month(maxrd)],year(maxrd), "Probable Maximum Inflow with a Lake Powell release of",
                "7.48", "maf in WY", "2024", "and", 
                "11.57", "maf in WY", "2025")
#######################################################################################

lab_names <- c("Historical Elevations",
               maxlab, 
               mostlab,
               minlab
               )


names(lab_names) <- c("Historical", "24MS Max", "24MS Most",  "24MS Min")
nn <- lab_names[1:4]

custom_colors <- c('grey20', '#104E8B','#26AE44','#DA3139')
custom_size <- c(1,rep(1.2, 3))
custom_lt <- c(1,rep(2, 3))
custom_alpha <- c(rep(1, 4))
names(custom_colors) <- names(custom_size) <- names(custom_lt) <- 
  names(custom_alpha) <- nn
cloud_color <- '#DDCBA4'#'grey85'

df_24MS_m = df_24MS %>% mutate(trace_labels = lab_names[Trace])

df_24MS_m$trace_labels = factor(df_24MS_m$trace_labels,levels = lab_names[c(4,1,2,3)]) 

# Create cloud range
cloud_name = '24MS Projections Range'
df_stat <-  df_24MS_m %>% select(c("run","slot","Date", "value")) %>% 
  # filter(Date >= run_date) %>% 
  group_by(run, slot, Date) %>%
  summarise(cloud.max = max(value),
            cloud.min = min(value)) %>% 
  mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date))

df_24MS_m1 = df_24MS_m %>% mutate(Cloud = factor(cloud_name), Date= as.yearmon(Date))

df_stat_p = df_stat %>% filter(slot == 'Powell.Pool Elevation')
df_stat_m = df_stat %>% filter(slot == 'Mead.Pool Elevation')
df_24MS_p = df_24MS_m1 %>% filter(slot == 'Powell.Pool Elevation') %>%
  mutate(trace_labels = lab_names[Trace])
df_24MS_m = df_24MS_m1 %>% filter(slot == 'Mead.Pool Elevation') %>%
  mutate(trace_labels = lab_names[Trace])

## Powell tier for figures
powell_line = eq_tier_df() %>%
  mutate(Cloud = factor(cloud_name))

## Powell -----------------------------------------------------------------------
p_breaks <- seq(3350, 3725, 25)
p_breaks2 <- seq(3350, 3725, 5)
yy <- NULL #c(3400, 3675) # NULL for default ylim
gg <-
  ggplot(df_stat_p, aes(x = Date, fill = Cloud)) +
  scale_fill_discrete(breaks = c(''), type = cloud_color) +
  geom_ribbon(aes(ymin = cloud.min, ymax = cloud.max), alpha = 0.2) +
  geom_line(data = df_24MS_p, 
            aes(x = Date, y = value, color = trace_labels, 
                alpha = trace_labels, group = trace_labels,
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
  labs(y = "Pool Elevation (ft)", x = NULL, color = NULL, linetype = NULL,
       size = NULL,
       fill = NULL,
       title = bquote('Lake Powell End-of-Month'~Elevations),
       subtitle = paste('Projections from', 'October 2023 24-Month Study Inflow Scenarios'),
       caption = "The Drought Response Operations Agreement (DROA) is available online at https://www.usbr.gov/dcp/finaldocs.html.                  "
  ) +
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
  geom_vline(
    xintercept = as.yearmon(ym(run_date) %m-% months(1)),
    size = 1, color = "black"  #"#ffdc70" or "grey45"
      # alpha = 0.8
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
         color = guide_legend(nrow = 5, order = 1),
         linetype = guide_legend(nrow = 5, order = 1),
         size = guide_legend(nrow = 5, order = 1)
         #, fill = guide_legend(order = 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    legend.position = "bottom",
    legend.justification = "left",
    legend.key.width = unit(1.2, "cm"),
    plot.margin = unit(c(0.2,0.1,1,0.1), "cm"),
    plot.title = element_text(face="bold", hjust = 0.5, size = 14,margin=margin(5,0,0,0)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin=margin(5,0,5,0)),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    legend.text = element_text(size=10)
  )

ggsave("C:/Temp/crmms24MS_powell.png", 
       width = 11, height = 8)
crmms_p <- image_read("C:/Temp/crmms24MS_powell.png")
logo_raw <- image_read("https://www.usbr.gov/lc/region/g4000/BofR-vert.png")
test_plot <- image_composite(crmms_p,image_resize(logo_raw,"325"),offset = "+2860+2060")
image_write(test_plot, "C:/Temp/Powell24MS.png")


## Mead -------------------------------------------------------------------------
m_breaks <- seq(900, 1250, 25)
m_breaks2 <- seq(900, 1250, 5)
yy <- c(1000, 1125) # NULL for default ylimit
gg <-
  ggplot(df_stat_m, aes(x = Date, fill = Cloud)) +
  scale_fill_discrete(breaks = c(''), type = cloud_color) + #type = cloud_color) +
  geom_ribbon(aes(ymin = cloud.min, ymax = cloud.max), alpha = 0.2) +
  # ggplot(df_24MS, aes(x = Date)) +

  geom_line(data = df_24MS_m, 
            aes(x = Date, y = value, color = trace_labels, 
                alpha = trace_labels, group = trace_labels,
                linetype = trace_labels, size = trace_labels)) +
  # geom_line(data = filter(df_24MS_m, trace_labels != esp_label), 
  #           aes(x = Date, y = value, color = trace_labels, 
  #               alpha = trace_labels, group = Trace,
  #               linetype = trace_labels, size = trace_labels)) +
  
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
       subtitle = paste('Projections from', 'October 2023 24-Month Study Inflow Scenarios'),
       caption = "The Drought Response Operations Agreement (DROA) is available online at https://www.usbr.gov/dcp/finaldocs.html.                  "
       ) +
  # tier stuff
  geom_hline(
    yintercept = c(1110, 1090, 1045), 
    colour = 'black', linetype = 'dashed'
  ) +
  geom_hline(yintercept = c(1145, 1075, 1050, 1025), color = "black", linetype = 1) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1147.5, label="Surplus Condition (>1,145')", angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1100, label="Normal Condition\n(1,075' to 1,145')", 
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date)),
           y=1107, label="Elevation 1,110 ft",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date)),
           y=1087, label="Elevation 1,090 ft",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date)),
           y=1042, label="Elevation 1,045 ft",
           angle=00, size=3, hjust = 0) +
  # annotate("text", x = as.yearmon(ym(run_date) - months(5)),
  #          y=1083, label="Drought Contingency Plan Contributions\n(<1,090')",
  #          angle=00, size=3, hjust = 0) +
  # annotate("text", x = as.yearmon(ym(run_date) - months(6)),
  #          y=1063, label="Shortage Condition\n(<1,075')",
  #          angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1060, label="Level 1 Shortage Condition\n(1,050' to 1,075')",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1037, label="Level 2 Shortage Condition\n(1,025' to 1,050')",
           angle=00, size=3, hjust = 0) +
  annotate("text", x = as.yearmon(ym(run_date) - months(6)),
           y=1015, label="Level 3 Shortage Condition\n(<1,025')",
           angle=00, size=3, hjust = 0) +
  geom_vline(
    xintercept = as.yearmon(c("Dec 2023", "Dec 2024")),
    size = 1, color = "#ffdc70",  #"#ffdc70" or "grey45"
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = as.yearmon(ym(run_date) %m-% months(1)),
    size = 1, color = "black"  #"#ffdc70" or "grey45"
    # alpha = 0.8
  ) +
  theme_bw(base_size = 14) +
  guides(alpha = 'none',
         color = guide_legend(nrow = 5, order = 1),
         linetype = guide_legend(nrow = 5, order = 1),
         size = guide_legend(nrow = 5, order = 1)
          #, fill = guide_legend(order = 1)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5), 
    legend.position = "bottom",
    legend.justification = "left",
    legend.key.width = unit(1.2, "cm"),
    plot.margin = unit(c(0.2,0.1,1,0.1), "cm"),
    plot.title = element_text(face="bold", hjust = 0.5, size = 14,margin=margin(5,0,0,0)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin=margin(5,0,5,0)),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
    legend.text = element_text(size=10)
  )

# crssplot:::add_logo_vertical(gg, .87, .01, .97, .13) # add usbr logo

ggsave("C:/Temp/crmms24MS_mead.png", 
       width = 11, height = 8)

crmms_m <- image_read("C:/Temp/crmms24MS_mead.png")
logo_raw <- image_read("https://www.usbr.gov/lc/region/g4000/BofR-vert.png")
test_plot <- image_composite(crmms_m,image_resize(logo_raw,"325"),offset = "+2860+2060")
image_write(test_plot, "C:/Temp/Mead24MS.png")

