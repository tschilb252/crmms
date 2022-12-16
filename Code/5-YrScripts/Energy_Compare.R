# ============================================================================
#   Analyses: 
#     Energy at major reservoirs
#     Last set of figures are produced for LC power group
#
# ============================================================================
rm(list=ls())

## Inputs - control file
library(rhdb)
library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)
library(patchwork)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Output Data', fig_dir_nm)
data_dir <- file.path('rdfOutput', scenario_dir)
dir.create(fig_dir, showWarnings = F)

## Max Date
# max_date = c('2023-09')
max_date = c('2027-09')

## Trace Select?
single_Trace <- FALSE

# single_Trace <- TRUE
# sel_trace <- c(10, 13, 14, 15, 16, 19, 21, 22, 25, 27, 28, 29, 30)
# # sel_trace <- c(25)
# trace_yr = 1991 + sel_trace - 4

file_nm_end <- ifelse(single_Trace, 
                      paste0('_thru', format(ym(max_date), "%Y"), '_', trace_yr),
                      paste0('_thru', format(ym(max_date), "%Y")))


# slots and associated rdfs
slots = c(
  "Mead.Energy",
  "Mohave.Energy",
  "Havasu.Energy",
  "Powell.Energy"
)

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

# read/process RDFs
df<- NULL
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
  mutate(Scenario = factor(Scenario, levels = scenarios),
         yr_fy = ifelse(month(Date) >= 10,
                     year(Date) + 1, year(Date))) 

## Read historical data from hdb
sdis <- c("Mohave.Energy" = 2071, "Havasu.Energy" = 2072, 
          "Mead.Energy" = 2070, "Powell.Energy" = 2305)
start_date = format(ym("1999-10"), "%Y-%m")
end_date = format(min(df_scens$Date) - 1/12, "%Y-%m") 

df_hdb <- bind_rows(
  hdb_query(sdis["Powell.Energy"], "uc", "m", start_date, end_date),
  hdb_query(sdis["Mead.Energy"], "lc", "m", start_date, end_date),
  hdb_query(sdis["Mohave.Energy"], "lc", "m", start_date, end_date),
  hdb_query(sdis["Havasu.Energy"], "lc", "m", start_date, end_date)
)

df_hdb <- df_hdb %>%
  mutate(slot = names(sdis)[match(sdi, sdis)],
         Date = as.yearmon(parse_date_time(time_step, "m/d/y H:M:S")),
         yr_fy = ifelse(month(Date) >= 10,
                        year(Date) + 1, year(Date)),
         value = value/1000)
  
df_hist <- df_hdb %>%
  group_by(yr_fy, slot) %>%
  mutate(Scenario = 'Historical')

# Add historical data to model projections
df_histAdd <- df_hdb %>% 
  filter(yr_fy %in% min(df_scens$yr_fy)) %>%
  select(Variable = slot, Date, yr_fy, Value = value)
df_add <- df_scens %>% 
  filter(yr_fy %in% min(df_scens$yr_fy)) %>% 
  select(Scenario, Variable, Trace, yr_fy) %>% distinct()

addHist = left_join(df_histAdd, df_add, by = c("yr_fy", "Variable")) %>%
  select(colnames(df_scens))

df_scens = rbind.data.frame(df_scens, addHist)

## colors
custom_col <- "#138d75"#scales::hue_pal()(length(scenarios))
# custom_col <- c(scales::hue_pal()(3), "#C77CFF")


## Calc Annual WY/FY energy 
df_fy = df_scens %>% 
  group_by(Scenario, Variable, Trace, yr_fy) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(yr_fy = as.factor(yr_fy))
  # mutate(fy_tot = ifelse(Variable == 'Havasu.Energy', fy_tot/1000, fy_tot))

openxlsx::write.xlsx(df_fy, file = file.path(fig_dir, paste0('EnergybyTrace_thru', file_nm_end, '.xlsx')))

df_fy_hist <- df_hist %>% 
  filter(yr_fy < min(df_scens$yr_fy)) %>% 
  select(Variable = slot, Date, yr_fy, Value = value) %>%
  mutate(Scenario = 'Historical') %>%
  group_by(Scenario, Variable, yr_fy) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(yr_fy = as.factor(yr_fy))


if (single_Trace) {
  df_fy <- df_fy %>%
    filter(Trace %in% sel_trace)
  df_scens <- df_scens %>%
    filter(Trace %in% sel_trace)
}

# test = df_fy %>% filter(Variable ==  'Mead.Energy' & yr_fy == 2023) %>%
#   filter(Trace %in% c(10, 13, 14, 15, 16, 19, 21, 22, 25, 27, 28, 29, 30))
# # filter(Trace %in% c(33))
# range(test$Value)

# df_scens = df_scens %>% filter(Date >= as.yearmon(format(ym('2022-04'), "%Y-%m")))

## -- Loop through each reservoir Powell / Glen Energy
for (i in 1:length(slots)) {
  
  if (single_Trace) {
    gg <-
      ggplot(df_fy %>% filter(Variable == slots[i]),
             aes(yr_fy, Value, color = Scenario, group = Scenario)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = custom_col) +
      scale_x_discrete(expand = c(0.1,0.1)) 
    ggmon <-  
      ggplot(df_scens %>% filter(Variable == slots[i]), #%>%
             # filter(Date >= as.yearmon(format(ym('2022-04'), "%Y-%m"))),
                    aes(factor(Date), Value, color = Scenario, group = Scenario)) +
      geom_hline(yintercept = 0, color = 'grey') +
      geom_line(size = 1.2) +
      scale_color_manual(values = custom_col)
  } else {
    gg <- 
      ggplot(df_fy %>% filter(Variable == slots[i]),
             aes(yr_fy, Value, fill = Scenario)) +
      CRSSIO::stat_boxplot_custom(position = "dodge") +
      scale_fill_manual(values = custom_col) 
    ggmon <- 
      ggplot(df_scens %>% filter(Variable == slots[i]),
             aes(factor(Date), Value, fill = Scenario)) +
      geom_hline(yintercept = 0, color = 'grey') +
      CRSSIO::stat_boxplot_custom(position = "dodge") +
      scale_fill_manual(values = custom_col) 
  }
  
  gg <- gg +
    theme_bw() +
    scale_y_continuous(labels = scales::comma)  +
    labs(x = "Fiscal Year", y = 'Energy (GWh)', fill = NULL, color = NULL) +
    guides(fill = guide_legend(nrow = length(scenarios), order = 2)) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm")
    )
  ggmon <- ggmon +
    theme_bw() +
    scale_y_continuous(labels = scales::comma)  +
    labs(x = NULL, y = 'Energy (GWh)', fill = NULL, color = NULL) +
    guides(fill = guide_legend(nrow = length(scenarios), order = 2)) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm"),
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
  ggsave(plot = gg, file.path(fig_dir, paste0(slots[i], file_nm_end, '.png')), 
         width = length(unique(df_fy$yr_fy))*0.75 + 3, height = 4.5)
  ggsave(plot = ggmon, file.path(fig_dir, paste0(slots[i], file_nm_end, 'monthly.png')), 
         width = length(unique(df_fy$yr_fy))*1 + 3, height = 5)
}


## --- get all combos

# Powell and Mead plotted
df_fy_all <-  rbind(
  df_fy_hist %>% mutate(Trace =1), 
  df_fy) %>% 
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(`Glen Canyon + Hoover` = Mead.Energy + Powell.Energy,
         `Davis + Parker` = Havasu.Energy + Mohave.Energy) %>%
  rename(Hoover = Mead.Energy, 
         `Glen Canyon` = Powell.Energy,
         Davis = Mohave.Energy, 
         Parker = Havasu.Energy) %>%
  pivot_longer(cols = 4:9) %>%
  mutate(name = factor(name, 
                       levels = c("Glen Canyon", "Hoover", 
                                  "Davis","Parker", 
                                  "Glen Canyon + Hoover",
                                  "Davis + Parker")))

## --- Choose what agg to plot
df_PM = df_fy_all %>% 
  filter(name %in% c('Hoover', "Glen Canyon", "Glen Canyon + Hoover") &
           Scenario != "Historical") 
fl_prt = "GlenHoover"
  
# df_PM = df_fy_all %>% 
#   filter(name %in% c("Davis", "Parker", "Davis + Parker") &
#            Scenario != "Historical") 
# fl_prt = "DavisParker"

if (single_Trace) {
  gg <-
    ggplot(df_PM,
           aes(yr_fy, value, color = Scenario, group = Scenario)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = custom_col) +
    scale_x_discrete(expand = c(0.1,0.1)) 
} else {
  gg <- 
    ggplot(df_PM, aes(yr_fy, value, fill = Scenario)) +
    CRSSIO::stat_boxplot_custom(position = "dodge") +
    scale_fill_manual(values = custom_col) 
}

gg +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)  +
  scale_y_continuous(labels = scales::comma, limits = c(0, 10000), expand = c(0,0))  +
  labs(x = "FY", y = 'Energy (GWh)', fill = NULL, color = NULL) +
  guides(fill = guide_legend(nrow = 1, order = 2)) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.2, "cm")
  ) +
  facet_grid(name ~ ., scales = "free_y")
ggsave(file.path(fig_dir, paste0('HooverGlenTotalEnergy', file_nm_end, '.png')), 
       width = length(unique(df_fy$yr_fy))*0.75 + 3, height = 8)


## -- Get stats on overall energy
test = df_fy_all %>%
  group_by(Scenario, name, yr_fy) %>%
  summarise(CRMMSESP.max = round(max(value)),
            CRMMSESP.90 = round(quantile(value, .9)), 
            CRMMSESP.50 = round(quantile(value, .5)), 
            CRMMSESP.10 = round(quantile(value, .1)), 
            CRMMSESP.min = round(min(value)))
openxlsx::write.xlsx(test, 
                     file = file.path(fig_dir, paste0('EnergyChange_thru', file_nm_end, '.xlsx')))



## --- Plot annual cloud with historical data

plottedName = c("Davis + Parker", "Hoover")

for (i in 1:length(plottedName)) {
  df_plot = df_fy_all %>% 
  filter(name == plottedName[i] & Scenario != 'Historical') %>%
  mutate(yr_fy = as.numeric(as.character(yr_fy)))
df_plot_hist = df_fy_all %>% 
  filter(name == plottedName[i] & Scenario == 'Historical') %>%
  mutate(yr_fy = as.numeric(as.character(yr_fy)))

# connect hist and projection data
df_i = df_plot %>% select(Scenario, Trace, name) %>% distinct() 
df_ihist = df_plot_hist %>% 
  select(-Trace, -Scenario) %>%
  filter(yr_fy == min(df_plot$yr_fy)-1)
df_add = left_join(df_i, df_ihist, by = c('name'))
df_plot = rbind(df_add, df_plot)

df_plot_agg = df_plot %>%
  group_by(Scenario, yr_fy, name) %>%
  summarise(mdl.max = max(value),
            mdl.90 = quantile(value, .9), 
            mdl.50 = quantile(value, .5), 
            mdl.10 = quantile(value, .1), 
            mdl.min = min(value)) %>%
  ungroup() 

df_avg_hist = df_plot_hist %>%
  filter(yr_fy %in% 2000:2020) %>%
  summarise(avg = mean(value),
            min = min(value),
            max = max(value))

df_plot_hist = df_plot_hist

ggplot() + 
  geom_line(data = df_plot_agg,
            aes(x = yr_fy, y = mdl.50,
                color = Scenario)) +
  geom_line(data = df_plot_hist %>% filter(yr_fy >= 2015),
            aes(x = yr_fy, y = value, 
                color = Scenario)) +
  scale_color_manual(values = c(custom_col,'black')) +
  scale_size_manual(values = 1.15) +
  geom_ribbon(data = df_plot_agg, 
              aes(x = yr_fy, 
                  ymin = mdl.10, ymax = mdl.90, fill = Scenario), 
              alpha = 0.3) +
  scale_fill_manual(values = custom_col)  +
  geom_hline(yintercept = df_avg_hist$avg, 
             linetype = 'dashed', color = 'black') +
  annotate("text", x = 2020,
           y=df_avg_hist$avg + (df_avg_hist$max - df_avg_hist$min)*.02, 
           label="2000-2020 Average", angle=00, size=3.5, hjust = 0) +
  scale_x_continuous(expand = c(0,0), 
                     breaks = seq(2010, 2030, by = 1), 
                     minor_breaks = NULL) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = "Energy (GWh)", x = "FY/WY", 
    color = "Historical and\nmedian projections", 
    linetype = NULL, size = NULL, 
    fill = "10th to 90th\npercentile of\nfull range",
    title = paste(plottedName[i], "Energy from CRMMS-ESP")
  ) +
  # bor_theme() +
  theme(
    text = element_text(size = 12),
    # axis.text.x = element_text(angle = 90, vjust = 0.5), 
    legend.position = "right",
    legend.key.width = unit(1.2, "cm")
  )

ggsave(file.path(fig_dir, paste0(plottedName[i], '_CloudEnergy', file_nm_end, '.png')), 
       width = 10, height = 7)
}
