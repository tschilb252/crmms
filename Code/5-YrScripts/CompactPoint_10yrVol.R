# LF 10yr flows
rm(list=setdiff(ls(), c("scenario_dir", "fig_dir_nm", "custom_colors")))

library(tidyverse)
library(CRSSIO)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(rhdb)

## -- Inputs
# run 0_Master_xxx.R for inputs

## Directories & Data
scenarios = names(scenario_dir)
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

# Max Date
# max_date = c('2023-12')
max_date = c('2026-09')

MultESP_PlotTogether = T # plot multiple ESP forecasts together (names sep by '-')
# custom_colors <- c("#e94849", "#5f98c6", "#71bf6e", "#b783bf", "#B79F00", "#00BFC4")
# custom_colors <- c("#6e61ab", "#b783bf") #DROA sensitivity analysis
# custom_colors <- c("#5f98c6", "#386cb0") # 1a vs 1b sensitivity analysis


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
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i],
    find_all_slots = F
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = sort(unique(scen_res$TraceNumber))
  tr_keep = trces[(length(trces)-29):length(trces)]
  scen_res = scen_res %>% filter(TraceNumber %in% tr_keep)
  
  df <- rbind(df, scen_res)
}
df <- df %>% na.omit()

## -- process and combine hydrologies
df_scens <- data.table::as.data.table(df)  %>% 
  mutate(Date = as.yearmon(paste0(Month, Year), "%B%Y")) %>%
  dplyr::select(Scenario, Variable, Date, Trace = TraceNumber, Value) %>%
  mutate(Scenario = factor(Scenario, levels = scenarios),
         Trace = 1991 + Trace - min(df$TraceNumber, na.rm = T),
         Year = year(Date)) 

if (MultESP_PlotTogether) {
  sep_scen = str_split_fixed(as.character(df_scens$Scenario), "-", 2)
  sep_scen_nm = unique(str_split_fixed(scenarios, "-", 2)[,1])
  df_scens = df_scens %>% 
    mutate(Scenario = factor(sep_scen[,1], levels = sep_scen_nm),
           Trace = paste0(Trace, sep_scen[,2] ))
  ESP_type = paste0(unique( sep_scen[,2]), collapse =",")
  scenarios_2 = sep_scen_nm
} else {
  scenarios_2 = scenarios
  ESP_type =""
}

# Plot colors
custom_colors2 = na.omit(custom_colors[scenarios_2] )
if (length(scenarios_2) <= length(custom_colors2)) {
  custom_Tr_col <- custom_colors2[1:length(scenarios_2)] 
} else {
  custom_Tr_col <- scales::hue_pal()(length(scenarios_2)) # use show_col() to view
}

# calc crmms compact pt volume
df_lf <- df_scens %>% group_by(Scenario, Trace, Date) %>%
  summarise(Value = sum(Value)) %>%
  mutate(wy = ifelse(month(Date) >= 10,
                        year(Date) + 1, year(Date)))


## Read historical data from hdb, sum for compact pt
sdis <- c("Lees Ferry" = 1578, # actually Lees Ferry!
          "Paria" = 1579) 
start_date = format(ym("2005-10"), "%Y-%m")
end_date = format(min(df_scens$Date) - 1/12, "%Y-%m") 
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

df_hist <- df_hdb %>% ungroup() %>%
  filter(min(df_lf$wy) > wy) %>%
  group_by(wy) %>%
  summarise(Value = sum(Value))

# current wy's obs
df_currentHist <- df_hdb %>% ungroup() %>%
  filter(min(df_lf$wy) == wy) %>%
  summarise(Value = sum(Value))

# add current wy's vol
df_crmms <- df_lf %>% ungroup() %>%
  group_by(Scenario, Trace, wy) %>%
  summarise(Value = sum(Value)) %>% 
  ungroup() %>%
  mutate(Value = ifelse(wy == min(df_lf$wy),
                        Value + df_currentHist$Value,
                        Value)) 
# get 10 yr sum
df_rollHist <- df_hist %>% filter(wy >= min(df_lf$wy) - 9)

# enumerate historical data for combining with crmms traces
df_hist_i = merge(df_crmms %>%
                    select(Scenario, Trace) %>% distinct(), 
                  df_hist)

df_crmms2 = rbind.data.frame(df_crmms, df_hist_i) %>%
  arrange(Scenario, Trace, wy) %>%
  group_by(Scenario, Trace) %>%
  mutate(roll_sum = RcppRoll::roll_sum(Value, 10, align = "right", fill = NA)/1000)  %>%
  na.omit() 

df_hist10 <- df_crmms2 %>% ungroup() %>%
  filter(min(df_lf$wy) > wy) %>%
  select(wy, roll_sum) %>% 
  distinct() %>%
  mutate(Scenario = factor('Historical'))

df_crmmsPlot <- df_crmms2 %>%
  filter(min(df_lf$wy) <= wy) 
  

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
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill='transparent')
  )
    


filter(df_crmmsPlot, wy %in% 2024:2026) %>%
  ggplot(aes(factor(wy), roll_sum, fill = Scenario)) +
  stat_boxplot_custom(position = 'dodge', outlier.size = 1) +
  # geom_point(position = position_dodge(0.75), size = .75) +
  theme_bw() +
  scale_fill_manual(values = custom_Tr_col) +
  labs(
    x = 'Water Year', y = 'million acre-feet',
    fill = NULL,
    title = "Lees Ferry 10-Year Running Total"
  )

ggsave(file.path(fig_dir, "lees_ferry_10yr.png"),width = 6.5, height = 5)


# df_crmmsPlot %>%
#   filter(wy %in% 2024:2026) %>%
#   group_by(Scenario, wy)%>%
#   summarise(
#     min = min(roll_sum),
#     med = quantile(roll_sum, 0.5),
#     q75 = quantile(roll_sum, 0.75),
#     q88 = quantile(roll_sum, 0.12),
#     q25 = quantile(roll_sum, 0.25),
#     max = max(roll_sum))
