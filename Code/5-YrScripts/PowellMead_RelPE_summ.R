# ============================================================================
# Compare CRMMS-ESP run 
#   Powell + Mead Summary Boxplots: PE and Outflow
#   
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "fig_dir_nm")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)
library(CRSSIO)
library(patchwork)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
scenarios = names(scenario_dir)
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code', 'add_MeadPowell_tiers.R'))
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

## Max Date
year_range = 2023:2026
max_yr = 2026
end_file_nm = paste0('_', max_yr)
MultESP_PlotTogether = T # plot multiple ESP forecasts together (names sep by '-')
custom_colors <- c('#f1c40f', '#8077ab')

##### ------ Tiers | Powell Release ------ #####

# set up 
slots = c(
  "Powell.Pool Elevation", "Mead.Pool Elevation", 
  "Powell.Outflow", "Mead.Outflow",
  'Mohave.Outflow', 'Havasu.Outflow'
)
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
df <- NULL
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i],
    find_all_slots = FALSE
  )
  scen_res$Scenario <- scenarios[i]
  
  # keep only last 30 traces (ESP)
  trces = unique(scen_res$TraceNumber)
  trces = trces[trces >= 0]
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
         Year = year(Date)) %>%
  filter(Year <= max_yr) 

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
if (length(scenarios_2) <= length(custom_colors)) {
  custom_Tr_col <- custom_colors[1:length(scenarios_2)] 
} else {
  custom_Tr_col <- scales::hue_pal()(length(scenarios_2)) # use show_col() to view
}

df_i = df_scens %>%
  filter(Variable == 'Mead.Outflow') %>%
  mutate(Year = year(Date)) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(Value = sum(Value)/1000)

## -- Mead Release
ggplot(df_i, aes(factor(Year), Value, fill = Scenario )) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by =1000)) +
  labs(x = 'Year', y = 'Annual CY Release (kaf)', fill = NULL,
       title = paste('Releases at Lake Mead'),
       subtitle = ESP_type) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios_2), order = 2)) 

ggsave(filename = file.path(fig_dir, paste0('Rel_Mead_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_i$Scenario)), height=6)



## -- Powell Release
df_i = df_scens %>%
  filter(Variable == 'Powell.Outflow') %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1, year(Date))) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(Value = sum(Value))

ggplot(df_i, 
       aes(factor(Year), Value, fill = Scenario)) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  # geom_violin(position = "dodge") +
  # geom_dotplot(binaxis='y', stackdir='center', 
  #              position=position_dodge(0.8)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 50000, by =1000)) +
  labs(x = 'Water Year', y = 'Annual WY Release (kaf)', fill = NULL,
       title = paste('Releases at Lake Powell'),
       subtitle = ESP_type) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios_2), order = 2)) 
ggsave(filename = file.path(fig_dir, paste0('Rel_Powell_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_i$Scenario)), height=6)

## -- Mohave Release
df_i = df_scens %>%
  filter(Variable == 'Mohave.Outflow') %>%
  mutate(Year = year(Date)) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(Value = sum(Value)/1000)

ggplot(df_i, aes(factor(Year), Value, fill = Scenario )) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by =1000)) +
  labs(x = 'Year', y = 'Annual CY Release (kaf)', fill = NULL,
       title = paste('Releases at Lake Mohave'),
       subtitle = ESP_type) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios_2), order = 2)) 

ggsave(filename = file.path(fig_dir, paste0('Rel_Mohave_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_i$Scenario)), height=6)

## -- Havasu Release
df_i = df_scens %>%
  filter(Variable == 'Havasu.Outflow') %>%
  mutate(Year = year(Date)) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario) %>%
  summarise(Value = sum(Value)/1000)

ggplot(df_i, aes(factor(Year), Value, fill = Scenario )) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by =1000)) +
  labs(x = 'Year', y = 'Annual CY Release (kaf)', fill = NULL,
       title = paste('Releases at Lake Havasu'),
       subtitle = ESP_type) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios_2), order = 2)) 

ggsave(filename = file.path(fig_dir, paste0('Rel_Havasu_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_i$Scenario)), height=6)

## -- Powell PE
df_i = df_scens %>%
  filter(Variable == 'Powell.Pool Elevation' & month(Date) == 9) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1, year(Date))) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario) 

ig_label= '2023'; ann_size=2.9
ggplot(df_i %>% filter(Year %in% 2024:2026), 
       aes(factor(Year), Value, fill = Scenario)) +
  bor_theme()   +
  geom_hline(yintercept = 3490, color ='#808080', linetype = 3 ) +
  geom_hline(yintercept = 3370, color ='#808080', linetype = 3 ) +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 50000, by =25),
                     limits = c(3370, 3700), expand = c(0,0)) +
  labs(x = 'Water Year', y = 'End-of-WY Powell Elevation (ft)', fill = NULL,
       title = paste('End-of-WY Powell Elevation'),
       subtitle = ESP_type) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios_2), order = 2))

ggsave(filename = file.path(fig_dir, paste0('Powell_EOWY_box', end_file_nm, '.png')), 
       width=6+0.5*length(scenarios_2), height=6)

## -- Mead PE
df_i = df_scens %>%
  filter(Variable == 'Mead.Pool Elevation' & month(Date) == 12) %>%
  mutate(Year = year(Date)) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario) 

ggplot(df_i, aes(factor(Year), Value, fill = Scenario)) +
  bor_theme() +
  geom_hline(yintercept = 950, color ='#808080', linetype = 3 ) +
  geom_hline(yintercept = 895, color ='#808080', linetype = 3 ) +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 50000, by =25),
                     limits = c(895, max(df_i$Value)+15),
                     expand = c(0,0))+
  labs(x = 'Year', y = 'End-of-CY Mead Elevation (ft)', fill = NULL,
       title = paste('End-of-CY Mead Elevation'),
       subtitle = ESP_type) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios_2), order = 2)) 
ggsave(filename = file.path(fig_dir, paste0('Mead_EOCY_box', end_file_nm, '.png')), 
       width=6+0.5*length(scenarios_2), height=6)

## Summary
# df_i %>%
#   filter(Year %in% 2024:2026) %>%
#   group_by(Scenario, Year)%>%
#   summarise(
#     min = min(Value),
#     med = quantile(Value, 0.5),
#     q75 = quantile(Value, 0.75),
#     q88 = quantile(Value, 0.12),
#     q25 = quantile(Value, 0.25),
#     max = max(Value),
#     qx5 = quantile(Value, 0.05))
