# ============================================================================
# Compare CRMMS-ESP run 
#   Powell + Mead Summary Boxplots: PE and Outflow
#   
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "fig_dir_nm", "custom_Tr_col")))

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
year_range = 2024:2028
max_yr = max(year_range)
end_file_nm = paste0('_', max_yr)

##### ------ Tiers | Powell Release ------ #####

# set up 
slots = c(
  "Powell.Pool Elevation", "Mead.Pool Elevation", 
  "Powell.Outflow", "Mead.Outflow",
  'Mohave.Outflow', 'Havasu.Outflow',
  'PowellData.ActualAnnualReleaseVolume'
)
rdfs = c(rep('res.rdf', length(slots)-1), 'flags.rdf')

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

df_cy = df_scens %>%
  filter(Variable %in% c('Mead.Outflow', 'Mohave.Outflow', 'Havasu.Outflow')) %>%
  mutate(Year = year(Date)) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario, Variable) %>%
  summarise(Value = sum(Value)/1000, 
            n = n()) %>%
  filter(n == 12)

## -- Mead Release
ggplot(df_cy %>% filter(Variable == 'Mead.Outflow'), 
       aes(factor(Year), Value, fill = Scenario )) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
  labs(x = 'Year', y = 'Annual Release (kaf)', fill = NULL,
       title = paste('Lake Mead Calendar Year Releases')) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios), order = 2)) 

ggsave(filename = file.path(fig_dir, paste0('Rel_Mead_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_cy$Scenario)), height=6)


## -- Powell Release
df_wy_pow = df_scens %>%
  filter(Variable == 'PowellData.ActualAnnualReleaseVolume') %>%
  filter(Year %in% year_range) %>%
  mutate(Value = Value/1000)

ggplot(df_wy_pow, 
       aes(factor(Year), Value, fill = Scenario)) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  # geom_violin(position = "dodge") +
  # geom_dotplot(binaxis='y', stackdir='center', 
  #              position=position_dodge(0.8)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 50000, by = 1000)) +
  labs(x = 'Water Year', y = 'Annual Release (kaf)', fill = NULL,
       title = paste('Lake Powell Water Year Releases')) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios), order = 2)) 
ggsave(filename = file.path(fig_dir, paste0('Rel_Powell_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_cy$Scenario)), height=6)

## -- Mohave Release
ggplot(df_cy %>% filter(Variable == 'Mohave.Outflow'), 
       aes(factor(Year), Value, fill = Scenario )) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
  labs(x = 'Year', y = 'Annual Release (kaf)', fill = NULL,
       title = paste('Lake Mohave Calendar Year Releases')) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios), order = 2)) 

ggsave(filename = file.path(fig_dir, paste0('Rel_Mohave_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_cy$Scenario)), height=6)

## -- Havasu Release
ggplot(df_cy %>% filter(Variable == "Havasu.Outflow"), 
       aes(factor(Year), Value, fill = Scenario )) +
  bor_theme() +
  stat_boxplot_custom(position = "dodge") +
  geom_point(position = position_dodge(0.75), size = 0.75) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 10000, by = 500)) +
  labs(x = 'Year', y = 'Annual Release (kaf)', fill = NULL,
       title = paste('Lake Havasu Calendar Year Releases')) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios), order = 2)) 

ggsave(filename = file.path(fig_dir, paste0('Rel_Havasu_box', end_file_nm, '.png')), 
       width=6+0.5*length(unique(df_cy$Scenario)), height=6)

## -- Powell PE
df_i = df_scens %>%
  filter(Variable == 'Powell.Pool Elevation' & month(Date) == 9) %>%
  mutate(Year = ifelse(month(Date)>=10, year(Date) + 1, year(Date))) %>%
  filter(Year %in% year_range) %>%
  group_by(Year, Trace, Scenario) 

ggplot(df_i %>% filter(Year %in% year_range), 
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
       title = paste('End-of-Water Year Powell Elevation')) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios), order = 2))

ggsave(filename = file.path(fig_dir, paste0('Powell_EOWY_box', end_file_nm, '.png')), 
       width=6+0.5*length(scenarios), height=6)

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
       title = paste('End-of-Calendar Year Mead Elevation')) +
  theme(legend.position="right") +
  guides(fill = guide_legend(nrow = length(scenarios), order = 2)) 
ggsave(filename = file.path(fig_dir, paste0('Mead_EOCY_box', end_file_nm, '.png')), 
       width=6+0.5*length(scenarios), height=6)

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
