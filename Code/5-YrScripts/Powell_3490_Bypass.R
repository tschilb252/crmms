# ============================================================================
# Compare Last official CRMMS-ESP run 
#   Powell Bypass Release
#   Exceedance plot of Vol below 3490 
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "scenarios", "fig_dir_nm")))

library(tidyverse)
library(lubridate)
library(zoo)
library(RWDataPlyr)

## -- Inputs if run alone
# source(file.path('Code', '0_MasterInputs.R'))

## Directories & Data
# Sys.getenv('CRMMS_DIR') # can be used to change directory to CRMMS_DIR
fig_dir <- file.path('Results', fig_dir_nm)
data_dir <- file.path('Scenario', scenario_dir)
dir.create(fig_dir, showWarnings = F)
source(file.path('Code','5-YrScripts', 'helper_functions.R'))

## Max Date
max_date = '2027-12' #'2024-12'

stor_minPP = 3997.1625 # from CRMMS at 3490

slots = c("Powell.Pool Elevation", "Powell.Storage",
          "Powell.Bypass", "Powell.Outflow", "Powell.Regulated Spill",
          'PowellData.TargetAnnualReleaseVolume',
          'PowellData.ActualAnnualReleaseVolume') 

rdfs = c(rep('res.rdf', length(slots)-2), rep('flags.rdf', times = 2))


## -- CRMMS results

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

# df_scens %>% filter(Variable == 'Powell.Pool Elevation',
#                     Value < 3490) 
  

## -- Combine and process
df_st <- df_scens %>%
  filter(Variable == 'Powell.Storage') %>%
  mutate(Vol_needed = (stor_minPP - (Value / 1000)))


df_q <- df_st %>%
  group_by(Date, Scenario, Variable) %>%
  summarise(q = list(quantile(Vol_needed, seq(0,1, by = 0.1)))) %>%
  unnest_wider(q, names_repair = ~paste0(sub('%', '', .))) %>%
  ungroup() 

df_q2 = df_q %>%
  pivot_longer(cols = contains('0'), names_to = 'Quant') %>%
  mutate(value = value) %>%
  mutate(Ex = factor(paste0(100 -as.numeric(Quant), '%')),
         Exceedance = factor(Ex, levels = paste0(seq(0,100, by = 10), '%')))

## -- plot
custom_Tr_colI <- scales::hue_pal()(length(scenarios))  
custom_Tr_col <- c('grey', custom_Tr_colI[-1])
cust_col_q = rev(c('#08519c', '#3182bd','#6baed6','#bdd7e7','#eff3ff', 'white',
      '#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15'))

xbreaks = df_q2 %>% filter(month(Date) %in% c(4,8,12)) %>% 
  select(Date) %>% distinct() 

ggplot(df_q2, aes(Date, value, #fill = Exceedance,
                  # color = Scenario, linetype = Ex)) +
                  linetype = Scenario, color = Exceedance)) +
  geom_line(size = 1.15) + 
  scale_y_continuous(#limits = c(-1000, max(df_q2$value)*1.05), 
    expand = c(0,0),
                     labels = scales::comma) +
  coord_cartesian(ylim=c(0, max(df_q2$value)*1.05)) +
  # scale_color_manual(values = custom_Tr_col) +
  scale_color_manual(values = cust_col_q) +
  bor_theme() +
  scale_x_yearmon(expand = c(0,0), breaks = xbreaks$Date,
                            minor_breaks = unique(df_q2$Date),
                            limits = c(min(df_q2$Date), max(df_q2$Date))) +
  labs(
    y = 'Volume (kaf)', x = NULL, 
    # color = NULL, linetype = NULL, size = NULL, fill = NULL,
    title = "Exceedances for Volume below Lake Powell Minimum Power Pool (3,490ft)"
    # subtitle = slot_i
  )  +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1,  hjust = 1), 
    # legend.position = "bottom",
    legend.key.width = unit(1.2, "cm")
  )

ggsave(file.path(fig_dir, paste0("Powell_minPP_Vol_", year(as.yearmon(format(ym(max_date), "%Y-%m"))), ".png")), 
       width = 8, height = 6)


## -- Bypass
df_out <- df_scens %>%
  filter(Variable %in% c('Powell.Outflow', 'Powell.Regulated Spill')) %>%
  mutate(wy = ifelse(month(Date) >= 10, year(Date) + 1, year(Date))) %>%
  group_by(Scenario, wy, Trace, Variable) %>%
  summarise(Value = sum(Value)) %>%
  filter(wy %in% 2023:2026) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(Powell.Outflow = Powell.Outflow / 10^3,
         turbineRel = Powell.Outflow - `Powell.Regulated Spill`)
  
df_tarv <- df_scens %>%
  filter(Variable %in% c('PowellData.TargetAnnualReleaseVolume', 
                         'PowellData.ActualAnnualReleaseVolume')) %>%
  mutate(wy = year(Date)) %>%
  select(-Date) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(rel_dif = PowellData.TargetAnnualReleaseVolume - PowellData.ActualAnnualReleaseVolume)
df_tarv %>% filter(rel_dif > 0 )


df_plot = df_out %>% filter(`Powell.Regulated Spill` > 0) #set to >= to get all data points in plot

## -- Setup plot
if (length(scenarios) == 2) {
  custom_Tr_col <- c('#f1c40f', '#8077ab')
} else {
  custom_Tr_col <- scales::hue_pal()(length(scenarios))
}

ggplot(df_plot, aes(factor(wy), `Powell.Regulated Spill`, #color = Scenario, 
                    fill = Scenario)) +
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(0.8)) +
  scale_fill_manual(values = custom_Tr_col) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0), 
                     limits = c(0, 1.05*max(df_plot$`Powell.Regulated Spill`))) +
  labs(x = 'Water Year', y = 'Bypass Release (kaf)') +
  bor_theme()

ggsave(file.path(fig_dir, 
                 paste0("Powell_BypassRel_", year(as.yearmon(format(ym(max_date), "%Y-%m"))), ".png")), 
       width = 8, height = 6)
