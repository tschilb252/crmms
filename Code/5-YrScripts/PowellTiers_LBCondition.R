# ============================================================================
# Compare CRMMS-ESP run 
#   Powell Tiers / Powell TARV / LB Condition
#   
# ============================================================================
rm(list=setdiff(ls(), c("scenario_dir", "scenarios", "fig_dir_nm")))

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
source(file.path('Code', 'add_MeadPowell_tiers.R'))

## Max Date
max_yr = 2027
end_file_nm = paste0('_', max_yr)

##### ------ Tiers | Powell Release ------ #####

# set up 
slots = c(
  'Shortage.Shortage Flag', 'PowellData.ReleaseTier',
  'PowellData.TargetAnnualReleaseVolume',
  'PowellData.ActualAnnualReleaseVolume', 'DCP BWSCP Flags.LB DCP BWSCP'#,
  # 'MeadData.EffectiveEOCYPoolElev', "PowellData.EffectiveEOCYPoolElevWith823Rel"
)
rdfs = rep('flags.rdf', length(slots))

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
df<- NULL
for (i in 1:length(scenarios)) {
  scen_res <- rdf_aggregate(  
    agg = rwa1, 
    rdf_dir = data_dir[i]
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
  mutate(Scenario = factor(Scenario, levels = scenarios))

## Colors and Labels
DCPlab = c('Mead > 1,090 ft'= '#a6cee3', 
           'Mead 1,075-1,090 ft' = '#1f78b4',
           'Mead 1,050-1,075 ft' = '#b2df8a',
           'Mead 1,045-1,050 ft' = '#fb9a99',
           'Mead 1,040-1,045 ft' = '#e31a1c',
           'Mead 1,035-1,040 ft' = '#fdbf6f',
           'Mead 1,030-1,035 ft' = '#ff7f00',
           'Mead 1,025-1,030 ft' = '#ffff99',
           'Mead 895-1,025 ft' = '#cab2d6')
PowellTierLab = c('Equalization Tier' = '#a6cee3', 
                  'Upper Elevation\nBalancing Tier' = '#1f78b4',
                  'Mid Elevation\nRelease Tier' = '#b2df8a',
                  'Lower Elevation\nBalancing Tier' = '#33a02c')
ShortLabs = c('Normal Condition' = '#a6cee3', 
              'Shortage - Level 1' = '#1f78b4',
              'Shortage - Level 2' = '#b2df8a',
              'Shortage - Level 3' = '#cab2d6')

## process df
df_i = df_scens %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(year(Date) <= max_yr) %>%
  mutate(Year = factor(year(Date)),
         act_TARV = PowellData.ActualAnnualReleaseVolume/10^3,
         tarv = PowellData.TargetAnnualReleaseVolume/10^3,
         `Powell Tiers` = factor(PowellData.ReleaseTier, 
                                 levels = c(0,1,2,3),
                                 labels = names(PowellTierLab)),
         `Lower Basin Shortage`= factor(`Shortage.Shortage Flag`, 
                                        levels = c(0,1,2,3),
                                        labels = names(ShortLabs)),
         `DCP Contribution` = factor(`DCP BWSCP Flags.LB DCP BWSCP`, 
                                     levels = 8:0, 
                                     labels = names(DCPlab))) %>%#,
         # MeadPEDeter = MeadData.PEforCondition,
         # PowellPEDeter = PowellData.EffectiveEOCYPoolElevWith823Rel) %>%
  select(-all_of(slots), -Date) 


tiers_pwl = df_i %>% select(Scenario, Trace, Year, `Powell Tiers`) %>%
  group_by(Scenario, Year, `Powell Tiers`) %>%
  summarise(cnt = n()/30*100)

# Stacked + percent
g <- ggplot(tiers_pwl, aes(fill=`Powell Tiers`, y=cnt, x=Scenario)) + 
  bor_theme() +
  scale_fill_manual(values = PowellTierLab) +
  geom_bar(stat="identity") +
  labs(x = '', y = 'Percent of Traces') +
  scale_y_continuous(breaks = seq(0,100, by =20), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 55,  hjust = 1)) +
  facet_grid(~ Year)
print(g)
ggsave(filename = file.path(fig_dir, paste0('Powell_tiers', end_file_nm, '.png')), 
       width=7, height=6)

## Output Powell difs to excel file
tier_difs = df_i %>% select(Scenario, Year, Trace, `Powell Tiers`) %>%
  pivot_wider(names_from = Scenario, values_from = `Powell Tiers`) 
tier_difs$diff = ifelse(tier_difs[,3] == tier_difs[,4],
                        T,F)
tarv_difs = df_i %>% select(Scenario, Year, Trace, tarv) %>%
  pivot_wider(names_from = Scenario, values_from = tarv) 
powell_difs = left_join(tier_difs, tarv_difs, 
                        by = c('Year', 'Trace'),
                        suffix = c('_Tier', '_TARV')) %>%
  as.data.frame()
powell_difs$TARV_newVog = powell_difs[,7] - powell_difs[,6] 
wb1 <- openxlsx::createWorkbook("PwlDifs")
openxlsx::addWorksheet(wb1, "PwlDifs")
openxlsx::writeData(wb1, "PwlDifs", powell_difs)

tr_dif = tier_difs %>% filter(diff == FALSE) %>% select(Year, Trace)
tier_difs = df_i %>% select(Scenario, Year, Trace, `Powell Tiers`)#, PowellPEDeter)
diffsCom = left_join(tr_dif, tier_difs)
openxlsx::addWorksheet(wb1, 'diffs')
openxlsx::writeData(wb1, 'diffs', diffsCom)

openxlsx::saveWorkbook(wb1, file.path(fig_dir, paste0('Powell_diffs', end_file_nm, '.xlsx')), overwrite = T)

## testing
# df_i %>% filter(tarv == 7000 & Year == 2023) %>%
#   select(Scenario, `DCP Contribution`) %>% table()

# LB Shortage
df_short = df_i %>% select(Scenario, Trace, Year, `Lower Basin Shortage`) %>%
  group_by(Scenario, Year, `Lower Basin Shortage`) %>%
  summarise(cnt = n()/30*100)

g <- ggplot(df_short, aes(fill=`Lower Basin Shortage`, y=cnt, x=Scenario)) + 
  bor_theme() +
  scale_fill_manual(values = ShortLabs) +
  geom_bar(stat="identity") +
  labs(x = '', y = 'Percent of Traces') +
  scale_y_continuous(breaks = seq(0,100, by =20), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 55,  hjust = 1)) +
  facet_grid(~ Year)
print(g)
ggsave(filename = file.path(fig_dir, paste0('LB_shortage', end_file_nm, '.png')), 
       width=7, height=6)

# DCP Contribution
df_dcp = df_i %>% select(Scenario, Trace, Year, `DCP Contribution`) %>%
  group_by(Scenario, Year, `DCP Contribution`) %>%
  summarise(cnt = n()/30*100)

g <- ggplot(df_dcp, aes(fill=`DCP Contribution`, y=cnt, x=Scenario)) +
  bor_theme() +
  scale_fill_manual(values = DCPlab)+
  geom_bar(stat="identity") +
  labs(x = '', y = 'Percent of Traces') +
  scale_y_continuous(breaks = seq(0,100, by =20), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 55,  hjust = 1)) +
  facet_grid(~ Year)
print(g)
ggsave(filename = file.path(fig_dir, paste0('LB_DCP_Contribution', end_file_nm, '.png')), 
       width=7, height=6)

## check Mead difs & save to wb
tier_difs = df_i %>% select(Scenario, Year, Trace, `DCP Contribution`) %>%
  pivot_wider(names_from = Scenario, values_from = `DCP Contribution`) 
tier_difs$diff = ifelse(tier_difs[,3] == tier_difs[,4],
                        T,F)
wb <- openxlsx::createWorkbook("MeadDifs")
openxlsx::addWorksheet(wb, 'MeadDCP')
openxlsx::writeData(wb, 'MeadDCP', tier_difs)

# check Mead eocy pes and shortage
tr_dif = tier_difs %>% filter(diff == FALSE) %>% select(Year, Trace)
pe_difs = df_i %>% select(Scenario, Year, Trace, `DCP Contribution`, act_TARV)#, MeadPEDeter) #%>%
diffsCom = left_join(tr_dif, pe_difs)
openxlsx::addWorksheet(wb, 'diffs')
openxlsx::writeData(wb, 'diffs', diffsCom)

openxlsx::saveWorkbook(wb, file.path(fig_dir, paste0('MeadDCP_diffs', end_file_nm, '.xlsx')), overwrite = T)


# act_TARV boxplot doplot
i_breaks = c(seq(1000,7000, by = 500), 7480, seq(8000,20000, by = 500)) 
g <- ggplot(df_i, aes(Year, act_TARV, fill = Scenario)) +
  bor_theme() +
  # stat_boxplot_custom(position = "dodge") +
  # geom_violin(position = "dodge") +
  geom_dotplot(binaxis='y', stackdir='center', 
               position=position_dodge(0.8)) +
  scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
  labs(x = 'Year', y = 'Annual Powell Release (kaf)', fill = NULL) +
  theme(legend.position="top") +
  facet_grid(`Powell Tiers` ~ ., scales = 'free_y')
print(g)
ggsave(filename = file.path(fig_dir, paste0('Powell_RelbyTier', end_file_nm, '.png')), width=6, height=7)

# Total TARV; no facet
g <- ggplot(df_i, aes(Year, act_TARV, fill = Scenario)) +
  bor_theme() +
  CRSSIO::stat_boxplot_custom(position = "dodge") +
  scale_y_continuous(labels = scales::comma, breaks = i_breaks) +
  labs(x = 'Water Year', y = 'Annual Powell Release (kaf)', fill = NULL) +
  guides(fill = guide_legend(nrow = length(scenarios), order = 2)) +
  theme(legend.position="top")
print(g)
ggsave(filename = file.path(fig_dir, paste0('Powell_Rel', end_file_nm, '.png')), width=6, height=7)

## 
# df_i %>% 
#   filter(`Powell Tiers` == "Lower Elevation\nBalancing Tier") %>%
#   group_by(Year, Scenario) %>%
#   filter(act_TARV < 8230) %>%
#   summarise(tr_les = n()/30)