library(readxl)
library(flextable)
library(lubridate)
library(tidyverse)
library(cowplot)
library(tidyr)
library(reshape2)
library(ggpattern)

#Instructions
#--This file must be in the same folder as EnsembleOutput.xlsx for the current ESP run.
#--Install packages if not already installed. Ex: paste and run install.packages("readxl") from the Console.
#--Update Last month's file path.
#--Change Current_month and Previous month to "Month Year" if running code for an ESP run outside the current month (Ex: running March ESP results in June). Put # symbol in front of current code assigned to those objects.
#--Update Year1 as needed. Year2, Current_WY, and Next_WY are set by code based on Year1. 
#--Run code. Tables will show up in the "Viewer" and figures will show up in "Plots" (bottom right corner panel of R Studio). Use left and right arrow buttons to scroll through tables and figrues.

Last_month_file <- "N:/COM460/RiverWare Models/Monthly Models/CRMMS/2022/06-June/Output Data/EnsembleOutput.xlsx"       #In file path use either / or \\, do NOT use \

date <- Sys.Date()  
Current_month <- format(date, '%B %Y') 
Previous_month <- format(date %m-% months(1), '%B %Y')

Year1 <- "2022-01-01"     #Update Year ONLY. Year is for traces projected to decline below Powell min power pool.
EOCYear1 <- as.character(as.Date(Year1) %m+% months(11) %m+% days(30))

Year2 <- as.character(as.Date(Year1) %m+% years(1))     #Year2 calculated from Year1. This is for traces projected to operate next WY in UEBT. 
#Can overwrite Year2 manually by putting # in front of code and assigning Year2 with "XXXX-01-01", where XXXX is specified year.
EOCYear2 <- as.character(as.Date(Year2) %m+% months(11) %m+% days(30))

Year3 <- as.character(as.Date(Year2) %m+% years(1))

Current_WY <- as.numeric(format(as.Date(Year1), '%Y'))
Next_WY <- as.numeric(format(as.Date(Year2), '%Y'))
Third_WY <- as.numeric(format(as.Date(Year3), '%Y'))

PowellUnregInf_Avg <- 9603.36   #Update average, 90th percentile, and 10th percentile when needed
Powell90th <- 14466.59
Powell10th <- 4900.16

Traces <- c("Trace4", "Trace5", "Trace6", "Trace7", "Trace8", "Trace9", "Trace10", 
            "Trace11", "Trace12", "Trace13", "Trace14", "Trace15", "Trace16", "Trace17", 
            "Trace18", "Trace19", "Trace20", "Trace21", "Trace22", "Trace23", "Trace24", 
            "Trace25", "Trace26", "Trace27", "Trace28", "Trace29", "Trace30", "Trace31", 
            "Trace32", "Trace33")

Years <- as.data.frame(list("Trace4" = 1991, "Trace5" = 1992, "Trace6" = 1993, "Trace7" = 1994, 
                            "Trace8" = 1995, "Trace9" = 1996, "Trace10" = 1997, "Trace11" = 1998, 
                            "Trace12" = 1999, "Trace13" = 2000, "Trace14" = 2001, "Trace15" = 2002, 
                            "Trace16" = 2003, "Trace17" = 2004, "Trace18" = 2005, "Trace19" = 2006, 
                            "Trace20" = 2007, "Trace21" = 2008, "Trace22" = 2009, "Trace23" = 2010, 
                            "Trace24" = 2011, "Trace25" = 2012, "Trace26" = 2013, "Trace27" = 2014, 
                            "Trace28" = 2015, "Trace29" = 2016, "Trace30" = 2017, "Trace31" = 2018, 
                            "Trace32" = 2019, "Trace33" = 2020))


#Powell traces operating next WY in the UEBT

PowellOT <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "PowellData.ReleaseTier"))
rownames(PowellOT) <- PowellOT[,1]
TracesUEBT <- colnames(PowellOT[Year2, Traces])[which(PowellOT[Year2, Traces]==1)]
Number_UEBT <- length(as.list(TracesUEBT))
YearsUEBT <- Years[1, TracesUEBT]

PowellUnregInf <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "PowellAJUnregInflow"))
rownames(PowellUnregInf) <- PowellUnregInf[,1]
UnregInfUEBT <- PowellUnregInf[EOCYear1,TracesUEBT]

UEBT_traces <- data.frame()

if(Number_UEBT>1){
  for(i in 1:Number_UEBT){
    UEBT_traces_data <- c(YearsUEBT[1, i], Current_WY, format(UnregInfUEBT[1,i]/1000, digits=5), sprintf("%1.0f%%", UnregInfUEBT[1, i]/PowellUnregInf_Avg*100))
    UEBT_traces <- rbind(UEBT_traces, UEBT_traces_data)}
} else if(Number_UEBT == 1){
    UEBT_traces_data <- c(YearsUEBT, Current_WY, format(UnregInfUEBT/1000, digits=5), sprintf("%1.0f%%", UnregInfUEBT/PowellUnregInf_Avg*100))
    UEBT_traces <- rbind(UEBT_traces, UEBT_traces_data)
} else if(Number_UEBT == 0){
    UEBT_traces_data <- c("No traces", " ", " ", " ")
    UEBT_traces <- rbind(UEBT_traces, UEBT_traces_data)
}


colnames(UEBT_traces) <- c("Trace", "WY for Unreg Inflow", "Unreg. Inflow", "Avg")
UEBT_caption <- paste("Traces projecting Powell to operate WY", Next_WY, "in UEBT")
UEBT_traces_tab <- flextable(UEBT_traces)
UEBT_traces_tab <- set_caption(UEBT_traces_tab, caption = UEBT_caption)
UEBT_traces_tab


#Powell Below Min Power Pool Traces

Powell3490 <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Powell3490Annual"))
rownames(Powell3490) <- Powell3490[,1]
Powell3490df <- Powell3490[Year1, Traces]
Traces3490 <- colnames(Powell3490df)[which(Powell3490df==1)]
Number_3490 <- length(as.list(Traces3490))
Years3490 <- Years[1, Traces3490]

UnregInf3490 <- PowellUnregInf[EOCYear1,Traces3490]

MinPowerPool <- data.frame()

if(Number_3490 > 1){
  for(i in 1:Number_3490){
    MinPowerPool_data <- c(Years3490[1, i], Current_WY, format(UnregInf3490[1, i]/1000, digits=4), sprintf("%1.0f%%", UnregInf3490[1, i]/PowellUnregInf_Avg*100))
    MinPowerPool <- rbind(MinPowerPool, MinPowerPool_data)}
} else if(Number_3490 == 1){
    MinPowerPool_data <- c(Years3490, Current_WY, format(UnregInf3490/1000, digits=4), sprintf("%1.0f%%", UnregInf3490/PowellUnregInf_Avg*100))
    MinPowerPool <- rbind(MinPowerPool, MinPowerPool_data)
} else if(Number_3490 == 0){
    MinPowerPool_data <- c("No traces", " ", " ", " ")
    MinPowerPool <- rbind(MinPowerPool, MinPowerPool_data)
}

colnames(MinPowerPool) <- c("Trace", "WY", "Unreg. Inflow", "Avg")
MinPowerPool_caption <- paste("Traces projecting Powell to decline below minimum power pool in", Current_WY)
MinPowerPool_tab <- flextable(MinPowerPool)
MinPowerPool_tab <- set_caption(MinPowerPool_tab, caption = MinPowerPool_caption)
MinPowerPool_tab


#EOCY Ranges
EOCY_before_Year1 <- as.character(as.Date(Year1) %m+% years(-1) %m+% months(11))
EOCY_Year1 <- as.character(as.Date(Year1) %m+% months(11))
EOCY_Year2 <- as.character(as.Date(EOCY_Year1) %m+% years(1))
EOCY_Year3 <- as.character(as.Date(EOCY_Year1) %m+% years(2))
EOCY_Year4 <- as.character(as.Date(EOCY_Year1) %m+% years(3))
EOCY_Year5 <- as.character(as.Date(EOCY_Year1) %m+% years(4))
EOCY_col <- c(format(as.Date(EOCY_before_Year1), '%Y'), format(as.Date(EOCY_Year1), '%Y'), format(as.Date(EOCY_Year2), '%Y'), format(as.Date(EOCY_Year3), '%Y'), format(as.Date(EOCY_Year4), '%Y'), format(as.Date(EOCY_Year5), '%Y'))

Elev_min <- function(x, y){round(min(x[y, Traces]), digits = 2)}
Elev_max <- function(x, y){round(max(x[y, Traces]), digits = 2)}

Powell_Elev <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Powell.Pool Elevation"))
rownames(Powell_Elev) <- Powell_Elev[,1]
Powell_min <- c(Elev_min(Powell_Elev, EOCY_before_Year1), Elev_min(Powell_Elev, EOCY_Year1), Elev_min(Powell_Elev, EOCY_Year2), Elev_min(Powell_Elev, EOCY_Year3), Elev_min(Powell_Elev, EOCY_Year4), Elev_min(Powell_Elev, EOCY_Year5))
Powell_max <- c(Elev_max(Powell_Elev, EOCY_before_Year1), Elev_max(Powell_Elev, EOCY_Year1), Elev_max(Powell_Elev, EOCY_Year2), Elev_max(Powell_Elev, EOCY_Year3), Elev_max(Powell_Elev, EOCY_Year4), Elev_max(Powell_Elev, EOCY_Year5))

EOCY_PowellElev <- data.frame(EOCY_col, Powell_min, Powell_max)
colnames(EOCY_PowellElev) <- c("EOCY", "Powell Min Elev", "Powell Max Elev")
EOCY_PowellElev_tab <- flextable(EOCY_PowellElev)
EOCY_PowellElev_tab <- set_caption(EOCY_PowellElev_tab, "Powell EOCY Physical Elevations:")
EOCY_PowellElev_tab

Mead_Elev <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Mead.Pool Elevation"))
rownames(Mead_Elev) <- Mead_Elev[,1]
Mead_min <- c(Elev_min(Mead_Elev, EOCY_before_Year1), Elev_min(Mead_Elev, EOCY_Year1), Elev_min(Mead_Elev, EOCY_Year2), Elev_min(Mead_Elev, EOCY_Year3), Elev_min(Mead_Elev, EOCY_Year4), Elev_min(Mead_Elev, EOCY_Year5))
Mead_max <- c(Elev_max(Mead_Elev, EOCY_before_Year1), Elev_max(Mead_Elev, EOCY_Year1), Elev_max(Mead_Elev, EOCY_Year2), Elev_max(Mead_Elev, EOCY_Year3), Elev_max(Mead_Elev, EOCY_Year4), Elev_max(Mead_Elev, EOCY_Year5))

EOCY_MeadElev <- data.frame(EOCY_col, Mead_min, Mead_max)
colnames(EOCY_MeadElev) <- c("EOCY", "Mead Min Elev", "Mead Max Elev")
EOCY_MeadElev_tab <- flextable(EOCY_MeadElev)
EOCY_MeadElev_tab <- set_caption(EOCY_MeadElev_tab, "Mead EOCY Physical Elevations:")
EOCY_MeadElev_tab


#Figures

#Annual Powell Inflow Analysis
PowellRT <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "PowellData.ReleaseTier"))
rownames(PowellRT) <- PowellRT[,1]
PowellRT_df <- data.frame(Trace_Years = as.character(t(Years)), Powell_UnregInf = as.numeric(PowellUnregInf[EOCYear1,Traces]), Rel_Tier = as.numeric(PowellRT[Year2,Traces]))
PowellRT_df_NextWY <- data.frame(Trace_Years = as.character(t(Years)), Powell_UnregInf = as.numeric(PowellUnregInf[EOCYear2,Traces]), Rel_Tier = as.numeric(PowellRT[Year3,Traces]))

Operating_Tier <- character()
for(i in 1:30){
  if(PowellRT_df[i, 3]==0){
    x = "Eq"
  } else if(PowellRT_df[i, 3]==3){
    x = "LEBT"
  } else if(PowellRT_df[i, 3]==2){
    x = "MERT"
  } else if(PowellRT_df[i, 3]==1){
    x = "UEBT"
  }
  Operating_Tier <- c(Operating_Tier, x)
}

Operating_Tier_NextWY <- character()
for(i in 1:30){
  if(PowellRT_df_NextWY[i, 3]==0){
    x = "Eq"
  } else if(PowellRT_df_NextWY[i, 3]==3){
    x = "LEBT"
  } else if(PowellRT_df_NextWY[i, 3]==2){
    x = "MERT"
  } else if(PowellRT_df_NextWY[i, 3]==1){
    x = "UEBT"
  }
  Operating_Tier_NextWY <- c(Operating_Tier_NextWY, x)
}

Tier_colors <- c("#CC6666", "#E69F00", "#33CC33", "#0072B2")
names(Tier_colors) <- c("LEBT", "MERT", "UEBT", "Eq")
manual_scale <- scale_fill_manual(name = paste("WY", Next_WY, "Operating Tier"), values = Tier_colors)
manual_scale_NextWY <- scale_fill_manual(name = paste("WY", Third_WY, "Operating Tier"), values = Tier_colors)

ggplot(PowellRT_df, aes(fct_reorder(PowellRT_df[,"Trace_Years"], Powell_UnregInf), Powell_UnregInf)) + geom_bar(stat="identity", aes(fill = Operating_Tier)) + manual_scale + 
  labs(x = "Hydrologic Trace", y = "Powell Unregulated Inflow (1,000 AF)") + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  geom_hline(yintercept = c(Powell90th, Powell10th), linetype = 'dashed') + geom_hline(yintercept = PowellUnregInf_Avg, color = 'darkred') + 
  geom_text(aes(0, Powell10th, label = "Historic 10th Percentile"), vjust = -0.5, hjust = "left") + geom_text(aes(0, Powell90th, label = "Historic 90th Percentile"), vjust = -0.5, hjust = "left") + 
  geom_text(aes(0, PowellUnregInf_Avg, label = "Historic Average"), vjust = -0.5, hjust = "left") + ggtitle(paste("Annual Powell Inflow Analysis based on WY", Current_WY, "Inflow Volumes"))

ggplot(PowellRT_df_NextWY, aes(fct_reorder(PowellRT_df[,"Trace_Years"], Powell_UnregInf), Powell_UnregInf)) + geom_bar(stat="identity", aes(fill = Operating_Tier_NextWY)) + manual_scale_NextWY +
  labs(x = "Hydrologic Trace", y = "Powell Unregulated Inflow (1,000 AF)") + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + 
  geom_hline(yintercept = c(Powell90th, Powell10th), linetype = 'dashed')  + geom_hline(yintercept = PowellUnregInf_Avg, color = 'darkred') + 
  geom_text(aes(0, Powell10th, label = "Historic 10th Percentile"), vjust = -0.5, hjust = "left") + geom_text(aes(0, Powell90th, label = "Historic 90th Percentile"), vjust = -0.5, hjust = "left") + 
  geom_text(aes(0, PowellUnregInf_Avg, label = "Historic Average"), vjust = -0.5, hjust = "left") + ggtitle(paste("Annual Powell Inflow Analysis based on WY", Next_WY, "Inflow Volumes")) 


#Annual Mead Inflow Analysis
Mead_Inflow <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Mead.Inflow"))
Mead_years <- as.numeric(format(as.Date(Mead_Inflow[,1]), '%Y'))
Mead_Inflow_df <- cbind(Mead_years, Mead_Inflow[-1])
Mead_Inflow_yr <- aggregate(.~Mead_years, data = Mead_Inflow_df, FUN=sum)
rownames(Mead_Inflow_yr) <- Mead_Inflow_yr[,1]

Mead_Outflow <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Mead.Outflow"))
Mead_years_Outflow <- as.numeric(format(as.Date(Mead_Outflow[,1]), '%Y'))
Mead_Outflow_df <- cbind(Mead_years, Mead_Outflow[-1])
Mead_Outflow_yr <- aggregate(.~Mead_years_Outflow, data = Mead_Outflow_df, FUN=sum)
rownames(Mead_Outflow_yr) <- Mead_Outflow_yr[,1]

LCShortage <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Shortage.Shortage Flag"))
rownames(LCShortage) <- LCShortage[,1]
LCSurplus <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Surplus.AnnualSurplusFlag"))
rownames(LCSurplus) <- LCSurplus[,1]

Mead_Inflow_yr_df <- data.frame(Trace_Years = as.character(t(Years)), Inflow = as.numeric(Mead_Inflow_yr[as.character(Next_WY),Traces]), Outflow = as.numeric(Mead_Outflow_yr[as.character(Next_WY),Traces]), ShortageFlag = t(LCShortage[as.character(Next_WY + 1),Traces]), SurplusFlag = t(LCSurplus[as.character(Next_WY + 1),Traces]))
Mead_Inflow_yr2_df <- data.frame(Trace_Years = as.character(t(Years)), Inflow = as.numeric(Mead_Inflow_yr[as.character(Next_WY+1),Traces]), Outflow = as.numeric(Mead_Outflow_yr[as.character(Next_WY+1),Traces]), ShortageFlag = t(LCShortage[as.character(Next_WY + 2),Traces]), SurplusFlag = t(LCSurplus[as.character(Next_WY + 2),Traces]))

Operations <- character()
for(i in 1:30){
  if(Mead_Inflow_yr_df[i, 4]==0 & Mead_Inflow_yr_df[i, 5]==1){
    x = "Surplus"
  } else if(Mead_Inflow_yr_df[i, 4]==0 & Mead_Inflow_yr_df[i, 5]==0){
    x = "Normal"
  } else if(Mead_Inflow_yr_df[i, 4]==1){
    x = "Shortage - Level 1"
  } else if(Mead_Inflow_yr_df[i, 4]==2){
    x = "Shortage - Level 2"
  } else if(Mead_Inflow_yr_df[i, 4]==3){
    x = "Shortage - Level 3"
  }
  Operations <- c(Operations, x)
}

Operations2 <- character()
for(i in 1:30){
  if(Mead_Inflow_yr2_df[i, 4]==0 & Mead_Inflow_yr2_df[i, 5]==1){
    x = "Surplus"
  } else if(Mead_Inflow_yr2_df[i, 4]==0 & Mead_Inflow_yr2_df[i, 5]==0){
    x = "Normal"
  } else if(Mead_Inflow_yr2_df[i, 4]==1){
    x = "Shortage - Level 1"
  } else if(Mead_Inflow_yr2_df[i, 4]==2){
    x = "Shortage - Level 2"
  } else if(Mead_Inflow_yr2_df[i, 4]==3){
    x = "Shortage - Level 3"
  }
  Operations2 <- c(Operations2, x)
}

Tier_colors2 <- c("#ddd6ac","#0072B2", "#33CC33", "#E69F00", "#CC6666")
names(Tier_colors2) <- c("Surplus","Normal", "Shortage - Level 1", "Shortage - Level 2", "Shortage - Level 3")
manual_scale2 <- scale_fill_manual(name = paste("CY", Next_WY + 1, "Operating Condition"), values = Tier_colors2, guide = guide_legend(override.aes = list(pattern = "none")))
manual_scale3 <- scale_fill_manual(name = paste("CY", Next_WY + 2, "Operating Condition"), values = Tier_colors2, guide = guide_legend(override.aes = list(pattern = "none")))

Mead_Inflow_yr_df$OrderedInflow <- reorder(Mead_Inflow_yr_df$Trace_Years, Mead_Inflow_yr_df$Inflow)
Mead_Inflow_yr_df.melt <- melt(Mead_Inflow_yr_df[,c('Trace_Years', 'Inflow', 'Outflow')], id.vars = 1)
Mead_Inflow_yr_df.melt$Trace_Years <- factor(Mead_Inflow_yr_df.melt$Trace_Years, levels = levels(Mead_Inflow_yr_df$OrderedInflow))
Mead_Flow <- cbind( Mead_Inflow_yr_df.melt, Operations = rep(Operations, time = 2))

Mead_Inflow_yr2_df$OrderedInflow <- reorder(Mead_Inflow_yr2_df$Trace_Years, Mead_Inflow_yr2_df$Inflow)
Mead_Inflow_yr2_df.melt <- melt(Mead_Inflow_yr2_df[,c('Trace_Years', 'Inflow', 'Outflow')], id.vars = 1)
Mead_Inflow_yr2_df.melt$Trace_Years <- factor(Mead_Inflow_yr2_df.melt$Trace_Years, levels = levels(Mead_Inflow_yr2_df$OrderedInflow))
Mead_Flow2 <- cbind( Mead_Inflow_yr2_df.melt, Operations = rep(Operations2, time = 2))

ggplot(Mead_Flow, aes(x = Trace_Years, y = value)) + 
  geom_col_pattern(aes(fill = Operations, pattern = variable, alpha = variable), colour = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.02,
                   position = 'identity') +
  scale_alpha_manual(values = c(1,0.3), name = "Flow") + 
  scale_pattern_manual(values = c("none", "stripe"), guide = guide_legend(override.aes = list(fill = "grey100")), name = "Flow") + manual_scale2 +
  labs(x = "Hydrologic Trace", y = "Mead Inflow (1,000 AF)") + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + ggtitle(paste("Annual Mead Analysis based on CY", Next_WY, "Inflow and Outflow Volumes")) + 
  coord_cartesian(ylim = c(5000, 11000)) + scale_y_continuous(breaks = c(5000,7000,9000,11000))

ggplot(Mead_Flow2, aes(x = Trace_Years, y = value)) + 
  geom_col_pattern(aes(fill = Operations, pattern = variable, alpha = variable), colour = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.02,
                   position = 'identity') +
  scale_alpha_manual(values = c(1,0.3), name = "Flow") + 
  scale_pattern_manual(values = c("none", "stripe"), guide = guide_legend(override.aes = list(fill = "grey100")), name = "Flow") + manual_scale3 +
  labs(x = "Hydrologic Trace", y = "Mead Inflow (1,000 AF)") + theme(axis.text.x = element_text(angle = 90, vjust=0.5)) + ggtitle(paste("Annual Mead Analysis based on CY", Next_WY + 1, "Inflow and Outflow Volumes")) + 
  coord_cartesian(ylim = c(5000, 11000)) + scale_y_continuous(breaks = c(5000,7000,9000,11000))


#Mead Stacked Bar Charts - Shortage Condition
LCShortage <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Shortage.Shortage Flag"))
rownames(LCShortage) <- LCShortage[,1]
LCShortage_lm <- as.data.frame(read_excel(Last_month_file, sheet = "Shortage.Shortage Flag"))
rownames(LCShortage_lm) <- LCShortage_lm[,1]
Year <- c(as.character(Next_WY), as.character(Next_WY+1), as.character(Next_WY+2), as.character(Next_WY+3), as.character(Next_WY+4))

LCSurplus <- as.data.frame(read_excel("EnsembleOutput.xlsx", sheet = "Surplus.AnnualSurplusFlag"))
rownames(LCSurplus) <- LCSurplus[,1]
LCSurplus_lm <- as.data.frame(read_excel(Last_month_file, sheet = "Surplus.AnnualSurplusFlag"))
rownames(LCSurplus_lm) <- LCSurplus_lm[,1]

TSum <- function(x, y, z){
  sum(x[y, Traces]==z)
}

LC_Condition <- c('Surplus','Normal', 'Shortage - Level 1', 'Shortage - Level 2', 'Shortage - Level 3')

LCShortage_df <- data.frame(ESPRun = rep(c(Current_month, Previous_month), each = 5), 
                            Condition = rep(LC_Condition), 
                            Year_data = rep(Year, each = 10),
                            Trace_data = c(TSum(LCSurplus,"2023",1), TSum(LCShortage, "2023",0) - TSum(LCSurplus,"2023",1), TSum(LCShortage, "2023",1), TSum(LCShortage, "2023",2), TSum(LCShortage, "2023",3), TSum(LCSurplus_lm,"2023",1), TSum(LCShortage_lm, "2023",0) - TSum(LCSurplus_lm,"2023",1), TSum(LCShortage_lm, "2023",1), TSum(LCShortage_lm, "2023",2), TSum(LCShortage_lm, "2023",3),
                                           TSum(LCSurplus,"2024",1), TSum(LCShortage,"2024",0) - TSum(LCSurplus,"2024",1), TSum(LCShortage,"2024",1), TSum(LCShortage,"2024",2), TSum(LCShortage,"2024",3), TSum(LCSurplus_lm,"2024",1), TSum(LCShortage_lm,"2024",0) - TSum(LCSurplus_lm,"2024",1), TSum(LCShortage_lm,"2024",1), TSum(LCShortage_lm,"2024",2), TSum(LCShortage_lm,"2024",3),
                                           TSum(LCSurplus,"2025",1), TSum(LCShortage,"2025",0) - TSum(LCSurplus,"2025",1), TSum(LCShortage,"2025",1), TSum(LCShortage,"2025",2), TSum(LCShortage,"2025",3), TSum(LCSurplus_lm,"2025",1), TSum(LCShortage_lm,"2025",0) - TSum(LCSurplus_lm,"2025",1), TSum(LCShortage_lm,"2025",1), TSum(LCShortage_lm,"2025",2), TSum(LCShortage_lm,"2025",3),
                                           TSum(LCSurplus,"2026",1), TSum(LCShortage,"2026",0) - TSum(LCSurplus,"2026",1), TSum(LCShortage,"2026",1), TSum(LCShortage,"2026",2), TSum(LCShortage,"2026",3), TSum(LCSurplus_lm,"2026",1), TSum(LCShortage_lm,"2026",0) - TSum(LCSurplus_lm,"2026",1), TSum(LCShortage_lm,"2026",1), TSum(LCShortage_lm,"2026",2), TSum(LCShortage_lm,"2026",3),
                                           TSum(LCSurplus,"2027",1), TSum(LCShortage,"2027",0) - TSum(LCSurplus,"2027",1), TSum(LCShortage,"2027",1), TSum(LCShortage,"2027",2), TSum(LCShortage,"2027",3), TSum(LCSurplus_lm,"2027",1), TSum(LCShortage_lm,"2027",0) - TSum(LCSurplus_lm,"2027",1), TSum(LCShortage_lm,"2027",1), TSum(LCShortage_lm,"2027",2), TSum(LCShortage_lm,"2027",3)))

#Figure to use with no shading or note about post-2026 operations
#ggplot(LCShortage_df, aes(x = ESPRun, y = Trace_data, fill = Condition)) + geom_bar(position='stack', stat='identity') + theme_bw() + facet_grid(~Year_data, scales = "free_x") +
#  labs(x = ' ',y = "Number of Traces") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + scale_y_continuous(breaks = seq(0,35, by =5), expand = c(0,0)) +
#  scale_fill_manual(values = c('#ddd6ac', '#a6cee3','#1f78b4','#b2df8a','#33a02c'), breaks = LC_Condition) + ggtitle("Lower Basin Operating Condition")


#Figure to use to include shading and note about post-2026 operations
ggplot(LCShortage_df, aes(x = ESPRun, y = Trace_data, fill = Condition)) + 
  geom_bar(position='stack', stat='identity') + theme_bw() + facet_grid(~Year_data, scales = "free_x") +
  labs(x = ' ',y = "Number of Traces") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + scale_y_continuous(breaks = seq(0,35, by =5), expand = c(0,0)) +
  geom_rect(data = subset(LCShortage_df, Year_data > 2026), aes(xmin=-Inf, xmax = Inf, ymin = 0, ymax = Inf), fill = 'black', alpha = 0.03) +
  scale_fill_manual(values = c('#ddd6ac', '#a6cee3','#1f78b4','#b2df8a','#33a02c'), breaks = LC_Condition) + 
  ggtitle(bquote("Lower Basin Operating Condition")) + labs(caption = bquote('Note: For modeling purposes, these projections assume a continuation of the Interim Guidelines, DCP and Minute 323 \nbeyond 2026. These agreements expire at the end of the 2026, and Reclamation is beginning a process \nto develop operating strategies for post-2026.')) + 
  theme(plot.caption=element_text(hjust = 0, size = 10))



