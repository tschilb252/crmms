library(tidyverse)
library(readxl)

# input -------------------------------------
res_folder <- "Output Data/CRMMS_EnsembleOutput.xlsm"
plot_years <- 2022:2026

# read data ---------------------------------
short <- read_excel(res_folder, "Shortage.Shortage Flag", skip = 2)
names(short)[1] <- "date"
short <- short %>%
  pivot_longer(-date, names_to = "trace") %>%
  mutate(variable = "shortage")

surplus <- read_excel(res_folder, "Surplus.AnnualSurplusFlag", skip = 2)
names(surplus)[1] <- "date"
surplus <- surplus %>%
  pivot_longer(-date, names_to = "trace") %>%
  mutate(variable = "surplus")

var_name <- c("Surplus of any amount", "Shortage of any amount")
names(var_name) <- c('surplus', 'shortage')

zz <- bind_rows(short, surplus) %>%
  # filter of years with NA, and the min/most/max probable 24MS
  filter(!(trace %in% c("Trace1", "Trace2", "Trace3")),
         !is.na(value)) %>%
  mutate(value = if_else(value >= 1, 1, 0)) %>%
  group_by(variable, date) %>%
  summarise(value = mean(value)) %>%
  mutate(year = lubridate::year(date)) %>%
  select(-date) %>%
  filter(year %in% plot_years) %>%
  mutate(
    variable = var_name[variable],
    variable = stringr::str_wrap(variable, 12)
  )

cols <- c("Full Hydrology" = "#138d75",  "Stress Test Hydrology" = "#f1c40f")
names(cols) <- stringr::str_wrap(names(cols), 20)
           
ss <- c(20, 18)
names(ss) <- stringr::str_wrap(unname(var_name), 12)

ggplot(zz, aes(year, value)) +
  geom_line(aes(linetype = variable), size = 1, color = cols[2]) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), 
    expand = c(0.01, 0.01), 
    breaks = seq(0, 1, .1), 
    minor_breaks = c(0, 1, .1)
  ) +
  scale_x_continuous(
    breaks = 2021:2026, 
    minor_breaks = 2021:2026, 
    expand = c(.02,0)
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal() + 
  geom_point(aes(shape = variable), size = 4, color = cols[2]) +
  #scale_shape_manual(values = ss) +
  labs(
    x = NULL, y = NULL, 
    color = "Hydrology", linetype = "Lower Basin Condition", 
    shape = "Lower Basin Condition"
  ) +
  theme(
    legend.key.size = unit(1, "cm"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

ggsave("short_surplus.png", width = 11, height = 5.5)
