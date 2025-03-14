###############################################################
# Summarize annual Klag Lake non-sockeye escapement data
# kyle.rosendale@sitkatribe-nsn.gov
# October 2023

###############################################################
# Run Me First!

# This script can be run on its own. 


###############################################################
# Outputs

# appendices/10_other_spp_1.png       daily weir counts of non-target species (part 1)
# appendices/10_other_spp_2.png       daily weir counts of non-target species (part 2)
# plots/10_coho_w_staff.png           daily coho weir count with stage data plot
# plots/10_pink_w_staff.png           daily pink weir count with stage data plot
# results/10_other_spp_estimates.csv  weir counts for non-target species


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(lubridate)
library(gt)


###############################################################
# Source file

here::i_am("code/10_other_species.R") # not sure if this is best practice
daily <- read_csv(here("data", "02_daily_data.csv"))
wx <- read_csv(here("data", "02_daily_wx.csv"))


###############################################################
# Start analysis

# Daily counts by species
daily <- daily %>%
  select(date, coho:chum) %>%
  mutate(date = mdy(date)) %>%
  group_by(date) %>%
  summarize(coho = sum(coho),
            pink = sum(pink),
            chum = sum(chum),
            dolly = sum(dolly))

# Add weather and stat week data
wx <- wx %>%
  mutate(date = mdy(date))

daily <- daily %>%
  full_join(wx, by = "date") %>%
  mutate(jul = yday(date),
         stat_wk = epiweek(date))


###############################################################
# Plots

# Plot daily escapement with staff gauge
ggplot(daily, aes(date, coho)) +
  geom_col(aes(date, coho), fill = "darkgray") + 
  geom_line(aes(x = date, y = staff_gauge * 3), size = 2, color = "cyan3") +
  ylab("Coho") + theme_classic() +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Staff Gauge (cm)")) +
  xlab(element_blank()) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "10_coho_w_staff.png")) 

ggplot(daily, aes(date, pink)) +
  geom_col(aes(date, pink), fill = "darkgray") + 
  geom_line(aes(x = date, y = staff_gauge * 50), size = 2, color = "cyan3") +
  ylab("Pink") + theme_classic() +
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~./50, name = "Staff Gauge (cm)")) +
  xlab(element_blank()) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "10_pink_w_staff.png")) 


#############################################################################
# Output .csv with results

# Make .csv for numeric results
final_estimates <- tribble(
  ~parameter, ~estimate,
  "coho", sum(daily$coho),
  "pink", sum(daily$pink),
  "chum", sum(daily$chum),
  "dolly", sum(daily$dolly),
) %>%
  write_csv(file = here::here("results", "10_other_spp_estimates.csv"))


############################################################################
# Appendices

appx <- daily %>%
  select(date:dolly) %>%
  gt() %>%
  cols_label(
    date = "Date",
    coho = "Coho",
    pink = "Pink",
    chum = "Chum",
    dolly = "Dolly Varden"
  ) %>%
  opt_stylize(style = 6, color = 'gray') %>%
  gt_split(row_every_n = 36)

# Save each of the tables
appx1 <- grp_pull(appx, 1) %>%
  gtsave(here("appendices", "10_other_spp_1.png"))
appx2 <- grp_pull(appx, 2) %>%
  gtsave(here("appendices", "10_other_spp_2.png"))