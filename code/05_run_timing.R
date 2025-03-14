###############################################################
# Summarize historic Klag Lake sockeye run timing
# kyle.rosendale@sitkatribe-nsn.gov
# October 2023

###############################################################
# Run Me First!

# 02_annual_sockeye.R must be run prior to this script.


###############################################################
# Outputs

# outputs/05_hist_daily_esc.csv   historical daily weir counts
# plots/05_med_esc_date.png       median escapement date plot
# plots/05_run_timing.png         historical run timing plot
# results/05_med_dates.csv        historical median and current year median dates


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(lubridate)


###############################################################
# Source file

here::i_am("code/05_run_timing.R")
tmg <- read_csv(here("data", "05_hist_daily_esc.csv"))

# Need to pull files from results folder for current year estimates
tmg_curr <- read_csv(here("inputs", "05_esc_daily_curr.csv"))


###############################################################
# Begin analysis

# Format current year daily escapement
tmg_curr <- tmg_curr %>%
  mutate(year = year(date),
         jul_day = yday(date)) %>%
  rename(esc_daily = sock_esc)

# Combine historical data with current year; find distance from median.
# First save historic escapement for future years.
tmg <- full_join(tmg, tmg_curr) %>%
  write_csv(here("outputs", "05_hist_daily_esc.csv"))

tmg <- tmg %>%
  mutate(jul_leap = ifelse(year %% 4, jul_day, jul_day - 1)) %>% # Account for leap years for plots below
  group_by(year) %>%
  mutate(prop_cum = cumsum(esc_daily) / sum(esc_daily),
         dist_med = abs(prop_cum - 0.5),
         esc_cum = cumsum(esc_daily))

# Create data frame to estimate median run timing over all years
med <- tmg %>%
  select(year, date, jul_leap, dist_med) %>%
  slice_min(dist_med) %>%
  slice_max(jul_leap)


###################################################################
# Plots

# Plot month-day rather than Julian day
# Make alternative axis for Julian day
doy <- date(c("2001-06-15", "2001-07-15", "2001-08-15", "2001-09-15"))
date_axis <- tibble(doy,
              moda = paste(month(doy, label = T), '15', sep = " "),
              jul = yday(doy))

# Plot run timing with color gradient and 25th, 50th, and 75th percentiles
ggplot(tmg, aes(jul_leap, year)) + geom_tile(aes(fill = dist_med)) +
  geom_vline(data = med, xintercept = quantile(med$jul_leap, probs = c(0.25)), linetype = 2) +
  geom_vline(data = med, xintercept = median(med$jul_leap), linetype = 5) +
  geom_vline(data = med, xintercept = quantile(med$jul_leap, probs = c(0.75)), linetype = 2) +
  labs(x = element_blank(), y = element_blank()) +
  theme_classic() +
  scale_x_continuous(breaks = date_axis$jul, labels = date_axis$moda) +
  scale_fill_gradient(low = "brown1", high = "azure2") +
  theme(text = element_text(size = 20)) +
  labs(fill = "Distance from Median") +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 8))
ggsave(here("plots", "05_run_timing.png"))

# Make new alternative axis for Julian day with finer resolution
doy <- c("2001-07-25", "2001-08-01", "2001-08-08",
         "2001-08-15", "2001-08-22", "2001-08-29")
moda <- c("Jul 25", "Aug 1", "Aug 8", "Aug 15", "Aug 22", "Aug 29")
date_axis <- tibble(doy, moda, jul = yday(doy))

# Plot median date of escapement
ggplot(med, aes(year, jul_leap)) + geom_point(size = 2.5) +
  theme_classic() +
  scale_y_continuous(breaks = date_axis$jul, labels = date_axis$moda) +
  geom_hline(yintercept = quantile(med$jul_leap, probs = c(0.25)), linetype = 2) +  
  geom_hline(yintercept = median(med$jul_leap), linetype = 2) +
  geom_hline(yintercept = quantile(med$jul_leap, probs = c(0.75)), linetype = 2) +
  labs(x = element_blank(), y = "Median Date of Escapement") +
  theme(text = element_text(size = 20))
ggsave(here("plots", "05_med_esc_date.png"))


#############################################################################
# Output .csv with results

# Find median run date for this year, compare to long-term median
med_hist <- median(med$jul_leap) %>%
  as.Date(med_hist, origin = as.Date("2001-01-01")) # Ignore year for now
  
med_curr <- med %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  select(year, date) %>%
  slice_max(year) %>%
  pull()

# Make .csv with current year median date
# Get current year
yr <- med %>%
  ungroup() %>%
  select(year) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  slice_max(year) %>%
  pull()

date_estimate <- tribble(
  ~parameter, ~estimate,
  "med_hist", str_sub(as.character(med_hist), -5),
  "med_curr", str_sub(as.character(med_curr), -5)) %>%
  write_csv(file = here::here("results", "05_med_dates.csv"))