###############################################################
# Summarize annual Klag Lake sockeye salmon escapement data
# kyle.rosendale@sitkatribe-nsn.gov
# October 2023

###############################################################
# Run Me First!

# 01_annual_harvest.R must be run prior to this R script.


###############################################################
# Outputs

# appendices/02_sock_wx_1.png     daily weir count / weather data (part 1)
# appendices/02_sock_wx_2.png     daily weir count / weather data (part 2)
# plots/02_cum_esc_harv.png       cumulative escapement and harvest plot
# plots/02_esc_w_staff.png        daily escapement with stage data plot
# plots/02_escapement.png         daily escapement plot (no stage data)
# plots/02_mark_samp_rate.png     running mark and sample rates plot
# results/02_final_estimates.csv  key weir summary stats
# results/02_weir_dates.csv       key weir dates


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(lubridate)
library(gt)
library(scales)


###############################################################
# Source file

here::i_am("code/02_annual_sockeye.R")
daily <- read_csv(here("data", "02_daily_data.csv"))
est_harv <- read_csv(here("inputs", "02_daily_harv.csv"))
wx <- read_csv(here("data", "02_daily_wx.csv"))


###############################################################
# Start analysis

# Daily and cumulative sockeye mark, sample, and escapement
daily <- daily %>%
  select(date, sock_pass_um : sock_samp_m) %>%
  mutate(date = mdy(date)) %>%
  group_by(date) %>%
  summarize(sock_mark = sum(sock_pass_m) + sum(sock_samp_m),
            sock_samp = sum(sock_samp_um) + sum(sock_samp_m),
            sock_esc = sum(sock_pass_m) + sum(sock_pass_um) + # cont. next line
              sum(sock_samp_m) + sum(sock_samp_um)) %>%
  mutate(mark_rate = cumsum(sock_mark) / cumsum(sock_esc),
         samp_rate = cumsum(sock_samp) / cumsum(sock_esc),
         sock_cum = cumsum(sock_esc))

# Export daily escapement for use in 05_run_timing.R
daily %>%
  select(date, sock_esc) %>%
  write_csv(here("inputs", "05_esc_daily_curr.csv"))

# Add weather, harvest, and stat week data
# Format dates
wx <- wx %>%
  mutate(date = mdy(date))

est_harv <- est_harv %>%
  mutate(date = mdy(date))

daily <- daily %>%
  full_join(wx, by = "date") %>%
  full_join(est_harv, by = "date") %>%
  mutate(harv_est = ifelse(is.na(harv_est), 0, harv_est), # harv_est only contains dates with harvest, must fill in zeroes.
         exp_rt = cumsum(harv_est) / (sock_cum + cumsum(harv_est)),
         esc_prop = sock_esc / sum(sock_esc),
         jul = yday(date),
         stat_wk = epiweek(date))

# Save weather and escapement data for other analyses
daily_wx <- daily %>%
  select(date, staff_gauge:temp_air, sock_esc) %>%
  write_csv(here("data", "08_staff_curr.csv"))


###############################################################
# Plots

# Plot daily escapement
ggplot(daily, aes(date, sock_esc)) + geom_col() +
  ylab("Sockeye Escapement") + theme_classic()
ggsave(here("plots", "02_escapement.png")) 

# Plot daily escapement with staff gauge
# Double check scaling for secondary axis
ggplot(daily, aes(date, sock_esc)) +
  geom_col(aes(date, sock_esc), fill = "darkgray") + 
  geom_line(aes(x = date, y = staff_gauge * 10), size = 1.5, color = "#006AA7") +
  ylab("Sockeye Escapement") + 
  theme_classic() +
  scale_y_continuous(labels = comma, sec.axis = sec_axis(~./10, name = "Stage (cm)")) +
  xlab(element_blank()) +
  theme(text = element_text(size = 15))
ggsave(here("plots", "02_esc_w_staff.png")) 

# Plot cumulative harvest and escapement
daily %>%
  mutate(harv_cum = cumsum(harv_est)) %>%
  dplyr::select(date, sock_cum, harv_cum) %>%
  rename("Cumulative Escapement" = sock_cum) %>%
  rename("Cumulative Harvest" = harv_cum) %>%
  gather("Cumulative Escapement", "Cumulative Harvest", # cont. next line
         key = "key", value = "value") %>%
  ggplot(aes(x = date, y = value, color = key)) + geom_line(size = 2) +
  theme_classic() +
  scale_color_manual(values = c("cornflowerblue", "lightcoral")) +
  labs(x = element_blank(), y = "Sockeye") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = c(0.25, 0.9)) + 
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "02_cum_esc_harv.png"))

# Plot mark and sample rates vs. escapement
rates <- daily %>%
  select(date, mark_rate:sock_cum) %>%
  rename("Mark Rate" = mark_rate) %>%
  rename("Sample Rate" = samp_rate) %>%
  gather("Mark Rate", "Sample Rate", key = "rate", value = "prop")

ggplot() +
  geom_line(data = rates, aes(x = date, y = prop, color = rate), linewidth = 2) +
  theme_classic() + labs(x = element_blank(), y = "Proportion") +
  theme(legend.position = c(0.2, 0.9)) + 
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = sum(daily$sock_mark) / sum(daily$sock_esc),
             linetype = "dashed") +
  geom_hline(yintercept = sum(daily$sock_samp) / sum(daily$sock_esc),
             linetype = "dashed") +
  geom_line(data = daily, aes(x = date, y = sock_cum / (sum(daily$sock_esc) * 4)),
            linewidth = 2) +
  scale_y_continuous(sec.axis = sec_axis(~. * (sum(daily$sock_esc) * 4),
                                         name = "Cumulative Escapement"),
                     labels = scales::comma) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "02_mark_samp_rates.png"))


############################################################################
# Manipulate data for run timing and peaks

# Find week with greatest escapement
max_wk <- daily %>%
  group_by(stat_wk) %>%
  summarize(sum = sum(sock_esc)) %>%
  arrange(desc(sum))

# Test if there is a tie for date with largest harvest
if(max_wk$sum[1] == max_wk$sum[2]) {
    max_wk$sum[1] <- NA # Value will show NA in case of tie for maximum harvest date
  } else {}# Do nothing if false

# Take date for reporting  
max_wk <- max_wk %>%
  slice_head(n = 1)

# Find date with greatest escapement
max_date <- daily %>%
  arrange(desc(sock_esc)) %>%
  dplyr::select(date, sock_esc)

# Test if there is a tie for date with largest harvest
if(max_date$sock_esc[1] == max_date$sock_esc[2]) {
  max_date$sock_esc[1] <- NA # Value will show NA in case of tie for maximum harvest date
} else {}# Do nothing if false

# Take date for reporting  
max_date <- max_date %>%
  slice_head(n = 1)
  
# Find date of first sockeye through weir
first_sock <- daily %>%
  filter(sock_esc > 0) %>%
  arrange(date) %>%
  dplyr::select(date, sock_esc) %>%
  slice_head(n = 1)

# Find date of last sockeye through the weir
last_sock <- daily %>%
  filter(sock_esc > 0) %>%
  arrange(desc(date)) %>%
  dplyr::select(date, sock_esc) %>%
  slice_head(n = 1)

# Convert stat week with greatest escapement to calendar dates
peak_wk <- daily %>%
  filter(stat_wk == max_wk$stat_wk)


#############################################################################
# Output .csv with results

# Make .csv for numeric results
final_estimates <- tribble(
  ~parameter, ~estimate,
  "weir_count", sum(daily$sock_esc),
  "marked", sum(daily$sock_mark),
  "marked_prop", sum(daily$sock_mark) / sum(daily$sock_esc),
  "sampled", sum(daily$sock_samp),
  "sample_prop", sum(daily$sock_samp) / sum(daily$sock_esc),
  "peak_daily", max(daily$sock_esc),
  "peak_weekly", max_wk$sum,
  "stat_wk_peak", max_wk$stat_wk,
  "peak_wk_prop", max_wk$sum / sum(daily$sock_esc),
  "weir_days", as.integer(max(daily$date) - min(daily$date)) + 1,
) %>%
  write_csv(file = here::here("results", "02_final_estimates.csv"))

# Make .csv with important dates
date_estimates <- tribble(
  ~parameter, ~estimate,
  "weir_start", min(daily$date),
  "weir_end", max(daily$date),
  "sock_first", first_sock$date,
  "sock_last", last_sock$date,
  "peak_date", max_date$date,
  "peak_wk_start", min(peak_wk$date),
  "peak_wk_end", max(peak_wk$date)) %>%
  write_csv(file = here::here("results", "02_weir_dates.csv"))


#############################################################################
# Appendix

# Create table; split into two to better fit on page
appx <- daily %>%
  select(date, sock_esc, staff_gauge, rain_gauge) %>%
  gt() %>%
  cols_label(
    date = "Date",
    sock_esc = "Daily Escapement",
    staff_gauge = "Staff Gauge",
    rain_gauge = "Rain Gauge"
  ) %>%
  opt_stylize(style = 6, color = 'gray') %>%
  gt_split(row_every_n = 36)

# Save each of the tables
appx1 <- grp_pull(appx, 1) %>%
  gtsave(here("appendices", "02_sock_wx_1.png"))
appx2 <- grp_pull(appx, 2) %>%
  gtsave(here("appendices", "02_sock_wx_2.png"))