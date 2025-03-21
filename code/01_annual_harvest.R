###############################################################
# Summarize annual Klag Lake sockeye salmon creel survey data
# kyle.rosendale@sitkatribe-nsn.gov
# October 2023

###############################################################
# Run Me First!

# This script can be run on its own.


###############################################################
# Outputs

# appendices/01_creel.png         individual creel survey results
# plots/01_creel_summary.png      creel survey summary table
# results/01_harv_dates.csv       key harvest dates
# results/01_harv_estimates.csv   key creel summary stats


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(gt)
library(chromote)

###############################################################
# Source file

here::i_am("code/01_annual_harvest.R")
creel <- read_csv(here("data", "01_creel.csv"))

###############################################################
# Begin analysis

# Create input file with daily harvest data for 02_annual_sockeye.R
creel_daily <- creel %>%
  group_by(gear) %>% # Harvest estimates are expanded by gear type
  mutate(sock = ifelse(is.na(sock), mean(sock, na.rm = TRUE), sock)) %>% # Use gear type mean for missed interviews
  ungroup() %>%
  group_by(date) %>%
  summarize(harv_est = round(sum(sock))) %>%
  write_csv(file = here::here("inputs", "02_daily_harv.csv"))

# Find maximum harvest dates
harv_date <- creel_daily %>%
  arrange(desc(harv_est))

# Test if there is a tie for date with largest harvest
  if(harv_date$harv_est[1] == harv_date$harv_est[2]) {
                    harv_date$date[1] <- NA # Value will show NA in case of tie for maximum harvest date
                    } else {}# Do nothing if false

# Take date for reporting
harv_date <- harv_date %>%
  slice_head(n = 1)


#########################################################################
# Make output files with results

# Make .csv for reporting numeric results
harv_estimates <- tribble(
  ~parameter, ~estimate,
  "parties", nrow(creel),
  "interviews", sum(creel$interview),
  "interview_prop", sum(creel$interview) / nrow(creel),
  "max_harv", harv_date$harv_est) %>%
  write_csv(file = here::here("results", "01_harv_estimates.csv"))

# Make .csv for reporting dates
harv_dates <- tribble(
  ~parameter, ~estimate,
  "first_sock", min(creel_daily$date),
  "last_sock", max(creel_daily$date),
  "max_harv", harv_date$date) %>%
  write_csv(file = here::here("results", "01_harv_dates.csv"))


###########################################################################
# Make table for report

# Estimate sport and subsistence harvest by gear type, sd, CV%
creel_table <- creel %>%
  group_by(gear) %>%
  summarize(n = n(),
            harv_rep = sum(sock, na.rm = TRUE),
            mean = mean(sock, na.rm = TRUE),
            sd = sd(sock, na.rm = TRUE),
            int = sum(interview),
            harv_exp = ceiling(harv_rep * n / int),
            var = var(sock, na.rm = TRUE), # break CV% into 3 steps
            var_2 = n ^ 2 * (1 - int / n) * var * (n / (int * int - 1)),
            cv = sqrt(var_2) / harv_exp * 100) %>%
  select(gear, n, int, harv_rep, mean, sd, cv, harv_exp) %>%
  arrange(desc(harv_exp))

# Make table for report
creel_table %>%
  gt() %>%
  
    # Make bold labels for columns
  cols_label(
    gear = md("**Gear**"),
    n = md("**Parties<br>Observed**"),
    int = md("**Interviews**"),
    harv_rep = md("**Creel Reported<br>Harvest**"),
    mean = md("**Mean**"),
    sd = md("**SD**"),
    cv = md("**CV%**"),
    harv_exp = md("**Expanded<br>Harvest**")
  ) %>%
    
    # Create summary row and format numbers
  grand_summary_rows(
    columns = c(n, int, harv_rep, harv_exp),
    fns = list(Total = ~ sum(.)),
    formatter = fmt_number,
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(mean, sd, cv),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(harv_rep, harv_exp),
    sep_mark = ",",
    decimals = 0
  ) %>%
    
    # Format table appearance
  tab_options(
    table.border.top.color = "black",
    table.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    table.border.bottom.width = px(3)
  ) %>%

# Save table
gtsave(here("plots", "01_creel_summary.png"))


###########################################################################
# Appendix

appx <- creel %>%
  gt() %>%
  cols_label(
    date = "Date",
    gear = "Gear",
    count = "Count",
    hours = "Hours Fished",
    sock = "Sockeye",
    coho = "Coho",
    chum = "Chum",
    king = "King",
    pink = "Pink",
    interview = "Interview?"
  ) %>%
  opt_stylize(style = 6, color = 'gray') %>%
  gtsave(here("appendices", "01_creel.png"))