###############################################################
# Mark-recapture estimate for Klag Lake sockeye salmon
# kyle.rosendale@sitkatribe-nsn.gov
# October 2023


###############################################################
# Run Us First!

# 01_annual_harvest.R and 02_annual_sockeye.R must be run before this file.


###############################################################
# Outputs

# results/03_mr_dates.csv              recapture survey dates
# results/03_recap_estimates.csv       mark-recap summary stats


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(recapr)


###############################################################
# Source file

here::i_am("code/03_mark_recap.R")
mr <- read_csv(here("data", "03_mark_recap.csv"))
weir <- read_csv(here("results", "02_final_estimates.csv"))


###############################################################
# Begin analysis

# Assign number marked at weir to an object for ease
weir_mark <- weir %>%
  filter(parameter == "marked")
m <- weir_mark$estimate

# Output file with Petersen M-R estimate with bootstrapped CI
# NOTE: recapr documentation suggests bootstrapped CIs are best choice.
# NOTE: for R < 50, use table in Ricker 1975 (p. 343) to compute CIs.
recap_estimates <- tribble(
  ~parameter, ~estimate,
  "r", sum(mr$r),
  "c", sum(mr$c),
  "n", ciPetersen(m, sum(mr$c), sum(mr$r))$Nhat,
  "recap_prop", sum(mr$r) / sum(mr$c),
  "lwr95", ciPetersen(m, sum(mr$c), sum(mr$r))$ciBoot[1],
  "upr95", ciPetersen(m, sum(mr$c), sum(mr$r))$ciBoot[2],
  "cv", sqrt(vPetersen(m, sum(mr$c), sum(mr$r))) / 
    ciPetersen(m, sum(mr$c), sum(mr$r))$Nhat * 100,
  "ricker_lwr95", m * sum(mr$c) / (4 + 1.92 + 1.96 * sqrt(4 + 1)), #see Ricker 1975 p. 343
  "ricker_upr95", m * sum(mr$c) / (4 + 1.92 - 1.96 * sqrt(4 + 1))
  ) %>%
  write_csv(here("results", "03_recap_estimates.csv"))

# Output file with dates of mark-recap events
mr_dates <- tibble(
  dates = mdy(mr$date)) %>%
  write_csv(here("results", "03_mr_dates.csv"))
