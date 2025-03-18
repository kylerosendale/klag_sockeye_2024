---
title: "Klag Report TEST"
author: "Kyle Rosendale"
date: "2025-03-18"
output: 
  html_document: 
    keep_md: true
---


``` r
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
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(here)
```

```
## here() starts at R:/Weirs/Current Weir Projects/Klag Bay/2022-2025/2024/Data/Analyses/klag_sockeye_2024
```

``` r
library(gt)
library(chromote)

###############################################################
# Source file

here::i_am("code/01_annual_harvest.R")
```

```
## here() starts at R:/Weirs/Current Weir Projects/Klag Bay/2022-2025/2024/Data/Analyses/klag_sockeye_2024
```

``` r
creel <- read_csv(here("data", "01_creel.csv"))
```

```
## Rows: 9 Columns: 10
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): date, gear
## dbl (8): count, hours, sock, coho, chum, king, pink, interview
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
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
```

```
## NULL
```

``` r
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
```

```
## Warning: Since gt v0.9.0, the `formatter` argument (and associated `...`) has been
## deprecated.
## • Please use the `fmt` argument to provide formatting directives.
## This warning is displayed once every 8 hours.
```

```
## file:///C:/Users/KYLE~1.ROS/AppData/Local/Temp/RtmpgHIWMM/file83ec163055e0.html screenshot completed
```

``` r
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
```

```
## file:///C:/Users/KYLE~1.ROS/AppData/Local/Temp/RtmpgHIWMM/file83ec7ad46362.html screenshot completed
```
