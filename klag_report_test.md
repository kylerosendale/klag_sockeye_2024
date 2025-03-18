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
  print()
```

```
## # A tibble: 4 × 2
##   parameter      estimate
##   <chr>             <dbl>
## 1 parties           9    
## 2 interviews        7    
## 3 interview_prop    0.778
## 4 max_harv        324
```

``` r
# Make .csv for reporting dates
harv_dates <- tribble(
  ~parameter, ~estimate,
  "first_sock", min(creel_daily$date),
  "last_sock", max(creel_daily$date),
  "max_harv", harv_date$date) %>%
  print()
```

```
## # A tibble: 3 × 2
##   parameter  estimate 
##   <chr>      <chr>    
## 1 first_sock 6/29/2024
## 2 last_sock  8/18/2024
## 3 max_harv   7/29/2024
```

``` r
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
  arrange(desc(harv_exp)) %>%
  print()
```

```
## # A tibble: 2 × 8
##   gear      n   int harv_rep  mean    sd    cv harv_exp
##   <chr> <int> <dbl>    <dbl> <dbl> <dbl> <dbl>    <dbl>
## 1 seine     2     2      574 287   52.3    0        574
## 2 rod       7     5       12   2.4  2.51  29.8       17
```

``` r
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
  )
```

```
## Warning: Since gt v0.9.0, the `formatter` argument (and associated `...`) has been
## deprecated.
## • Please use the `fmt` argument to provide formatting directives.
## This warning is displayed once every 8 hours.
```

```{=html}
<div id="nsgewwaixv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#nsgewwaixv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#nsgewwaixv thead, #nsgewwaixv tbody, #nsgewwaixv tfoot, #nsgewwaixv tr, #nsgewwaixv td, #nsgewwaixv th {
  border-style: none;
}

#nsgewwaixv p {
  margin: 0;
  padding: 0;
}

#nsgewwaixv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #000000;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#nsgewwaixv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#nsgewwaixv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#nsgewwaixv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#nsgewwaixv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#nsgewwaixv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nsgewwaixv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#nsgewwaixv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#nsgewwaixv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#nsgewwaixv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#nsgewwaixv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#nsgewwaixv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#nsgewwaixv .gt_spanner_row {
  border-bottom-style: hidden;
}

#nsgewwaixv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#nsgewwaixv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#nsgewwaixv .gt_from_md > :first-child {
  margin-top: 0;
}

#nsgewwaixv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#nsgewwaixv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#nsgewwaixv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#nsgewwaixv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#nsgewwaixv .gt_row_group_first td {
  border-top-width: 2px;
}

#nsgewwaixv .gt_row_group_first th {
  border-top-width: 2px;
}

#nsgewwaixv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nsgewwaixv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#nsgewwaixv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#nsgewwaixv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nsgewwaixv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#nsgewwaixv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#nsgewwaixv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#nsgewwaixv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#nsgewwaixv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#nsgewwaixv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#nsgewwaixv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nsgewwaixv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#nsgewwaixv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#nsgewwaixv .gt_left {
  text-align: left;
}

#nsgewwaixv .gt_center {
  text-align: center;
}

#nsgewwaixv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#nsgewwaixv .gt_font_normal {
  font-weight: normal;
}

#nsgewwaixv .gt_font_bold {
  font-weight: bold;
}

#nsgewwaixv .gt_font_italic {
  font-style: italic;
}

#nsgewwaixv .gt_super {
  font-size: 65%;
}

#nsgewwaixv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#nsgewwaixv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#nsgewwaixv .gt_indent_1 {
  text-indent: 5px;
}

#nsgewwaixv .gt_indent_2 {
  text-indent: 10px;
}

#nsgewwaixv .gt_indent_3 {
  text-indent: 15px;
}

#nsgewwaixv .gt_indent_4 {
  text-indent: 20px;
}

#nsgewwaixv .gt_indent_5 {
  text-indent: 25px;
}

#nsgewwaixv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#nsgewwaixv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="gear"><span class='gt_from_md'><strong>Gear</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n"><span class='gt_from_md'><strong>Parties<br>Observed</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="int"><span class='gt_from_md'><strong>Interviews</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="harv_rep"><span class='gt_from_md'><strong>Creel Reported<br>Harvest</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean"><span class='gt_from_md'><strong>Mean</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd"><span class='gt_from_md'><strong>SD</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="cv"><span class='gt_from_md'><strong>CV%</strong></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="harv_exp"><span class='gt_from_md'><strong>Expanded<br>Harvest</strong></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_1 gear" class="gt_row gt_left">seine</td>
<td headers="stub_1_1 n" class="gt_row gt_right">2</td>
<td headers="stub_1_1 int" class="gt_row gt_right">2</td>
<td headers="stub_1_1 harv_rep" class="gt_row gt_right">574</td>
<td headers="stub_1_1 mean" class="gt_row gt_right">287.0</td>
<td headers="stub_1_1 sd" class="gt_row gt_right">52.3</td>
<td headers="stub_1_1 cv" class="gt_row gt_right">0.0</td>
<td headers="stub_1_1 harv_exp" class="gt_row gt_right">574</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_2 gear" class="gt_row gt_left">rod</td>
<td headers="stub_1_2 n" class="gt_row gt_right">7</td>
<td headers="stub_1_2 int" class="gt_row gt_right">5</td>
<td headers="stub_1_2 harv_rep" class="gt_row gt_right">12</td>
<td headers="stub_1_2 mean" class="gt_row gt_right">2.4</td>
<td headers="stub_1_2 sd" class="gt_row gt_right">2.5</td>
<td headers="stub_1_2 cv" class="gt_row gt_right">29.8</td>
<td headers="stub_1_2 harv_exp" class="gt_row gt_right">17</td></tr>
    <tr><th id="grand_summary_stub_1" scope="row" class="gt_row gt_left gt_stub gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">Total</th>
<td headers="grand_summary_stub_1 gear" class="gt_row gt_left gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">—</td>
<td headers="grand_summary_stub_1 n" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">9</td>
<td headers="grand_summary_stub_1 int" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">7</td>
<td headers="grand_summary_stub_1 harv_rep" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">586</td>
<td headers="grand_summary_stub_1 mean" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">—</td>
<td headers="grand_summary_stub_1 sd" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">—</td>
<td headers="grand_summary_stub_1 cv" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">—</td>
<td headers="grand_summary_stub_1 harv_exp" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">591</td></tr>
  </tbody>
  
  
</table>
</div>
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
  print()
```

```
## <div id="yjsznwgwtf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
##   <style>#yjsznwgwtf table {
##   font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
##   -webkit-font-smoothing: antialiased;
##   -moz-osx-font-smoothing: grayscale;
## }
## 
## #yjsznwgwtf thead, #yjsznwgwtf tbody, #yjsznwgwtf tfoot, #yjsznwgwtf tr, #yjsznwgwtf td, #yjsznwgwtf th {
##   border-style: none;
## }
## 
## #yjsznwgwtf p {
##   margin: 0;
##   padding: 0;
## }
## 
## #yjsznwgwtf .gt_table {
##   display: table;
##   border-collapse: collapse;
##   line-height: normal;
##   margin-left: auto;
##   margin-right: auto;
##   color: #333333;
##   font-size: 16px;
##   font-weight: normal;
##   font-style: normal;
##   background-color: #FFFFFF;
##   width: auto;
##   border-top-style: solid;
##   border-top-width: 2px;
##   border-top-color: #5F5F5F;
##   border-right-style: none;
##   border-right-width: 2px;
##   border-right-color: #D3D3D3;
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
##   border-left-style: none;
##   border-left-width: 2px;
##   border-left-color: #D3D3D3;
## }
## 
## #yjsznwgwtf .gt_caption {
##   padding-top: 4px;
##   padding-bottom: 4px;
## }
## 
## #yjsznwgwtf .gt_title {
##   color: #333333;
##   font-size: 125%;
##   font-weight: initial;
##   padding-top: 4px;
##   padding-bottom: 4px;
##   padding-left: 5px;
##   padding-right: 5px;
##   border-bottom-color: #FFFFFF;
##   border-bottom-width: 0;
## }
## 
## #yjsznwgwtf .gt_subtitle {
##   color: #333333;
##   font-size: 85%;
##   font-weight: initial;
##   padding-top: 3px;
##   padding-bottom: 5px;
##   padding-left: 5px;
##   padding-right: 5px;
##   border-top-color: #FFFFFF;
##   border-top-width: 0;
## }
## 
## #yjsznwgwtf .gt_heading {
##   background-color: #FFFFFF;
##   text-align: center;
##   border-bottom-color: #FFFFFF;
##   border-left-style: none;
##   border-left-width: 1px;
##   border-left-color: #D3D3D3;
##   border-right-style: none;
##   border-right-width: 1px;
##   border-right-color: #D3D3D3;
## }
## 
## #yjsznwgwtf .gt_bottom_border {
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
## }
## 
## #yjsznwgwtf .gt_col_headings {
##   border-top-style: solid;
##   border-top-width: 2px;
##   border-top-color: #5F5F5F;
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
##   border-left-style: none;
##   border-left-width: 1px;
##   border-left-color: #D3D3D3;
##   border-right-style: none;
##   border-right-width: 1px;
##   border-right-color: #D3D3D3;
## }
## 
## #yjsznwgwtf .gt_col_heading {
##   color: #FFFFFF;
##   background-color: #5F5F5F;
##   font-size: 100%;
##   font-weight: normal;
##   text-transform: inherit;
##   border-left-style: none;
##   border-left-width: 1px;
##   border-left-color: #D3D3D3;
##   border-right-style: none;
##   border-right-width: 1px;
##   border-right-color: #D3D3D3;
##   vertical-align: bottom;
##   padding-top: 5px;
##   padding-bottom: 6px;
##   padding-left: 5px;
##   padding-right: 5px;
##   overflow-x: hidden;
## }
## 
## #yjsznwgwtf .gt_column_spanner_outer {
##   color: #FFFFFF;
##   background-color: #5F5F5F;
##   font-size: 100%;
##   font-weight: normal;
##   text-transform: inherit;
##   padding-top: 0;
##   padding-bottom: 0;
##   padding-left: 4px;
##   padding-right: 4px;
## }
## 
## #yjsznwgwtf .gt_column_spanner_outer:first-child {
##   padding-left: 0;
## }
## 
## #yjsznwgwtf .gt_column_spanner_outer:last-child {
##   padding-right: 0;
## }
## 
## #yjsznwgwtf .gt_column_spanner {
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
##   vertical-align: bottom;
##   padding-top: 5px;
##   padding-bottom: 5px;
##   overflow-x: hidden;
##   display: inline-block;
##   width: 100%;
## }
## 
## #yjsznwgwtf .gt_spanner_row {
##   border-bottom-style: hidden;
## }
## 
## #yjsznwgwtf .gt_group_heading {
##   padding-top: 8px;
##   padding-bottom: 8px;
##   padding-left: 5px;
##   padding-right: 5px;
##   color: #333333;
##   background-color: #FFFFFF;
##   font-size: 100%;
##   font-weight: initial;
##   text-transform: inherit;
##   border-top-style: solid;
##   border-top-width: 2px;
##   border-top-color: #5F5F5F;
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
##   border-left-style: none;
##   border-left-width: 1px;
##   border-left-color: #D3D3D3;
##   border-right-style: none;
##   border-right-width: 1px;
##   border-right-color: #D3D3D3;
##   vertical-align: middle;
##   text-align: left;
## }
## 
## #yjsznwgwtf .gt_empty_group_heading {
##   padding: 0.5px;
##   color: #333333;
##   background-color: #FFFFFF;
##   font-size: 100%;
##   font-weight: initial;
##   border-top-style: solid;
##   border-top-width: 2px;
##   border-top-color: #5F5F5F;
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
##   vertical-align: middle;
## }
## 
## #yjsznwgwtf .gt_from_md > :first-child {
##   margin-top: 0;
## }
## 
## #yjsznwgwtf .gt_from_md > :last-child {
##   margin-bottom: 0;
## }
## 
## #yjsznwgwtf .gt_row {
##   padding-top: 8px;
##   padding-bottom: 8px;
##   padding-left: 5px;
##   padding-right: 5px;
##   margin: 10px;
##   border-top-style: none;
##   border-top-width: 1px;
##   border-top-color: #D5D5D5;
##   border-left-style: none;
##   border-left-width: 1px;
##   border-left-color: #D5D5D5;
##   border-right-style: none;
##   border-right-width: 1px;
##   border-right-color: #D5D5D5;
##   vertical-align: middle;
##   overflow-x: hidden;
## }
## 
## #yjsznwgwtf .gt_stub {
##   color: #333333;
##   background-color: #D5D5D5;
##   font-size: 100%;
##   font-weight: initial;
##   text-transform: inherit;
##   border-right-style: solid;
##   border-right-width: 2px;
##   border-right-color: #D5D5D5;
##   padding-left: 5px;
##   padding-right: 5px;
## }
## 
## #yjsznwgwtf .gt_stub_row_group {
##   color: #333333;
##   background-color: #FFFFFF;
##   font-size: 100%;
##   font-weight: initial;
##   text-transform: inherit;
##   border-right-style: solid;
##   border-right-width: 2px;
##   border-right-color: #D3D3D3;
##   padding-left: 5px;
##   padding-right: 5px;
##   vertical-align: top;
## }
## 
## #yjsznwgwtf .gt_row_group_first td {
##   border-top-width: 2px;
## }
## 
## #yjsznwgwtf .gt_row_group_first th {
##   border-top-width: 2px;
## }
## 
## #yjsznwgwtf .gt_summary_row {
##   color: #333333;
##   background-color: #FFFFFF;
##   text-transform: inherit;
##   padding-top: 8px;
##   padding-bottom: 8px;
##   padding-left: 5px;
##   padding-right: 5px;
## }
## 
## #yjsznwgwtf .gt_first_summary_row {
##   border-top-style: solid;
##   border-top-color: #5F5F5F;
## }
## 
## #yjsznwgwtf .gt_first_summary_row.thick {
##   border-top-width: 2px;
## }
## 
## #yjsznwgwtf .gt_last_summary_row {
##   padding-top: 8px;
##   padding-bottom: 8px;
##   padding-left: 5px;
##   padding-right: 5px;
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
## }
## 
## #yjsznwgwtf .gt_grand_summary_row {
##   color: #333333;
##   background-color: #D5D5D5;
##   text-transform: inherit;
##   padding-top: 8px;
##   padding-bottom: 8px;
##   padding-left: 5px;
##   padding-right: 5px;
## }
## 
## #yjsznwgwtf .gt_first_grand_summary_row {
##   padding-top: 8px;
##   padding-bottom: 8px;
##   padding-left: 5px;
##   padding-right: 5px;
##   border-top-style: double;
##   border-top-width: 6px;
##   border-top-color: #5F5F5F;
## }
## 
## #yjsznwgwtf .gt_last_grand_summary_row_top {
##   padding-top: 8px;
##   padding-bottom: 8px;
##   padding-left: 5px;
##   padding-right: 5px;
##   border-bottom-style: double;
##   border-bottom-width: 6px;
##   border-bottom-color: #5F5F5F;
## }
## 
## #yjsznwgwtf .gt_striped {
##   background-color: #F4F4F4;
## }
## 
## #yjsznwgwtf .gt_table_body {
##   border-top-style: solid;
##   border-top-width: 2px;
##   border-top-color: #5F5F5F;
##   border-bottom-style: solid;
##   border-bottom-width: 2px;
##   border-bottom-color: #5F5F5F;
## }
## 
## #yjsznwgwtf .gt_footnotes {
##   color: #333333;
##   background-color: #FFFFFF;
##   border-bottom-style: none;
##   border-bottom-width: 2px;
##   border-bottom-color: #D3D3D3;
##   border-left-style: none;
##   border-left-width: 2px;
##   border-left-color: #D3D3D3;
##   border-right-style: none;
##   border-right-width: 2px;
##   border-right-color: #D3D3D3;
## }
## 
## #yjsznwgwtf .gt_footnote {
##   margin: 0px;
##   font-size: 90%;
##   padding-top: 4px;
##   padding-bottom: 4px;
##   padding-left: 5px;
##   padding-right: 5px;
## }
## 
## #yjsznwgwtf .gt_sourcenotes {
##   color: #333333;
##   background-color: #FFFFFF;
##   border-bottom-style: none;
##   border-bottom-width: 2px;
##   border-bottom-color: #D3D3D3;
##   border-left-style: none;
##   border-left-width: 2px;
##   border-left-color: #D3D3D3;
##   border-right-style: none;
##   border-right-width: 2px;
##   border-right-color: #D3D3D3;
## }
## 
## #yjsznwgwtf .gt_sourcenote {
##   font-size: 90%;
##   padding-top: 4px;
##   padding-bottom: 4px;
##   padding-left: 5px;
##   padding-right: 5px;
## }
## 
## #yjsznwgwtf .gt_left {
##   text-align: left;
## }
## 
## #yjsznwgwtf .gt_center {
##   text-align: center;
## }
## 
## #yjsznwgwtf .gt_right {
##   text-align: right;
##   font-variant-numeric: tabular-nums;
## }
## 
## #yjsznwgwtf .gt_font_normal {
##   font-weight: normal;
## }
## 
## #yjsznwgwtf .gt_font_bold {
##   font-weight: bold;
## }
## 
## #yjsznwgwtf .gt_font_italic {
##   font-style: italic;
## }
## 
## #yjsznwgwtf .gt_super {
##   font-size: 65%;
## }
## 
## #yjsznwgwtf .gt_footnote_marks {
##   font-size: 75%;
##   vertical-align: 0.4em;
##   position: initial;
## }
## 
## #yjsznwgwtf .gt_asterisk {
##   font-size: 100%;
##   vertical-align: 0;
## }
## 
## #yjsznwgwtf .gt_indent_1 {
##   text-indent: 5px;
## }
## 
## #yjsznwgwtf .gt_indent_2 {
##   text-indent: 10px;
## }
## 
## #yjsznwgwtf .gt_indent_3 {
##   text-indent: 15px;
## }
## 
## #yjsznwgwtf .gt_indent_4 {
##   text-indent: 20px;
## }
## 
## #yjsznwgwtf .gt_indent_5 {
##   text-indent: 25px;
## }
## 
## #yjsznwgwtf .katex-display {
##   display: inline-flex !important;
##   margin-bottom: 0.75em !important;
## }
## 
## #yjsznwgwtf div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
##   height: 0px !important;
## }
## </style>
##   <table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
##   <thead>
##     <tr class="gt_col_headings">
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="date">Date</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="gear">Gear</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="count">Count</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="hours">Hours Fished</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sock">Sockeye</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="coho">Coho</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="chum">Chum</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="king">King</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="pink">Pink</th>
##       <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="interview">Interview?</th>
##     </tr>
##   </thead>
##   <tbody class="gt_table_body">
##     <tr><td headers="date" class="gt_row gt_right">6/29/2024</td>
## <td headers="gear" class="gt_row gt_left">rod</td>
## <td headers="count" class="gt_row gt_right">NA</td>
## <td headers="hours" class="gt_row gt_right">NA</td>
## <td headers="sock" class="gt_row gt_right">NA</td>
## <td headers="coho" class="gt_row gt_right">NA</td>
## <td headers="chum" class="gt_row gt_right">NA</td>
## <td headers="king" class="gt_row gt_right">NA</td>
## <td headers="pink" class="gt_row gt_right">NA</td>
## <td headers="interview" class="gt_row gt_right">0</td></tr>
##     <tr><td headers="date" class="gt_row gt_right gt_striped">7/11/2024</td>
## <td headers="gear" class="gt_row gt_left gt_striped">rod</td>
## <td headers="count" class="gt_row gt_right gt_striped">1</td>
## <td headers="hours" class="gt_row gt_right gt_striped">2.5</td>
## <td headers="sock" class="gt_row gt_right gt_striped">3</td>
## <td headers="coho" class="gt_row gt_right gt_striped">0</td>
## <td headers="chum" class="gt_row gt_right gt_striped">0</td>
## <td headers="king" class="gt_row gt_right gt_striped">0</td>
## <td headers="pink" class="gt_row gt_right gt_striped">0</td>
## <td headers="interview" class="gt_row gt_right gt_striped">1</td></tr>
##     <tr><td headers="date" class="gt_row gt_right">7/12/2024</td>
## <td headers="gear" class="gt_row gt_left">seine</td>
## <td headers="count" class="gt_row gt_right">1</td>
## <td headers="hours" class="gt_row gt_right">4.0</td>
## <td headers="sock" class="gt_row gt_right">250</td>
## <td headers="coho" class="gt_row gt_right">0</td>
## <td headers="chum" class="gt_row gt_right">0</td>
## <td headers="king" class="gt_row gt_right">0</td>
## <td headers="pink" class="gt_row gt_right">0</td>
## <td headers="interview" class="gt_row gt_right">1</td></tr>
##     <tr><td headers="date" class="gt_row gt_right gt_striped">7/20/2024</td>
## <td headers="gear" class="gt_row gt_left gt_striped">rod</td>
## <td headers="count" class="gt_row gt_right gt_striped">NA</td>
## <td headers="hours" class="gt_row gt_right gt_striped">2.0</td>
## <td headers="sock" class="gt_row gt_right gt_striped">6</td>
## <td headers="coho" class="gt_row gt_right gt_striped">0</td>
## <td headers="chum" class="gt_row gt_right gt_striped">0</td>
## <td headers="king" class="gt_row gt_right gt_striped">0</td>
## <td headers="pink" class="gt_row gt_right gt_striped">0</td>
## <td headers="interview" class="gt_row gt_right gt_striped">1</td></tr>
##     <tr><td headers="date" class="gt_row gt_right">7/27/2024</td>
## <td headers="gear" class="gt_row gt_left">rod</td>
## <td headers="count" class="gt_row gt_right">NA</td>
## <td headers="hours" class="gt_row gt_right">1.0</td>
## <td headers="sock" class="gt_row gt_right">0</td>
## <td headers="coho" class="gt_row gt_right">8</td>
## <td headers="chum" class="gt_row gt_right">0</td>
## <td headers="king" class="gt_row gt_right">0</td>
## <td headers="pink" class="gt_row gt_right">0</td>
## <td headers="interview" class="gt_row gt_right">1</td></tr>
##     <tr><td headers="date" class="gt_row gt_right gt_striped">7/28/2024</td>
## <td headers="gear" class="gt_row gt_left gt_striped">rod</td>
## <td headers="count" class="gt_row gt_right gt_striped">NA</td>
## <td headers="hours" class="gt_row gt_right gt_striped">1.5</td>
## <td headers="sock" class="gt_row gt_right gt_striped">0</td>
## <td headers="coho" class="gt_row gt_right gt_striped">4</td>
## <td headers="chum" class="gt_row gt_right gt_striped">0</td>
## <td headers="king" class="gt_row gt_right gt_striped">0</td>
## <td headers="pink" class="gt_row gt_right gt_striped">0</td>
## <td headers="interview" class="gt_row gt_right gt_striped">1</td></tr>
##     <tr><td headers="date" class="gt_row gt_right">7/28/2024</td>
## <td headers="gear" class="gt_row gt_left">rod</td>
## <td headers="count" class="gt_row gt_right">NA</td>
## <td headers="hours" class="gt_row gt_right">2.0</td>
## <td headers="sock" class="gt_row gt_right">3</td>
## <td headers="coho" class="gt_row gt_right">1</td>
## <td headers="chum" class="gt_row gt_right">0</td>
## <td headers="king" class="gt_row gt_right">0</td>
## <td headers="pink" class="gt_row gt_right">0</td>
## <td headers="interview" class="gt_row gt_right">1</td></tr>
##     <tr><td headers="date" class="gt_row gt_right gt_striped">7/29/2024</td>
## <td headers="gear" class="gt_row gt_left gt_striped">seine</td>
## <td headers="count" class="gt_row gt_right gt_striped">NA</td>
## <td headers="hours" class="gt_row gt_right gt_striped">4.5</td>
## <td headers="sock" class="gt_row gt_right gt_striped">324</td>
## <td headers="coho" class="gt_row gt_right gt_striped">5</td>
## <td headers="chum" class="gt_row gt_right gt_striped">1</td>
## <td headers="king" class="gt_row gt_right gt_striped">0</td>
## <td headers="pink" class="gt_row gt_right gt_striped">4</td>
## <td headers="interview" class="gt_row gt_right gt_striped">1</td></tr>
##     <tr><td headers="date" class="gt_row gt_right">8/18/2024</td>
## <td headers="gear" class="gt_row gt_left">rod</td>
## <td headers="count" class="gt_row gt_right">NA</td>
## <td headers="hours" class="gt_row gt_right">NA</td>
## <td headers="sock" class="gt_row gt_right">NA</td>
## <td headers="coho" class="gt_row gt_right">NA</td>
## <td headers="chum" class="gt_row gt_right">NA</td>
## <td headers="king" class="gt_row gt_right">NA</td>
## <td headers="pink" class="gt_row gt_right">NA</td>
## <td headers="interview" class="gt_row gt_right">0</td></tr>
##   </tbody>
##   
##   
## </table>
## </div>
```
