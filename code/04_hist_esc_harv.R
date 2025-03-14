###############################################################
# Summarize historic Klag Lake sockeye escapement and harvest data
# kyle.rosendale@sitkatribe-nsn.gov
# October 2023

###############################################################
# Run Us First!

# 01_annual_harvest.R, 02_annual_sockeye.R, and 03_mark_recap.R
# must all be run prior to this script.

# NOTE: YOU MUST SELECT ESCAPEMENT AND HARVEST ESTIMATES METHODS (SEE LINE 55)
# NOTE: YOU MUST MANUALLY ENTER PERMIT HARVEST BELOW (SEE LINE 62)


###############################################################
# Outputs

# appendices/04_eh_hist.png       historical annual return data
# outputs/04_esc_hist_harv.csv    historical escapement and harvest estimates
# plots/04_hist_esc.png           historical escapement estimates plot with M-R data
# plots/04_hist_esc_harv.png      historical escapement and harvest plot
# results/04_esc_harv_ranks.csv   ranked summary escapement and harvest data


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(gt)


###############################################################
# Source file

here::i_am("code/04_hist_esc_harv.R")
hist <- read_csv(here("data", "04_esc_harv_hist.csv"))

# Need to pull files from results folder for current year estimates
harv_curr <- read_csv(here("inputs", "02_daily_harv.csv"))
esc_curr <- read_csv(here("results", "02_final_estimates.csv"))
mr_curr <- read_csv(here("results", "03_recap_estimates.csv"))


###############################################################
# Begin analysis

# Make new row on data frame for current year estimates
# Pull current year estimates into single vector
# DOUBLE CHECK ENTRIES BELOW FOR ESCAPEMENT ESTIMATE METHOD AND HARVEST ESTIMATE METHOD
# Escapement estimate can be weir count (most common), mark-recapture, or expanded escapement (unlikely)
# Harvest estimate can be creel survey estimate (most common) or permit reported harvest
# Confirm values in hist_curr (line 59)
est_curr <- c(max(hist$year) + 1,
              subset(esc_curr, parameter == "weir_count")$estimate, # Escapement estimate; select method
              subset(esc_curr, parameter == "weir_count")$estimate, # Weir count
              subset(mr_curr, parameter == "n")$estimate, # M-R estimate
              subset(mr_curr, parameter == "lwr95")$estimate, # M-R lower 95% CI
              subset(mr_curr, parameter == "upr95")$estimate, # M-R upper 95% CI
              sum(harv_curr$harv_est), # Harvest estimate; select harvest method
              491, # permit reported harvest; CONTACT ADFG AND MANUALLY ENTER
              sum(harv_curr$harv_est) # Creel survey harvest estimate
              )
  
# Make data frame with current year data matching historical format
# Remove columns added from last year's table
hist <- hist %>%
  select(-(exp_rate:mr_error))

hist_curr <- tibble(parameter = colnames(hist), estimates = est_curr) %>%
  pivot_wider(names_from = parameter, values_from = estimates)

# Change 2024 escapement estimate to account for expanded escapement
hist_curr$esc_est <- 8507

# Add current year data to historical data and write to .csv for next year
hist <- rbind(hist, hist_curr) %>%
  mutate(exp_rate = harv_est / (esc_est + harv_est),
         return = esc_est + harv_est,
         mr_error = abs(esc_mr - esc_weir) / esc_weir * 100) %>%
  write_csv(here("outputs", "04_esc_harv_hist.csv")) 


###############################################################
# Make plots

# Plot weir count, M-R estimate and final escapement for all years
# Make a new data frame for this plot
hist_plot <- hist %>%
  select(year:upp_bnd) %>%
  gather(esc_weir, esc_mr, esc_est, key = "key", value = "value") %>%
  arrange(key) # Helps with assigning colors in next step

ggplot(hist_plot, aes(year, value)) +
  geom_col(data = filter(hist_plot, key == "esc_est"), fill = "gray") +
  geom_point(aes(color = key), size = 4) +
  geom_errorbar(data = hist, aes(y = esc_mr, ymin = lwr_bnd, ymax = upp_bnd),
                color = "#FFCD00", width = 0.1, size = 1) +
  geom_hline(yintercept = median(hist$esc_est), linetype = 2) +
  geom_text(x = 2014, y = 18000, angle = 90, label = "Weir Breached") + 
  geom_text(x = 2015, y = 8000, angle = 90, label = "Weir Breached") +
  scale_color_manual(labels = c("Final Estimate", "Mark-Recapture Estimate",
                                "Weir Count"),
                     values = c(esc_est = "gray",
                                esc_weir = "#006AA7",
                                esc_mr = "#FFCD00")) +
  theme_classic() +
  theme(legend.position = c(0.4, 0.9)) +
  theme(legend.title = element_blank()) +
  labs(x = element_blank(), y = "Sockeye") +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "04_hist_esc.png"))

# Plot historic escapement, harvest, and exploitation rate
hist_plot <- hist %>%
  select(year, esc_est, harv_est, exp_rate) %>%
  gather(esc_est, harv_est, key = "estimate", value = "sockeye") %>%
  mutate(estimate = fct_relevel(estimate, "harv_est", "esc_est"))

ggplot(hist_plot) +
  geom_col(aes(x = year, y = sockeye, fill = estimate)) +
  geom_line(aes(x = year, y = exp_rate * max(hist_plot$sockeye)), size = 2) +
  theme_classic() +
  labs(x = element_blank(), y = "Sockeye") +
  scale_fill_manual(labels = c("Harvest", "Escapement"),
                    values = c(harv_est = "coral1", esc_est = "cyan2")) +
  theme(legend.position = c(0.85, 0.85)) + 
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = scales::comma,
                     sec.axis = sec_axis(~./max(hist_plot$sockeye),
                                         name = "Exploitation Rate")) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "04_hist_esc_harv.png"))


#######################################################################
# Data summaries

# Annual ranks for escapement, harvest, return, and exploitation rate.
hist %>%
  select(year, esc_est, harv_est, return, exp_rate) %>%
  mutate(rank_esc = rank(-esc_est),
         rank_harv = rank(-harv_est),
         rank_ret = rank(-return),
         rank_exp = rank(-exp_rate)) %>%
  arrange(desc(year)) %>%
  write.csv(here("results", "04_esc_harv_ranks.csv"), row.names = FALSE)


#######################################################################
# Appendix

hist %>%
  select(year, esc_est, return, harv_creel, harv_rep) %>%
  gt() %>%
  cols_label(
    year = "Year",
    esc_est = "Escapement",
    return = "Return",
    harv_creel = "Creel Harvest",
    harv_rep = "Permit Harvest"
  ) %>%
  fmt_number(columns = esc_est : harv_rep,
             use_seps = TRUE,
             drop_trailing_zeros = TRUE) %>%
  grand_summary_rows(
    columns = c(esc_est, return, harv_creel, harv_rep),
    fns = list(
      Median = ~median(.),
      Mean = ~mean(.),
      SD = ~sd(.)
    ),
    formatter = fmt_number, drop_trailing_zeros = TRUE
  ) %>%
  cols_width(
    year ~px(60),
    everything() ~ px(100)
  ) %>%
  opt_stylize(style = 6, color = 'gray') %>%
  gtsave(here("appendices", "04_eh_hist.png"))