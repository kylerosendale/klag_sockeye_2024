##############################################################
# Summarize annual Klag Lake sockeye salmon ASL data
# kyle.rosendale@sitkatribe-nsn.gov
# February 2023

###############################################################
# Run Us First!

# Must run 04_hist_esc_harv.R and 06_asl_annual.R before running this script.


###############################################################
# Outputs

# appendices/07_age_prop.png      historical age proportion data
# outputs/07_asl_hist.csv         historical scale aging results
# plots/07_brood_prop_by_age.png  age proportion of returns by brood year plot
# plots/07_len_13_over_time.png   historical length-at-age of age 1.3 fish plot
# plots/07_ln_rs_by_year.png      annual log-transformed R/S estimates plot
# plots/07_prod_vs_esc.png        log-transformed R/S vs. brood escapement plot
# plots/07_ret_by_brood_yr.png    age composition of brood year returns plot
# plots/07_sr.png                 spawners vs. recruits plot (total return)
# plots/07_sr_esc.png             spawners vs. recruits plot (escapement only)
# results/07_asl_summary.csv      historical summary of scales read
# results/07_returns_summ.csv     mean + sd of historical return data
# results/07_sr_models.txt        summary of suite of spawner-recruit models


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(gt)
library(lubridate)
library(ggrepel)
library(janitor)
library(fishmethods)


###############################################################
# Source file

here::i_am("code/07_asl_hist.R")
asl_curr <- read_csv(here("data", "06_asl_curr.csv"))
asl_hist <- read_csv(here("data", "07_asl_hist.csv"))
esc_hist <- read_csv(here("outputs", "04_esc_harv_hist.csv"))


###############################################################
# Start analysis

# Combine current year and historical data. Save for future.
asl_curr <- asl_curr %>%
  mutate(`Sample Date` = mdy(`Sample Date`))

asl_hist <- asl_hist %>%
  mutate(`Sample Date` = mdy(`Sample Date`))

asl <- bind_rows(asl_curr, asl_hist)

asl %>%
write.csv(here("outputs", "07_asl_hist.csv"), row.names = FALSE) # Use for next year

# Eliminate unnecessary columns and rename
asl <- asl %>%
  dplyr::select(1, 6, 7, 19, 21, 24, 34) %>%
  rename(year = 1, date = 2, stat_wk = 3,
         sex = 4, len = 5, age = 6, comment = 7) %>%
  mutate(sex = tolower(sex), # Convert sex to lower case
         sex = case_when(age == 11 ~ "male", # Make sure all x.1 fish are male
                         age == 21 ~ "male",
                         TRUE ~ sex))

# Create summary of ASL data over time
asl_summ <- asl %>%
  group_by(year) %>%
  summarize(samples = n(),
            scales_read = samples - sum(is.na(age)),
            prop_read = scales_read / samples,
            resorb = sum(comment == "resorbed", na.rm = TRUE),
            prop_resorb = resorb / samples,
            m_f = sum(sex == "male") / sum(sex == "female"))
asl_summ %>%
  write.csv(here("results", "07_asl_summary.csv"), row.names = FALSE)

# Summarize age class proportions over time
sr <- asl %>%
  filter(!is.na(age)) %>%
  mutate(brood_yr = year - # Generate brood year
           (1 + as.numeric(substr(age, 1, 1)) + as.numeric(substr(age, 2, 2))),
         fw = substr(age, 1, 1), # Format age class
         sw = substr(age, 2, 2),
         age_class = paste(fw, sw, sep = "."))

sr_prop <- sr %>%
  group_by(year) %>%
  summarize(samples = n(),
            prop_11 = sum(age_class == "1.1") / samples,
            prop_12 = sum(age_class == "1.2") / samples,
            prop_13 = sum(age_class == "1.3") / samples,
            prop_14 = sum(age_class == "1.4") / samples,
            prop_21 = sum(age_class == "2.1") / samples,
            prop_22 = sum(age_class == "2.2") / samples,
            prop_23 = sum(age_class == "2.3") / samples,
            prop_24 = sum(age_class == "2.4") / samples,
            prop_1x = prop_11 + prop_12 + prop_13 + prop_14,
            prop_2x = 1 - prop_1x,
            prop_x1 = prop_11 + prop_21,
            prop_x2 = prop_12 + prop_22,
            prop_x3 = prop_13 + prop_23)

# Analyze by brood year
esc_hist <- esc_hist %>%
  select(year, esc_est, return)

# Need to add leading years for brood table
sr_ret <- full_join(sr_prop, esc_hist, by = "year") %>%
  rename(brood_yr = year,
         spawners = esc_est) %>%
  mutate(ret_11 = lead(prop_11 * return, n = 3),
         ret_12 = lead(prop_12 * return, n = 4),
         ret_13 = lead(prop_13 * return, n = 5),
         ret_14 = lead(prop_14 * return, n = 6),
         ret_21 = lead(prop_21 * return, n = 4),
         ret_22 = lead(prop_22 * return, n = 5),
         ret_23 = lead(prop_23 * return, n = 6),
         ret_24 = lead(prop_24 * return, n = 7)
         ) %>%
  filter(brood_yr <= max(brood_yr) - 6) %>% # Include only complete years
  select(-(2:15)) %>% # Remove unnecessary columns
  rowwise() %>%
  mutate(by_ret = sum(c_across(ret_11:ret_24), na.rm = TRUE),
         rs = by_ret / spawners,
         ln_rs = log(rs, base = exp(1))) %>%
  ungroup() %>%
  mutate(prod_rank = rank(-ln_rs))

sr_ret %>%
  summarize(mean_term = mean(return),
            sd_term = sd(return),
            mean_esc = mean(spawners),
            sd_esc = sd(spawners),
            mean_by_ret = mean(by_ret),
            sd_by_ret = sd(by_ret)) %>%
  write.csv(here("results", "07_returns_summ.csv"), row.names = FALSE)

# Spawner-recruit models
sr_model <- sr(sr_ret$by_ret, sr_ret$spawners, c(0:10, 13:14)) # Deriso-Schnute models don't have enough data
capture.output(sr_model, file = here("results", "07_sr_models.txt")) # .txt is easier to read than .csv


##################################################################################
# Plots

# Plot recruits per spawner
ggplot(sr_ret, aes(x = spawners, y = by_ret, label = brood_yr)) +
  geom_point() +
  geom_text_repel(size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth() +
  expand_limits(x = 0) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  labs(x = "Spawners", y = "Recruits (Escapement + Harvest)")
ggsave(here("plots", "07_sr.png"))

# Plot escapement per spawner
sr_esc <- full_join(sr_prop, esc_hist, by = "year") %>%
  rename(brood_yr = year) %>%
  mutate(esc_11 = lead(prop_11 * esc_est, n = 3),
         esc_12 = lead(prop_12 * esc_est, n = 4),
         esc_13 = lead(prop_13 * esc_est, n = 5),
         esc_14 = lead(prop_14 * esc_est, n = 6),
         esc_21 = lead(prop_21 * esc_est, n = 4),
         esc_22 = lead(prop_22 * esc_est, n = 5),
         esc_23 = lead(prop_23 * esc_est, n = 6),
         esc_24 = lead(prop_24 * esc_est, n = 7)
  ) %>%
  filter(brood_yr <= max(brood_yr) - 6) %>%
  select(1, esc_est, 18:25) %>% # Select only relevant columns
  rowwise() %>%
  mutate(by_esc = sum(c_across(esc_11:esc_24), na.rm = TRUE))

ggplot(sr_esc, aes(x = esc_est, y = by_esc, label = brood_yr)) +
  geom_point() +
  geom_text_repel(size = 6) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  expand_limits(x = 0) +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  labs(x = "Spawners", y = "Recruits (Escapement Only)")
ggsave(here("plots", "07_sr_esc.png"))

# Plot recruits-per-spawner
ggplot(sr_ret, aes(brood_yr, ln_rs)) +
  geom_col() +
  theme_classic() +
  labs(x = "Brood Year", y = "ln(R/S)") +
  theme(text = element_text(size = 20))
ggsave(here("plots", "07_ln_rs_by_year.png"))

# Plot productivity vs. escapement
ggplot(sr_ret, aes(spawners, ln_rs, label = brood_yr, color = brood_yr)) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_text_repel(size = 6) +
  labs(x = "Brood Year Escapement", y = "Productivity ln(R/S)") +
  scale_x_continuous(labels = scales::comma) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "07_prod_vs_esc.png"))

# Plot recruits per brood year; need to use long format
ret_n <- sr_ret %>%
  replace(is.na(.), 0) %>% # Allows analysis of latest brood year with just 2.4 missing
  gather(ret_11:ret_24, key = "age_class", value = "return") %>%
  mutate(age_class = fct_relevel(age_class,
                                 "ret_24", "ret_23", "ret_22", "ret_21",
                                 "ret_14", "ret_13", "ret_12", "ret_11"))

ggplot(ret_n) + geom_col(aes(x = brood_yr, y = return, fill = age_class)) +
  theme_classic() +
  scale_fill_brewer(palette = "Spectral",
                    labels = c("2.4", "2.3", "2.2", "2.1",
                               "1.4", "1.3", "1.2", "1.1")) +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Brood Year", y = "Return")
ggsave(here("plots", "07_ret_by_brood_yr.png"))


# Plot age class proportion by brood year
by_prop <- sr_ret %>%
  select(-c(spawners, return, rs:prod_rank)) %>%
  replace(is.na(.), 0) %>%
  mutate(prop_11 = ret_11 / by_ret,
         prop_12 = ret_12 / by_ret,
         prop_13 = ret_13 / by_ret,
         prop_14 = ret_14 / by_ret,
         prop_21 = ret_21 / by_ret,
         prop_22 = ret_22 / by_ret,
         prop_23 = ret_23 / by_ret,
         prop_24 = ret_24 / by_ret) %>%
  select(-c(ret_11:by_ret)) %>%
  mutate(tot = rowSums(across(c(prop_11:prop_24))))

by_age <- by_prop %>%
  gather(prop_11:prop_24, key = "age_class", value = "prop") %>%
  mutate(age_class = fct_relevel(age_class,
                                 "prop_24", "prop_23", "prop_22", "prop_21",
                                 "prop_14", "prop_13", "prop_12", "prop_11"))

ggplot(by_age) +
  geom_col(aes(x = brood_yr, y = prop, fill = age_class)) +
  theme_classic() +
  scale_fill_brewer(palette = "Spectral",
                    labels = c("2.4", "2.3", "2.2", "2.1",
                               "1.4", "1.3", "1.2", "1.1")) +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20)) +
  labs(x = "Brood Year", y = "Proportion")
ggsave(here("plots", "07_brood_prop_by_age.png"))

# Plot length of 1.3 fish over time
len_13 <- asl %>%
  filter(age == 13) %>%
  filter(sex != c("indiscernible")) %>%
  filter(sex != c("not observed")) %>%
  group_by(year, sex) %>%
  summarize(length = mean(len, na.rm = TRUE),
            sd = sd(len, na.rm = TRUE)) %>%
  mutate(lwr = length - sd,
         upr = length + sd)

ggplot(len_13, aes(x = year, y = length, color = sex)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  theme_classic() +
  labs(y = "Length (mm)", x = element_blank()) +
  theme(legend.title = element_blank()) +
  geom_smooth() +
  theme(text = element_text(size = 20))
ggsave(here("plots", "07_len_13_over_time.png"))


###########################################################################
# Appendix

sr_prop %>%
  select(year:prop_24) %>%
  gt() %>%
  opt_stylize(style = 6, color = 'gray') %>%
  gtsave(here("appendices", "07_age_prop.png"))