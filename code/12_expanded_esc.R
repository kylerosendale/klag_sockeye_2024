###############################################################
# Exploratory analysis to expand early and late season Klag weir counts
# kyle.rosendale@sitkatribe-nsn.gov
# November 2024

###############################################################
# Run Us First!

# 02_annual_sockeye.R and 05_run_timing.R must be run prior to this script.


###############################################################
# Outputs

# plots/12_exp_excp.png         historical estimates of expanded harvest plot


###############################################################
# Load libraries

library(tidyverse)
library(here)


###############################################################
# Source file

here::i_am("code/12_expanded_esc.R")
df <- read_csv(here("outputs", "05_hist_daily_esc.csv"))


###############################################################
# Start analysis; explore data

# Get weir count for all years and save for later.
weir_count <- df %>%
  group_by(year) %>%
  summarize(weir_count = sum(esc_daily))

# Eliminate years with mid-season weir breaches (2014 and 2015)
df <- df %>%
  filter(!(year == 2014 | year == 2015))

# How many years of data do we have for each Julian day?
df %>%
  group_by(jul_day) %>%
  tally() %>%
  print(n = Inf)
# Fewer than 70% of years have data for jul_day < 181 or jul_day > 249.
# Let's not use those dates to expand.

# Save counts from the shoulder season for later.
count_shoulder <- df %>%
  filter(jul_day < 182 | jul_day > 249)

# Let's see what we are cutting.
count_shoulder %>%
  summarize(days = n(),
            count = sum(esc_daily))
# Through 2024, observations on jul_day < 181 or jul_day > 249 total
# 125 weir-days and 3,194 sockeye. Nearly half of days have 0 sockeye observed.

count_shoulder %>%
  group_by(year) %>%
  summarize(days = n(),
            count = sum(esc_daily)) %>%
  arrange(desc(count)) %>%
  print(n = Inf) %>%
  ungroup()
# However, over 80% of the sockeye observed come from 39 weir-days between 2006-2009.
# I am comfortable with the date cutoffs given the very limited data over the last 15 years.

################################################################################
# Begin expansion. Methods based on van Alen's model for Redoubt Lake (unpublished)
# Excel copy of model obtained from Ashley Bolwerk at USFS.

# Calculate proportion of escapement on each Julian day
df <- df %>%
  group_by(year) %>%
  mutate(prop = esc_daily / sum(esc_daily)) %>%
  ungroup()

jul_prop <- df %>%
  filter(!(jul_day < 182 | jul_day > 249)) %>%
  group_by(jul_day) %>%
  summarize(med_prop = median(prop),
            mean_prop = mean(prop),
            sd_prop = sd(prop)) %>%
  ungroup()

# Incorporate days with missing data
exp_esc <- df %>%
  select(-prop, -date) %>%
  pivot_wider(names_from = year, values_from = esc_daily) %>%
  filter(!(jul_day < 182 | jul_day > 249)) %>%
  pivot_longer(!jul_day, names_to = "year", values_to = "esc") %>%
  arrange(jul_day) 

# Add daily proportion
jul_prop <- jul_prop %>%
  select(-med_prop, -sd_prop)

daily <- full_join(exp_esc, jul_prop)

###############################################################################
# Estimate escapement for days with NA.

daily_na <- daily[is.na(daily$esc), ] %>%
  group_by(year) %>%
  summarize(days = n(),
            prop_na = sum(mean_prop)) %>%
  mutate(year = as.numeric(year))

count_shoulder <- count_shoulder %>%
  group_by(year) %>%
  summarize(count_shoulder = sum(esc_daily))

expansion <- reduce(list(weir_count, count_shoulder, daily_na), full_join, by = "year") %>%
  mutate(count_shoulder = if_else(is.na(count_shoulder), 0, count_shoulder),
         exp_esc = if_else(is.na(prop_na), weir_count, 
                           round(((weir_count - count_shoulder) / (1 - prop_na)) + count_shoulder)),
         prop_missed = (exp_esc - weir_count) / exp_esc * 100,
         missed_fish = exp_esc - weir_count)
# For all years, less than 0.6% of run missed during peak of season.
# The exception is 2024 at 14.6%!
# The 2024 estimate is higher than expected. However, the expanded
# escapement estimate still falls within 95% CI for mark-recapture.
# Note that 77.8% of weir counts occurred after Aug. 24 in 2007.
# This approach may not be appropriate for Klag, especially in years
# that are dry during the peak of the season and have heavy rain late.

# Plot the difference.
exp_plot <- expansion %>%
  mutate(diff = exp_esc - weir_count) %>%
  filter(!(year == 2014 | year == 2015)) %>%
  select(year, weir_count, diff) %>%
  pivot_longer(!year, names_to = "type", values_to = "esc")

ggplot(exp_plot, aes(x = year, y = esc, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  labs(x = element_blank(), y = "Escapement") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#006AA7", "#FFCD00")) +
  theme(text = element_text(size = 20)) +
  theme(legend.position = "none")
ggsave(here("plots", "12_exp_esc.png"))