###############################################################
# Investigate temp data for Klag Lake sockeye
# kyle.rosendale@sitkatribe-nsn.gov
# November 2023


###############################################################
# Run Me First!

# 02_annual_sockeye.R must be run before this script. 


###############################################################
# Outputs

# outputs/08_staff_hist.csv       historical stage and precip data
# outputs/08_temp_hist.csv        historical mean and max daily temp data
# plots/08_esc_temp_max.png       historical daily weir count and daily max temp plot
# plots/08_esc_temp_mean.png      historical daily weir count and daily mean temp plot
# plots/08_prop_by_year.png       proportion of days and Jul/Aug weir counts w/ mean daily temp >17C
# plots/08_prop_max_by_year.png   proportion of days and Jul/Aug weir counts w/ max daily temp >17C
# plots/08_temp_by_year.png       historical mean daily mean and daily max temp plot
# results/08_temp_summ.csv        historical summary temperature / weir count data


###############################################################
# Load libraries

library(tidyverse)
library(lubridate)
library(janitor)
library(here)


###############################################################
# Source file

# Double check working directory
here::i_am("code/08_temp_staff.R")

# Load historic staff gauge and escapement data
staff <- dir(here("data", "08_staff_sock"), full.names = T) %>% map_dfr(read_csv)

# Load current year staff and escapement data
staff_curr <- read_csv(here("data", "08_staff_curr.csv"))

# Load raw current year data
tmp_curr <- read_csv(here("data", "08_temp_curr.csv"))

# Load data from previous years
tmp_hist <- read_csv(here("data", "08_temp_hist.csv"))


###############################################################
# Begin data manipulation; create a single data frame

# Find daily mean and max temps from current year
tmp_curr <- tmp_curr %>%
  mutate(date = as.POSIXct(date_time, format = "%m/%d/%Y %H:%M")) %>%
  mutate(date = as_date(date)) %>%
  select(-date_time)

#Find maximum daily temperature
tmp_curr <- tmp_curr %>%
  group_by(date) %>%
  summarize(temp_max = max(temp),
            temp_mean = mean(temp))

temp <- rbind(tmp_curr, tmp_hist) %>%
  write_csv(file = here::here("outputs", "08_temp_hist.csv"))

# Combine temperature, staff gauge, and escapement data
# Convert character to date format
staff <- staff %>%
  mutate(date = mdy(Date)) %>%
  select(-Date) %>%
  clean_names()

# Convert current year staff gauge to match historic staff gauge
colnames(staff_curr) <- c('date', 'staff_gauge_cm', 'rain_gauge_mm', 
                         'water_temp_c', 'air_temp_c', 'daily_escapement')

# Combine historical weather and escapement data and export for next year
staff <- rbind(staff, staff_curr) %>%
  write_csv(file = here::here("outputs", "08_staff_hist.csv"))

data <- full_join(staff, temp) # All of our data is now in one data frame!

################################################################################

# Summarize data. Filter to keep dates between July 1 and August 31
df <- data %>%
  filter(!is.na(daily_escapement)) %>% # Remove NAs for days weir was not operational
  mutate(jul = yday(date),
         mo = month(date),
         yr = as.integer(year(date))) %>%
  filter(mo == 7 | mo == 8) %>% # Keep dates between July 1 and August 31
  filter(yr != 2020) %>% # Remove 2020 - partner sent bad logger
  filter(yr != 2021) # Remove 2021 - incomplete July / August data

# Summarize data. Find the mean of the mean daily temps and the max temp observed.
# Use 17C as a threshold based on synthesis by McCullough (2001).
temp_summ <- df %>%
  group_by(yr) %>%
  summarize(max_temp = max(temp_max),
            mean_temp = mean(temp_mean),
            days = n(),
            mean_17 = sum(temp_mean > 17),
            prop_17 = mean_17 / days,
            esc = sum(daily_escapement),
            esc_17 = sum(daily_escapement[temp_mean > 17]),
            prop_esc = esc_17 / esc,
            max_17 = sum(temp_max > 17),
            prop_max_temp = max_17 / days,
            esc_max = sum(daily_escapement[temp_max > 17]),
            prop_max_esc = esc_max / esc) %>%
  write_csv(file = here::here("results", "08_temp_summ.csv"))


###########################################################################
# Make plots

# Plot mean and max July and August temp
temp_summ %>%
  select(yr, max_temp, mean_temp) %>%
  pivot_longer(cols = max_temp:mean_temp,
               names_to = "temp") %>%
  ggplot((aes(x = yr, y = value, fill = reorder(temp, value)))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 17, linetype = 2) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#006AA7", "red")) +
  labs(x = element_blank(), y = "Temp (C)") +
  theme(text = element_text(size = 20)) +
  theme(legend.position = "none")
ggsave(here("plots", "08_temp_by_year.png"))

# What proportion of days and proportion of escapement are above 17C?
temp_summ %>%
  select(yr, prop_17, prop_esc) %>%
  pivot_longer(cols = c(prop_17, prop_esc),
               names_to = "prop") %>%
  ggplot((aes(x = yr, y = value, fill = reorder(prop, -value)))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  scale_fill_discrete(labels = c("Days over 17C", "Jul/Aug Weir Counts")) +
  theme(legend.title = element_blank()) +
  labs(x = element_blank(), y = "Proportion") +
  theme(text = element_text(size = 20))
ggsave(here("plots", "08_prop_by_year.png"))
# Does not account for weir breaches in 2014 and 2015. 

# What proportion of days and proportion of escapement have a max temp above 17C?
temp_summ %>%
  select(yr, prop_max_temp, prop_max_esc) %>%
  pivot_longer(cols = c(prop_max_temp, prop_max_esc),
               names_to = "prop") %>%
  ggplot((aes(x = yr, y = value, fill = reorder(prop, -value)))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_classic() +
  scale_fill_discrete(labels = c("Days with max temp >17C", "Jul/Aug Weir Counts")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Proportion") +
  theme(text = element_text(size = 20))
ggsave(here("plots", "08_prop_max_by_year.png"))


###########################################################################
#Plots with all data

# Plot mean temps
ggplot(df) + 
  geom_point(aes(x = staff_gauge_cm, y = temp_mean,
                 size = daily_escapement, color = jul)) +
  geom_hline(yintercept = 17, linetype = 2, color = "red") +
  theme_classic() +
  scale_fill_gradient(name = "Julian Day") + # CAN'T FIGURE OUT HOW TO FIX THIS LEGEND LABEL
  scale_size_continuous(name = "Daily Weir Count") +
  labs(x = "Staff Gauge (cm)", y = "Mean Daily Temp (C)") +
  theme(text = element_text(size = 20))
ggsave(here("plots", "08_esc_temp_mean.png"))
# daily mean 17C results in impaired maturation and increased stress (Macdonald et al. 2000)
# holding RBT above 13 reduced egg survival; 15C more generally cited (see McCullough et al. 2001)

# Plot max temps (same as above)
ggplot(df) + 
  geom_point(aes(x = staff_gauge_cm, y = temp_max,
                 size = daily_escapement, color = jul)) +
  geom_hline(yintercept = 17, linetype = 2, color = "red") +
  theme_classic() +
  scale_fill_gradient(name = "Julian Day") +
  scale_size_continuous(name = "Daily Weir Count") +
  labs(x = "Staff Gauge (cm)", y = "Max Daily Temp (C)") +
  theme(text = element_text(size = 20))
ggsave(here("plots", "08_esc_temp_max.png"))