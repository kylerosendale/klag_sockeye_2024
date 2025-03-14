##############################################################
# Summarize annual Klag Lake sockeye salmon limno data
# kyle.rosendale@sitkatribe-nsn.gov
# February 2023

###############################################################
# Run Me First!

# This script can be run on its own.


###############################################################
# Outputs

# plots/09_do_profiles.png      monthly DO profiles
# plots/09_temp_profiles.png    monthly temperature profiles
# results/09_ezd.csv            monthly estimates of EZD


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(gt)
library(lubridate)
library(broom)


###############################################################
# Source file

here::i_am("code/09_limno.R") # not sure if this is best practice
df <- read_csv(here("data", "09_limno.csv"))


###############################################################
# Start analysis

# Calculate EZD.
data <- df %>%
  mutate(date = mdy(date),
         mon = month(date, label = TRUE)) %>%
  group_by(mon) %>%
  mutate(ln_prop = log(100 * light / max(light, na.rm = TRUE)))

# Find EZD for each month
ezd <- data %>%
  select(mon, depth, ln_prop) %>%
  na.omit() %>%
  nest(data = -mon) %>%
  mutate(model = map(data, ~lm(depth ~ ln_prop, data =.)), 
         tidied = map(model, tidy)) %>%
  unnest(tidied) %>%
  select(mon, term, estimate) %>%
  filter(term == "(Intercept)") %>%
  rename(ezd = estimate)

ezd %>% write.csv(here("results", "09_ezd.csv"))


###############################################################
# Plots

# Plot temperature profiles
ggplot(data, aes(x = depth, y = temp, color = mon)) + # Switch x and y axes
  geom_point() +
  geom_smooth() +
  scale_x_reverse() +
  coord_flip() + # Coord_flip helps geom_smooth look nicer
  theme_classic() +
  labs(y = "Temperature (C)", x = "Depth (m)") + # axes are switched
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "09_temp_profiles.png"))
         
# Plot DO profiles
ggplot(data, aes(x = depth, y = do, color = mon)) + # Switch x and y axes
  geom_point() +
  geom_smooth() +
  scale_x_reverse() +
  coord_flip() + # Coord_flip helps geom_smooth look nicer
  theme_classic() +
  labs(y = "Dissolved Oxygen (Percent Saturation)", x = "Depth (m)") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "09_do_profiles.png"))