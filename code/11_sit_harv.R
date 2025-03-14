##############################################################
# Summarize annual Sitka area permit-reported sockeye harvest
# kyle.rosendale@sitkatribe-nsn.gov
# February 2024

###############################################################
# Run Me First!

# This script can be run on its own.


###############################################################
# Outputs

# plots/11_sit_harv_prop.png    historical proportional harvest by system
# plots/11_sit_harv_tot.png     historical permit reported harvest by system


###############################################################
# Load libraries

library(tidyverse)
library(here)


###############################################################
# Source file

here::i_am("code/11_sit_harv.R")
df <- read_csv(here("data", "11_sit_harv.csv"))


###############################################################
# Start analysis

harv <- df %>%
  replace(is.na(.), 0) %>%
  mutate(redoubt = redoubt_head + redoubt_outlet) %>%
  select(-c(redoubt_head, redoubt_outlet)) %>%
  mutate(red_prop = redoubt / rowSums(across(c(klag, necker, redfish, redoubt))))


# Plot total harvest over time
harv_tot <- harv %>%
  gather(klag:redoubt, key = "system", value = "permit_harv") %>%
  mutate(system = str_to_title(system),
         system = fct_relevel(system, "Redfish", "Necker", "Redoubt", "Klag"))

ggplot(harv_tot, aes(x = year, y = permit_harv, fill = system)) +
  geom_area() +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = median(harv$klag), linetype = 2) +
  labs(x = element_blank(), y = "Permit Reported Harvest") +
  theme_classic() +
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "11_sit_harv_tot.png"))


# Plot proportional harvest over time
harv_prop <- harv %>%
  rowwise() %>%
  mutate(klag_prop = klag / sum(c_across(klag:redoubt))) %>%
  ungroup()

ggplot(harv_tot, aes(x = year, y = permit_harv, fill = system)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = median(harv_prop$klag_prop), linetype = 2) +
  labs(x = element_blank(), y = "Proportion of Permit Reported Harvest") +
  theme_classic() +
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "11_sit_harv_prop.png"))
