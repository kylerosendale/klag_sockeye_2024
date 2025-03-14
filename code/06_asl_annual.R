##############################################################
# Summarize annual Klag Lake sockeye salmon ASL data
# kyle.rosendale@sitkatribe-nsn.gov
# February 2024

###############################################################
# Run Us First!

# 01_annual_harvest.R and 02_annual_sockeye.R must be run prior to this script.

# NOTE: DEFAULT ESCAPEMENT ESTIMATE METHOD IS WEIR COUNT;
# YOU WILL NEED TO UPDATE FILE IF M-R OR EXPANDED ESC METHODS ARE USED;
# SEE LINES 59, 96, AND 106


###############################################################
# Outputs

# plots/06_asl_table.png          age proportion table
# plots/06_len_table.png          length-at-age table
# plots/06_length_freq.png        length-frequency plot
# results/06_asl_summary.csv      summary stats on scale aging


###############################################################
# Load libraries

library(tidyverse)
library(here)
library(gt)
library(plotrix)


###############################################################
# Source file

here::i_am("code/06_asl_annual.R")
asl <- read_csv(here("data", "06_asl_curr.csv"))
weir <- read_csv(here("results", "02_final_estimates.csv"))


###############################################################
# Start analysis

# Eliminate unnecessary columns and rename
asl <- asl %>%
  select(1, 6, 7, 19, 21, 24, 34) %>%
  rename(year = 1, date = 2, stat_wk = 3,
         sex = 4, len = 5, age = 6, comment = 7) %>%
  mutate(sex = tolower(sex)) %>% # Convert sex to lower case
  mutate(sex = case_when(age == 11 ~ "male", # Make sure all x.1 fish are male
                         age == 21 ~ "male",
                         TRUE ~ sex))

# Output summary of sampling
asl_summ <- asl %>%
  summarize(samples = n(),
            scales_read = samples - sum(is.na(age)),
            prop_samp = samples / filter(weir, parameter == "weir_count")$estimate,
            prop_read = scales_read / samples,
            resorb = sum(comment == "resorbed", na.rm = TRUE),
            prop_resorb = resorb / samples,
            m_f = sum(sex == "male") / sum(sex == "female"))
write.csv(asl_summ, here("results", "06_asl_summary.csv"), row.names = FALSE)


###################################################################################
# Make plots

# Plot length frequency table
ggplot(asl) +
  geom_histogram(aes(x = len, fill = sex), position = "dodge") +
  theme_classic() +
  labs(x = "Length (mm)", y = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.2, .8)) +
  theme(text = element_text(size = 20))
ggsave(here("plots", "06_length_freq.png"))


##############################################################################
# The remainder of the script builds two tables:
# Table 1: Proportion by age and sex
# Table 2: Length-at-age


##################################################################################
# Table 1: Proportion by age and sex

# Manipulate data; age proportions of all data
asl_prop <- asl %>%
  filter(!is.na(age)) %>%
  group_by(age) %>%
  summarize(n = n(),
            prop = n / asl_summ$scales_read,
            se = sqrt((1 - (n / filter(weir, parameter == "weir_count")$estimate)) *
                         ((prop * (1 - prop)) / (asl_summ$scales_read - 1))),
            sex = "total") # To help join in next step

# Age proportion by sex
asl_prop_sex <- asl %>%
  filter(!is.na(age)) %>%
  group_by(sex, age) %>%
  summarize(n = n(),
            prop = n / asl_summ$scales_read,
            se = sqrt((1 - (n / filter(weir, parameter == "weir_count")$estimate)) *
                        ((prop * (1 - prop)) / (asl_summ$scales_read - 1))))

# Join together. Add brood year.
asl_prop <- full_join(asl_prop, asl_prop_sex) %>%
         mutate(brood_yr = filter(asl, row_number() == 1)$year - # Generate current year
           (1 + as.numeric(substr(age, 1, 1)) + as.numeric(substr(age, 2, 2))))


############################################################################
# Make age proportion table

# Need to make table wide format
asl_tab <- asl_prop %>%
  mutate(fw = substr(age, 1, 1), # Format age class
         sw = substr(age, 2, 2),
         age_class = paste(fw, sw, sep = ".")) %>%
  select(-c(age, fw, sw)) %>%
  mutate(across(c(prop, se), round, 3)) %>% # Set number of decimal places
  pivot_wider(names_from = sex,
              values_from = c(n, prop, se)) %>%
  relocate(age_class, brood_yr, # Reorder columns so we can select by index
           n_male, prop_male, se_male,
           n_female, prop_female, se_female,
           n_total, prop_total, se_total)

# Make a vector to change column names to what we want in table
colnames_long <- colnames(asl_tab)
colnames_tab <- colnames_long %>% # Format new column names
  str_replace('prop', 'proportion') %>%
  str_remove('_(total|male|female)') %>%
  str_replace('age_class', 'Age Class') %>%
  str_replace('brood_yr', 'Brood Year') %>%
  str_to_title() %>%
  str_replace('Se', 'SE')

names(colnames_tab) <- colnames_long

# Finally make the table
asl_tab %>%
  gt() %>%
  cols_label(.list = colnames_tab) %>% # Use nicely formatted column labels
  tab_spanner(
    label = md('**Male**'),
    columns = 3:5
  ) %>%
  tab_spanner(
    label = md('**Female**'),
    columns = 6:8
  ) %>%
  tab_spanner(
    label = md('**Total**'),
    columns = 9:11
  ) %>%
  opt_stylize(style = 1, color = "gray") %>%
  tab_style( # add light dividers around Female and after Brood Year
    style = cell_borders(
      sides = c("left"),
      weight = px(1)),
    locations = cells_body(
      columns = 6
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1)),
    locations = cells_body(
      columns = 8
    )
  )  %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1)),
    locations = cells_body(
      columns = 2
    )
  ) %>%
  cols_width( # Give more space to N columns
  contains("N") ~ px(40)
  ) %>%
  gtsave(here("plots", "06_asl_table.png"))


############################################################################
# Table 2: Length-at-age

asl_len <- asl %>%
  filter(!is.na(age)) %>%
  group_by(sex, age) %>%
  summarize(n = n(),
            length = mean(len, na.rm = TRUE),
            se = std.error(len)) %>%
  mutate(brood_yr = filter(asl, row_number() == 1)$year - # generate current year
           (1 + as.numeric(substr(age, 1, 1)) +
              as.numeric(substr(age, 2, 2))),
         fw = substr(age, 1, 1), # Format age class
         sw = substr(age, 2, 2),
         age_class = paste(fw, sw, sep = ".")) %>%
  mutate(across(c(length, se), round, 0)) %>% # Set number of decimal places)
           select(-c(age, fw, sw))


############################################################################
# Make length-at-age table

# Pivot wider for gt() table format
len_tab <- asl_len %>%
  pivot_wider(names_from = sex,
              values_from = c(n, length, se)) %>%
  relocate(age_class, brood_yr, # Reorder columns so we can select by index
           n_male, length_male, se_male,
           n_female, length_female, se_female) %>%
  arrange((age_class))

# Make vector to change column names to what we want in table
colnames_len_long <- colnames(len_tab)
colnames_len_tab <- colnames_len_long %>% # Format new column names
  str_remove('_(male|female)') %>%
  str_replace('age_class', 'age class') %>%
  str_replace('brood_yr', 'brood year') %>%
  str_to_title() %>%
  str_replace('Se', 'SE')

names(colnames_len_tab) <- colnames_len_long

# Finally make the table
len_tab %>%
  gt() %>%
  cols_label(.list = colnames_len_tab) %>% # Use nicely formatted column names
  tab_spanner(
    label = md('**Male**'),
    columns = 3:5
  ) %>%
  tab_spanner(
    label = md('**Female**'),
    columns = 6:8
  ) %>%
  opt_stylize(style = 1, color = "gray") %>%
  tab_style( # Add light dividers around Female and after Brood Year
    style = cell_borders(
      sides = c("left"),
      weight = px(1)),
    locations = cells_body(
      columns = 6
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("right"),
      weight = px(1)),
    locations = cells_body(
      columns = 2
    )
  ) %>%
  cols_width( # Give more space to N columns
    contains("N") ~ px(40)
  ) %>%
  gtsave(here("plots", "06_len_table.png"))
