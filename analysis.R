library(tidyverse)
library(sf)
library(ggplot2)
library(corrplot)

### THIS FILE IS STRICTLY FOR ANALYSIS ###

## Reading in relevant datasets and filtering for FL ##

# 2020 demographics by county
demo_county2020 <- readRDS("demographic_data_by_county_2020.rds")
demo_county2020 <- demo_county2020 %>%
  filter(state_name == "Florida")

# election results by county 2012, 2016, 2020
pres_2012_2020 <- readRDS("election_data_president_2012_2020.rds")
pres_2012_2020 <- pres_2012_2020 %>%
  filter(state_name == "Florida")

# Merge demographics and election results by county
# 'pop_poc' indicates people of color population
merged_data <- demo_county2020 %>%
  inner_join(pres_2012_2020, by = "fips") %>%
  mutate(pop_poc = pop_black + pop_asian + pop_native_american + pop_hispanic)
# removing housing price outlier
merged_data <- merged_data[merged_data$median_housing_price != 558100, ]

# County democratic vote share in descending order
# 2016
democratic_counties_2016 <- merged_data %>%
  filter(year == 2016) %>%
  mutate(democratic_vote_share = dem_votes / total_vote * 100) %>%
  arrange(desc(democratic_vote_share))

# 2020
democratic_counties_2020 <- merged_data %>%
  filter(year == 2020) %>%
  mutate(democratic_vote_share = dem_votes / total_vote * 100) %>%
  arrange(desc(democratic_vote_share))

# top 5 counties - highest Democratic vote share
# 2016
top_democratic_counties_2016 <- democratic_counties_2016 %>%
  top_n(5)
# 2020
top_democratic_counties_2020 <- democratic_counties_2020 %>%
  top_n(5)

# bottom 5 counties - lowest Democratic vote share
# 2016
bottom_democratic_counties_2016 <- democratic_counties_2016 %>%
  tail(5)

# 2020
bottom_democratic_counties_2020 <- democratic_counties_2020 %>%
  tail(5)

# demographics for top and bottom 5 counties
demographic_data2020 <- merged_data %>%
  filter(year == 2020) %>%
  select(name, median_housing_price, median_household_income, pop, pop_white, pop_black, pop_hispanic, pop_native_american, pop_asian, median_age) %>%
  mutate(share_white = pop_white / pop) %>%
  mutate(share_poc = 1 - share_white)

top_demographic_data_2020 <- left_join(top_democratic_counties_2020, demographic_data2020, by = "name")
bottom_demographic_data_2020 <- left_join(bottom_democratic_counties_2020, demographic_data2020, by = "name")

cat("Demographics for the top five counties in the 2020 election:\n")
print(top_demographic_data_2020)
cat("\nDemographics for the bottom five counties in the 2020 election:\n")
print(bottom_demographic_data_2020)


# lr for predicting Republican votes based on median household income
lm_model <- lm(rep_votes ~ median_housing_price, data = florida_data_2020)
print("Regression for vote share predict based on income: \n")
summary(lm_model)


regression_data <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(pop_white, pop_poc, dem_votes, total_vote)

model <- lm(dem_votes / total_vote * 100 ~ pop_poc, data = regression_data)
summary(model)



