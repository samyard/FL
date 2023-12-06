library(tidyverse)
library(sf)
library(ggplot2)

### THIS FILE IS STRICTLY FOR ANALYSIS ###

# Reading in relevant datasets and filtering for FL
demo_county2020 <- readRDS("demographic_data_by_county_2020.rds")
demo_county2020 <- demo_county2020 %>%
  filter(state_name == "Florida")

elec_county2022 <- readRDS("election_results_2022_county.rds")
elec_county2022 <- elec_county2022 %>%
  filter(state == "Florida")

elec_state2022 <- readRDS("election_results_2022_state.rds")
elec_state2022 <- elec_state2022 %>%
  filter(state == "Florida")

pres_2012_2020 <- readRDS("election_data_president_2012_2020.rds")
pres_2012_2020 <- pres_2012_2020 %>%
  filter(state_name == "Florida")

# Merge demographics and election results by county
merged_data <- demo_county2020 %>%
  inner_join(pres_2012_2020, by = "fips")

florida_data <- merged_data %>%
  filter(state_name.x == "Florida") %>%
  group_by(geographic_name) %>%
  summarise(dem_votes = sum(dem_votes),
            rep_votes = sum(rep_votes),
            total_vote = sum(total_vote),
            pop_white = sum(pop_white),
            pop_black = sum(pop_black),
            pop_hispanic = sum(pop_hispanic),
            median_household_income = mean(median_household_income),
            median_housing_price = mean(median_housing_price))

# Normalize for comparisons of diff units
normalized_data <- scale(florida_data[, c("pop_white", "pop_black", "pop_hispanic", "median_household_income", "median_housing_price")])
florida_data <- cbind(florida_data[, c("geographic_name", "dem_votes", "rep_votes", "total_vote")], normalized_data)


# Data by year
florida_data_2020 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(name, dem_votes, total_vote, median_household_income, median_housing_price, pop_white, pop_black, pop_hispanic, pop_native_american, pop_asian)

florida_data_2016 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2016) %>%
  select(name, dem_votes, total_vote, median_household_income, median_housing_price, pop_white, pop_black, pop_hispanic, pop_native_american, pop_asian)



# Select relevant columns for correlation analysis
cor_data <- merged_data %>%
  select(pop_white, pop_black, pop_hispanic, median_household_income, median_housing_price, dem_votes, rep_votes)

correlation_matrix <- cor(cor_data)
# Display the correlation matrix
 # print(correlation_matrix)



# lr for predicting Democratic votes based on median household income
lm_model <- lm(dem_votes ~ median_household_income, data = merged_data)
print("Regression for vote share predict based on income: \n")
summary(lm_model)




democratic_counties_2016 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2016) %>%
  select(name, dem_votes, total_vote) %>%
  mutate(democratic_vote_share = dem_votes / total_vote * 100) %>%
  arrange(desc(democratic_vote_share))

# top 5 counties - highest Democratic vote share in 2016
top_democratic_counties_2016 <- democratic_counties_2016 %>%
  top_n(5)

# bottom 5 counties - lowest Democratic vote share in 2016
bottom_democratic_counties_2016 <- democratic_counties_2016 %>%
  tail(5)

# Similarly for 2020
democratic_counties_2020 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(name, dem_votes, total_vote) %>%
  mutate(democratic_vote_share = dem_votes / total_vote * 100) %>%
  arrange(desc(democratic_vote_share))

# Counties with the highest Democratic vote share in 2020
top_democratic_counties_2020 <- democratic_counties_2020 %>%
  top_n(5)

# Counties with the lowest Democratic vote share in 2020
bottom_democratic_counties_2020 <- democratic_counties_2020 %>%
  tail(5)

# # top 5 results
# cat("Counties with the highest Democratic vote share in 2016:\n")
# print(top_democratic_counties_2016)
# cat("\nCounties with the highest Democratic vote share in 2020:\n")
# print(top_democratic_counties_2020)
# 
# # bottom 5 results
# cat("\nCounties with the lowest Democratic vote share in 2016:\n")
# print(bottom_democratic_counties_2016)
# cat("\nCounties with the lowest Democratic vote share in 2020:\n")
# print(bottom_democratic_counties_2020)



# demographics for top and bottom 5 counties
demographic_data2020 <- merged_data %>%
  filter(year == 2020) %>%
  select(name, median_housing_price, median_household_income, pop, pop_white, pop_black, pop_hispanic, pop_native_american, pop_asian, median_age) %>%
  mutate(share_white = pop_white / pop) %>%
  mutate(share_poc = 1 - share)

top_demographic_data_2020 <- left_join(top_democratic_counties_2020, demographic_data2020, by = "name")
bottom_demographic_data_2020 <- left_join(bottom_democratic_counties_2020, demographic_data2020, by = "name")


cat("Demographics for the top five counties in the 2020 election:\n")
print(top_demographic_data_2020)
cat("\nDemographics for the bottom five counties in the 2020 election:\n")
print(bottom_demographic_data_2020)






# 2020 Regression
predictors <- c("pop_white", "pop_black", "pop_hispanic", "pop_native_american", "pop_asian", "median_household_income", "median_housing_price")
response <- "dem_votes / total_vote * 100"
regression_model <- lm(as.formula(paste(response, "~", paste(predictors, collapse = "+"))), data = florida_data_2020)

summary(regression_model)

# 2016 Regression
predictors <- c("pop_white", "pop_black", "pop_hispanic", "pop_native_american", "pop_asian", "median_household_income", "median_housing_price")
response <- "dem_votes / total_vote * 100"
regression_model <- lm(as.formula(paste(response, "~", paste(predictors, collapse = "+"))), data = florida_data_2016)

summary(regression_model)


