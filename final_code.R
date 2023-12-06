library(tidyverse)
library(sf)
library(ggplot2)


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


# Assuming your merged data is named 'merged_data'
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
normalized_data <- scale(florida_data[, c("pop_white", "pop_black", "pop_hispanic", "median_household_income", "median_housing_price")])
florida_data <- cbind(florida_data[, c("geographic_name", "dem_votes", "rep_votes", "total_vote")], normalized_data)

florida_data_2016 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2016) %>%
  select(fips, geographic_name, dem_votes, rep_votes, total_vote)

florida_data_2020 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(fips, geographic_name, dem_votes, rep_votes, total_vote,name, median_household_income)

merged_data <- demo_county2020 %>%
  inner_join(pres_2012_2020, by = "fips")

# summary(merged_data)

# Select relevant columns for correlation analysis
cor_data <- merged_data %>%
  select(pop_white, pop_black, pop_hispanic, median_household_income, median_housing_price, dem_votes, rep_votes)

correlation_matrix <- cor(cor_data)
# Display the correlation matrix
print(correlation_matrix)



# Example: Linear regression for predicting Democratic votes based on median household income
lm_model <- lm(dem_votes ~ median_household_income, data = merged_data)
summary(lm_model)


florida_shapefile <- readRDS("FL_shapefile.rds")
florida_map_data2016 <- merge(florida_shapefile, florida_data_2016, by.x = "fips", by.y = "fips")
florida_map_data2020 <- merge(florida_shapefile, florida_data_2020, by.x = "fips", by.y = "fips")


ggplot() +
  geom_sf(data = florida_map_data2016, aes(fill = dem_votes), color = "white", size = 0.2) +
  scale_fill_gradient(low = "gray", high = "blue") +
  labs(title = "Florida Democratic Votes by County 2016",
       fill = "Democratic Votes") +
  theme_minimal()

ggplot() +
  geom_sf(data = florida_map_data2020, aes(fill = dem_votes), color = "white", size = 0.2) +
  scale_fill_gradient(low = "gray", high = "blue") +
  labs(title = "Florida Democratic Votes by County 2020",
       fill = "Democratic Votes") +
  theme_minimal()











ggplot(scatter_data, aes(x = median_housing_price, y = dem_votes / total_vote * 100, color = "Democratic")) +
  geom_point(aes(color = "Democratic"), alpha = 0.5) +  # Adjust alpha for points
  geom_smooth(aes(y = dem_votes / total_vote * 100), method = "lm", se = FALSE, color = "blue", linetype = "solid", alpha = 0.5) +  # Adjust alpha for lines
  geom_point(aes(y = rep_votes / total_vote * 100, color = "Republican"), alpha = 0.5) +  # Adjust alpha for points
  geom_smooth(aes(y = rep_votes / total_vote * 100), method = "lm", se = FALSE, color = "red", linetype = "solid", alpha = 0.5) +  # Adjust alpha for lines
  labs(title = "Scatter Plot of Median Household Income vs. Vote Share",
       x = "Median Household Income",
       y = "Vote Share",
       color = "Party") +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  guides(color = guide_legend(title = "Party"))










democratic_counties_2016 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2016) %>%
  select(name, dem_votes, total_vote) %>%
  mutate(democratic_vote_share = dem_votes / total_vote * 100) %>%
  arrange(desc(democratic_vote_share))

# Counties with the highest Democratic vote share in 2016
top_democratic_counties_2016 <- democratic_counties_2016 %>%
  top_n(5)

# Counties with the lowest Democratic vote share in 2016
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

# Print the results for 2016
cat("Counties with the highest Democratic vote share in 2016:\n")
print(top_democratic_counties_2016)

cat("\nCounties with the lowest Democratic vote share in 2016:\n")
print(bottom_democratic_counties_2016)

# Print the results for 2020
cat("\nCounties with the highest Democratic vote share in 2020:\n")
print(top_democratic_counties_2020)

cat("\nCounties with the lowest Democratic vote share in 2020:\n")
print(bottom_democratic_counties_2020)






# Assuming your merged data is named 'merged_data'
demographic_data <- merged_data %>%
  filter(state_name.x == "Florida")


# Merge with demographic data
top_demographic_data_2020 <- left_join(top_democratic_counties_2020, demographic_data, by = "name")
bottom_demographic_data_2020 <- left_join(bottom_democratic_counties_2020, demographic_data, by = "name")





# Print the results
cat("Demographics for the top five counties in the 2020 election:\n")
print(top_demographic_data_2020)

cat("\nDemographics for the bottom five counties in the 2020 election:\n")
print(bottom_demographic_data_2020)







florida_data_2020 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(name, dem_votes, total_vote, median_household_income, median_housing_price, pop_white, pop_black, pop_hispanic, pop_native_american, pop_asian)

florida_data_2016 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2016) %>%
  select(name, dem_votes, total_vote, median_household_income, median_housing_price, pop_white, pop_black, pop_hispanic, pop_native_american, pop_asian)



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




florida_data_2020 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(fips, geographic_name, dem_votes, rep_votes, total_vote,name, median_household_income, median_housing_price)

florida_map_data2020 <- merge(florida_shapefile, florida_data_2020, by.x = "fips", by.y = "fips")


ggplot() +
  geom_sf(data = florida_map_data2020, aes(fill = median_household_income), color = "white", size = 0.2) +
  scale_fill_gradient(low = "gray", high = "blue") +
  labs(title = "Florida Household Income by County 2020",
       fill = "Median Household Income") +
  theme_minimal()


ggplot() +
  geom_sf(data = florida_map_data2020, aes(fill = median_housing_price), color = "white", size = 0.2) +
  scale_fill_gradient(low = "gray", high = "blue") +
  labs(title = "Florida Household Pricing by County 2020",
       fill = "Median Housing Pricing") +
  theme_minimal()