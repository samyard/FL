library(tidyverse)
library(sf)
library(ggplot2)

### THIS FILE IS STRICTLY FOR MAPPING AND VISUALS ###

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

demographic_data2020 <- merged_data %>%
  filter(year == 2020) %>%
  select(fips, pop_white, pop, geographic_name, dem_votes, rep_votes, total_vote,name, median_household_income, median_housing_price) %>%
  mutate(share_white = pop_white / pop * 100) %>%
  mutate(share_poc = 100 - share_white)

# Normalize for comparisons of diff units
normalized_data <- scale(florida_data[, c("pop_white", "pop_black", "pop_hispanic", "median_household_income", "median_housing_price")])
florida_data <- cbind(florida_data[, c("geographic_name", "dem_votes", "rep_votes", "total_vote")], normalized_data)


florida_shapefile <- readRDS("FL_shapefile.rds")

# Data by year
florida_data_2016 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2016) %>%
  select(fips, geographic_name, dem_votes, rep_votes, total_vote)

florida_data_2020 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(fips, geographic_name, dem_votes, rep_votes, total_vote,name, median_household_income, median_housing_price)

demographic_data2020 <- demographic_data2020 %>%
  select(fips, share_white, share_poc, geographic_name, dem_votes, rep_votes, total_vote,name, median_household_income, median_housing_price)



# prep data for fl heatmap by county
florida_map_data2016 <- merge(florida_shapefile, florida_data_2016, by.x = "fips", by.y = "fips")
florida_map_data2020 <- merge(florida_shapefile, florida_data_2020, by.x = "fips", by.y = "fips")
demographic_mapdata2020 <- merge(florida_shapefile, demographic_data2020, by.x = "fips", by.y = "fips")

# Votes heatmap 2016
ggplot() +
  geom_sf(data = florida_map_data2016, aes(fill = dem_votes), color = "white", size = 0.2) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Florida Democratic Votes by County 2016",
       fill = "Democratic Votes") +
  theme_minimal()

# Votes heatmap 2020
ggplot() +
  geom_sf(data = florida_map_data2020, aes(fill = dem_votes), color = "white", size = 0.2) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Florida Democratic Votes by County 2020",
       fill = "Democratic Votes") +
  theme_minimal()

# poc pop heatmap 2020
ggplot() +
  geom_sf(data = demographic_mapdata2020, aes(fill = share_poc), color = "white", size = 0.2) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Florida POC Population by County 2020",
       fill = "POC Population") +
  theme_minimal()

# Scatterplot of vote shares by household income
ggplot(scatter_data, aes(x = median_household_income, y = dem_votes / total_vote * 100, color = "Democratic")) +
  geom_point(aes(color = "Democratic"), alpha = 0.5) +  
  geom_smooth(aes(y = dem_votes / total_vote * 100), method = "lm", se = FALSE, color = "blue", linetype = "solid", alpha = 0.5) +  
  geom_point(aes(y = rep_votes / total_vote * 100, color = "Republican"), alpha = 0.5) +  # Adjust alpha for points
  geom_smooth(aes(y = rep_votes / total_vote * 100), method = "lm", se = FALSE, color = "red", linetype = "solid", alpha = 0.5) + 
  labs(title = "Scatter Plot of Median Household Income vs. Vote Share",
       x = "Median Household Income",
       y = "Vote Share by Party",
       color = "Party") +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  guides(color = guide_legend(title = "Party"))



# income heatmap 2020
ggplot() +
  geom_sf(data = florida_map_data2020, aes(fill = median_household_income), color = "white", size = 0.2) +
  scale_fill_gradient(low = "gray", high = "black") +
  labs(title = "Florida Household Income by County 2020",
       fill = "Median Household Income") +
  theme_minimal()

# household prices heatmap 2020
ggplot() +
  geom_sf(data = florida_map_data2020, aes(fill = median_housing_price), color = "white", size = 0.2) +
  scale_fill_gradient(low = "white", high = "black") +
  labs(title = "Florida Household Pricing by County 2020",
       fill = "Median Housing Pricing") +
  theme_minimal()

