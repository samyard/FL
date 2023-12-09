library(tidyverse)
library(sf)
library(ggplot2)

### THIS FILE IS STRICTLY FOR MAPPING AND VISUALS ###

# Reading in relevant datasets and filtering for FL
demo_county2020 <- readRDS("demographic_data_by_county_2020.rds")
demo_county2020 <- demo_county2020 %>%
  filter(state_name == "Florida")

pres_2012_2020 <- readRDS("election_data_president_2012_2020.rds")
pres_2012_2020 <- pres_2012_2020 %>%
  filter(state_name == "Florida")

# Merge demographics and election results by county
merged_data <- demo_county2020 %>%
  inner_join(pres_2012_2020, by = "fips") %>%
  mutate(pop_poc = pop_black + pop_asian + pop_native_american + pop_hispanic)
merged_data <- merged_data[merged_data$median_housing_price != 558100, ]

demographic_data2020 <- merged_data %>%
  filter(year == 2020) %>%
  select(fips, pop_white, pop, geographic_name, dem_votes, rep_votes, total_vote,name, median_household_income, median_housing_price) %>%
  mutate(share_white = pop_white / pop * 100) %>%
  mutate(share_poc = 100 - share_white)

florida_shapefile <- readRDS("FL_shapefile.rds")

# Data by year
florida_data_2016 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2016) %>%
  select(fips, geographic_name, dem_votes, rep_votes, total_vote, median_household_income)

florida_data_2020 <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(fips, geographic_name, dem_votes, rep_votes, total_vote,name, median_household_income, median_housing_price)


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
  theme_void()+
  theme(legend.position = c(0.25, 0.5),
        legend.justification = c(0, 0.5))

# Votes heatmap 2020
ggplot() +
  geom_sf(data = florida_map_data2020, aes(fill = dem_votes), color = "white", size = 0.2) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Florida Democratic Votes by County 2020",
       fill = "Democratic Votes") +
  theme_void() +
  theme(legend.position = c(0.25, 0.5),
        legend.justification = c(0, 0.5))

# poc pop heatmap 2020
ggplot() +
  geom_sf(data = demographic_mapdata2020, aes(fill = share_poc), color = "white", size = 0.2) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "Florida POC Population by County 2020",
       fill = "POC Population") +
  theme_void()+
  theme(legend.position = c(0.25, 0.5),
        legend.justification = c(0, 0.5))

# scatterplot data 
scatter_data <- merged_data %>%
  filter(state_name.x == "Florida" & year == 2020) %>%
  select(name, pop_white, dem_votes, pop, rep_votes, total_vote, median_household_income, median_housing_price) %>%
  mutate(democratic_vote_share = dem_votes / total_vote * 100,
         republican_vote_share = rep_votes / total_vote * 100,
         white_percentage = pop_white / pop * 100)
# remove outlier
scatter_data <- scatter_data[scatter_data$median_housing_price != 558100, ]


# Scatterplot of vote shares by housing cost
ggplot(scatter_data, aes(x = median_housing_price, y = dem_votes / total_vote * 100, color = "Democratic")) +
  geom_point(aes(color = "Democratic"), alpha = 0.5) +  
  geom_smooth(aes(y = dem_votes / total_vote * 100), method = "lm",formula = y ~ poly(x, 2), se = FALSE, color = "blue", linetype = "solid", alpha = 0.5) +  
  geom_point(aes(y = rep_votes / total_vote * 100, color = "Republican"), alpha = 0.5) +  # Adjust alpha for points
  geom_smooth(aes(y = rep_votes / total_vote * 100), method = "lm",formula = y ~ poly(x, 2), se = FALSE, color = "red", linetype = "solid", alpha = 0.5) + 
  labs(title = "Scatter Plot of Median Housing Price vs. Vote Share",
       x = "Median Housing Price",
       y = "Vote Share by Party",
       color = "Party") +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  guides(color = guide_legend(title = "Party"))+
  theme(legend.position = c(0.7, 0.87),
      legend.justification = c(0, 0.5),
      axis.text = element_text(size = 15), 
      axis.title = element_text(size = 15))

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



# party vote share vs white population %
ggplot(scatter_data, aes(x = white_percentage, y = democratic_vote_share)) +
  geom_point(aes(color = "Democratic"), alpha = 0.5, shape = 16) +
  geom_smooth(aes(color = "Democratic"), method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  geom_point(aes(x = white_percentage, y = republican_vote_share, color = "Republican"), alpha = 0.5, shape = 15) +
  geom_smooth(aes(x = white_percentage, y = republican_vote_share, color = "Republican"), method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(title = "White Population vs. 2020 Vote Share",
       x = "White Population (%)",
       y = "2020 Vote Share by Party") +
  scale_color_manual(values = c("Democratic" = "blue", "Republican" = "red")) +
  theme(legend.position = c(0.05, 0.87),
        legend.justification = c(0, 0.5),
        axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15))

# color correlation matrix
cor_data <- merged_data %>%
  select(dem_votes, rep_votes, pop_white, pop, median_household_income, median_housing_price)

cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", col = colorRampPalette(c("#b2182b", "#f4a582", "#f7f7f7", "#92c5de", "#2166ac"))(100),
         tl.col = "black")  

