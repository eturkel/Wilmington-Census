library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(data.table)

### Get API Key from Census website
# https://walkerke.github.io/tidycensus/articles/basic-usage.html

variables <- c("B02001_002", "B02001_003", "B03003_003", "B02001_005", "B05002_013", "B19013_001", "B19001_017", "B25077_001", "B25064_001", "B15003_017", "B15003_022", "B15003_023")

# Code to API to get data
# de_get_data <- function(c) {
#   get_acs(geography = "tract",
#     state = "DE",
#     variables = c,
#     geometry = TRUE,
#     cb = TRUE)
#   }

#de_census_data <- lapply(variables, de_get_data)
#save(de_census_data, file = "data/de_census_data.RData")

# Load Data
load("data/de_census_data.RData")

# Split 'NAME' into three separate variables. Currently, data on census tract, county, and state are all contained in same variable.
# Splitting 'NAME' into 'Census_Tract', 'County', & 'State' provides more opportunities to subset and join data with other datasets.
de_clean_data1 <- function(a, b)
 {separate(a, col = NAME, into = c("Census_Tract", "County", "State"), sep = ",")}

de_clean_data1 <- function(a)
        {separate(a, col = NAME, into = c("Census_Tract", "County", "State"), sep = ",")}

de_census_data2 <- lapply(de_census_data, de_clean_data1)

# After creating a variable for 'Census_Tract', it is neccessary to conduct another split b/c
# We want to isolate the number of the census tract. We don't want each observation to read 'Census Tract x'

de_clean_data2 <- function(b)
        {separate(b, col = Census_Tract, into = c("Census", "Tract", "Number"), sep = " ")}

de_census_data3 <- lapply(de_census_data2, de_clean_data2)

# Next we rename the variable so that it has a clearer meaning
de_clean_data3 <- function(c)
{setnames(c, old=c( "Number"), new=c("Census_Tract_Number"))}

de_census_data4 <- lapply(de_census_data3, de_clean_data3)

# Ultimately, we want to create a dataset that combines a bunch of census tract variables.
# In order to do this we need to rename variables across data sets to reflect that each is unique, otherwise R won't allow us to join them together.
# Since we need to rename variables in data sets so that they are unique, it is neccessary to transform the data set from a list to a data frame.
# The following statements transform individual elements in our list to a data frame.

de_white <- as.data.frame(de_census_data3[[1]])
de_black <- as.data.frame(de_census_data3[[2]])
de_hispanic <- as.data.frame(de_census_data3[[3]])
de_asian <- as.data.frame(de_census_data3[[4]])
de_foreignborn <- as.data.frame(de_census_data3[[5]])
de_median_income <- as.data.frame(de_census_data3[[6]])
de_households_earning_over_200k <- as.data.frame(de_census_data3[[7]])
de_median_house_value <- as.data.frame(de_census_data3[[8]])
de_median_monthly_rent <- as.data.frame(de_census_data3[[9]])
de_high_school_diplomas <- as.data.frame(de_census_data3[[10]])
de_bachleor_degrees <- as.data.frame(de_census_data3[[11]])
de_masters_degrees <- as.data.frame(de_census_data3[[12]])

# Rename unique variables in each data set
setnames(de_white, old=c("estimate", "moe"), new=c("B02001_002_estimate", "B02001_002_moe"))
setnames(de_black, old=c("estimate", "moe"), new=c("B02001_003_estimate", "B02001_003_moe"))
setnames(de_hispanic, old=c("estimate", "moe"), new=c("B03003_003_estimate", "B03003_003_moe"))
setnames(de_asian, old=c("estimate", "moe"), new=c("B02001_005_estimate", "B02001_005_moe"))
setnames(de_foreignborn, old=c("estimate", "moe"), new=c("B05002_013_estimate", "B05002_013_moe"))
setnames(de_median_income, old=c("estimate", "moe"), new=c("B19013_001_estimate", "B19013_001_moe"))
setnames(de_households_earning_over_200k, old=c("estimate", "moe"), new=c("B19001_017_estimate", "B19001_017_moe"))
setnames(de_median_house_value, old=c("estimate", "moe"), new=c("B25077_001_estimate", "B25077_001_moe"))
setnames(de_median_monthly_rent, old=c("estimate", "moe"), new=c("B25064_001_estimate", "B25064_001_moe"))
setnames(de_high_school_diplomas, old=c("estimate", "moe"), new=c("B15003_017_estimate", "B15003_017_moe"))
setnames(de_bachleor_degrees, old=c("estimate", "moe"), new=c("B15003_022_estimate", "B15003_022_moe"))
setnames(de_masters_degrees, old=c("estimate", "moe"), new=c("B15003_023_estimate", "B15003_023_moe"))

# The next step to join our data sets together is to create a data frame that contains the variables common to all data sets.
de_census_geo <- select(de_white, GEOID, Census_Tract_Number, County, geometry)

# Then, we create data frames that contain only the variables unique to each data frame, plus GEOID - which we will join by.
# If we didn't do this, then our final dataset would contain a bunch of duplicates and be difficult to read.
de_white_join <- select(de_white, GEOID, B02001_002_estimate, B02001_002_moe)
de_black_join <- select(de_black, GEOID, B02001_003_estimate, B02001_003_moe)
de_hispanic_join <- select(de_hispanic, GEOID, B03003_003_estimate, B03003_003_moe)
de_asian_join <- select(de_asian, GEOID, B02001_005_estimate, B02001_005_moe)
de_foreignborn_join <- select(de_foreignborn, GEOID, B05002_013_estimate, B05002_013_moe)
de_median_income_join <- select(de_median_income, GEOID, B19013_001_estimate, B19013_001_moe)
de_households_earning_over_200k_join <- select(de_households_earning_over_200k, GEOID, B19001_017_estimate, B19001_017_moe)
de_median_house_value_join <- select(de_median_house_value, GEOID, B25077_001_estimate, B25077_001_moe)
de_median_monthly_rent_join <- select(de_median_monthly_rent, GEOID, B25064_001_estimate, B25064_001_moe)
de_high_school_diploma_join <- select(de_high_school_diplomas, GEOID, B15003_017_estimate, B15003_017_moe)
de_bachleor_degrees_join <- select(de_bachleor_degrees, GEOID, B15003_022_estimate, B15003_022_moe)
de_masters_degree_join <- select(de_masters_degrees, GEOID, B15003_023_estimate, B15003_023_moe)

# Next, we join the variables unique to each data set to our newly created data frame which contains common variables.
de_census_data_export <- left_join(de_census_geo, de_white_join, by = c("GEOID" = "GEOID")) %>%
                            left_join(., de_black_join, by="GEOID") %>%
                            left_join(., de_hispanic_join, by="GEOID") %>%
                            left_join(., de_asian_join, by="GEOID") %>%
                            left_join(., de_foreignborn_join, by="GEOID") %>%
                            left_join(., de_median_income_join, by="GEOID") %>%
                            left_join(., de_households_earning_over_200k_join, by="GEOID") %>%
                            left_join(., de_median_house_value_join, by="GEOID") %>%
                            left_join(., de_median_monthly_rent_join, by="GEOID") %>%
                            left_join(., de_high_school_diploma_join, by="GEOID") %>%
                            left_join(., de_bachleor_degrees_join, by="GEOID") %>%
                            left_join(., de_masters_degree_join, by="GEOID")

write.csv(de_census_data_export, file = "de_census_data_export.csv")

# Subset dataframe to only include Wilmington tracts
AllTracts <- as.numeric(as.character(de_census_data_export$Census_Tract_Number))
AllTracts <- data.frame(AllTracts)
Wilm_census_data <- cbind(de_census_data_export, AllTracts)
Wilm_census_data <- Wilm_census_data %>%
  filter(AllTracts %in% c(2, 3, 4, 5, 6.01, 6.02, 9, 11, 12, 13, 14, 15, 16, 19.02, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30.02))

# "B02001_002", "B02001_003", "B03003_003", "B02001_005", "B05002_013", "B19013_001"
# "B19001_017", "B25077_001", "B25064_001", "B15003_017", "B15003_022", "B15003_023"

# Plot all our data
ggplot(Wilm_census_data, aes(fill = B02001_002_estimate, color = B02001_002_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "White Population by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_white_pop.jpeg")

ggplot(Wilm_census_data, aes(fill = B02001_003_estimate, color = B02001_003_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "African-American Population by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_african_american_pop.jpeg")

ggplot(Wilm_census_data, aes(fill = B03003_003_estimate, color = B03003_003_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Hispanic Population by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_hispanic_pop.jpeg")

ggplot(Wilm_census_data, aes(fill = B02001_005_estimate, color = B02001_005_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Asian Population by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_asian_pop.jpeg")

ggplot(Wilm_census_data, aes(fill = B05002_013_estimate, color = B05002_013_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Foreign Born Population by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_foreign_born.jpeg")

ggplot(Wilm_census_data, aes(fill = B19013_001_estimate, color = B19013_001_estimate)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::dollar) +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Median Income by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_median_income.jpeg")

ggplot(Wilm_census_data, aes(fill = B19001_017_estimate, color = B19001_017_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Number of Households Earning over 200K by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_over200k.jpeg")

ggplot(Wilm_census_data, aes(fill = B25077_001_estimate, color = B25077_001_estimate)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::dollar) +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Median Housing Value by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_median_housing_value.jpeg")

ggplot(Wilm_census_data, aes(fill = B25064_001_estimate, color = B25064_001_estimate)) +
  geom_sf() +
  scale_fill_viridis_c(labels = scales::dollar) +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Median Monthly Rent by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_median_monthly_rent.jpeg")

ggplot(Wilm_census_data, aes(fill = B15003_017_estimate, color = B15003_017_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Number of High School Diplomas by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_highschool_dipolmas.jpeg")

ggplot(Wilm_census_data, aes(fill = B15003_022_estimate, color = B15003_022_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Number of Bachelors Degrees by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_bachelor_degrees.jpeg")

ggplot(Wilm_census_data, aes(fill = B15003_023_estimate, color = B15003_023_estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Number of Masters Degrees by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate")

ggsave("wilm_masters_degrees.jpeg")
