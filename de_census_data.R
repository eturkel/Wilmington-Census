library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(data.table)

### Get API Key from Census website
# https://walkerke.github.io/tidycensus/articles/basic-usage.html
demo_variables <- c(white = "B02001_002", black = "B02001_003", hispanic = "B03003_003",
               asian = "B02001_005", foreignborn = "B05002_013", median_income = "B19013_001",
               households_earning_over_200k = "B19001_017", median_house_value = "B25077_001",
               median_monthly_rent = "B25064_001", high_school_diplomas = "B15003_017",
               bachelor_degrees = "B15003_022", masters_degrees = "B15003_023")


variables <- c("B02001_002", "B02001_003", "B03003_003", "B02001_005", "B05002_013", "B19013_001", "B19001_017", "B25077_001", "B25064_001", "B15003_017", "B15003_022", "B15003_023")

# Code to API to get data
de_census_data <- get_acs(geography = "tract",
  state = "DE",
  variables = demo_variables,
  geometry = TRUE,

## Cleaning Steps
# 1. Split 'NAME' into three separate variables. Currently, data on census tract, county, and state are all contained in same variable.
#    Splitting 'NAME' into 'Census_Tract', 'County', & 'State' provides more opportunities to subset and join data with other datasets.
# 2. After creating a variable for 'Census_Tract', it is neccessary to conduct another split b/c
#    We want to isolate the number of the census tract. We don't want each observation to read 'Census Tract x'
# 3. Next we rename the variable so that it has a clearer meaning
de_census_data_clean <- de_census_data %>%
    separate(col = NAME, into = c("Census_Tract", "County", "State"), sep = ",") %>%
    separate(col = Census_Tract, into = c("Census", "Tract", "Number"), sep = " ") %>%
    setnames(old=c( "Number"), new=c("Census_Tract_Number"))

# Ultimately, we want to create a dataset that combines a bunch of census tract variables.
write.csv(de_census_data_clean, file = "de_census_data_export.csv")

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
