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
               bachelor_degrees = "B15003_022", masters_degrees = "B15003_023", Less_than_10k =
               "B19001_002", households_earning_10k_14999k = "B19001_003", households_earning_15k_19999k = "B19001_004", households_earning_20k_24999k =
               "B19001_005", households_earning_25k_29999k = "B19001_006", Owner_occupied = "B25003_002", Renter_occupied = "B25003_003", 
               Occupied = "B25002_002", Vacant = "B25002_003")

# Code to API to get data
# One thing to make sure we know is what year we are analayzing. I believe that 2016 so that is the year we are analyzing.
# Future iterations could build on this by adding decennial census data. TidyCensus has the 1990, 2000, and 2010 surveys.
# Another aspect future iterations could also add data from other sources, such as crime statistics from the Uniform Crime Reporting database maintained by the FBI.
de_census_data <- get_acs(geography = "tract",
  state = "DE",
  variables = demo_variables,
  geometry = TRUE,
  cb = TRUE)

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
# Another question I have is about tidy format. Is the data set we are creating in tidy format?
write.csv(de_census_data_clean, file = "de_census_data_export.csv")

# Subset dataframe to only include Wilmington tracts
Wilm_census_data <- de_census_data_clean %>%
  filter(Census_Tract_Number %in% c(2, 3, 4, 5, 6.01, 6.02, 9, 11, 12, 13, 14, 15, 16, 19.02, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30.02))

# Plot all our data
ggplot(Wilm_census_data, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Estimates by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate") +
  facet_wrap(~variable)
# one issue here is most plots are black since the scales are fixed
# so we could view race separate from education/income

# Plot only race/ethnicity
## filter the ones we want to see
Wilm_census_race <- Wilm_census_data %>%
  filter(variable %in% c("hispanic", "black", "asian", "white", "foreignborn"))

ggplot(Wilm_census_race, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Population Estimates by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate") +
  facet_wrap(~variable)


# Plot the education variables
Wilm_census_ed <- Wilm_census_data %>%
  filter(variable %in% c("high_school_diplomas", "bachelor_degrees", "masters_degrees"))

ggplot(Wilm_census_ed, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Education Estimates by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate") +
  facet_wrap(~variable)

# Plot the income variables
Wilm_census_incexp <- Wilm_census_data %>%
  filter(variable %in% c("households_earning_over_200k", "median_income", "median_monthly_rent", "median_house_value"))

ggplot(Wilm_census_incexp, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  theme_minimal() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Income, Rent Estimates by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data source: 2016 ACS.\nData acquired with the R tidycensus package.",
       fill = "ACS estimate") +
  facet_wrap(~variable)
ggsave("income-exp.jpeg")

## Food for thought
#What are the variables we'd most want to compare?Do we want to show margin of error? ratios of education to income?
#Could change the estimates to comparison between counties - normalize by subtracting average value
