library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(data.table)

variables <- c("B02001_002", "B02001_003", "B03003_003", "B02001_005", "B05002_013", "B19013_001", "B19001_017", "B25077_001", "B25064_001", "B15003_017", "B15003_022", "B15003_023")

de_get_data <- function(c)
              {get_acs(geography = "tract", 
                    state = "DE",
                    variables = c,
                    geometry = TRUE,
                    cb = TRUE)}

de_census_data <- lapply(variables, de_get_data)

de_clean_data1 <- function(a)
        {separate(a, col = NAME, into = c("Census_Tract", "County", "State"), sep = ",")}

de_census_data2 <- lapply(de_census_data, de_clean_data1)

de_clean_data2 <- function(b)
        {separate(b, col = Census_Tract, into = c("Census", "Tract", "Number"), sep = " ")}

de_census_data3 <- lapply(de_census_data2, de_clean_data2)

de_clean_data3 <- function(c)
        {setnames(c, old=c( "estimate"), new=c(paste(variables, "estimate", sep = " ")))}

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

setnames(de_white, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B02001_002_estimate", "B02001_002_moe"))
setnames(de_black, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B02001_003_estimate", "B02001_003_moe"))
setnames(de_hispanic, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B03003_003_estimate", "B03003_003_moe"))
setnames(de_asian, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B02001_005_estimate", "B02001_005_moe"))
setnames(de_foreignborn, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B05002_013_estimate", "B05002_013_moe"))
setnames(de_median_income, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B19013_001_estimate", "B19013_001_moe"))
setnames(de_households_earning_over_200k, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B19001_017_estimate", "B19001_017_moe"))
setnames(de_median_house_value, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B25077_001_estimate", "B25077_001_moe"))
setnames(de_median_monthly_rent, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B25064_001_estimate", "B25064_001_moe"))
setnames(de_high_school_diplomas, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B15003_017_estimate", "B15003_017_moe"))
setnames(de_bachleor_degrees, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B15003_022_estimate", "B15003_022_moe"))
setnames(de_masters_degrees, old=c("Number", "estimate", "moe"), new=c("Census_Tract_Number", "B15003_023_estimate", "B15003_023_moe"))

de_census_geo <- select(de_white, GEOID, Census_Tract_Number, County, geometry)

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

AllTracts <- as.numeric(as.character(de_census_data_export$Census_Tract_Number))
AllTracts <- data.frame(AllTracts)
Wilm_census_data <- cbind(de_census_data_export, AllTracts)
Wilm_census_data <- Wilm_census_data %>% 
  filter(AllTracts %in% c(2, 3, 4, 5, 6.01, 6.02, 9, 11, 12, 13, 14, 15, 16, 19.02, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30.02))    

# "B02001_002", "B02001_003", "B03003_003", "B02001_005", "B05002_013", "B19013_001" 
# "B19001_017", "B25077_001", "B25064_001", "B15003_017", "B15003_022", "B15003_023"

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