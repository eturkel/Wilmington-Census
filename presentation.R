## ------------------------------------------------------------------------
v1 <- c(1, 2, 3, 4)
v2 <- c(5, 6, 7, 8)
v2-v1

# install.packages("sf")
# install.packages("tidycensus")
#


## ---- include=FALSE, results='hide'--------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(tidycensus)
library(tigris)
library(purrr)


## ----API-------------------------------------------------------
# census_api_key("<YOUR API KEY>")
# demo_variables <- # define the variables you want to analyze here
# de_census_data <- get_acs(geography = "tract",
#                           state = "DE",
#                           variables = demo_variables,
#                           geometry = TRUE,
#                           cb = TRUE)


## ------------------------------------------------------------------------
load("data/de_census_data.RData")


## ------------------------------------------------------------------------
de_census_data


## ------------------------------------------------------------------------
ggplot(de_census_data, aes(fill = estimate))


## ------------------------------------------------------------------------
ggplot(de_census_data, aes(fill = estimate)) +
  geom_sf() 


## ------------------------------------------------------------------------
ggplot(de_census_data, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_minimal() 


## ------------------------------------------------------------------------
ggplot(de_census_data, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Estimates by Census Tract")


## ------------------------------------------------------------------------
de_census_data_clean <- de_census_data %>%
    separate(col = NAME, into = c("Census_Tract", "County", "State"), 
             sep = ",") %>%
    separate(col = Census_Tract, into = c("Census", "Tract", "Census_Tract_Number"),
             sep = " ") 

# Let's look at the above code step by step using the 'grammar'
de_census_data %>% # take the data, and then
    separate(col = NAME, into = c("Census_Tract", "County", "State"), 
             sep = ",")


## ------------------------------------------------------------------------
de_census_data %>% # take the data, and then
    separate(col = NAME, into = c("Census_Tract", "County", "State"), 
             sep = ",") %>% # separate, and then
    separate(col = Census_Tract, into = c(NA, NA, "Census_Tract_Number"),
             sep = " ") # separate out Number and then
# this result is assigned to de_census_data_clean using the assignment operator '<-'


## ------------------------------------------------------------------------
# Let's look at the number of foreignborn in each Tract
de_census_fb <- de_census_data_clean %>%
  filter(variable %in% c("foreignborn"))

## ----fb------------------------------------------------------------------
ggplot(de_census_fb, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Foreign-Born Estimates by DE Census Tract",
       caption = "Data: 2013-2017 5-year ACS
       \nData acquired with the R tidycensus package.",
       fill = "ACS estimate")


## ------------------------------------------------------------------------
# Wilmington Tracts 
wilm_census_data <- de_census_data_clean %>%
  filter(Census_Tract_Number %in% c(2, 3, 4, 5, 6.01, 6.02, 9, 11, 12,
                                    13, 14, 15, 16, 19.02, 21, 22, 23, 
                                    24, 25, 26, 27, 28, 29, 30.02))

## ------------------------------------------------
# Plot all our data
ggplot(wilm_census_data, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  scale_color_viridis_c(guide = FALSE) +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Estimates by Census Tract",
       subtitle = "Wilmington, DE",
       caption = "Data: 2013-2017 5-year ACS
       \nData acquired with the R tidycensus package.",
       fill = "ACS estimate") +
  facet_wrap(~variable)


## ------------------------------------------------------------------------
## filter race
wilm_census_race <- wilm_census_data %>%
  filter(variable %in% c("hispanic", "black", "asian", "white"))

## ----race, echo=FALSE----------------------------------------------------
ggplot(wilm_census_race, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Population Estimates",
       subtitle = "Wilmington, DE",
       fill = "ACS estimate") +
  facet_wrap(~variable)


## ------------------------------------------------------------------------
# get the total population by summing up the different race estimates
wilm_tract_pop <- wilm_census_race %>%
  group_by(Census_Tract_Number) %>%
  summarise(Population_Estimate = sum(estimate))

st_geometry(wilm_tract_pop) <- NULL # remove geometry, coerce to data.frame

wilm_tract_percpop <- wilm_census_race %>%
  left_join(wilm_tract_pop, by = "Census_Tract_Number") %>%
  mutate(Percentage = estimate/Population_Estimate)


## ----percpop-------------------------------------------------------------
ggplot(wilm_tract_percpop, aes(fill = Percentage)) +
  geom_sf() +
  scale_fill_viridis_c() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Percentage of Population (Estimates) by Census Tract",
       subtitle = "Wilmington, DE",
       fill = "ACS estimate") +
  facet_wrap(~variable)


## ----YOUR TURN----------------------------------------------------------
# Answer 
wilm_census_edu <- wilm_census_data %>%
  filter(variable %in% c("high_school_diplomas", "bachelor_degrees", "masters_degrees"))

ggplot(wilm_census_edu, aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_c() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Education Estimates",
       subtitle = "Wilmington, DE",
       fill = "ACS estimate") +
  facet_wrap(~variable)

## ------------------------------------------------------------------------
# As a percentage of education data available (this isn't perfect)
# get the total population by summing up the different race estimates
wilm_tract_totedu <- wilm_census_edu %>%
  group_by(Census_Tract_Number) %>%
  summarise(Education_Estimate = sum(estimate))

st_geometry(wilm_tract_totedu) <- NULL # remove geometry

wilm_tract_percedu <- wilm_tract_totedu %>%
  left_join(wilm_census_edu, by = "Census_Tract_Number") %>%
  mutate(Percentage = estimate/Education_Estimate)

# Plot
ggplot(wilm_tract_percedu, aes(fill = Percentage)) +
  geom_sf() +
  scale_fill_viridis_c() +
  coord_sf(crs = 26916, datum = NA) +
  labs(title = "Percentage of Education Estimates by Census Tract",
       subtitle = "Wilmington, DE",
       fill = "ACS estimate") +
  facet_wrap(~variable)


## ------------------------------------------------------------------------
wm_dots <- map(c("white", "black", "asian", "hispanic"), function(group) {
    wilm_census_data %>%
        filter(variable == group) %>%
        st_sample(., size = .$estimate / 10, exact = FALSE) %>%
        st_sf() %>%
        mutate(group = group) 
}) %>%
    reduce(rbind) %>%
    group_by(group) %>%
    summarize()


## ---- If you want to see an intermediate step run the below-----
# group <- "asian"
# wilm_census_data %>%
#         filter(variable == group) %>%
#         st_sample(., size = .$estimate / 10, exact = FALSE) %>%
#         st_sf() %>%
#         mutate(group = group)


## ------------------------------------------------------------------------
ggplot() + 
    geom_sf(data = wilm_census_data, color = "grey95", fill = "white") + 
    geom_sf(data = wm_dots, aes(color = group, fill = group), size = 0.1, alpha = 0.5) +
    theme_minimal()

