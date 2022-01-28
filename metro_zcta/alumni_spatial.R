

## SCRIPT TO MAKE SPATIAL DATA FOR ALUMNI APP

library(tidyverse)
library(sf)

### load zcta file

options(tigris_class = "sf", tigris_use_cache = TRUE)
zcta <- tigris::zctas(year = 2018)
zcta$zip5 <- zcta$ZCTA5CE10

# create centroids of all zcta

z_centroid <- zcta %>% 
    select(zip5) %>% 
    st_transform(5070) %>% 
    st_centroid() 

write_rds(z_centroid, "zcta_centroid.rds")

# import states

states <- tigris::states(year = 2018)
states_USA <- filter(states, !STUSPS %in% c("MP", "VI", "PR", "AS", "GU")) %>% 
    st_transform(5070)

# intersect states and zcta to filter zctas by state

state_zcta <- st_intersection(states_USA, z_centroid) %>% 
    select(STUSPS, zip5)

write_rds(state_zcta, "zcta_centroid.rds")


# load metropolitan areas

metro <- tigris::combined_statistical_areas() %>% 
    st_transform(5070) %>% 
    select(-c(GEOID, NAMELSAD:INTPTLON)) %>% 
    rename(metro_id = CSAFP,
           metro_name = NAME) %>% 
    filter(!str_detect(metro_name, "PR"))

write_rds(metro, "US_metro_areas.rds")
