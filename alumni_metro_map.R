

## map showing alumni density by metropolitan area

library(tidyverse)
library(sf)

d_docs <- read_rds("alumni_geo.rds")
metro <- read_rds("./metro_zcta/US_metro_areas.rds")

alumni_sf <- d_docs %>% 
    select(-c(metro_id, metro_name))
   
metro_alumni <- st_intersection(alumni_sf, metro) %>% 
    select(ID, metro_id, metro_name) %>% 
    st_drop_geometry()

metro_join <- inner_join(metro, metro_alumni) 

metro_alumni <- metro_join %>% 
    group_by(metro_name) %>% 
    summarise(n_docs = n()) %>% 
    arrange(desc(n_docs)) %>% 
    filter(row_number() < 11) %>% 
    mutate(normalized_docs = ifelse(n_docs > 1000, 200, n_docs))

mapview::mapview(metro_alumni,
        zcol = "normalized_docs")

