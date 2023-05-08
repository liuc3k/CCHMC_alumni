
### script to geocode alumni addresses============
# Here is the information for our Google account: 
#     
# Username: cincykidsalumni@gmail.com
# Password: Alumni@cchmc
###=====================================

library(ggmap)
library(tidyverse)
library(sf)
setwd('C:/Users/LIUC3K/Desktop/Rshiny project for Alumni/alumni_pangyu/CCHMC_alumni')
source("alumni_data_format.R")



unique_adds_files <- sort(list.files("./unique_adds_rds/"))
unique_adds_paths <- paste0("./unique_adds_rds/", unique_adds_files)


u_adds_previous <- read_rds(unique_adds_paths[length(unique_adds_paths) - 1])
u_adds_new_fname <- paste0("./unique_adds_rds/", 
                           list.files("./unique_adds_rds/", pattern = ".rds")[length(unique_adds_files)])
u_adds_new <- read_rds(u_adds_new_fname)


# geocode options

# alumni_google_key = Sys.getenv("alumni_google_key")
alumni_google_key<-'AIzaSyBAU92ve9HNR7vNihHMuHHldWVBi-VtqNE'
register_google(key = alumni_google_key)


# # function to geocode list of addresses and write .rds file
# 
# alumni_geocode <- function(df) {
# 
#     geo_list <- df %>%
#         mutate(google_result = map(add_string, ~ geocode(.x, output='more', source='google')))
# 
#     geo_df <- geo_list %>%
#         mutate(lat = map_dbl(google_result, 'lat'),
#                lon = map_dbl(google_result, 'lon'),
#                precision = map_chr(google_result, "loctype"),
#                google_address = map_chr(google_result, "address")) %>%
#         select(-google_result)
#     date_suffix <- str_replace_all(Sys.time(), "-|:| ", "_")
#     geo_df_name <- paste0("./geocode_cache/alumni_geocodes_", date_suffix, ".rds")
#     write_rds(geo_df, geo_df_name)
# }
# alumni_geocode(u_adds_new)
geo_list <- u_adds_new %>%
    mutate(google_result = map(add_string, ~ geocode(.x, output='more', source='google')))


geo_df <- geo_list %>%
    mutate(lat = map_dbl(google_result, 'lat'),
           lon = map_dbl(google_result, 'lon')#,
           # precision =map_chr(google_result, "loctype") ,
           # google_address = map_chr(google_result, "address")
           ) %>%
    select(-google_result)
date_suffix <- str_replace_all(Sys.time(), "-|:| ", "_")
geo_df_name <- paste0("./geocode_cache/alumni_geocodes_", date_suffix, ".rds")
write_rds(geo_df, geo_df_name)
## merge geocodes with d_address and physician data

geocode_files <- sort(list.files("./geocode_cache/", pattern = ".rds"))
geocode_paths <- paste0("./geocode_cache/", geocode_files)


# u_adds_previous <- read_rds(unique_adds_paths[length(unique_adds_paths) - 1])
# u_adds_new_fname <- paste0("./unique_adds_rds/", 
#                            list.files("./unique_adds_rds/", pattern = ".rds")[length(unique_adds_files)])

geocodes_new <- read_rds(geocode_paths)

geo_merge <- full_join(d_address, geocodes_new)
geo_merge <- geo_merge %>% 
    filter(!is.na(ID),
           !is.na(lat)) %>% 
    select(ID, add_string, lat, lon)

### create master df 

master_merge <- inner_join(d, geo_merge)

# alumni_sf <- master_merge %>%
#     st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
#     st_transform(5070)
alumni_sf <- master_merge %>%
    mutate(longitude=lon,
           latitude =lat)%>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
    st_transform(5070)


# intersect metro areas and physician locations

metro <- read_rds("./metro_zcta/US_metro_areas.rds")

alumni_metro <- st_intersection(metro, alumni_sf) 

no_metro <- alumni_sf[!lengths(st_intersects(alumni_sf, metro)), ]
no_metro <- st_join(no_metro, metro)

alumni_geo <- rbind(alumni_metro, no_metro)

### EXPORT

write_rds(alumni_geo, "alumni_geo.rds")


