

# **** GEOCODING ORIGINAL UNIQUE ADDRESSES ****


# geo_list_original <- u_adds_new %>%
#     mutate(google_result = map(add_string, ~ geocode(.x, output='more', source='google')))

l_ncol <- function(x) {
    geo_list_original$google_result[[x]] %>% 
        ncol()
}




geo_fail_original <- geo_list_original %>%
    mutate(geo_cols = map_dbl(1:nrow(geo_list_original), l_ncol),
           add_string = str_replace_all(add_string, "[^[:alnum:]]", " ")) %>%
    filter(geo_cols < 9)

geo_true_original <- geo_list_original %>%
    mutate(geo_cols = map_dbl(1:nrow(geo_list_original), l_ncol),
           add_string = str_replace_all(add_string, "[^[:alnum:]]", " ")) %>%
    filter(geo_cols == 9)


geo_df_true <- geo_true_original %>%
    mutate(lat = map_dbl(google_result, 'lat'),
           lon = map_dbl(google_result, 'lon'),
           type = map_chr(google_result, "type"),
           precision = map_chr(google_result, "loctype"),
           google_address = map_chr(google_result, "address")) %>%
    select(-google_result)

geo_df_fail <- geo_fail_original %>%
    mutate(lat = map_dbl(google_result, 'lat'),
           lon = map_dbl(google_result, 'lon')) %>%
    select(-google_result)

geocode_bind <- bind_rows(geo_df_true, geo_df_fail)

geocoded_original_adds <- geocode_bind %>% 
    select(-geo_cols)

date_suffix <- str_replace_all(Sys.time(), "-|:| ", "_")
geo_df_name <- paste0("./geocode_cache/alumni_geocodes_", date_suffix, ".rds")

write_rds(geocoded_original_adds, geo_df_name)

