

library(tidyverse)

# import
setwd('C:/Users/LIUC3K/Desktop/Rshiny project for Alumni/alumni_pangyu/CCHMC_alumni')
# d_original <- read_csv("cchmc_alumni_original.csv") # alumni data
##Latest Alumni data (2/28/2022)
d_original <- read_csv("Geocode Export.csv")%>%
     mutate(CnAttrCat_1_01_Description=`CnAttrCat_1_01_Description (General Specialty)`,
            CnAttrCat_2_01_Description=`CnAttrCat_2_01_Description (General subspecialty)`)%>%
    select(-c(`Deceased?`,Nickname,`Current MedSt`,`CnAttrCat_1_01_Description (General Specialty)`,`CnAttrCat_2_01_Description (General subspecialty)`))

### function to remove invalid miltibyte strings

rm_invalid_str <- function(x) {
    iconv(x, "latin1", "ASCII", sub="")
}

# remove invalid chars

d <- map_df(d_original, rm_invalid_str) 

d <- d %>% 
    rename_at(vars(contains("CnPrBsAdr")), list(~str_replace(., "CnPrBsAdr_", ""))) %>%
    rename_at(vars(contains("CnBio_")), list(~str_replace(., "CnBio_", ""))) %>%
    mutate(jr_suffix = str_replace_all(Suffix_1, ",", ""),
           jr_suffix = str_trim(jr_suffix),
           lname_suffix = ifelse(!is.na(jr_suffix), paste(Last_Name, jr_suffix), Last_Name),
           credentials = str_remove_all(Suffix_2, ","),
           credentials = paste0(", ", str_trim(credentials)),
           institution = ifelse(CnPrBs_Org_Name == "<None Specified>", '', CnPrBs_Org_Name),
           org_search = str_replace_all(institution, "'", ""),
           phys_name = paste(lname_suffix, paste(First_Name, Middle_Name),
                             sep = ", "),
           phys_name = str_replace_all(phys_name, "NA", ""),
           phys_search = paste(First_Name, Middle_Name, paste0(Last_Name,
                                                               credentials), org_search, sep = "%20"),
           phys_search = str_replace_all(phys_search, "NA", ""),
           phys_link = paste0("<a href='", "https://google.com/search?q=", phys_search,
                              " 'target='_blank'>", phys_name, "</a>")) %>%
    rename(Country = ContryLongDscription,
           specialty1 = CnAttrCat_1_01_Description,
           specialty2 = CnAttrCat_2_01_Description) %>% 
    separate(ZIP, c("zip5", "last_four"), sep = "-") %>% 
    select(-c(last_four, CnPrBs_Org_Name, Suffix_1))

# remove physicians with completely blank information

d_blank <- d %>% 
    filter_at(vars(Addrline1, Addrline2, Addrline3, City, State, zip5, Country, org_search), all_vars(is.na(.))) ## decide addrline

d <- anti_join(d, d_blank)

# extract address string to geocode

d_address <- d %>% 
    select(ID, contains("Addrline"), City, State, zip5, Country, org_search)

d_address <- map_df(d_address, str_to_upper)
d_address <- map_df(d_address, ~replace_na(., ""))

d_address <- d_address %>% 
    mutate(add_numeric = ifelse(str_detect(Addrline1, "[0-9]"), Addrline1,
                                ifelse(str_detect(Addrline2, "[0-9]"), Addrline2,
                                       ifelse(str_detect(Addrline3, "[0-9]"), Addrline3, Addrline1))),
           comma_count = str_count(add_numeric, ",")) 

#### function to pull out numeric elements 

l_pull <- function(x) {
    num_subset <- add_subset[[x]]
    num_list <- add_split[[x]][num_subset]
    return(num_list)
}

####

add_split <- str_split(d_address$add_numeric, ",")
add_subset <- map(add_split, ~grep("[0-9]", .))

add_numeric1 <- map(1:length(add_split), l_pull) %>% 
    map(., 1) %>% 
    as.character() %>% 
    str_trim() %>%
    enframe(name = NULL) %>% 
    rename(add_numeric1 = value) %>% 
    mutate(add_numeric1 = ifelse(str_detect(add_numeric1, "NULL"), '', add_numeric1))

### need to find unique of numerical street, city, state, zip, institution...

d_address <- bind_cols(d_address, add_numeric1)

d_address <- d_address %>% 
    mutate(add_string = str_trim(paste(add_numeric1, City, State, zip5, Country, org_search)),
           add_string = str_replace_all(add_string, "[[:punct:]]", ""))

d_add_unique <- d_address %>% 
    select(add_string) %>% 
    unique()

# write unique adds to file

previous_files <- sort(list.files("./unique_adds_rds/"))
previous_paths <- paste0("./unique_adds_rds/", previous_files)

previous_adds <- if (length(previous_paths) - 1 == 0) {
    read_rds(previous_paths)
} else {
    read_rds(previous_paths[length(previous_paths) - 1])
}

# new file only written if differs from previous file

unique_adds_fname = paste0("alumni_unique_adds_", str_replace_all(Sys.Date(), "-", "_"), ".rds")
write_add_file <- nrow(anti_join(d_add_unique, previous_adds))
    
if (write_add_file > 0) {
    write_rds(d_add_unique, path =  paste0("./unique_adds_rds/",  unique_adds_fname))
}
