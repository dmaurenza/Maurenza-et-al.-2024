setwd("/Chapter_2/Database/")
library(sf)
library(tidyverse)

# Loading data
datafull <- read_sf("Data/datafull.gpkg") # This database is equal to Database SES_MA
landscape_met <- read_csv("Data/lsm.csv")
landscape_met <- landscape_met %>% 
  filter(class == 1)

# Transforming sf_object to tibble object
st_geometry(datafull) <- NULL


# Distinct each site as id_unique
datafull_sites <- datafull %>% 
  distinct(study_id, id_site, .keep_all = T) %>% 
  dplyr::select(study_id, id_site, year_median, effort) %>% 
  mutate(id_unique = as.double(1:nrow(.)))



# Test if there is any  site with more than 100% of forest cover - errors ----

Forest_pland <- landscape_met %>% 
  dplyr::select(id_unique, contains("pland"))

Forest_pland %>% 
  filter(pland_0.5k > 100 | pland_1k > 100 | pland_2k > 100 | pland_4k > 100 | pland_8k > 100 )


# Testing sites with NA - It is possible even considering that collections were conducted in fragments. As the resolution is 30x30, fragments shorter than this size of pixels are not captured
nas <- Forest_pland %>% 
  filter(is.na(pland_0.5k) | is.na(pland_1k) | is.na(pland_2k) | is.na(pland_4k) | is.na(pland_8k) ) %>% 
  dplyr::select(id_unique) %>% 
  pull()

# Eliminating NAs
datafull_sites <- datafull_sites %>% 
  filter(!id_unique %in% nas)


# Eliminating studies without abundance information ----

datafull_sites <- datafull_sites %>% 
  filter(!study_id == "Marcelo_Magioli") %>% # no abundance data
  filter(!study_id == "Fabiano_Farah") %>% # no abundance data
  filter(!study_id == "Jack_Hatfield") # no abundance data


final_datafull <- datafull_sites %>% 
  left_join(datafull)

colnames(landscape_met)

write_csv(final_datafull, "Data/Pre_processed.csv")

#rm(list=ls())


