setwd("/Chapter_2/Database/")
# Defining categories based on FC
library(tidyverse)
library(sf)

datafull <- read_csv("Data/Pre_processed.csv") 
landscape_met <- read_csv("Data/lsm.csv")
landscape_met <- landscape_met %>% 
  filter(class == 1)


# Joing id_unique with landscape metrics
datafull_sites <- datafull %>% 
  distinct(study_id, id_unique, .keep_all = T) %>% 
  left_join(landscape_met)

colnames(datafull_sites)
cat <- as_tibble(datafull_sites) %>% 
  mutate(cat_500 = if_else(pland_0.5k  <= 30, true = "Low", false = if_else(pland_0.5k > 30 & pland_0.5k <= 60, true = "Intermediate", false = "High")),
         cat_1k = if_else(pland_1k  <= 30, true = "Low", false = if_else(pland_1k > 30 & pland_1k <= 60, true = "Intermediate", false = "High")),
         cat_2k = if_else(pland_2k  <= 30, true = "Low", false = if_else(pland_2k > 30 & pland_2k <= 60, true = "Intermediate", false = "High")),
         cat_4k = if_else(pland_4k  <= 30, true = "Low", false = if_else(pland_4k > 30 & pland_4k <= 60, true = "Intermediate", false = "High")),
         cat_8k = if_else(pland_8k  <= 30, true = "Low", false = if_else(pland_8k > 30 & pland_8k <= 60, true = "Intermediate", false = "High")))
colnames(cat)

write_csv(cat, "Data/sites_categories.csv")

# Atributing coordinates

# Organizing datatables ----
data <- read_sf("Data/datafull.gpkg")

datafull_sites <- data %>% 
  distinct(study_id, id_site, .keep_all = T) %>% 
  mutate(id_unique = as.double(1:nrow(.))) %>% 
  dplyr::select(id_unique)

cat_sf <- datafull_sites %>% 
  left_join(cat) %>% 
  filter(id_unique %in% cat$id_unique)

#cat_sf <- st_as_sf(cat_sf)
class(cat_sf)
#st_write(cat_sf, "Data/sites_categories_sf.gpkg", delete_dsn = F)
saveRDS(cat_sf, "Data/sites_categories_sf.RData")


#rm(list=ls())