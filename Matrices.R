setwd("/Chapter_2/Database/")

#Creating matrices for each study and its categories

library(tidyverse)

datafull <- read_csv("Data/Pre_processed.csv") 
# datafull <- datafull %>% 
#   distinct(study_id, id_site, .keep_all = T) %>% 
#   dplyr::select(study_id, id_unique, species, total_abund)

# Alocating each study in a element of list ----
study_list <- datafull %>% 
  select(study_id, id_unique, species, total_abund) %>% 
  group_split(study_id, .keep = T)

# Spreading species for each site, for each element of study_list

matrix <- lapply(study_list, FUN = spread, key = species, value = total_abund, fill = 0)

names(matrix) <- unique(datafull$study_id)

saveRDS(matrix, "Data/Overall_Matrices.Rdata")
