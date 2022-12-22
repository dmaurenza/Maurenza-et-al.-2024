library(tidyverse)
library(data.table)
library(rstatix)

datafull <- read_csv("Data/Pre_processed.csv") 

datafull <- datafull %>%
  select(id_unique, study_id, id_site, effort, species, total_abund)

study_list <- datafull %>% 
  group_split(study_id, .keep = T)

names <- c()

for(i in 1:length(study_list)){
  x <-  study_list[[i]][1,"study_id"]
  names<-as.character(append(names, x))
}
names(study_list) <- names


effort <- as.character()

for(i in 1:length(study_list)){
  #i = 30
  
  x <- study_list[[i]][,"effort"]  
  effort[i] <- if (length(unique(x$effort)) >1) { 
    print("True")
  }  else {print("False")
  }
}

df <- data.frame(study_id = names, effort) # True = different effort; False = equal effort
df
count(df, effort)
equal_effort <- as.vector(df[df$effort == "False", "study_id"])


# Tables with equal effort and unequal effort ----

database_equal <- datafull %>% 
  filter(study_id %in% equal_effort)

database_n_equal <- datafull %>% 
  filter(!study_id %in% equal_effort)
unequal_effort <- unique(database_n_equal$study_id)



cat <- read_csv("Data/sites_categories.csv")
cat <- cat %>% 
  dplyr::select(id_unique, starts_with("cat_"))

study_list <- database_n_equal %>% 
  group_split(study_id, .keep = T)

# Spreading species for all studies

matrix <- lapply(study_list, FUN = spread, key = species, value = total_abund, fill = 0)

# Adding sites categories for each study

matrix_cat <- lapply(matrix, FUN = left_join, y = cat)

for (i in 1:length(matrix_cat)){
  #i = 3
  names(matrix_cat)[[i]] <- matrix_cat[[i]][1,"study_id"] %>% pull
}
names <- names(matrix_cat)
matrix_cat_cor <- list()

for (i in names){
  #i = 17
  matrix_cat.i = matrix_cat[[i]]
  ext <- matrix_cat.i %>% 
    identify_outliers(effort) %>% 
    filter(is.extreme == T) %>% 
    pull(id_unique)
  
  matrix_cat_out <- matrix_cat.i %>% 
    filter(!id_unique %in% ext)
  
  desv <- sd(matrix_cat_out$effort)
  desv_id <- matrix_cat_out %>% 
    filter(effort > mean(effort)+2*desv | effort < mean(effort)-2*desv) %>% 
    pull(id_unique)
  
  matrix_cat_cor [[i]] <- matrix_cat_out %>% 
    filter(!id_unique %in% desv_id)
}

saveRDS(matrix_cat_cor, "Data/unequal_outlier.RData")




#rm(list=ls())
