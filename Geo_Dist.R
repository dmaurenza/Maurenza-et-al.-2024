setwd("/Chapter_2/Database/")
library(tidyverse)
library(sf)
library(geosphere)
library(data.table)
library(mapview)
matrix <- readRDS("Data/Overall_Matrices.Rdata") # From Matrices script

cat <- readRDS("Data/sites_categories_sf.RData")
cat <- cat %>% 
  dplyr::select(id_unique, starts_with("cat_"), geom)
#processed <- read_csv("Data/Pre_processed.csv") 

# # Organizing datatables ----
# 
# x <- cat %>% 
#   st_join(datafull_sites, "id_unique")
# 
#   mutate(lon = sf::st_coordinates(.)[,1],
#          lat = sf::st_coordinates(.)[,2]) # Separateing coordinates from sf to two collumns
# 
# 
# # Selecting only site choosed after preprocessing. Final data frame contain selected unique id with geometry
# processed_id <- unique(processed$id_unique)
# datafull_sites <- datafull_sites %>% 
#   filter(id_unique %in% processed_id)
# 
# # Joing selected sites with categories
# 
# datafull_sites_cat <- datafull_sites %>% 
#   left_join(cat, by = "id_unique") %>% # joining with forest cover
#   select(id_unique, lat, lon, starts_with("cat_")) 





# Separating each study into each element of a list, and joint witf categories
matrix_cat <- lapply(matrix, left_join, y = cat, by = "id_unique")
cat_vector <- c("cat_500","cat_1k","cat_2k", "cat_4k", "cat_8k")
# Calculating geographic distance for all database, for different buffer sizes. After generate distance matrics, mean distance for each category of FC is calculated ----

# Function for Haversine -----

# my_fun_haver <- function(cat, matrix){
#   geo_list_500 <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_1k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_2k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_4k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_8k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   
#   Geo_dist <- list(geo_list_500, geo_list_1k, geo_list_2k, geo_list_4k, geo_list_8k)
#   names(Geo_dist) <- c("500", "1k", "2k", "4k", "8k")
#  
#   # # Transforming geo_point to crs = albers
#   # Albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs" #102033 - More info: https://epsg.io/102033
#   
# 
#   for (i in 1:length(matrix)){
#     # i = 3
#     
#     matrix.i <- matrix[[i]] %>% 
#       st_as_sf()
#     
#     study_name <- names(matrix[i])
#     
#     site_id <- matrix.i$id_unique # a vector with unique id names
#     
#     geo.i <- distm(x = as_Spatial(matrix.i), fun = distHaversine)
#    
#     for (m in 1:ncol(geo.i)){ # Transforming 0 to NA
#       geo.i[m,m] <- NA
#     }
#     
#     #Naming cols and rows with unique id for final matrix
#     rownames(geo.i) <- site_id
#     colnames(geo.i) <- site_id
#     
#     #saveRDS(matrix.i, paste0("Geo_Dist/", study_name, ".Rdata"))
#     
#     for(o in 1:5){
#       #o = 1
#       
#       #Initial ----
#       # For buffer 500
#       # Identifing unique_id in category Low
#       id_geo_Low <- matrix.i %>% 
#         select(id_unique, cat[o]) %>% 
#         filter(.[[cat[o]]] == "Low") %>% 
#         pull(id_unique)
#       
#       # Selecting only unique id in Low category
#       geo.i_Low <- geo.i[as.character(id_geo_Low),as.character(id_geo_Low)] %>% as.matrix()
#       
#       # Identifing unique_id in category Inter
#       id_geo_Inter <- matrix.i %>% 
#         select(id_unique, cat[o]) %>% 
#         filter(.[[cat[o]]] =="Intermediate") %>% 
#         pull(id_unique)
#       
#       # Selecting only unique id in Inter category
#       geo.i_Inter <- geo.i[as.character(id_geo_Inter),as.character(id_geo_Inter)] %>% as.matrix()
#       
#       
#       # Identifing unique_id in category High
#       id_geo_High <- matrix.i %>% 
#         select(id_unique, cat[o]) %>% 
#         filter(.[[cat[o]]] =="High") %>% 
#         pull(id_unique)
#       
#       # Selecting only unique id in High category
#       geo.i_High <- geo.i[as.character(id_geo_High),as.character(id_geo_High)] %>% as.matrix()
#       
#       # Summarizing the results
#       result <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#       
#       
#       # calculating mean distance
#       if(ncol(geo.i_Low)>2){
#         result[i, "Low"] <- mean(geo.i_Low, na.rm = T)} else {
#           result[i, 1] <- NA}
#       
#       # calculating mean distance
#       if(ncol(geo.i_Inter)>2){
#         result[i, "Inter"] <- mean(geo.i_Inter, na.rm = T)} else {
#           result[i, "Inter"] <- NA}
#       
#       # calculating mean distance
#       if(ncol(geo.i_High)>2){
#         result[i, "High"] <- mean(geo.i_High, na.rm = T)} else {
#           result[i, "High"] <- NA}
#       
#       result[i,"study_id"] <- study_name
#       
#       Geo_dist[[o]][i,] <- result[i,]
#       
#       
#     }
#   }
#   
#   return(Geo_dist)
# }
# 
# dist_geo_haver <- my_fun_haver(cat = cat_vector, matrix = matrix_cat)

# The same function, but using median geographic distance in meters ----

my_fun_haver_md <- function(cat, matrix){
  geo_list_500 <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  geo_list_1k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  geo_list_2k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  geo_list_4k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  geo_list_8k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  
  Geo_dist <- list(geo_list_500, geo_list_1k, geo_list_2k, geo_list_4k, geo_list_8k)
  names(Geo_dist) <- c("500", "1k", "2k", "4k", "8k")
  
  # # Transforming geo_point to crs = albers
  # Albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs" #102033 - More info: https://epsg.io/102033
  
  
  for (i in 1:length(matrix)){
    # i = "Juliano_Bogoni"
    
    matrix.i <- matrix[[i]] %>% 
      st_as_sf()
    
    study_name <- names(matrix[i])
    
    site_id <- matrix.i$id_unique # a vector with unique id names
    
    geo.i <- distm(x = as_Spatial(matrix.i), fun = distHaversine)
    
    for (m in 1:ncol(geo.i)){ # Transforming 0 to NA
      geo.i[m,m] <- NA
    }
    
    #Naming cols and rows with unique id for final matrix
    rownames(geo.i) <- site_id
    colnames(geo.i) <- site_id
    
    #saveRDS(matrix.i, paste0("Geo_Dist/", study_name, ".Rdata"))
    
    for(o in 1:5){
      #o = 1
      
      #Initial ----
      # For buffer 500
      # Identifing unique_id in category Low
      id_geo_Low <- matrix.i %>% 
        select(id_unique, cat[o]) %>% 
        filter(.[[cat[o]]] == "Low") %>% 
        pull(id_unique)
      
      # Selecting only unique id in Low category
      geo.i_Low <- geo.i[as.character(id_geo_Low),as.character(id_geo_Low)] %>% as.matrix()
      
      # Identifing unique_id in category Inter
      id_geo_Inter <- matrix.i %>% 
        select(id_unique, cat[o]) %>% 
        filter(.[[cat[o]]] =="Intermediate") %>% 
        pull(id_unique)
      
      # Selecting only unique id in Inter category
      geo.i_Inter <- geo.i[as.character(id_geo_Inter),as.character(id_geo_Inter)] %>% as.matrix()
      
      
      # Identifing unique_id in category High
      id_geo_High <- matrix.i %>% 
        select(id_unique, cat[o]) %>% 
        filter(.[[cat[o]]] =="High") %>% 
        pull(id_unique)
      
      # Selecting only unique id in High category
      geo.i_High <- geo.i[as.character(id_geo_High),as.character(id_geo_High)] %>% as.matrix()
      
      # Summarizing the results
      result <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
      
      
      # calculating mean distance
      if(ncol(geo.i_Low)>2){
        result[i, "Low"] <- median(geo.i_Low, na.rm = T)} else {
          result[i, 1] <- NA}
      
      # calculating mean distance
      if(ncol(geo.i_Inter)>2){
        result[i, "Inter"] <- median(geo.i_Inter, na.rm = T)} else {
          result[i, "Inter"] <- NA}
      
      # calculating mean distance
      if(ncol(geo.i_High)>2){
        result[i, "High"] <- median(geo.i_High, na.rm = T)} else {
          result[i, "High"] <- NA}
      
      result[i,"study_id"] <- study_name
      
      Geo_dist[[o]][i,] <- result[i,]
      
      
    }
  }
  
  return(Geo_dist)
}

dist_geo_haver_md <- my_fun_haver_md(cat = cat_vector, matrix = matrix_cat)





# # The same function, but using geographic distance in meters ----
# 
# my_fun_alb <- function(cat, matrix){
#   geo_list_500 <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_1k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_2k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_4k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   geo_list_8k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#   
#   Geo_dist <- list(geo_list_500, geo_list_1k, geo_list_2k, geo_list_4k, geo_list_8k)
#   names(Geo_dist) <- c("500", "1k", "2k", "4k", "8k")
#   
#   # # Transforming geo_point to crs = albers
#    Albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs" #102033 - More info: https://epsg.io/102033
#   
#   
#   for (i in 1:length(matrix)){
#     # i = 3
#     
#     matrix.i <- matrix[[i]] %>% 
#       st_as_sf() %>% 
#       st_transform(crs = Albers)
#     
#     study_name <- names(matrix[i])
#     
#     site_id <- matrix.i$id_unique # a vector with unique id names
#     
#     geo.i <- st_distance(matrix.i$geom)
#     
#     for (m in 1:ncol(geo.i)){ # Transforming 0 to NA
#       geo.i[m,m] <- NA
#     }
#     
#     #Naming cols and rows with unique id for final matrix
#     rownames(geo.i) <- site_id
#     colnames(geo.i) <- site_id
#     
#     #saveRDS(matrix.i, paste0("Geo_Dist/", study_name, ".Rdata"))
#     
#     for(o in 1:5){
#       #o = 1
#       
#       #Initial ----
#       # For buffer 500
#       # Identifing unique_id in category Low
#       id_geo_Low <- matrix.i %>% 
#         select(id_unique, cat[o]) %>% 
#         filter(.[[cat[o]]] == "Low") %>% 
#         pull(id_unique)
#       
#       # Selecting only unique id in Low category
#       geo.i_Low <- geo.i[as.character(id_geo_Low),as.character(id_geo_Low)] %>% as.matrix()
#       
#       # Identifing unique_id in category Inter
#       id_geo_Inter <- matrix.i %>% 
#         select(id_unique, cat[o]) %>% 
#         filter(.[[cat[o]]] =="Intermediate") %>% 
#         pull(id_unique)
#       
#       # Selecting only unique id in Inter category
#       geo.i_Inter <- geo.i[as.character(id_geo_Inter),as.character(id_geo_Inter)] %>% as.matrix()
#       
#       
#       # Identifing unique_id in category High
#       id_geo_High <- matrix.i %>% 
#         select(id_unique, cat[o]) %>% 
#         filter(.[[cat[o]]] =="High") %>% 
#         pull(id_unique)
#       
#       # Selecting only unique id in High category
#       geo.i_High <- geo.i[as.character(id_geo_High),as.character(id_geo_High)] %>% as.matrix()
#       
#       # Summarizing the results
#       result <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
#       
#       
#       # calculating mean distance
#       if(ncol(geo.i_Low)>2){
#         result[i, "Low"] <- mean(geo.i_Low, na.rm = T)} else {
#           result[i, 1] <- NA}
#       
#       # calculating mean distance
#       if(ncol(geo.i_Inter)>2){
#         result[i, "Inter"] <- mean(geo.i_Inter, na.rm = T)} else {
#           result[i, "Inter"] <- NA}
#       
#       # calculating mean distance
#       if(ncol(geo.i_High)>2){
#         result[i, "High"] <- mean(geo.i_High, na.rm = T)} else {
#           result[i, "High"] <- NA}
#       
#       result[i,"study_id"] <- study_name
#       
#       Geo_dist[[o]][i,] <- result[i,]
#       
#     }
#   }
#   
#   return(Geo_dist)
# }
# 
# 
# dist_geo_alb <- my_fun_alb(cat = cat_vector, matrix = matrix_cat)

# saveRDS(dist_geo_haver,"Geo_Dist/geo_dist_haver.Rdata")
saveRDS(dist_geo_haver_md,"Geo_Dist/geo_dist_haver_md.Rdata")
# saveRDS(dist_geo_alb,"Geo_Dist/geo_dist_alb.Rdata")


  
  









# 
# 
# Geo_dist_500 <- gather(Geo_dist[["500"]], key = "cat", value = "Dist_mean", -study_id)
# 
# Geo_dist_1k <- gather(Geo_dist[["1k"]], key = "cat", value = "Dist_mean", -study_id)
# 
# Geo_dist_2k <- gather(Geo_dist[["2k"]], key = "cat", value = "Dist_mean", -study_id)
# 
# Geo_dist_4k <- gather(Geo_dist[["4k"]], key = "cat", value = "Dist_mean", -study_id)
# 
# Geo_dist_8k <- gather(Geo_dist[["8k"]], key = "cat", value = "Dist_mean", -study_id)
# 
# 
# # Selecting studies with at least 2 categories  ---
# cat_2_500 <- Geo_dist_500 %>% 
#   filter(!is.na(Dist_mean)) %>% # Removendo todas categorias = NA
#   count(study_id) %>% # Contando número de categorias
#   filter(n > 1) %>% # Selecionando estudos com 3 categorias
#   pull(study_id) # Criando um vetor dos estudos selecionados
# 
# cat_2_1k <- Geo_dist_1k %>% 
#   filter(!is.na(Dist_mean)) %>% # Removendo todas categorias = NA
#   count(study_id) %>% # Contando número de categorias
#   filter(n > 1) %>% # Selecionando estudos com 3 categorias
#   pull(study_id) # Criando um vetor dos estudos selecionados
# 
# cat_2_2k <- Geo_dist_2k %>% 
#   filter(!is.na(Dist_mean)) %>% # Removendo todas categorias = NA
#   count(study_id) %>% # Contando número de categorias
#   filter(n > 1) %>% # Selecionando estudos com 3 categorias
#   pull(study_id) # Criando um vetor dos estudos selecionados
# 
# cat_2_4k <- Geo_dist_4k %>% 
#   filter(!is.na(Dist_mean)) %>% # Removendo todas categorias = NA
#   count(study_id) %>% # Contando número de categorias
#   filter(n > 1) %>% # Selecionando estudos com 3 categorias
#   pull(study_id) # Criando um vetor dos estudos selecionados
# 
# cat_2_8k <- Geo_dist_8k %>% 
#   filter(!is.na(Dist_mean)) %>% # Removendo todas categorias = NA
#   count(study_id) %>% # Contando número de categorias
#   filter(n > 1) %>% # Selecionando estudos com 3 categorias
#   pull(study_id) # Criando um vetor dos estudos selecionados
# 
# # Filtering studies with at least 2 categories
# 
# # Geo_dist data
# Geo_dist_500 <- Geo_dist_500 %>% 
#   filter(study_id %in% cat_2_500)
# 
# Geo_dist_1k <- Geo_dist_1k %>% 
#   filter(study_id %in% cat_2_1k)
# 
# Geo_dist_2k <- Geo_dist_2k %>% 
#   filter(study_id %in% cat_2_2k)
# 
# Geo_dist_4k <- Geo_dist_4k %>% 
#   filter(study_id %in% cat_2_4k)
# 
# Geo_dist_8k <- Geo_dist_8k %>% 
#   filter(study_id %in% cat_2_8k)
# 
# Geo_dist_500 <- spread(Geo_dist_500, key = "cat", value = "Dist_mean") %>% select(study_id, Low, Inter, High)
# Geo_dist_1k <- spread(Geo_dist_1k, key = "cat", value = "Dist_mean") %>% select(study_id, Low, Inter, High)
# Geo_dist_2k <- spread(Geo_dist_2k, key = "cat", value = "Dist_mean") %>% select(study_id, Low, Inter, High)
# Geo_dist_4k <- spread(Geo_dist_4k, key = "cat", value = "Dist_mean") %>% select(study_id, Low, Inter, High)
# Geo_dist_8k <- spread(Geo_dist_8k, key = "cat", value = "Dist_mean") %>% select(study_id, Low, Inter, High)
# 

saveRDS(Geo_dist_500, "Geo_Dist/geo_dist_500.Rdata")
saveRDS(Geo_dist_1k, "Geo_Dist/geo_dist_1k.Rdata")
saveRDS(Geo_dist_2k, "Geo_Dist/geo_dist_2k.Rdata")
saveRDS(Geo_dist_4k, "Geo_Dist/geo_dist_4k.Rdata")
saveRDS(Geo_dist_8k, "Geo_Dist/geo_dist_8k.Rdata")


#rm(list=ls())
#   