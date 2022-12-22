library(measurements)
library(tidyverse)
library(data.table)
# read and adjusting tables ----

cat <- read_csv("Data/sites_categories.csv")
cat <- cat %>% 
  dplyr::select(id_unique, starts_with("cat_"))

datafull <- read_csv("Data/Pre_processed.csv") 

# n sampling sites
datafull %>% 
  distinct(id_unique)



data <- datafull %>%
  select(id_unique, id_site, study_id, species, total_abund, taxon_category)

study_list <- data %>% 
  group_split(study_id, .keep = T) %>% 
  lapply(spread, key = species, value = total_abund, fill = 0) %>% 
  lapply(left_join, y = cat)

for (i in 1:length(study_list)){
  #i = 1
  names(study_list)[[i]] <- study_list[[i]][1,"study_id"] %>% pull
}

study_initial <- names(study_list)

occur <- readRDS("Data/occur_final_out.RData")
abund <- readRDS("Data/abund_final_out.RData")
nome <- names(occur)


# Numero de estudos ----
occur_500 <- occur[["500"]]
occur_500_n <- occur_500 %>% 
  filter(!is.na(RC_occur)) %>% 
  count(study_id)

abund_4k <- abund[["4k"]]
abund_4k_n <- abund_4k %>% 
  filter(!is.na(RC_abund)) %>% 
  count(study_id)


n_study <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  x.i <- occur [[i]] %>% 
    filter(!is.na(RC_occur)) %>% 
    count(cat)
  n_study[i,"scale"] <- i
  n_study[i,"Low"] <- x.i[x.i$cat == "Low","n"]
  n_study[i,"Inter"] <- x.i[x.i$cat == "Inter","n"]
  n_study[i,"High"] <- x.i[x.i$cat == "High","n"]
}
n_study

# Number of dataset per FC level - for occurrence 500m ----
list_cat_500 <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), taxon = as.character(), scale = as.character(), stringsAsFactors=FALSE)

list_cat_1k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), taxon = as.character(), scale = as.character(), stringsAsFactors=FALSE)

list_cat_2k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), taxon = as.character(), scale = as.character(), stringsAsFactors=FALSE)

list_cat_4k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), taxon = as.character(), scale = as.character(), stringsAsFactors=FALSE)

list_cat_8k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), taxon = as.character(), scale = as.character(), stringsAsFactors=FALSE)

list_cat <- list(list_cat_500, list_cat_1k, list_cat_2k, list_cat_4k, list_cat_8k)
names(list_cat) <- nome

for(i in 1:length(study_list)) { 
  #i = 1
  occur.i <- study_list[[i]]
  
  for(o in nome){
    #o = "500"
    n_dataset_o <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), taxon = as.character(), scale = as.character(), stringsAsFactors=FALSE)
    
    n_sites.o <- as_tibble(occur.i) %>% 
      select(study_id, taxon_category, paste0("cat_",o)) %>% 
      rename(cat = paste0("cat_",o))
    n_sites.o <- n_sites.o %>% 
      count(cat) %>% 
      arrange(desc(1)) %>% 
      filter(n>2)
    
    if(nrow(n_sites.o[n_sites.o$cat == "Low",]) == 1){
      n_dataset_o[o,"Low"] <- n_sites.o[n_sites.o$cat == "Low", "n"]
    } else {
      n_dataset_o[o,"Low"] <- NA  }
    
    if(nrow(n_sites.o[n_sites.o$cat == "Intermediate",]) == 1){
      n_dataset_o[o,"Inter"] <- n_sites.o[n_sites.o$cat == "Intermediate", "n"]
    } else {
      n_dataset_o[o,"Inter"] <- NA  }
    
    if(nrow(n_sites.o[n_sites.o$cat == "High",]) == 1){
      n_dataset_o[o,"High"] <- n_sites.o[n_sites.o$cat == "High", "n"]
    } else {
      n_dataset_o[o,"High"] <- NA  }
    
    n_dataset_o[o, "study_id"] <- occur.i[1,"study_id"]
    n_dataset_o[o, "taxon"] <- occur.i[1,"taxon_category"]
    n_dataset_o[o, "scale"] <- o
  
    list_cat[[o]][i,] <- n_dataset_o
  }
}
list_cat



n_study_final <- lapply(occur, count, study_id)


n_study_final_500 <- list_cat[["500"]] %>% 
  filter(study_id %in% n_study_final[["500"]]$study_id)

n_study_final_1k <- list_cat[["1k"]] %>% 
  filter(study_id %in% n_study_final[["1k"]]$study_id)

n_study_final_2k <- list_cat[["2k"]] %>% 
  filter(study_id %in% n_study_final[["2k"]]$study_id)

n_study_final_4k <- list_cat[["4k"]] %>% 
  filter(study_id %in% n_study_final[["4k"]]$study_id)

n_study_final_8k <- list_cat[["8k"]] %>% 
  filter(study_id %in% n_study_final[["8k"]]$study_id)

# Number of studies per class

nstudy <- rbind(n_study_final_500, n_study_final_1k, n_study_final_2k, n_study_final_4k, n_study_final_8k)

nstudy <- gather(nstudy, "cat", "n_site", -c("study_id", "taxon", "scale"))
nstudy <- nstudy %>% 
  filter(!is.na(n_site))

listagem <- list()

for(i in nome){
  #For vertebrates
  #i = nome[1]
  n_study_vert <- nstudy[nstudy$taxon == "Vertebrado" & nstudy$scale == i,]
  n_study_tax_level <- n_study_vert %>% 
    count(study_id) %>%  
    filter(n>1) %>% 
    pull(1)
  vert <- nstudy[nstudy$taxon == "Vertebrado" & nstudy$scale == i,] %>% 
    filter(study_id %in% n_study_tax_level)

  n_study_plant <- nstudy[nstudy$taxon == "Planta" & nstudy$scale == i,]
  n_study_tax_level <- n_study_plant %>% 
    count(study_id) %>%  
    filter(n>1) %>% 
    pull(1)
  plant <- nstudy[nstudy$taxon == "Planta" & nstudy$scale == i,] %>% 
    filter(study_id %in% n_study_tax_level)
  
  n_study_invert <- nstudy[nstudy$taxon == "Invertebrado" & nstudy$scale == i,]
  n_study_tax_level <- n_study_invert %>% 
    count(study_id) %>%  
    filter(n>1) %>% 
    pull(1)
  invert <- nstudy[nstudy$taxon == "Invertebrado" & nstudy$scale == i,] %>% 
    filter(study_id %in% n_study_tax_level)
  
  listagem[[i]] <- rbind(vert, plant, invert)
}

# nsite_level_500 <- listagem[["500"]] %>% 
#   count(taxon, cat)
#   
# nsite_level_1k <- listagem[["1k"]] %>% 
#   count(taxon, cat)
# 
# nsite_level_2k <- listagem[["2k"]] %>% 
#   count(taxon, cat)
# 
# nsite_level_4k <- listagem[["4k"]] %>% 
#   count(taxon, cat)
# 
# nsite_level_8k <- listagem[["8k"]] %>% 
#   count(taxon, cat)



nsite_level_500 <- listagem[["500"]] %>%
  group_by(cat) %>% 
  summarise(mean_level = mean(n_site))

nsite_level_1k <- listagem[["1k"]] %>%
  group_by(cat) %>% 
  summarise(mean_level = mean(n_site))

nsite_level_2k <- listagem[["2k"]] %>%
  group_by(cat) %>% 
  summarise(mean_level = mean(n_site))

nsite_level_4k <- listagem[["4k"]] %>%
  group_by(cat) %>% 
  summarise(mean_level = mean(n_site))

nsite_level_8k <- listagem[["8k"]] %>%
  group_by(cat) %>% 
  summarise(mean_level = mean(n_site))


n_site_level <- rbind(nsite_level_500, nsite_level_1k, nsite_level_2k, nsite_level_4k, nsite_level_8k)

n_site_level <- n_site_level %>% 
  arrange(desc(cat)) %>% 
  mutate(cat = gsub("Inter", "Intermediate", cat),
         mean_level = round(mean_level, 1))
scale <- rep(c("500 m", "1 km", "2 km", "4 km", "8 km"), each = 1, times = 3)

n_site_level <- cbind(n_site_level, scale) %>% 
  arrange(desc(cat)) %>% 
  mutate(mean_level = round(mean_level, 1))

write_csv(n_site_level, "Results/landscapes_level.csv")
# Geographic distance ----


dist_500 <- occur[["500"]] %>% 
  mutate(dist_km = dist/1000) %>% 
  group_by(cat) %>% 
  summarise(mean_m = mean(dist_km), 
            max_dist = max(dist_km), 
            min_dist = min(dist_km))
  

dist_1k <- occur[["1k"]] %>% 
  mutate(dist_km = dist/1000) %>% 
  group_by(cat) %>% 
  summarise(mean_m = mean(dist_km), 
            max_dist = max(dist_km), 
            min_dist = min(dist_km))


dist_2k <- occur[["2k"]] %>% 
  mutate(dist_km = dist/1000) %>% 
  group_by(cat) %>% 
  summarise(mean_m = mean(dist_km), 
            max_dist = max(dist_km), 
            min_dist = min(dist_km))

dist_4k <- occur[["4k"]] %>% 
  mutate(dist_km = dist/1000) %>% 
  group_by(cat) %>% 
  summarise(mean_m = mean(dist_km), 
            max_dist = max(dist_km), 
            min_dist = min(dist_km))

dist_8k <- occur[["8k"]] %>% 
  mutate(dist_km = dist/1000) %>% 
  group_by(cat) %>% 
  summarise(mean_m = mean(dist_km), 
            max_dist = max(dist_km), 
            min_dist = min(dist_km))

dist_level <- rbind(dist_500, dist_1k, dist_2k, dist_4k, dist_8k)



nsite_dist <- cbind(dist_level) %>% 
  arrange(desc(cat)) %>% 
  mutate(cat = gsub("Inter", "Intermediate", cat),
         mean_level = round(mean_m, 1),
         max_dist = round(max_dist, 1),
         min_dist = round(min_dist, 1))

level_dist <- nsite_dist %>% 
  mutate(scale = scale) %>% 
  select(cat, scale, mean_level, min_dist, max_dist)

write_csv(level_dist, "Results/level_dist.csv")

# Geographic distances 

occur <- readRDS("Data/occur_final_out.RData")


occur_500 <- occur[["500"]] %>% 
  mutate(dist_km = dist/1000) %>% 
  mutate(cat = if_else(cat == "Low", true = "Low", false = if_else(cat == "Inter", true = "Intermediate", false = "High")))

occur_500 %>% 
  group_by(cat) %>% 
  summarise (max_dist = max(dist_km), min_dist = min(dist_km), mean_dist = mean(dist_km))



  
# Main results
# Occurance
scale_occur <- read_csv("Results/R2_occur.csv")
em_occur <- read_csv("Results/em_res_occur.csv") %>% 
  filter(scale == "500")
r2_occur <- read_csv("Results/R2_occur.csv")
anv_occur <- read_csv("Results/Anova_occur.csv")

#Abundance
scale_abund <- read_csv("Results/R2_abund.csv")
em_abund <- read_csv("Results/em_res_abund.csv") %>% 
  filter(scale == "4k")
r2_abund <- read_csv("Results/R2_abund.csv")
anv_abund <- read_csv("Results/Anova_abund.csv")

c("β diversity", "Landscape size", "Number of Datasets", "R2M", "R2C", "p")

RC = rep(c("βRC_occur", "βRC_abund"), each = 5)
scale <- rbind(scale_occur, scale_abund) 
scale <- scale %>% 
  mutate(R2M = round(R2m, 2), R2C = round(R2c, 2)) %>% 
  select(R2M, R2C)

n_data <- rbind(n_study, n_study)

stats_occur <- select(anv_occur, F_cat, P_cat)
stats_abund <- select(anv_abund, F_cat, P_cat)
stats <- rbind(stats_occur, stats_abund)
stats <- stats %>% 
  mutate(F = round(F_cat, 2), p = round(P_cat, 4)) %>% 
  select(F, p)
final_results <- data.frame(RC, n_data, scale, stats)
final_results

write_csv(final_results, "Results/Final_Results.csv")
# For taxon groups ----

taxon <- lapply(study_list, select, c("id_unique", "id_site", "study_id", "taxon_category"), starts_with("cat_"))
taxon <- rbindlist(taxon)

taxon_500 <- taxon %>% 
  select(id_unique, id_site, study_id, taxon_category, cat_500) %>% 
  rename(cat = cat_500) %>% 
  count(study_id, cat, taxon_category) %>% 
  filter(n>2)
taxon_500 <- taxon_500 %>% 
  group_by(taxon_category, cat) %>% 
  summarise(mean_level = mean(n, rm.na = T)) %>% 
  mutate(scale = "500")

taxon_1k <- taxon %>% 
  select(id_unique, id_site, study_id, taxon_category, cat_1k) %>% 
  rename(cat = cat_1k) %>% 
  count(study_id, cat, taxon_category) %>% 
  filter(n>2)
taxon_1k <- taxon_1k %>% 
  group_by(taxon_category, cat) %>% 
  summarise(mean_level = mean(n, rm.na = T)) %>% 
  mutate(scale = "1k")

taxon_2k <- taxon %>% 
  select(id_unique, id_site, study_id, taxon_category, cat_2k) %>% 
  rename(cat = cat_2k) %>% 
  count(study_id, cat, taxon_category) %>% 
  filter(n>2)
taxon_2k <- taxon_2k %>% 
  group_by(taxon_category, cat) %>% 
  summarise(mean_level = mean(n, rm.na = T)) %>% 
  mutate(scale = "2k")

taxon_4k <- taxon %>% 
  select(id_unique, id_site, study_id, taxon_category, cat_4k) %>% 
  rename(cat = cat_4k) %>% 
  count(study_id, cat, taxon_category) %>% 
  filter(n>2)
taxon_4k <- taxon_4k %>% 
  group_by(taxon_category, cat) %>% 
  summarise(mean_level = mean(n, rm.na = T)) %>% 
  mutate(scale = "4k")

taxon_8k <- taxon %>% 
  select(id_unique, id_site, study_id, taxon_category, cat_8k) %>% 
  rename(cat = cat_8k) %>% 
  count(study_id, cat, taxon_category) %>% 
  filter(n>2)
taxon_8k <- taxon_8k %>% 
  group_by(taxon_category, cat) %>% 
  summarise(mean_level = mean(n, rm.na = T)) %>% 
  mutate(scale = "8k")

taxon_500 <- taxon_500 %>% 
  spread(cat, mean_level) %>% arrange(desc(taxon_category))

taxon_1k <- taxon_1k %>% 
  spread(cat, mean_level) %>% arrange(desc(taxon_category))

taxon_2k <- taxon_2k %>% 
  spread(cat, mean_level) %>% arrange(desc(taxon_category))

taxon_4k <- taxon_4k %>% 
  spread(cat, mean_level) %>% arrange(desc(taxon_category))

taxon_8k <- taxon_8k %>% 
  spread(cat, mean_level) %>% arrange(desc(taxon_category))

taxon_table <- rbind(taxon_500, taxon_1k, taxon_2k, taxon_4k, taxon_8k) %>% 
  arrange(desc(taxon_category))


# Occurance
scale_occur_vert <- read_csv("Results/R2_occur_Vertebrates.csv") %>% 
  mutate(taxon_category ="Vertebrado")
r2_occur_vert <- read_csv("Results/R2_occur_Vertebrates.csv")%>% 
  mutate(taxon_category ="Vertebrado")
anv_occur_vert <- read_csv("Results/Anova_occur_Vertebrates.csv")%>% 
  mutate(taxon_category ="Vertebrado")

scale_occur_plant <- read_csv("Results/R2_occur_plants.csv")%>% 
  mutate(taxon_category ="Planta")
r2_occur_plant <- read_csv("Results/R2_occur_plants.csv")%>% 
  mutate(taxon_category ="Planta")
anv_occur_plant <- read_csv("Results/Anova_occur_plants.csv")%>% 
  mutate(taxon_category ="Planta")

scale_occur_invert <- read_csv("Results/R2_occur_Invet.csv")%>% 
  mutate(taxon_category ="Invertebrado")
r2_occur_invert <- read_csv("Results/R2_occur_Invet.csv")%>% 
  mutate(taxon_category ="Invertebrado")
anv_occur_invert <- read_csv("Results/Anova_occur_Invet.csv")%>% 
  mutate(taxon_category ="Invertebrado")

# abundance
scale_abund_vert <- read_csv("Results/R2_abund_Vertebrates.csv") %>% 
  mutate(taxon_category ="Vertebrado")
r2_abund_vert <- read_csv("Results/R2_abund_Vertebrates.csv")%>% 
  mutate(taxon_category ="Vertebrado")
anv_abund_vert <- read_csv("Results/Anova_abund_Vertebrates.csv")%>% 
  mutate(taxon_category ="Vertebrado")

scale_abund_plant <- read_csv("Results/R2_abund_plants.csv")%>% 
  mutate(taxon_category ="Planta")
r2_abund_plant <- read_csv("Results/R2_abund_plants.csv")%>% 
  mutate(taxon_category ="Planta")
anv_abund_plant <- read_csv("Results/Anova_abund_plants.csv")%>% 
  mutate(taxon_category ="Planta")

scale_abund_invert <- read_csv("Results/R2_abund_Invet.csv")%>% 
  mutate(taxon_category ="Invertebrado")
r2_abund_invert <- read_csv("Results/R2_abund_Invet.csv")%>% 
  mutate(taxon_category ="Invertebrado")
anv_abund_invert <- read_csv("Results/Anova_abund_Invet.csv")%>% 
  mutate(taxon_category ="Invertebrado")

scale_occur <- rbind(scale_occur_vert, scale_occur_plant, scale_occur_invert) %>% 
  mutate(R2M = round(R2m, 2), R2C = round(R2c, 2))
stats_occur <- rbind(anv_occur_vert, anv_occur_plant, anv_occur_invert) %>% 
  mutate(F = round(F_cat, 2), p = round(P_cat, 4))

table_occur <- taxon_table %>% 
  left_join(scale_occur) %>% 
  left_join(stats_occur) %>% 
  select(taxon_category, scale, High, Intermediate, Low, R2M,  R2C, F, p)



scale_abund <- rbind(scale_abund_vert, scale_abund_plant, scale_abund_invert) %>% 
  mutate(R2M = round(R2m, 2), R2C = round(R2c, 2))
stats_abund <- rbind(anv_abund_vert, anv_abund_plant, anv_abund_invert) %>% 
  mutate(F = round(F_cat, 2), p = round(P_cat, 4))



table_abund <- taxon_table %>% 
  left_join(scale_abund) %>% 
  left_join(stats_abund) %>% 
  select(taxon_category, scale, High, Intermediate, Low, R2M,  R2C, F, p)

RC = rep(c("βRC_occur", "βRC_abund"), each = 15)
taxon_table
table_taxon <- rbind(table_occur, table_abund) 

table_taxon <- as_tibble(RC) %>% 
  mutate(table_taxon) %>% 
  arrange(desc(taxon_category))

write_csv(table_taxon, "Results/final_results_taxon.csv")
# Geographic distance
# For 500m
# Taxonomic Groups 



taxon <- read_csv("Data/Pre_processed.csv")
taxon <- taxon %>% 
  distinct(study_id, taxon_category)

occur_tx <- lapply(occur, left_join, taxon)
abund_tx <- lapply(abund, left_join, taxon)

taxon_geo_occur_list <- list()
taxon_geo_abund_list <- list()
for(i in nome){
  #i = "500"
  taxon_geo_occur_list[[i]] <- occur_tx[[i]] %>% 
    group_by(taxon_category, cat) %>% 
    summarise(mean_m = mean(dist)) %>%
    mutate(mean_km = mean_m/1000)
  
  taxon_geo_abund_lista <- abund_tx[[i]] %>% 
    group_by(taxon_category, cat) %>% 
    summarise(mean_m = mean(dist)) %>%
    mutate(mean_km = mean_m/1000)
}
  
  
