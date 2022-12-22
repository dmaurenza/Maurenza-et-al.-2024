# This script deal to identify studies with differences in Effort Sampling


library(tidyverse)
library(data.table)

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
# 36,349
# 27,623
# 8,726
 unequal_study_list <- study_list[unequal_effort]

write_csv(as.data.frame(unequal_effort), "Data/unequal_study.csv")
write_csv(as.data.frame(equal_effort), "Data/equal_study.csv")
# Resampling #####
# # Testing with a dataset ----
# 
# unequal_study_list <- study_list[unequal_effort]
# 
# unequal.i <- unequal_study_list[[11]]
# 
# effort_abund <- unequal.i %>%
#   group_by(id_unique) %>% 
#   summarise(mn_effort = mean(effort), abund = sum(total_abund))
# 
# # esforço mínimo
# min_effort <- min(effort_abund$mn_effort)
# 
# # Definindo quais são os sites que possuem o esforço mínimo
# site_min <- effort_abund %>% 
#   filter(mn_effort %in% min_effort) %>% 
#   select(id_unique) %>% 
#   pull()
# 
# # calculando a abundancia de individuos esperada de cada site com o esforço minimo
# site_abund <- unequal.i %>% 
#   filter(!id_unique %in% site_min) %>% 
#   group_by(id_unique) %>% 
#   summarise(mn_effort = mean(effort), abund = sum(total_abund)) %>% 
#   mutate(abund_exp = round((min_effort*abund)/mn_effort, digits = 0), id_unique = as.character(id_unique))
# 
# # Definindo o tibble dos sites com esforços diferentes do esforço minimo
# 
# sp_sample <- unequal.i %>% 
#   filter(!id_unique %in% site_min) %>% 
#   mutate(id_unique = as.character(id_unique))
# 
# nsite <- unique(sp_sample$id_unique)
# 
# # construindo um pool de espécies de cada site 
# 
# list_site <- list()
# 
# for (m in 1:length(unique(sp_sample$id_unique))){
#   #m = 3
#   sp_sample.m <- sp_sample %>%
#     filter(id_unique == nsite[m])
#   #sum(sp_sample.m$total_abund)
#   pool<-c()
#   
#   for (o in 1:length(sp_sample.m$species)){
#     #o = 3
#     spec<-as.character(rep(sp_sample.m[o,"species"],sp_sample.m[o,"total_abund"] ))
#     
#     pool<-append(pool, spec)
#   }
#   
#   list_site[[m]] <- pool
# }
# names(list_site) <- nsite
# # Amostrando o numero de individos esperado do pool de indivíduos
# 
# list_site_final <- list()
# 
# for(i in names(list_site)){
#   #i = "240"
#   list_site_final[[i]] <- sample(list_site[[i]], as.double(site_abund[site_abund$id_unique == i,"abund_exp"] ), replace = F)
# }
# 
# 
# # Sumarizando as espécies e as abundâncias de cada site 
# new_list <- list()
# 
# for (i in names(list_site_final)){
#   #i = "Juliano_Bogoni"
#   new_list[[i]] <- list_site_final[[i]] %>% 
#     as_data_frame() %>% 
#     count(value) %>% 
#     mutate(id_unique = as.double(i))
# }
# 
# # Construindo os dados novos. Juntada com os dados dos sites com o minimo esforço
# 
# new_sample <- as_tibble(rbindlist(new_list)) %>% 
#   mutate(id_unique = as.character(id_unique))
# 
# new_sample_2 <- sp_sample %>% 
#   select(!c(effort, species, total_abund)) %>% 
#   distinct(study_id, id_unique, .keep_all = T) %>%
#   left_join(new_sample) %>% 
#   rename(species = value, total_abund = n) %>% 
#   mutate(effort = min_effort) %>% 
#   select(id_unique, study_id, id_site, effort, species, total_abund )
# 
# new_sample_final <- unequal.i %>% 
#   filter(id_unique %in% site_min) %>% 
#   rbind(new_sample_2)
# 
# 

# For all datasets with unequal sampling sites ----
resampling <- list()

for (r in 1:50){
  #r = 1
  listagem <- list()
  
  for(n in 1:length(unequal_study_list)){
    message(n)
    #n = 18
    unequal.i <- unequal_study_list[[n]]
    
    effort_abund <- unequal.i %>%
      group_by(id_unique) %>% 
      summarise(mn_effort = mean(effort), abund = sum(total_abund))
    
    # esforço mínimo
    min_effort <- min(effort_abund$mn_effort)
    
    # Definindo quais são os sites que possuem o esforço mínimo
    site_min <- effort_abund %>% 
      filter(mn_effort %in% min_effort) %>% 
      select(id_unique) %>%
      pull()
    
    # calculando a abundancia de individuos esperada  com o esforço minimo para cada site com esforço diferente do minimo
    site_abund <- unequal.i %>% 
      filter(!id_unique %in% site_min) %>%
      group_by(id_unique) %>% 
      summarise(mn_effort = mean(effort), abund = sum(total_abund)) %>%
      mutate(abund_exp = round((min_effort*abund)/mn_effort, digits = 0), id_unique = as.character(id_unique))
    
    # Definindo o tibble dos sites com esforços diferentes do esforço minimo
    sp_sample <- unequal.i %>% 
      filter(!id_unique %in% site_min) %>% 
      mutate(id_unique = as.character(id_unique))
    
    nsite <- unique(sp_sample$id_unique)
    
    # construindo um pool de espécies de cada site 
    
    list_site <- list()
    list_site_final <- list()
    for (m in nsite){
      #m = "1823"
      sp_sample.m <- sp_sample %>%
        filter(id_unique == m)
      #sum(sp_sample.m$total_abund)
      pool<-c()
      
      for (o in 1:length(sp_sample.m$species)){
        #o = 2
        spec<-as.character(rep(sp_sample.m[o,"species"],sp_sample.m[o,"total_abund"] ))
        
        pool<-append(pool, spec)
      }
      
      list_site[[m]] <- pool
      
      # Amostrando o numero de individos esperado do pool de indivíduos
      # # Sumarizando as espécies e as abundâncias de cada site     
      list_site_final[[m]] <- sample(list_site[[m]], as.double(site_abund[site_abund$id_unique == m,"abund_exp"] ), replace = F) %>% 
        as_data_frame() %>% 
        count(value) %>% 
        mutate(id_unique = m)
      
    }
    # Construindo os dados novos. Juntada com os dados dos sites com o minimo esforço 
    new_sample <- as_tibble(rbindlist(list_site_final)) 
    
    new_sample_2 <- sp_sample %>% 
      select(!c(effort, species, total_abund)) %>% 
      distinct(study_id, id_unique, .keep_all = T) %>%
      left_join(new_sample) %>% 
      rename(species = value, total_abund = n) %>% 
      mutate(effort = min_effort) %>% 
      select(id_unique, study_id, id_site, effort, species, total_abund )
    
    new_sample_final <- unequal.i %>% 
      filter(id_unique %in% site_min) %>% 
      rbind(new_sample_2) %>% 
      mutate(id_unique = as.double(id_unique))
    
    listagem[[n]] <- new_sample_final %>% 
      filter(!is.na(species))
  }
  
  resampling [[r]] <- listagem
  saveRDS(resampling[[r]], paste0("Resampling/resampling_", r,".RData"))
  
}



# for(i in 1:length(listagem)){
#   x <-  listagem[[i]][1,"study_id"]
#   names<-as.character(append(names, x))
# }
# names(resampling) <- names
# 


# # Tabela final
# 
# equal_study_list <- study_list[equal_effort]
# unequal_study_list <- resampling
# x <- rbindlist(equal_study_list)
# y <- rbindlist(unequal_study_list)
# datafull_resample <- rbind(x, y)


#rm(list = ls(all.names = TRUE))

