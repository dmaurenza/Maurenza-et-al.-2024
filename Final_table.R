 library(tidyverse)
# Final_table_NST

# occur <- readRDS("Data/NST_Occur.RData") 
# abund <- readRDS("Data/NST_Abund.RData")
occur <- readRDS("Data/NST_Occur_out.RData")
abund <- readRDS("Data/NST_Abund_out.RData")
dist <- readRDS("Geo_Dist/geo_dist_haver_md.Rdata")
dist <- lapply(dist, as_tibble)

# Mudando o nome das colunas
col <- c("Low", "Inter", "High", "study_id")
occur <- lapply(occur, setNames, nm = col)
abund <- lapply(abund, setNames, nm = col)


# Transformando os dados de colunas para linhas
occur <- lapply(occur, gather, key = "cat", value = "RC_occur", -study_id)
abund <- lapply(abund, gather, key = "cat", value = "RC_abund", -study_id)
dist <- lapply(dist, gather, key = "cat", value = "dist", -study_id)


occur_dist <- list()

for (i in names(occur)){
  #i = 1
  occur_dist[[i]] <- occur[[i]] %>% 
    left_join(dist[[i]])
}

abund_dist <- list()

for (i in names(abund)){
  #i = 1
  abund_dist[[i]] <- abund[[i]] %>% 
    left_join(dist[[i]])
}


occur_dist <- lapply(occur_dist, filter, !is.na(RC_occur)) %>% 
  lapply(group_by, study_id) %>% 
  lapply(filter, n()>1) %>% 
  lapply(ungroup)

abund_dist <- lapply(abund_dist, filter, !is.na(RC_abund)) %>% 
  lapply(group_by, study_id) %>% 
  lapply(filter, n()>1) %>% 
  lapply(ungroup)



saveRDS(occur_dist, "Data/occur_final_out.RData")
saveRDS(abund_dist, "Data/abund_final_out.RData")



#rm(list=ls())