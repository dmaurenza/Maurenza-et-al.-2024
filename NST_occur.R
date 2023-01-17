library(tidyverse)
library(NST)
library(data.table)
library(sf)
library(geosphere)
library(mapview)
#library(DirichletReg)
# occur ----
cat <- read_csv("Data/sites_categories.csv")
cat <- cat %>% 
  dplyr::select(id_unique, starts_with("cat_"))

datafull <- read_csv("Data/Pre_processed.csv") 

data <- datafull %>%
  select(id_unique, id_site, study_id, species, total_abund)
names <- unique(data$study_id)
study_list <- data %>% 
  group_split(study_id, .keep = T)

# Spreading species for all studies

matrix <- lapply(study_list, FUN = spread, key = species, value = total_abund, fill = 0)

# Adding sites categories for each study

matrix_cat <- lapply(matrix, FUN = left_join, y = cat)

for (i in 1:length(matrix_cat)){
  #i = 1
  names(matrix_cat)[[i]] <- matrix_cat[[i]][1,"study_id"] %>% pull
}


equal <- read_csv("Data/equal_study.csv") %>% pull()

matrix_equal <- matrix_cat [equal]

matrix_unequal <- readRDS("Data/unequal_outlier.RData")

matrix_unequal <- lapply(matrix_unequal, select, !c("effort"))


matrix_cat <- c(matrix_equal, matrix_unequal) 
matrix_cat = matrix_cat[order(names(matrix_cat))] 
names(matrix_cat)

# Dist ----

matrix_dist <- readRDS("Data/Overall_Matrices.Rdata") # From Matrices script

cat_dist <- readRDS("Data/sites_categories_sf.RData")
cat_dist <- cat_dist %>% 
  dplyr::select(id_unique, starts_with("cat_"), geom)

matrix_cat_dist <- lapply(matrix_dist, left_join, y = cat_dist, by = "id_unique")
cat_vector <- c("cat_500","cat_1k","cat_2k", "cat_4k", "cat_8k")



names(matrix_cat_dist)

as.data.frame(names(matrix_cat),names(matrix_cat_dist))



my_fun <- function(cat, matrix, m.dist){
  # occur
  # list_cat_500 <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # 
  # list_cat_1k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # 
  # list_cat_2k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # 
  # list_cat_4k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # 
  # list_cat_8k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  
  list_cat <- list()
  
  lista.i <- list()
  
  # #dist
  # 
  # geo_list_500 <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # geo_list_1k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # geo_list_2k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # geo_list_4k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # geo_list_8k <- data.frame(Low = as.double(), Inter = as.double(), High = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  # 
  # Geo_dist <- list(geo_list_500, geo_list_1k, geo_list_2k, geo_list_4k, geo_list_8k)
  # names(Geo_dist) <- c("500", "1k", "2k", "4k", "8k")
  # 
  
  for (i in names(matrix_cat)){ # length(matrix) 
    
    #i = "Juliano_Bogoni"
    matrix.i <- matrix[[i]] 
    
    # Organizing dataframe for biological data and categories
    matrix.comm <- matrix.i %>% 
      select(!starts_with("cat_")) 
    matrix.i.cat <- matrix.i %>% 
      select(starts_with("cat_")) 
    #df <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
    
    
    matrix.i_dist <- m.dist[[i]] %>% 
      st_as_sf()
    
    study_name_dist <- names(matrix_dist[i])
    
    site_id_dist <- matrix.i_dist$id_unique # a vector with unique id names
    
    geo.i <- distm(x = as_Spatial(matrix.i_dist), fun = distHaversine)
    
    for (m in 1:ncol(geo.i)){ # Transforming 0 to NA
      geo.i[m,m] <- NA
    }
    
    #Naming cols and rows with unique id for final matrix
    rownames(geo.i) <- site_id_dist
    colnames(geo.i) <- site_id_dist
    
    
    for(o in 1:5){
      #o = 5
      matrix.o <- as_tibble(matrix.i.cat[,cat[o]]) %>% 
        mutate(matrix.comm)
      
      
      comm <- as.data.frame(matrix.o[,5:ncol(matrix.o)]) 
      comm[comm>0]=1
      row.names(comm)<-matrix.o$id_unique
      
      FC <- as.data.frame(matrix.o[cat[o]])
      row.names(FC)<-row.names(comm)
      
      # Checking rows names
      samp.ck=NST::match.name(rn.list=list(comm=comm,group=FC))
      comm=samp.ck$comm
      comm=comm[,colSums(comm)>0,drop=FALSE]
      FC=samp.ck$group
      
      # Eliminating categories with 1 or 2 sites. Filter those sites from biological data and categories
      
      n_sites <- matrix.o %>% 
        count(matrix.o[cat[o]]) %>% 
        arrange(desc(1)) %>% 
        filter(n>2) %>% 
        pull(1)
      matrix.sites <- matrix.o %>% 
        filter(.[[cat[o]]] %in% n_sites)
      
      groupi=FC[FC[,1] %in% n_sites,1,drop=FALSE]
      
      comm <- comm[rownames(comm) %in% matrix.sites$id_unique,]
      
      # Calculanting species frequencies
      
      grp.lev=unique(groupi[,1])
      freq.grps=t(sapply(1:length(grp.lev),
                         function(n)
                           #n = 1
                         {
                           sampi=rownames(groupi)[groupi[,1]==grp.lev[n]]
                           colMeans(comm[which(rownames(comm) %in% sampi),,drop=FALSE])
                         }))
      
      meta.frequency=t(as.matrix(colMeans(freq.grps)))
      
      # Run NST
      if(nrow(comm)>1) {
        tnst=tNST(comm=comm, 
                  group=groupi, 
                  meta.group=NULL, 
                  meta.com=NULL,
                  meta.frequency = meta.frequency,
                  dist.method = "jaccard",
                  abundance.weighted=FALSE,
                  rand=10000,
                  output.rand=TRUE,
                  null.model = "PF",
                  between.group=FALSE,
                  SES=FALSE,
                  RC=TRUE)}
      
      # Selecting RC from $index.pair. Getting sites pairs from $index.pair.grp. Left_join pairs with RC. Calculating mean RC
      
      RC.jaccard <- tnst[[1]][,c("name1", "name2", "RC.jaccard")] %>% 
        mutate(RC.jaccard = as.numeric(RC.jaccard))
      
      pair.gr <- tnst[[3]][1:3]
      
      groups <- pair.gr %>% 
        left_join(RC.jaccard)
      #high <- groups[groups$group == "High",]
      
      # 
      # low.RC <- groups[groups$group=="Low", "RC.jaccard"]
      # inter.RC <- groups[groups$group=="Intermediate", "RC.jaccard"]
      # high.RC <- groups[groups$group=="High", "RC.jaccard"]
      # 
      # # Summarizing the results into element of list
      # 
      # # result <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
      # 
      # 
      # l <- tibble::lst(low.RC, inter.RC, high.RC)
      # df.i <- data.frame(lapply(l, `length<-`, max(lengths(l))))
      # df.i$study_id <- matrix.i[1,"study_id"] %>% pull()
      # 
      # # result[,1] <- low.RC
      # # result[,2] <- inter.RC
      # # result[,3] <- high.RC
      # # result[,4] <- matrix.i[1,"study_id"] %>% pull()
      # lista.i[[i]] <- df.i
      # list_cat[[o]] <- lista.i
      # 
      # # Dist
      # 
      # #o = 5
      # 
      
      for(p in 1:length(groups$group)){
        #i = 1
        groups$geo[p] <- geo.i[groups[p,2],groups[p,3]]
        
      }
      
      lista.i[[i]] <- groups
      list_cat[[o]] <- lista.i
    
    }
    
  }
  
  return(list_cat)
}
NST_occur <- my_fun(cat = cat_vector, matrix = matrix_cat, m.dist = matrix_cat_dist)


NST_occur_final <- list()
for(i in 1:5){
  NST_occur_final[[i]] <- rbindlist(NST_occur[[i]])
}


saveRDS(NST_occur_final[[1]], "NST/NST_occur_500.Rdata")
saveRDS(NST_occur_final[[2]], "NST/NST_occur_1k.Rdata")
saveRDS(NST_occur_final[[3]], "NST/NST_occur_2k.Rdata")
saveRDS(NST_occur_final[[4]], "NST/NST_occur_4k.Rdata")
saveRDS(NST_occur_final[[5]], "NST/NST_occur_8k.Rdata")


#rm(list=ls())