setwd("/Chapter_2/Database/")
##### Dados NST ocorrencia #####
# Com os dados limpos, cada estudo é separado em um elemento de uma lista
# Em cada estudo, left_join com as categorias. Assim, os sites são separados por categoria
# Criar um data.freme para armazenar os dados
# Inicio da função - Organização dos dados. É gerado um datafreme com com os dados biológicos (usando dados de ocorrência, todos os valores maior que zero = 1). É gerado um dataframe com 1 coluna, indicando os grupos. 
# Calcula a frequencia de cada espécie. Primeiro, calcula a frequencia das espécies para cada categoria (Mean). Depois calcula a média das frequencias das categorias (média da média).
# desfazer o datafreme dos grupos. Assim, RC e NST é calculado com o pool regional de espécies.
# Roda a Análise. Distancia Bray-curtis, modelo nulo PF, Ponderado pela abundância, Insere o vetor de frequencia, Incluir RC.
# Refazer o dataframe dos grupos. Rodar a análise novamente. Esta etapa serve somente para obter os pares de sites de cada grupo.
# Left_Join dos pares com os dados gerados usando Pool Regional.
# Calcula as médias das categorias


library(tidyverse)
library(NST)
#library(DirichletReg)

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

my_fun <- function(cat, matrix){
  
  list_cat_500 <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  
  list_cat_1k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  
  list_cat_2k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  
  list_cat_4k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  
  list_cat_8k <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
  
  list_cat <- list(list_cat_500, list_cat_1k, list_cat_2k, list_cat_4k, list_cat_8k)
  for (i in 1:length(matrix)){ # length(matrix)
    
    #i = 2
    matrix.i <- matrix[[i]] 
    
    # Organizing dataframe for biological data and categories
    matrix.comm <- matrix.i %>% 
      select(!starts_with("cat_")) 
    matrix.i.cat <- matrix.i %>% 
      select(starts_with("cat_")) 
    
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
      
      low.RC <- mean(groups[groups$group=="Low", "RC.jaccard"])
      inter.RC <- mean(groups[groups$group=="Intermediate", "RC.jaccard"])
      high.RC <- mean(groups[groups$group=="High", "RC.jaccard"])
      
      # Summarizing the results into element of list
      
      result <- data.frame(Low.RC.jaccard = as.double(), Inter.RC.jaccard = as.double(), High.RC.jaccard = as.double(), study_id = as.character(), stringsAsFactors=FALSE)
      
      result[1,1] <- low.RC
      result[1,2] <- inter.RC
      result[1,3] <- high.RC
      result[1,4] <- matrix.i[1,"study_id"] %>% pull()
      list_cat[[o]][i,] <- result
     
    }
  }
  
    return(list_cat)
}

cat_vector <- c("cat_500","cat_1k","cat_2k", "cat_4k", "cat_8k")

NST_occur <- my_fun(cat = cat_vector, matrix = matrix_cat)

saveRDS(NST_occur[[1]], "NST/NST_occur_500.Rdata")
saveRDS(NST_occur[[2]], "NST/NST_occur_1k.Rdata")
saveRDS(NST_occur[[3]], "NST/NST_occur_2k.Rdata")
saveRDS(NST_occur[[4]], "NST/NST_occur_4k.Rdata")
saveRDS(NST_occur[[5]], "NST/NST_occur_8k.Rdata")






#rm(list=ls())