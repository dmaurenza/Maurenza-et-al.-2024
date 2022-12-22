# ANOVA, using id study as random variable, for five distinct buffer sizes ----

# To calculate R2 - https://www.rdocumentation.org/packages/sjstats/versions/0.11.0/topics/r2
library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(visreg)
library(sjstats)
library(sf)
library(nlme)
library(MuMIn)
library(data.table)
library(corrplot)

#The marginal R2 value (R _GLMM(m)2) is the proportion of the variance in the dependent variable that is explained by the fixed variables only.

#The conditional R2 value (R _GLMM(c)2) is the proportion of the variance in the dependent variable that is explained by the fixed variables plus the random variables (i.e. the variance explained by the entire model).



# occur <- readRDS("Data/occur_final.RData")
# abund <- readRDS("Data/abund_final.RData")
occur <- readRDS("Data/occur_final_out.RData")
abund <- readRDS("Data/abund_final_out.RData")
nome <- as.character(names(occur))

occur[["500"]]
var <- c("RC_occur", "dist")
cor_occur <- cor(occur[["500"]][var])


#Analysis using occurrence data
mod_occur_500 <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur[["500"]])

# para distanciaa geo - testar com mediana e média, ver qual é o melhor com AIC
# Pq não fez mediana de RC? Porque é esses extremos que queremos captar.

# para estudos com esforço diferente. 1, excluir sites com esforço outliers, para diminuir a variação. 2 ecluir sites com esforço 1 ou 2 disvios padrões acima e abaixo da média. 3 Proceguir com os esforços desbalanceados.
# data1 %>% group_by(participante) %>%  identify_outliers(p_agua)

summary(mod_occur_500)

mod_occur_1k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur[["1k"]])
mod_occur_2k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur[["2k"]])
mod_occur_4k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur[["4k"]])
mod_occur_8k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur[["8k"]])

# R2

R2_occur_500 <- r.squaredGLMM(mod_occur_500)
R2_occur_1k <- r.squaredGLMM(mod_occur_1k)
R2_occur_2k <- r.squaredGLMM(mod_occur_2k)
R2_occur_4k <- r.squaredGLMM(mod_occur_4k)
R2_occur_8k <- r.squaredGLMM(mod_occur_8k)

# Anova
anov_occur_500 <- anova(mod_occur_500)
anov_occur_1k <- anova(mod_occur_1k)
anov_occur_2k <- anova(mod_occur_2k)
anov_occur_4k <- anova(mod_occur_4k)
anov_occur_8k <- anova(mod_occur_8k)

# emmeans
em_occur_500 <- emmeans(mod_occur_500, list(pairwise ~ cat), adjust = "tukey")
em_occur_1k <- emmeans(mod_occur_1k, list(pairwise ~ cat), adjust = "tukey")
em_occur_2k <- emmeans(mod_occur_2k, list(pairwise ~ cat), adjust = "tukey")
em_occur_4k <- emmeans(mod_occur_4k, list(pairwise ~ cat), adjust = "tukey")
em_occur_8k <- emmeans(mod_occur_8k, list(pairwise ~ cat), adjust = "tukey")



#Analysis using abundance data
mod_abund_500 <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund[["500"]])

mod_abund_1k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund[["1k"]])

mod_abund_2k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund[["2k"]])

mod_abund_4k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund[["4k"]])

mod_abund_8k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund[["8k"]])


# R2

R2_abund_500 <- r.squaredGLMM(mod_abund_500)
R2_abund_1k <- r.squaredGLMM(mod_abund_1k)
R2_abund_2k <- r.squaredGLMM(mod_abund_2k)
R2_abund_4k <- r.squaredGLMM(mod_abund_4k)
R2_abund_8k <- r.squaredGLMM(mod_abund_8k)

# Anova
anov_abund_500 <- anova(mod_abund_500)
anov_abund_1k <- anova(mod_abund_1k)
anov_abund_2k <- anova(mod_abund_2k)
anov_abund_4k <- anova(mod_abund_4k)
anov_abund_8k <- anova(mod_abund_8k)

# emmeans
em_abund_500 <- emmeans(mod_abund_500, list(pairwise ~ cat), adjust = "tukey")
em_abund_1k <- emmeans(mod_abund_1k, list(pairwise ~ cat), adjust = "tukey")
em_abund_2k <- emmeans(mod_abund_2k, list(pairwise ~ cat), adjust = "tukey")
em_abund_4k <- emmeans(mod_abund_4k, list(pairwise ~ cat), adjust = "tukey")
em_abund_8k <- emmeans(mod_abund_8k, list(pairwise ~ cat), adjust = "tukey")

# Final tables ----
# Models
mod_occur <- list(mod_occur_500, mod_occur_1k, mod_occur_2k, mod_occur_4k, mod_occur_8k)
names(mod_occur) <- nome

mod_abund <- list(mod_abund_500, mod_abund_1k, mod_abund_2k, mod_abund_4k, mod_abund_8k)
names(mod_abund) <- nome

#R2
R2_occur <- as_tibble(rbind(R2_occur_500, R2_occur_1k, R2_occur_2k, R2_occur_4k, R2_occur_8k))
R2_occur <- R2_occur %>% 
  mutate(scale = nome)


R2_abund <- as_tibble(rbind(R2_abund_500, R2_abund_1k, R2_abund_2k, R2_abund_4k, R2_abund_8k))
R2_abund <- R2_abund %>% 
  mutate(scale = nome)

#Anova
anova_occur <- list(anov_occur_500, anov_occur_1k, anov_occur_2k, anov_occur_4k, anov_occur_8k)
names(anova_occur) <- nome


Anov_results_occur <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_occur[i,"scale"] <- i
  Anov_results_occur[i, "F_cat"] <- anova_occur[[i]]$`F value`[1]
  Anov_results_occur[i, "F_dist"] <- anova_occur[[i]]$`F value`[2]
  Anov_results_occur[i, "P_cat"] <- anova_occur[[i]]$`Pr(>F)`[1]
  Anov_results_occur[i, "P_dist"] <- anova_occur[[i]]$`Pr(>F)`[2]
}
Anov_results_occur 



anova_abund <- list(anov_abund_500, anov_abund_1k, anov_abund_2k, anov_abund_4k, anov_abund_8k)
names(anova_abund) <- nome


Anov_results_abund <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_abund[i,"scale"] <- i
  Anov_results_abund[i, "F_cat"] <- anova_abund[[i]]$`F value`[1]
  Anov_results_abund[i, "F_dist"] <- anova_abund[[i]]$`F value`[2]
  Anov_results_abund[i, "P_cat"] <- anova_abund[[i]]$`Pr(>F)`[1]
  Anov_results_abund[i, "P_dist"] <- anova_abund[[i]]$`Pr(>F)`[2]
}
Anov_results_abund 


# Emmeans

em_res_occur <- list(as_tibble(em_occur_500$`emmeans of cat`), as_tibble(em_occur_1k$`emmeans of cat`), as_tibble(em_occur_2k$`emmeans of cat`), as_tibble(em_occur_4k$`emmeans of cat`), as_tibble(em_occur_8k$`emmeans of cat`))

for(i in 1:length(em_res_occur)) {
  #i = 2
  em_res_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_occur <- as_tibble(rbindlist(em_res_occur))


em_pair_occur <- list(as_tibble(em_occur_500$`pairwise differences of cat`),
                      as_tibble(em_occur_1k$`pairwise differences of cat`),
                      as_tibble(em_occur_2k$`pairwise differences of cat`),
                      as_tibble(em_occur_4k$`pairwise differences of cat`),
                      as_tibble(em_occur_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_occur)) {
  #i = 2
  em_pair_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_occur <- as_tibble(rbindlist(em_pair_occur))




em_res_abund <- list(as_tibble(em_abund_500$`emmeans of cat`), as_tibble(em_abund_1k$`emmeans of cat`), as_tibble(em_abund_2k$`emmeans of cat`), as_tibble(em_abund_4k$`emmeans of cat`), as_tibble(em_abund_8k$`emmeans of cat`))

for(i in 1:length(em_res_abund)) {
  #i = 2
  em_res_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_abund <- as_tibble(rbindlist(em_res_abund))


em_pair_abund <- list(as_tibble(em_abund_500$`pairwise differences of cat`),
                      as_tibble(em_abund_1k$`pairwise differences of cat`),
                      as_tibble(em_abund_2k$`pairwise differences of cat`),
                      as_tibble(em_abund_4k$`pairwise differences of cat`),
                      as_tibble(em_abund_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_abund)) {
  #i = 2
  em_pair_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_abund <- as_tibble(rbindlist(em_pair_abund))


# saving results ----
saveRDS(mod_occur, "Results/mod_occur.RData")
saveRDS(mod_abund, "Results/mod_abund.RData")
write_csv(R2_occur, "Results/R2_occur.csv")
write_csv(R2_abund, "Results/R2_abund.csv")
write_csv(Anov_results_occur, "Results/Anova_occur.csv")
write_csv(Anov_results_abund, "Results/Anova_abund.csv")
write_csv(em_res_occur, "Results/em_res_occur.csv")
write_csv(em_pair_occur, "Results/em_pair_occur.csv")
write_csv(em_res_abund, "Results/em_res_abund.csv")
write_csv(em_pair_abund, "Results/em_pair_abund.csv")


# Taxonomic Groups ----

taxon <- read_csv("Data/Pre_processed.csv")
taxon <- taxon %>% 
  distinct(study_id, taxon_category)

occur_tx <- lapply(occur, left_join, taxon)
abund_tx <- lapply(abund, left_join, taxon)

occur_tx_500 <- occur_tx[["500"]]
occur_tx_1k <- occur_tx[["1k"]]
occur_tx_2k <- occur_tx[["2k"]]
occur_tx_4k <- occur_tx[["4k"]]
occur_tx_8k <- occur_tx[["8k"]]

abund_tx_500 <- abund_tx[["500"]]
abund_tx_1k <- abund_tx[["1k"]]
abund_tx_2k <- abund_tx[["2k"]]
abund_tx_4k <- abund_tx[["4k"]]
abund_tx_8k <- abund_tx[["8k"]]

# Plants
#Analysis using occurrence data
mod_occur_500 <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_500 %>% filter(taxon_category == "Planta"))

mod_occur_1k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_1k %>% filter(taxon_category == "Planta"))

mod_occur_2k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_2k %>% filter(taxon_category == "Planta"))

mod_occur_4k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_4k %>% filter(taxon_category == "Planta"))

mod_occur_8k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_8k %>% filter(taxon_category == "Planta"))


# R2

R2_occur_500 <- r.squaredGLMM(mod_occur_500)
R2_occur_1k <- r.squaredGLMM(mod_occur_1k)
R2_occur_2k <- r.squaredGLMM(mod_occur_2k)
R2_occur_4k <- r.squaredGLMM(mod_occur_4k)
R2_occur_8k <- r.squaredGLMM(mod_occur_8k)

# Anova
anov_occur_500 <- anova(mod_occur_500)
anov_occur_1k <- anova(mod_occur_1k)
anov_occur_2k <- anova(mod_occur_2k)
anov_occur_4k <- anova(mod_occur_4k)
anov_occur_8k <- anova(mod_occur_8k)

# emmeans
em_occur_500 <- emmeans(mod_occur_500, list(pairwise ~ cat), adjust = "tukey")
em_occur_1k <- emmeans(mod_occur_1k, list(pairwise ~ cat), adjust = "tukey")
em_occur_2k <- emmeans(mod_occur_2k, list(pairwise ~ cat), adjust = "tukey")
em_occur_4k <- emmeans(mod_occur_4k, list(pairwise ~ cat), adjust = "tukey")
em_occur_8k <- emmeans(mod_occur_8k, list(pairwise ~ cat), adjust = "tukey")



#Analysis using abundance data
mod_abund_500 <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_500 %>% filter(taxon_category == "Planta"))

mod_abund_1k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_1k %>% filter(taxon_category == "Planta"))

mod_abund_2k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_2k %>% filter(taxon_category == "Planta"))

mod_abund_4k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_4k %>% filter(taxon_category == "Planta"))

mod_abund_8k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_8k %>% filter(taxon_category == "Planta"))


# R2

R2_abund_500 <- r.squaredGLMM(mod_abund_500)
R2_abund_1k <- r.squaredGLMM(mod_abund_1k)
R2_abund_2k <- r.squaredGLMM(mod_abund_2k)
R2_abund_4k <- r.squaredGLMM(mod_abund_4k)
R2_abund_8k <- r.squaredGLMM(mod_abund_8k)

# Anova
anov_abund_500 <- anova(mod_abund_500)
anov_abund_1k <- anova(mod_abund_1k)
anov_abund_2k <- anova(mod_abund_2k)
anov_abund_4k <- anova(mod_abund_4k)
anov_abund_8k <- anova(mod_abund_8k)

# emmeans
em_abund_500 <- emmeans(mod_abund_500, list(pairwise ~ cat), adjust = "tukey")
em_abund_1k <- emmeans(mod_abund_1k, list(pairwise ~ cat), adjust = "tukey")
em_abund_2k <- emmeans(mod_abund_2k, list(pairwise ~ cat), adjust = "tukey")
em_abund_4k <- emmeans(mod_abund_4k, list(pairwise ~ cat), adjust = "tukey")
em_abund_8k <- emmeans(mod_abund_8k, list(pairwise ~ cat), adjust = "tukey")


# Final tables ----
# Models
mod_occur <- list(mod_occur_500, mod_occur_1k, mod_occur_2k, mod_occur_4k, mod_occur_8k)
names(mod_occur) <- nome

mod_abund <- list(mod_abund_500, mod_abund_1k, mod_abund_2k, mod_abund_4k, mod_abund_8k)
names(mod_abund) <- nome

#R2
R2_occur <- as_tibble(rbind(R2_occur_500, R2_occur_1k, R2_occur_2k, R2_occur_4k, R2_occur_8k))
R2_occur <- R2_occur %>% 
  mutate(scale = nome)


R2_abund <- as_tibble(rbind(R2_abund_500, R2_abund_1k, R2_abund_2k, R2_abund_4k, R2_abund_8k))
R2_abund <- R2_abund %>% 
  mutate(scale = nome)

#Anova
anova_occur <- list(anov_occur_500, anov_occur_1k, anov_occur_2k, anov_occur_4k, anov_occur_8k)
names(anova_occur) <- nome


Anov_results_occur <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_occur[i,"scale"] <- i
  Anov_results_occur[i, "F_cat"] <- anova_occur[[i]]$`F value`[1]
  Anov_results_occur[i, "F_dist"] <- anova_occur[[i]]$`F value`[2]
  Anov_results_occur[i, "P_cat"] <- anova_occur[[i]]$`Pr(>F)`[1]
  Anov_results_occur[i, "P_dist"] <- anova_occur[[i]]$`Pr(>F)`[2]
}
Anov_results_occur 



anova_abund <- list(anov_abund_500, anov_abund_1k, anov_abund_2k, anov_abund_4k, anov_abund_8k)
names(anova_abund) <- nome


Anov_results_abund <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_abund[i,"scale"] <- i
  Anov_results_abund[i, "F_cat"] <- anova_abund[[i]]$`F value`[1]
  Anov_results_abund[i, "F_dist"] <- anova_abund[[i]]$`F value`[2]
  Anov_results_abund[i, "P_cat"] <- anova_abund[[i]]$`Pr(>F)`[1]
  Anov_results_abund[i, "P_dist"] <- anova_abund[[i]]$`Pr(>F)`[2]
}
Anov_results_abund 


# Emmeans

em_res_occur <- list(as_tibble(em_occur_500$`emmeans of cat`), as_tibble(em_occur_1k$`emmeans of cat`), as_tibble(em_occur_2k$`emmeans of cat`), as_tibble(em_occur_4k$`emmeans of cat`), as_tibble(em_occur_8k$`emmeans of cat`))

for(i in 1:length(em_res_occur)) {
  #i = 2
  em_res_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_occur <- as_tibble(rbindlist(em_res_occur))


em_pair_occur <- list(as_tibble(em_occur_500$`pairwise differences of cat`),
                      as_tibble(em_occur_1k$`pairwise differences of cat`),
                      as_tibble(em_occur_2k$`pairwise differences of cat`),
                      as_tibble(em_occur_4k$`pairwise differences of cat`),
                      as_tibble(em_occur_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_occur)) {
  #i = 2
  em_pair_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_occur <- as_tibble(rbindlist(em_pair_occur))




em_res_abund <- list(as_tibble(em_abund_500$`emmeans of cat`), as_tibble(em_abund_1k$`emmeans of cat`), as_tibble(em_abund_2k$`emmeans of cat`), as_tibble(em_abund_4k$`emmeans of cat`), as_tibble(em_abund_8k$`emmeans of cat`))

for(i in 1:length(em_res_abund)) {
  #i = 2
  em_res_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_abund <- as_tibble(rbindlist(em_res_abund))


em_pair_abund <- list(as_tibble(em_abund_500$`pairwise differences of cat`),
                      as_tibble(em_abund_1k$`pairwise differences of cat`),
                      as_tibble(em_abund_2k$`pairwise differences of cat`),
                      as_tibble(em_abund_4k$`pairwise differences of cat`),
                      as_tibble(em_abund_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_abund)) {
  #i = 2
  em_pair_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_abund <- as_tibble(rbindlist(em_pair_abund))


# saving results ----
saveRDS(mod_occur, "Results/mod_occur_plants.RData")
saveRDS(mod_abund, "Results/mod_abund_plants.RData")
write_csv(R2_occur, "Results/R2_occur_plants.csv")
write_csv(R2_abund, "Results/R2_abund_plants.csv")
write_csv(Anov_results_occur, "Results/Anova_occur_plants.csv")
write_csv(Anov_results_abund, "Results/Anova_abund_plants.csv")
write_csv(em_res_occur, "Results/em_res_occur_plants.csv")
write_csv(em_pair_occur, "Results/em_pair_occur_plants.csv")
write_csv(em_res_abund, "Results/em_res_abund_plants.csv")
write_csv(em_pair_abund, "Results/em_pair_abund_plants.csv")


# Vertebrates ----
#Analysis using occurrence data
mod_occur_500 <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_500 %>% filter(taxon_category == "Vertebrado"))

mod_occur_1k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_1k %>% filter(taxon_category == "Vertebrado"))

mod_occur_2k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_2k %>% filter(taxon_category == "Vertebrado"))

mod_occur_4k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_4k %>% filter(taxon_category == "Vertebrado"))

mod_occur_8k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_8k %>% filter(taxon_category == "Vertebrado"))


# R2

R2_occur_500 <- r.squaredGLMM(mod_occur_500)
R2_occur_1k <- r.squaredGLMM(mod_occur_1k)
R2_occur_2k <- r.squaredGLMM(mod_occur_2k)
R2_occur_4k <- r.squaredGLMM(mod_occur_4k)
R2_occur_8k <- r.squaredGLMM(mod_occur_8k)

# Anova
anov_occur_500 <- anova(mod_occur_500)
anov_occur_1k <- anova(mod_occur_1k)
anov_occur_2k <- anova(mod_occur_2k)
anov_occur_4k <- anova(mod_occur_4k)
anov_occur_8k <- anova(mod_occur_8k)

# emmeans
em_occur_500 <- emmeans(mod_occur_500, list(pairwise ~ cat), adjust = "tukey")
em_occur_1k <- emmeans(mod_occur_1k, list(pairwise ~ cat), adjust = "tukey")
em_occur_2k <- emmeans(mod_occur_2k, list(pairwise ~ cat), adjust = "tukey")
em_occur_4k <- emmeans(mod_occur_4k, list(pairwise ~ cat), adjust = "tukey")
em_occur_8k <- emmeans(mod_occur_8k, list(pairwise ~ cat), adjust = "tukey")



#Analysis using abundance data
mod_abund_500 <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_500 %>% filter(taxon_category == "Vertebrado"))

mod_abund_1k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_1k %>% filter(taxon_category == "Vertebrado"))

mod_abund_2k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_2k %>% filter(taxon_category == "Vertebrado"))

mod_abund_4k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_4k %>% filter(taxon_category == "Vertebrado"))

mod_abund_8k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_8k %>% filter(taxon_category == "Vertebrado"))


# R2

R2_abund_500 <- r.squaredGLMM(mod_abund_500)
R2_abund_1k <- r.squaredGLMM(mod_abund_1k)
R2_abund_2k <- r.squaredGLMM(mod_abund_2k)
R2_abund_4k <- r.squaredGLMM(mod_abund_4k)
R2_abund_8k <- r.squaredGLMM(mod_abund_8k)

# Anova
anov_abund_500 <- anova(mod_abund_500)
anov_abund_1k <- anova(mod_abund_1k)
anov_abund_2k <- anova(mod_abund_2k)
anov_abund_4k <- anova(mod_abund_4k)
anov_abund_8k <- anova(mod_abund_8k)

# emmeans
em_abund_500 <- emmeans(mod_abund_500, list(pairwise ~ cat), adjust = "tukey")
em_abund_1k <- emmeans(mod_abund_1k, list(pairwise ~ cat), adjust = "tukey")
em_abund_2k <- emmeans(mod_abund_2k, list(pairwise ~ cat), adjust = "tukey")
em_abund_4k <- emmeans(mod_abund_4k, list(pairwise ~ cat), adjust = "tukey")
em_abund_8k <- emmeans(mod_abund_8k, list(pairwise ~ cat), adjust = "tukey")


# Final tables ----
# Models
mod_occur <- list(mod_occur_500, mod_occur_1k, mod_occur_2k, mod_occur_4k, mod_occur_8k)
names(mod_occur) <- nome

mod_abund <- list(mod_abund_500, mod_abund_1k, mod_abund_2k, mod_abund_4k, mod_abund_8k)
names(mod_abund) <- nome

#R2
R2_occur <- as_tibble(rbind(R2_occur_500, R2_occur_1k, R2_occur_2k, R2_occur_4k, R2_occur_8k))
R2_occur <- R2_occur %>% 
  mutate(scale = nome)


R2_abund <- as_tibble(rbind(R2_abund_500, R2_abund_1k, R2_abund_2k, R2_abund_4k, R2_abund_8k))
R2_abund <- R2_abund %>% 
  mutate(scale = nome)

#Anova
anova_occur <- list(anov_occur_500, anov_occur_1k, anov_occur_2k, anov_occur_4k, anov_occur_8k)
names(anova_occur) <- nome


Anov_results_occur <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_occur[i,"scale"] <- i
  Anov_results_occur[i, "F_cat"] <- anova_occur[[i]]$`F value`[1]
  Anov_results_occur[i, "F_dist"] <- anova_occur[[i]]$`F value`[2]
  Anov_results_occur[i, "P_cat"] <- anova_occur[[i]]$`Pr(>F)`[1]
  Anov_results_occur[i, "P_dist"] <- anova_occur[[i]]$`Pr(>F)`[2]
}
Anov_results_occur 



anova_abund <- list(anov_abund_500, anov_abund_1k, anov_abund_2k, anov_abund_4k, anov_abund_8k)
names(anova_abund) <- nome


Anov_results_abund <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_abund[i,"scale"] <- i
  Anov_results_abund[i, "F_cat"] <- anova_abund[[i]]$`F value`[1]
  Anov_results_abund[i, "F_dist"] <- anova_abund[[i]]$`F value`[2]
  Anov_results_abund[i, "P_cat"] <- anova_abund[[i]]$`Pr(>F)`[1]
  Anov_results_abund[i, "P_dist"] <- anova_abund[[i]]$`Pr(>F)`[2]
}
Anov_results_abund 


# Emmeans

em_res_occur <- list(as_tibble(em_occur_500$`emmeans of cat`), as_tibble(em_occur_1k$`emmeans of cat`), as_tibble(em_occur_2k$`emmeans of cat`), as_tibble(em_occur_4k$`emmeans of cat`), as_tibble(em_occur_8k$`emmeans of cat`))

for(i in 1:length(em_res_occur)) {
  #i = 2
  em_res_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_occur <- as_tibble(rbindlist(em_res_occur))


em_pair_occur <- list(as_tibble(em_occur_500$`pairwise differences of cat`),
                      as_tibble(em_occur_1k$`pairwise differences of cat`),
                      as_tibble(em_occur_2k$`pairwise differences of cat`),
                      as_tibble(em_occur_4k$`pairwise differences of cat`),
                      as_tibble(em_occur_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_occur)) {
  #i = 2
  em_pair_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_occur <- as_tibble(rbindlist(em_pair_occur))




em_res_abund <- list(as_tibble(em_abund_500$`emmeans of cat`), as_tibble(em_abund_1k$`emmeans of cat`), as_tibble(em_abund_2k$`emmeans of cat`), as_tibble(em_abund_4k$`emmeans of cat`), as_tibble(em_abund_8k$`emmeans of cat`))

for(i in 1:length(em_res_abund)) {
  #i = 2
  em_res_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_abund <- as_tibble(rbindlist(em_res_abund))


em_pair_abund <- list(as_tibble(em_abund_500$`pairwise differences of cat`),
                      as_tibble(em_abund_1k$`pairwise differences of cat`),
                      as_tibble(em_abund_2k$`pairwise differences of cat`),
                      as_tibble(em_abund_4k$`pairwise differences of cat`),
                      as_tibble(em_abund_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_abund)) {
  #i = 2
  em_pair_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_abund <- as_tibble(rbindlist(em_pair_abund))


# saving results ----
saveRDS(mod_occur, "Results/mod_occur_Vertebrates.RData")
saveRDS(mod_abund, "Results/mod_abund_Vertebrates.RData")
write_csv(R2_occur, "Results/R2_occur_Vertebrates.csv")
write_csv(R2_abund, "Results/R2_abund_Vertebrates.csv")
write_csv(Anov_results_occur, "Results/Anova_occur_Vertebrates.csv")
write_csv(Anov_results_abund, "Results/Anova_abund_Vertebrates.csv")
write_csv(em_res_occur, "Results/em_res_occur_Vertebrates.csv")
write_csv(em_pair_occur, "Results/em_pair_occur_Vertebrates.csv")
write_csv(em_res_abund, "Results/em_res_abund_Vertebrates.csv")
write_csv(em_pair_abund, "Results/em_pair_abund_Vertebrates.csv")




# Invertebrates ----
#Analysis using occurrence data
mod_occur_500 <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_500 %>% filter(taxon_category == "Invertebrado"))

mod_occur_1k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_1k %>% filter(taxon_category == "Invertebrado"))

mod_occur_2k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_2k %>% filter(taxon_category == "Invertebrado"))

mod_occur_4k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_4k %>% filter(taxon_category == "Invertebrado"))

mod_occur_8k <- lmer(RC_occur ~ cat  + dist + (1|study_id), data = occur_tx_8k %>% filter(taxon_category == "Invertebrado"))


# R2

R2_occur_500 <- r.squaredGLMM(mod_occur_500)
R2_occur_1k <- r.squaredGLMM(mod_occur_1k)
R2_occur_2k <- r.squaredGLMM(mod_occur_2k)
R2_occur_4k <- r.squaredGLMM(mod_occur_4k)
R2_occur_8k <- r.squaredGLMM(mod_occur_8k)

# Anova
anov_occur_500 <- anova(mod_occur_500)
anov_occur_1k <- anova(mod_occur_1k)
anov_occur_2k <- anova(mod_occur_2k)
anov_occur_4k <- anova(mod_occur_4k)
anov_occur_8k <- anova(mod_occur_8k)

# emmeans
em_occur_500 <- emmeans(mod_occur_500, list(pairwise ~ cat), adjust = "tukey")
em_occur_1k <- emmeans(mod_occur_1k, list(pairwise ~ cat), adjust = "tukey")
em_occur_2k <- emmeans(mod_occur_2k, list(pairwise ~ cat), adjust = "tukey")
em_occur_4k <- emmeans(mod_occur_4k, list(pairwise ~ cat), adjust = "tukey")
em_occur_8k <- emmeans(mod_occur_8k, list(pairwise ~ cat), adjust = "tukey")



#Analysis using abundance data
mod_abund_500 <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_500 %>% filter(taxon_category == "Invertebrado"))

mod_abund_1k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_1k %>% filter(taxon_category == "Invertebrado"))

mod_abund_2k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_2k %>% filter(taxon_category == "Invertebrado"))

mod_abund_4k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_4k %>% filter(taxon_category == "Invertebrado"))

mod_abund_8k <- lmer(RC_abund ~ cat  + dist + (1|study_id), data = abund_tx_8k %>% filter(taxon_category == "Invertebrado"))


# R2

R2_abund_500 <- r.squaredGLMM(mod_abund_500)
R2_abund_1k <- r.squaredGLMM(mod_abund_1k)
R2_abund_2k <- r.squaredGLMM(mod_abund_2k)
R2_abund_4k <- r.squaredGLMM(mod_abund_4k)
R2_abund_8k <- r.squaredGLMM(mod_abund_8k)

# Anova
anov_abund_500 <- anova(mod_abund_500)
anov_abund_1k <- anova(mod_abund_1k)
anov_abund_2k <- anova(mod_abund_2k)
anov_abund_4k <- anova(mod_abund_4k)
anov_abund_8k <- anova(mod_abund_8k)

# emmeans
em_abund_500 <- emmeans(mod_abund_500, list(pairwise ~ cat), adjust = "tukey")
em_abund_1k <- emmeans(mod_abund_1k, list(pairwise ~ cat), adjust = "tukey")
em_abund_2k <- emmeans(mod_abund_2k, list(pairwise ~ cat), adjust = "tukey")
em_abund_4k <- emmeans(mod_abund_4k, list(pairwise ~ cat), adjust = "tukey")
em_abund_8k <- emmeans(mod_abund_8k, list(pairwise ~ cat), adjust = "tukey")


# Final tables ----
# Models
mod_occur <- list(mod_occur_500, mod_occur_1k, mod_occur_2k, mod_occur_4k, mod_occur_8k)
names(mod_occur) <- nome

mod_abund <- list(mod_abund_500, mod_abund_1k, mod_abund_2k, mod_abund_4k, mod_abund_8k)
names(mod_abund) <- nome

#R2
R2_occur <- as_tibble(rbind(R2_occur_500, R2_occur_1k, R2_occur_2k, R2_occur_4k, R2_occur_8k))
R2_occur <- R2_occur %>% 
  mutate(scale = nome)


R2_abund <- as_tibble(rbind(R2_abund_500, R2_abund_1k, R2_abund_2k, R2_abund_4k, R2_abund_8k))
R2_abund <- R2_abund %>% 
  mutate(scale = nome)

#Anova
anova_occur <- list(anov_occur_500, anov_occur_1k, anov_occur_2k, anov_occur_4k, anov_occur_8k)
names(anova_occur) <- nome


Anov_results_occur <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_occur[i,"scale"] <- i
  Anov_results_occur[i, "F_cat"] <- anova_occur[[i]]$`F value`[1]
  Anov_results_occur[i, "F_dist"] <- anova_occur[[i]]$`F value`[2]
  Anov_results_occur[i, "P_cat"] <- anova_occur[[i]]$`Pr(>F)`[1]
  Anov_results_occur[i, "P_dist"] <- anova_occur[[i]]$`Pr(>F)`[2]
}
Anov_results_occur 



anova_abund <- list(anov_abund_500, anov_abund_1k, anov_abund_2k, anov_abund_4k, anov_abund_8k)
names(anova_abund) <- nome


Anov_results_abund <- data.frame(F_cat = as.double(), F_dist = as.double(), P_cat = as.double(), P_dist = as.double(), scale = as.character(), stringsAsFactors=FALSE) 

for(i in nome) {
  #i = nome[1]
  
  Anov_results_abund[i,"scale"] <- i
  Anov_results_abund[i, "F_cat"] <- anova_abund[[i]]$`F value`[1]
  Anov_results_abund[i, "F_dist"] <- anova_abund[[i]]$`F value`[2]
  Anov_results_abund[i, "P_cat"] <- anova_abund[[i]]$`Pr(>F)`[1]
  Anov_results_abund[i, "P_dist"] <- anova_abund[[i]]$`Pr(>F)`[2]
}
Anov_results_abund 


# Emmeans

em_res_occur <- list(as_tibble(em_occur_500$`emmeans of cat`), as_tibble(em_occur_1k$`emmeans of cat`), as_tibble(em_occur_2k$`emmeans of cat`), as_tibble(em_occur_4k$`emmeans of cat`), as_tibble(em_occur_8k$`emmeans of cat`))

for(i in 1:length(em_res_occur)) {
  #i = 2
  em_res_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_occur <- as_tibble(rbindlist(em_res_occur))


em_pair_occur <- list(as_tibble(em_occur_500$`pairwise differences of cat`),
                      as_tibble(em_occur_1k$`pairwise differences of cat`),
                      as_tibble(em_occur_2k$`pairwise differences of cat`),
                      as_tibble(em_occur_4k$`pairwise differences of cat`),
                      as_tibble(em_occur_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_occur)) {
  #i = 2
  em_pair_occur[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_occur <- as_tibble(rbindlist(em_pair_occur))




em_res_abund <- list(as_tibble(em_abund_500$`emmeans of cat`), as_tibble(em_abund_1k$`emmeans of cat`), as_tibble(em_abund_2k$`emmeans of cat`), as_tibble(em_abund_4k$`emmeans of cat`), as_tibble(em_abund_8k$`emmeans of cat`))

for(i in 1:length(em_res_abund)) {
  #i = 2
  em_res_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_res_abund <- as_tibble(rbindlist(em_res_abund))


em_pair_abund <- list(as_tibble(em_abund_500$`pairwise differences of cat`),
                      as_tibble(em_abund_1k$`pairwise differences of cat`),
                      as_tibble(em_abund_2k$`pairwise differences of cat`),
                      as_tibble(em_abund_4k$`pairwise differences of cat`),
                      as_tibble(em_abund_8k$`pairwise differences of cat`))

for(i in 1:length(em_pair_abund)) {
  #i = 2
  em_pair_abund[[i]]$"scale" <- rep(nome[i], 3)
}

em_pair_abund <- as_tibble(rbindlist(em_pair_abund))


# saving results ----
saveRDS(mod_occur, "Results/mod_occur_Invet.RData")
saveRDS(mod_abund, "Results/mod_abund_Invet.RData")
write_csv(R2_occur, "Results/R2_occur_Invet.csv")
write_csv(R2_abund, "Results/R2_abund_Invet.csv")
write_csv(Anov_results_occur, "Results/Anova_occur_Invet.csv")
write_csv(Anov_results_abund, "Results/Anova_abund_Invet.csv")
write_csv(em_res_occur, "Results/em_res_occur_Invet.csv")
write_csv(em_pair_occur, "Results/em_pair_occur_Invet.csv")
write_csv(em_res_abund, "Results/em_res_abund_Invet.csv")
write_csv(em_pair_abund, "Results/em_pair_abund_Invet.csv")



#rm(list=ls())

