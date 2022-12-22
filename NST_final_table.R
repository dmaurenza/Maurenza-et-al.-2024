library(tidyverse)
library(data.table)

# Final table ----
  # For occurreance data
    #equal effort


occur_500 <- readRDS("NST/NST_occur_500.Rdata")
occur_1k <- readRDS("NST/NST_occur_1k.Rdata")
occur_2k <- readRDS("NST/NST_occur_2k.Rdata")
occur_4k <- readRDS("NST/NST_occur_4k.Rdata")
occur_8k <- readRDS("NST/NST_occur_8k.Rdata")
occur_list <- list(occur_500, occur_1k, occur_2k, occur_4k, occur_8k)

nome <- c("500", "1k", "2k", "4k", "8k")
names(occur_list) <- nome
study_id_vector <- occur_list[["500"]][,"study_id"]


occur_list <- lapply(occur_list, select, !c(study_id))
occur_list <- lapply(occur_list, mutate_all, function(x) ifelse(is.nan(x), NA, x))
occur_list <- lapply(occur_list, mutate, study_id = study_id_vector)
occur_list <- lapply(occur_list, as_tibble)


# For abundance data

abund_500 <- readRDS("NST/NST_abund_500.Rdata")
abund_1k <- readRDS("NST/NST_abund_1k.Rdata")
abund_2k <- readRDS("NST/NST_abund_2k.Rdata")
abund_4k <- readRDS("NST/NST_abund_4k.Rdata")
abund_8k <- readRDS("NST/NST_abund_8k.Rdata")

abund_list <- list(abund_500, abund_1k, abund_2k, abund_4k, abund_8k)

nome <- c("500", "1k", "2k", "4k", "8k")
names(abund_list) <- nome
study_id_vector <- abund_list[["500"]][,"study_id"]

abund_list <- lapply(abund_list, select, !c(study_id))
abund_list <- lapply(abund_list, mutate_all, function(x) ifelse(is.nan(x), NA, x))
abund_list <- lapply(abund_list, mutate, study_id = study_id_vector)
abund_list <- lapply(abund_list, as_tibble)

saveRDS(occur_list, "Data/NST_Occur_out.RData")
saveRDS(abund_list, "Data/NST_Abund_out.RData")


#rm(list=ls())
