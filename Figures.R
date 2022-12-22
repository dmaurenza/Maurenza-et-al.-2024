# Figure Distance for Supp Mat
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(measurements)
library(data.table)
setwd("/Chapter_2/Database/")

# Main Text
occur <- readRDS("Data/occur_final_out.RData")
scale <- read_csv("Results/R2_occur.csv")
em <- read_csv("Results/em_res_occur.csv") %>% 
  filter(scale == "500")
pair <- read_csv("Results/em_pair_occur.csv")%>% 
  filter(scale == "500")
occur_500 <- occur[["500"]]

res <- occur_500 %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_occur), n = length(RC_occur), standard_deviation = sd(RC_occur), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error) %>% 
  select(mean_value, lower_bound, upper_bound)

res <- res %>%
  mutate(significance = c("a", "b", "b")) %>%
  mutate(cat = c("High", "Intermediate", "Low"))

fig_occur <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-1, 1)+
  xlab("")+
  ylab(expression(β[RC_occur]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -2.3, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_occur

# Abundance
scale <- read_csv("Results/R2_abund.csv")
abund <- readRDS("Data/abund_final_out.RData")
abund_500 <- abund[["4k"]]

res <- abund_500 %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_abund), n = length(RC_abund), standard_deviation = sd(RC_abund), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error)%>% 
  select(mean_value, lower_bound, upper_bound)
res <- res %>% 
  mutate(significance = c("a", "b", "b")) %>% 
  mutate(cat = c("High", "Intermediate", "Low"))

fig_abund <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-1, 1)+
  xlab("")+
  ylab(expression(β[RC_abund]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -2.3, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_abund


fig <- ggarrange(fig_occur, fig_abund, labels = c("(a)", "(b)"), label.y = 0.88)
ggsave("Figures/Fig_main.jpeg", fig, height = 4, width = 8)





# Taxonomic groups ----

cat <- read_csv("Data/sites_categories.csv")
cat <- cat %>% 
  dplyr::select(id_unique, starts_with("cat_"))

datafull <- read_csv("Data/Pre_processed.csv") 

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

taxon <- lapply(study_list, select, c("id_unique", "id_site", "study_id", "taxon_category"), starts_with("cat_"))
taxon <- rbindlist(taxon) %>% 
  distinct(taxon_category, study_id)

# For Vertebrates ----
scale <- read_csv("Results/R2_occur_Vertebrates.csv")

pair <- read_csv("Results/em_pair_occur_Vertebrates.csv")%>% 
  filter(scale == "2k")

occur_2k <- occur[["2k"]]
vert <- taxon %>% 
  filter(taxon_category == "Vertebrado") %>% 
  pull(study_id)

occur_2k <- occur_2k %>% 
  filter(study_id %in% vert)
res <- occur_2k %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_occur), n = length(RC_occur), standard_deviation = sd(RC_occur), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error)%>% 
  select(mean_value, lower_bound, upper_bound)

res <- res %>%
  mutate(significance = c("ab", "a", "b")) %>%
  mutate(cat = c("High", "Intermediate", "Low"))

fig_occur_vert <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-1, 1)+
  xlab("")+
  ylab(expression(β[RC_occur]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -2, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_occur_vert

# Abundance
scale <- read_csv("Results/R2_abund_Vertebrates.csv")

em <- read_csv("Results/em_res_abund_Vertebrates.csv") %>% 
  filter(scale == "500")
pair <- read_csv("Results/em_pair_abund_Vertebrates.csv")%>% 
  filter(scale == "500")

abund_500 <- abund[["500"]]
vert <- taxon %>% 
  filter(taxon_category == "Vertebrado") %>% 
  pull(study_id)

abund_500 <- abund_500 %>% 
  filter(study_id %in% vert)
res <- abund_500 %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_abund), n = length(RC_abund), standard_deviation = sd(RC_abund), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error)%>% 
  select(mean_value, lower_bound, upper_bound)

res <- res %>% 
  mutate(significance = c("a", "a", "b")) %>% 
  mutate(cat = c("High", "Intermediate", "Low"))

fig_abund_vert <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-1, 1.2)+
  xlab("")+
  ylab(expression(β[RC_abund]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -2, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_abund_vert


# For Plants ----
scale <- read_csv("Results/R2_occur_plants.csv")

pair <- read_csv("Results/em_pair_occur_plants.csv")%>% 
  filter(scale == "4k")
occur_4k <- occur[["4k"]]
plant <- taxon %>% 
  filter(taxon_category == "Planta") %>% 
  pull(study_id)

occur_4k <- occur_4k %>% 
  filter(study_id %in% plant)
res <- occur_4k %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_occur), n = length(RC_occur), standard_deviation = sd(RC_occur), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error)%>% 
  select(mean_value, lower_bound, upper_bound)

res <- res %>% 
  mutate(significance = c("ab", "a", "b")) %>% 
  mutate(cat = c("High", "Intermediate", "Low"))

fig_occur_plant <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-1, 1)+
  xlab("")+
  ylab(expression(β[RC_occur]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -2, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_occur_plant

# Abundance
scale <- read_csv("Results/R2_abund_plants.csv")

pair <- read_csv("Results/em_pair_abund_plants.csv")%>% 
  filter(scale == "4k")

abund_4k <- abund[["4k"]]

abund_4k <- abund_4k %>% 
  filter(study_id %in% plant)
res <- abund_4k %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_abund), n = length(RC_abund), standard_deviation = sd(RC_abund), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error)%>% 
  select(mean_value, lower_bound, upper_bound)

res <- res %>% 
  mutate(significance = c("a", "a", "a")) %>% 
  mutate(cat = c("High", "Intermediate", "Low"))

fig_abund_plant <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-1, 1.6)+
  xlab("")+
  ylab(expression(β[RC_abund]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -2.5, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_abund_plant


# For Invertebrates ----
scale <- read_csv("Results/R2_occur_Invet.csv")

pair <- read_csv("Results/em_pair_occur_Invet.csv")%>% 
  filter(scale == "4k")

occur_4k <- occur[["4k"]]
invert <- taxon %>% 
  filter(taxon_category == "Invertebrado") %>% 
  pull(study_id)

occur_4k <- occur_4k %>% 
  filter(study_id %in% invert)
res <- occur_4k %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_occur), n = length(RC_occur), standard_deviation = sd(RC_occur), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error)%>% 
  select(mean_value, lower_bound, upper_bound)


res <- res %>% 
  mutate(significance = c("a", "a", "a")) %>% 
  mutate(cat = c("High", "Intermediate", "Low"))

fig_occur_invert <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-1.8, 1.5)+
  xlab("")+
  ylab(expression(β[RC_occur]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -4.5, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_occur_invert

# Abundance
scale <- read_csv("Results/R2_abund_Invet.csv")

pair <- read_csv("Results/em_pair_abund_Invet.csv")%>% 
  filter(scale == "4k")

abund_4k <- abund[["4k"]]

abund_4k <- abund_4k %>% 
  filter(study_id %in% invert)
res <- abund_4k %>% 
  group_by(cat) %>% 
  summarise(mean_value = mean(RC_abund), n = length(RC_abund), standard_deviation = sd(RC_abund), standard_error = standard_deviation / sqrt(n), alpha = 0.05, degrees_of_freedom = n - 1, t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F), margin_error = t_score * standard_error, lower_bound = mean_value - margin_error, upper_bound = mean_value + margin_error)%>% 
  select(mean_value, lower_bound, upper_bound)



res <- res %>% 
  mutate(significance = c("a", "a", "a")) %>% 
  mutate(cat = c("High", "Intermediate", "Low"))

fig_abund_invert <- ggplot(data = res, aes(x = cat, y = mean_value)) +
  geom_pointrange(aes(ymin=lower_bound, ymax=upper_bound), fatten = 5)+
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound), width = 0.5, show.legend = F)+
  ylim(-0.5, 2)+
  xlab("")+
  ylab(expression(β[RC_abund]))+
  labs(col = "")+
  geom_text(aes(label = significance), vjust = -3.7, size = 5)+
  theme(panel.background = element_rect(fill = "white", colour = "grey"),
        axis.line.x.top = element_line(color = "grey"),
        axis.line.x.bottom = element_line(color = "grey"),
        axis.line.y.left = element_line(color = "grey"),
        axis.line.y.right = element_line(color = "grey"),
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.title.y = element_text(size = 10, colour = "black"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none")
fig_abund_invert

fig <- ggarrange(fig_occur_vert, fig_abund_vert, fig_occur_plant, fig_abund_plant, fig_occur_invert, fig_abund_invert, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), label.y = 0.88, nrow = 3, ncol = 2)
fig
ggsave("Figures/Fig_taxon.jpeg", fig, height = 8, width = 8)



# Geographic distance ----

occur <- readRDS("Data/occur_final_out.RData")

for (i in names(occur)){
  #i = 1
  occur[[i]] <- occur[[i]] %>% 
    mutate(scale = i)
}

dist <- rbindlist(occur) %>% 
  mutate(dist_km = dist/1000) %>% 
  mutate(cat = if_else(cat == "Low", true = "Low", false = if_else(cat == "Inter", true = "Intermediate", false = "High")),
         scale = gsub("500", "500 m", scale),
         scale = gsub("1k", "1 km", scale),
         scale = gsub("2k", "2 km", scale),
         scale = gsub("4k", "4 km", scale),
         scale = gsub("8k", "8 km", scale))
  

geo_dist <- ggplot(data = dist)+
  geom_boxplot(aes(y = dist_km, x = cat))+
  scale_y_continuous(n.breaks = 10) +
  xlab("")+
  ylab("Median geographic distance (km)")+
  theme(
    panel.background = element_rect(fill = "white", colour = "grey"),
    axis.line.x.top = element_line(color = "grey"),
    axis.line.x.bottom = element_line(color = "grey"),
    axis.line.y.left = element_line(color = "grey"),
    axis.line.y.right = element_line(color = "grey"),
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"),
    axis.title.y = element_text(size = 10, colour = "black"),
  )+
  facet_wrap(~ scale)

ggsave("Figures/geo_dist.png", height = 4)







#rm(list=ls())
