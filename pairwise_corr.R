#Examining pairwise correlation in quantitative data 

#load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(reshape2)
library(purrr)
library(viridis)
library(ggpubr)

#Read in UV data 
UV_ORCC <- readRDS("/users/radha/Documents/Blackman Lab/Scripts/ORCC_UV_full.rds")
#Grouped environmental data
ORCC_env <- read.csv("/users/radha/Documents/Blackman Lab/data/bcbcm327_7099_PCG.csv")
#Ungrouped environmental data
ungrouped_env <- 
  read.csv("/users/radha/Documents/Blackman Lab/data/Environmental Data/USE_env-330_bioclim_elevation_230411_sorted.csv")

UV_cor <- UV_ORCC %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat) %>%
  mutate(Ventral = (Ventral), 
         R_Lateral = (R_Lateral),
         L_Lateral = (L_Lateral),
         R_Dorsal = (R_Dorsal),
         L_Dorsal = (L_Dorsal),
         Ventral_Throat = (Ventral_Throat)) 

ORCC_env %>%
  distinct(POP, .keep_all = TRUE) -> env2

UV_ORCC %>%
  left_join(env2, by = c("Pop" = "POP"), keep= FALSE) %>%
  #filter(Pop != c("DKR", "YVO")) %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
                Elevation, group1, group2, group3, group4, group5, group6, Pop) -> UV_env

#note: pca_out is from exploratory_PCA.R
UV_env %>%
  left_join(pca_out, by = "ID") %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
                Elevation, group1, group2, group3, group4, group5, group6, PC1, PC2) -> UV_env_full

#calculation of pearson correlation coefficient & p-value using a function
corr_test <- function(x, y) {
  test <- cor.test(x, y, method = "pearson")
  list(correlation = test$estimate, p_value = test$p.value)
}

# Get all pairwise correlations and p-values
corr_results <- expand.grid(Var1 = names(UV_env_full), Var2 = names(UV_env_full)) %>%
  mutate(result = map2(Var1, Var2, ~ corr_test(UV_env_full[[.x]], UV_env_full[[.y]])),
         correlation = map_dbl(result, "correlation"),
         p_value = map_dbl(result, "p_value")) %>%
  select(-result)

corr_melt <- corr_results %>%
  pivot_longer(cols = c(correlation, p_value), names_to = "Metric", values_to = "value")

ggplot(data = corr_results, aes(x = Var1, y = Var2, fill = p_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, digits = 3)), color = "darkgreen", size = 3, nudge_y = -0.2) + 
  geom_text(aes(label = round(p_value, digits = 3)), color = "blue", size = 3, nudge_y = 0.2) +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "p-value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("UV_quant_heatmap.jpeg")

#Same thing but with log transformed data: 
UV_cor_log <- UV_ORCC %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, Pop, ID) %>%
  mutate(Ventral = log(Ventral), 
         R_Lateral = log(R_Lateral),
         L_Lateral = log(L_Lateral),
         R_Dorsal = log(R_Dorsal),
         L_Dorsal = log(L_Dorsal),
         Ventral_Throat = log(Ventral_Throat)) 

ORCC_env %>%
  distinct(POP, .keep_all = TRUE) -> env2

UV_cor_log %>%
  left_join(env2, by = c("Pop" = "POP"), keep= FALSE) %>%
  #filter(Pop != c("DKR", "YVO")) %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
                Elevation, group1, group2, group3, group4, group5, group6, ID) -> UV_env_log

#note: pca_out is from exploratory_PCA.R
UV_env_log %>%
  left_join(pca_out, by = "ID") %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
                Elevation, group1, group2, group3, group4, group5, group6, PC1, PC2) -> UV_env_full_log


#calculation of pearson correlation coefficient & p-value using a function
corr_test <- function(x, y) {
  test <- cor.test(x, y, method = "pearson")
  list(correlation = test$estimate, p_value = test$p.value)
}

# Get all pairwise correlations and p-values
corr_results_log <- expand.grid(Var1 = names(UV_env_full_log), Var2 = names(UV_env_full_log)) %>%
  filter(Var1 != Var2) %>%
  mutate(result = map2(Var1, Var2, ~ corr_test(UV_env_full_log[[.x]], UV_env_full_log[[.y]])),
         correlation = map_dbl(result, "correlation"),
         p_value = map_dbl(result, "p_value")) %>%
  #p.adjust(p_value, method = "bonferroni") %>%
  select(-result) %>%
  mutate(p_adj = p.adjust(p_value, method = "bonferroni"))

#corr_melt <- corr_results %>%
#  pivot_longer(cols = c(correlation, p_value), names_to = "Metric", values_to = "value")

ggplot(data = corr_results_log, aes(x = Var1, y = Var2, fill = p_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, digits = 3)), color = "darkgreen", size = 3, nudge_y = -0.2) + 
  geom_text(aes(label = round(p_value, digits = 3)), color = "blue", size = 3, nudge_y = 0.2) +
  scale_fill_viridis(option = "magma",
                     name = "p-value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

UV_env_log %>%
  ggplot(aes(x = Ventral, y = group6)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  labs(title = "Ventral UV Correlation with Precipitation Variables")

UV_env_log %>%
  pivot_longer(cols = 7:13, names_to = "Groups", values_to = "Value") %>%
  ggplot(aes(x = Ventral, y = Value, color = Groups)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  labs(title = "Ventral UV Correlation with All Groups")

UV_env_log %>%
  pivot_longer(cols = 1:6, names_to = "Variables", values_to = "Value") %>%
  ggplot(aes(x = Elevation, y = Value, color = Variables)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  labs(title = "All Lobes correlations with Elevation")

#Same thing but with 'total UV':

UV_ORCC %>%
  mutate(All = Ventral + R_Lateral + L_Lateral + R_Dorsal + L_Dorsal + Ventral_Throat) %>%
  select(All, Pop) %>%
  mutate(All = log(All)) %>%
  left_join(env2, by = c("Pop" = "POP"), keep= FALSE) %>%
  #filter(Pop != c("DKR", "YVO")) %>%
  select(All, Elevation, group1, group2, group3, group4, group5, group6) -> UV_all_env

#note: pca_out is from exploratory_PCA.R
#V_env %>%
#  left_join(pca_out, by = "ID") %>%
#  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
 #               Elevation, group1, group2, group3, group4, group5, group6, PC1, PC2) -> UV_env_full

#calculation of pearson correlation coefficient & p-value using a function
corr_test <- function(x, y) {
  test <- cor.test(x, y, method = "pearson")
  list(correlation = test$estimate, p_value = test$p.value)
}

# Get all pairwise correlations and p-values
corr_results_all <- expand.grid(Var1 = names(UV_all_env), Var2 = names(UV_all_env)) %>%
  mutate(result = map2(Var1, Var2, ~ corr_test(UV_all_env[[.x]], UV_all_env[[.y]])),
         correlation = map_dbl(result, "correlation"),
         p_value = map_dbl(result, "p_value")) %>%
  select(-result)

ggplot(data = corr_results_all, aes(x = Var1, y = Var2, fill = p_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, digits = 3)), color = "darkgreen", size = 3, nudge_y = -0.2) + 
  geom_text(aes(label = round(p_value, digits = 3)), color = "blue", size = 3, nudge_y = 0.2) +
  scale_fill_viridis(option = "magma", direction = -1,
                     name = "p-value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



UV_env_full %>%
  pivot_longer(cols = 1:6, names_to = "Variables", values_to = "Value") %>%
 # filter(Variables == "Ventral") %>%
  ggplot(aes(x = Value, y = Elevation, color = Variables)) + 
  geom_jitter() + 
  geom_smooth(method = 'lm')




#Plotting without p-values, just correlation coeff
corr_full <- cor(UV_env_full, use = "complete.obs", method = "pearson")

corr_full_melt <- melt(corr_full)

ggplot(data = corr_full_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = round(value,3)), 
            color = "black", size = 4) +
  scale_fill_viridis(option="plasma")

ggsave("UV_env_heatmap.jpeg")

ggplot(data = UV_env, aes(x = log(Ventral), y = group6, color = Pop)) + 
         geom_point()


#plotting just group 6 bioclim variables (precipitation; initial heatmap shows weak 
#positive correlation with Ventral lobe)

ungrouped_env %>%
  distinct(POP, .keep_all = TRUE) -> env3

UV_ORCC %>%
  left_join(env3, by = c("Pop" = "POP"), keep= FALSE) %>%
  #filter(Pop != c("DKR", "YVO")) %>%
  dplyr::select(Ventral, bio13, bio16, bio19, bio12) -> UV_env_3

corr_6 <- cor(UV_env_3, use = "complete.obs", method = "pearson")

corr_6_melt <- melt(corr_6)

ggplot(data = corr_6_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = round(value,3)), 
            color = "black", size = 4) +
  scale_fill_viridis(option="plasma")

#Heatmpa isn't super useful, yields very similar correlations for all variables.

UV_env %>%
  ggplot(aes(x = Ventral, y = group6)) + 
  geom_point()

UV_env_3 %>%
  pivot_longer(cols = starts_with("bio"), names_to = "Group 6 Vars", values_to = "Value") %>%
  ggplot(aes(x = Ventral, y = Value, color = `Group 6 Vars`)) + 
  geom_point()

#group 1 and 2 bioclim variables with Ventral lobe: 

UV_ORCC %>%
  left_join(env3, by = c("Pop" = "POP"), keep= FALSE) %>%
  #filter(Pop != c("DKR", "YVO")) %>%
  dplyr::select(Ventral, bio18, bio14, bio17, bio9, bio10, bio5, bio11, bio8, bio6,
         bio1, bio15 ) -> UV_env_4

corr_1_2 <- cor(UV_env_4, use = "complete.obs", method = "pearson")

corr_1_2_melt <- melt(corr_1_2)

ggplot(data = corr_1_2_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = round(value,3)), 
            color = "black", size = 4) +
  scale_fill_viridis(option="plasma")

UV_env_4 %>%
  dplyr::select(bio1, Ventral) %>%
  pivot_longer(cols = starts_with("bio"), names_to = "Variables", values_to = "Value") %>%
  ggplot(aes(x = Ventral, y = Value, color = Variables)) + 
  geom_point()




