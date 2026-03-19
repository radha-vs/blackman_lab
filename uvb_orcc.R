##Incorporating UVB data into our environmental dataset

library(raster)
library(terra)
library(tidyverse)
library(dplyr)
library(viridis)
library(ggplot2)

#Load environmental & UVB data 
ORCC_env <- read.csv("/users/radha/Documents/Blackman Lab/data/Environmental Data/bcbcm327_7099_PCG.csv")
CCGP_UVB <- read.csv("/users/radha/Documents/Blackman Lab/data/Environmental Data/ClimateNA_data_puv_12292021.csv")
UV_ORCC <- readRDS("/users/radha/Documents/Blackman Lab/Scripts/ORCC_UV_full.rds")
UVB_data <- raster("/users/radha/Downloads/56459_UVB1_Annual_Mean_UV-B.asc")

print(UVB_data)

orcc_pops<- ORCC_env %>%
  filter(POP %in% UV_ORCC$Pop) %>%
  rename(Pop = POP) %>%
  dplyr::select(Pop, Long, Lat) %>%
  distinct(Pop, .keep_all = TRUE)

#uvb_points <- rasterToPoints(UVB_data)
#uvb_df <- as.data.frame(uvb_points)

#getValues(UVB_data)

UVB1 <- terra::extract(UVB_data, orcc_pops[, c("Long", "Lat")])
uvb_pops <- data.frame(orcc_pops, UVB1)

#add in UVB data to existing environmental data
ORCC_env %>%
  distinct(POP, .keep_all = TRUE) %>%
  rename(Pop = POP) %>%
  filter(Pop %in% UV_ORCC$Pop) -> env2

#CCGP_UVB %>%
#  mutate(Pop = Population, 
#         Long = long, 
#         UVB1 = annual_UVB1) %>%
#  filter(Pop %in% UV_ORCC$Pop) %>%
#  dplyr::select(Pop, Lat, Long, UVB1)-> ccgp2

#all_UVB <- rbind(ccgp2, uvb_pops) %>%
#  drop_na() %>%
#  arrange(Pop)

inner_join(env2, uvb_pops, by = c("Pop", "Lat", "Long")) -> env_full

#cleaning up environment a bit
#rm(missing_pops, UVB_data, uvb_data, uvb_points, uvb_pops)

#Load UV data
UV_ORCC <- readRDS("/users/radha/Documents/Blackman Lab/Scripts/ORCC_UV_full.rds")

#log transform UV data
UV_cor_log <- UV_ORCC %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, Pop, ID) %>%
  mutate(Ventral = log(Ventral), 
         R_Lateral = log(R_Lateral),
         L_Lateral = log(L_Lateral),
         R_Dorsal = log(R_Dorsal),
         L_Dorsal = log(L_Dorsal),
         Ventral_Throat = log(Ventral_Throat)) 

UV_cor_log %>%
  left_join(env_full, by = "Pop") %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
                Elevation, group1, group2, group3, group4, group5, group6, UVB1) -> UV_env_log

corr_test <- function(x, y) {
  test <- cor.test(x, y, method = "pearson")
  list(correlation = test$estimate, p_value = test$p.value)
}

# Get all pairwise correlations and p-values
corr_results_log <- expand.grid(Var1 = names(UV_env_log), Var2 = names(UV_env_log)) %>%
  filter(Var1 != Var2) %>%
  mutate(result = map2(Var1, Var2, ~ corr_test(UV_env_log[[.x]], UV_env_log[[.y]])),
         correlation = map_dbl(result, "correlation"),
         p_value = map_dbl(result, "p_value")) %>%
  #p.adjust(p_value, method = "bonferroni") %>%
  dplyr::select(-result) %>%
  mutate(p_adj = p.adjust(p_value, method = "bonferroni"))

#corr_melt <- corr_results %>%
#  pivot_longer(cols = c(correlation, p_value), names_to = "Metric", values_to = "value")

ggplot(data = corr_results_log, aes(x = Var1, y = Var2, fill = p_value < 0.05)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, digits = 3)), color = "darkgreen", size = 3, nudge_y = -0.2) + 
  geom_text(aes(label = round(p_value, digits = 3)), color = "blue", size = 3, nudge_y = 0.2) +
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "gray"), 
                    name = "Significance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

UV_env_log %>%
  pivot_longer(cols = 1:6, names_to = "Lobes", values_to = "Value") %>%
  ggplot(aes(x = Value, y = UVB1, color = Lobes)) + 
  geom_point() +
  geom_smooth(method = lm) + 
  labs(title = "UVB correlation with all lobes")

write.csv(env_full, "UVB_env_ORCC.csv")
