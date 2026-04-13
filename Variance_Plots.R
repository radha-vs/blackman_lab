#Plot of variance of each population 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)

UV_ORCC <- readRDS("/users/radha/Documents/blackman_lab/ORCC_UV_full.rds")
#ORCC_env <- read.csv("/users/radha/Documents/Blackman Lab/data/bcbcm327_7099_PCG.csv")

UV_ORCC %>%
  group_by(Pop) %>%
  mutate(Lat = R_Lateral + L_Lateral, 
         Dor = R_Dorsal + L_Dorsal) %>%
  select(Pop, Ventral_Length, Ventral, R_Lateral, L_Lateral, Dorsal_Length, R_Dorsal, L_Dorsal, Ventral_Throat, Lat, Dor) %>%
  summarise(across(Ventral_Length:Ventral_Throat, 
            list(min = min, max = max, range = ~ max(.) - min(.)))) -> stats
UV_ORCC %>%
  group_by(Pop) %>%
  drop_na() %>%
  mutate(Lat = R_Lateral + L_Lateral, 
         Dor = R_Dorsal + L_Dorsal, 
         All = Ventral + R_Lateral + L_Lateral + R_Dorsal + L_Dorsal + Ventral_Throat) %>%
  select(Pop, Ventral_Length, Ventral, R_Lateral, L_Lateral, Dorsal_Length, R_Dorsal, L_Dorsal, Ventral_Throat, Lat, Dor, All) %>%
  summarise(across(Ventral_Length:All, 
                   list(var = var))) %>%
  ungroup ()%>%
  pivot_longer(-Pop, names_to = "Variable", values_to = "Variance") -> variance
 
#variance %>%
 # filter(Variable == "All") %>%
 # group_by(Variable, Pop) %>%
 # summarise(perc95 = quantile(Variance, 0.95)) -> `95percentile`

variance %>%
  filter(Variable == "All_var") %>%
  ggplot(aes(x = Pop, y = Variance, fill = Pop)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  #geom_hline(data = `95percentile`, aes(yintercept = perc95, color = Pop), linetype = "dashed") +
  labs(title = "Variance of Populations by Variable", x = "Population", y = "Variance") +
  theme_minimal() +
  coord_flip() 

UV_ORCC %>%
  ggplot(aes(x = Pop, y = L_Lateral)) + 
  geom_boxplot() + 
  coord_flip()
