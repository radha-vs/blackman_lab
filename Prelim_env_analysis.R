#Correlating UV quantitative data with environmental variables
#Preliminary analysis

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)

UV_quant <- readRDS("/Users/radha/Downloads/UV_ORCC_CSV/ORCC_UV_full.rds")
ORCC_bioclim <- read.csv("/Users/radha/Downloads/USE_env-330_bioclim_elevation_230411_sorted.csv")

ORCC_bioclim %>%
  filter(POP %in% UV_quant$Pop) %>%
  select(POP, Long, Lat, bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, 
         bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19, 
         Elevation.meters.) %>%
  unique() -> ORCC_bioclim

bioclim_long <- ORCC_bioclim %>% 
  pivot_longer(cols = starts_with("bio"), 
               names_to = "Bioclim_", 
               values_to = "Value")

bioclim_long %>%
  filter(Bioclim_ %in% c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", 
                       "bio9", "bio10", "bio11")) %>%
  ggplot(aes(x = Bioclim_, y = POP, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +  
  labs(title = "Heatmap of Population Variables",
       x = "Bioclim Variables",
       y = "Population")
