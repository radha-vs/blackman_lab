#Correlating UV quantitative data with environmental variables
#Preliminary analysis

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(maps)

UV_quant <- readRDS("/Users/radha/Documents/blackman_lab/ORCC_UV_full.rds")
ORCC_bioclim <- read.csv("/Users/radha/Documents/Blackman Lab/data/Environmental Data/USE_env-330_bioclim_elevation_230411_sorted.csv")

#plotting fun temperature map
ORCC_bioclim %>%
  filter(POP %in% UV_quant$Pop) %>%
  select(POP, Long, Lat, bio1, Elevation.meters.) %>%
  unique() -> mapdata

oregon_cal <- map_data("state")
oregon_cal <- subset(oregon_cal, region == "oregon" | region == "california")

ggplot() +
  geom_polygon(data = oregon_cal,
               aes(x = long, y = lat, group = group),
               fill = "dark gray", color = "black") +
  geom_point(data = mapdata,
             aes(x = Long, y = Lat, color = bio1),
             size = 3) +
  scale_color_viridis_c(option = "inferno") +
  coord_fixed(1.3) +
  labs(title = "ORCC Population Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Average Monthly Temperature (C)") +
  theme(legend.position = "bottom")

ggsave("ORCC_Temperature.jpg")

ggplot() +
  geom_polygon(data = oregon_cal,
               aes(x = long, y = lat, group = group),
               fill = "dark gray", color = "black") +
  geom_point(data = mapdata,
             aes(x = Long, y = Lat, color = Elevation.meters.),
             size = 3) +
  scale_color_viridis_c(option = "viridis") +
  coord_fixed(1.3) +
  labs(title = "ORCC Population Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Elevation (m)") +
  theme(legend.position = "bottom")

ggsave("ORCC_Elevation.jpg")

###


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
