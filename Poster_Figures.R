#Updated figures for symposium poster

library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)
library(ordinal)

#plotting fun temperature map
ORCC_bioclim %>%
  filter(POP %in% UV_quant$Pop) %>%
  dplyr::select(POP, Long, Lat, bio1, bio17, Elevation.meters.) %>%
  unique() -> mapdata

oregon_cal <- map_data("state")
oregon_cal <- subset(oregon_cal, region == "oregon" | region == "california")

ggplot() +
  geom_polygon(data = oregon_cal,
               aes(x = long, y = lat, group = group),
               fill = "dark gray", color = "black") +
  geom_point(data = mapdata,
             aes(x = Long, y = Lat, color = bio17),
             size = 3) +
  scale_color_viridis_c(option = "viridis") +
  coord_fixed(1.3) +
  labs(title = "ORCC Population Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Precipitation (mm)") +
  theme(legend.position = "bottom")

ggsave("ORCC_Precipitation.jpg", width = 1624, height = 1728, units = "px")


#Frequency of phenotypes
UV_qual <- readRDS("/users/radha/Documents/blackman_lab/UV_qual.rds")

pivot_longer(
  UV_qual,
  cols = c(Ventral.Lobe),
  names_to = "Ventral Phenotype",
  values_to = "Value") -> UV_qual_long

UV_qual_long %>%
  ggplot(aes (x = Population, fill = as.factor(Value))) +
  geom_bar(position = "fill") +
  scale_fill_viridis(discrete = TRUE, begin = 0.85, end = 0.4) +
  labs(title = "Proportions of Ventral Phenotypes", x = "Population", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, size = 7))

ggsave("ventral_proportions.jpg")


#Quantitative measurements of ventral lobe by population 

#Correlations between quantitative measurements and elevation/mean temperature/UVB 
#UVB data taken from NREL UVB Database (watts per square meter per nanometer)
UV_qual <- readRDS("/users/radha/Documents/blackman_lab/UV_qual.rds")
orcc_quant <- readRDS("/users/radha/Documents/blackman_lab/ORCC_UV_full.rds")
read.csv("UVB_env_ORCC.csv") -> uvb_env_orcc
orcc_quant <- readRDS("/users/radha/Documents/blackman_lab/ORCC_UV_full.rds")

bioclim <- read.csv("/users/radha/Documents/Blackman Lab/data/Environmental Data/USE_env-330_bioclim_elevation_230411_sorted.csv")

orcc_quant %>%
  left_join(uvb_env_orcc, by = "Pop") %>%
  dplyr::select(Pop, Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
                Elevation, group1, group2, group3, group4, group5, group6, UVB1) -> UV_full
bioclim %>%
  mutate(Pop = POP) %>%
  left_join(UV_full, by = "Pop") %>%
  dplyr::select(Pop, Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
                Elevation, group6, bio1, bio17, UVB1) -> UV_full


UV_full %>%
  mutate(Population = Pop) %>%
  drop_na() -> UV_full

UV_qual %>%
  left_join(UV_full, by = "Population") -> UV_complete

UV_complete %>%
 mutate(Maternal_Line = str_extract(FULL.NAME, "^[^_]+_[^_]+")) -> UV_complete
        #, 
#         complete = Ventral + R_Lateral + L_Lateral + R_Dorsal + L_Dorsal + Ventral_Throat) -> UV_complete

#UV_complete %>%
#  filter(Population == "YVO") -> UV_complete

#Population != "LRD" & Population != "YVO" & Population != "MUG"  & Population != "WNR"

UV_complete %>%
  group_by(Maternal_Line, Population) %>%
  #drop_na() %>%
  summarize(Ventral = mean(Ventral), 
            L_Dorsal = mean(L_Dorsal),
            R_Dorsal = mean(R_Dorsal),
            L_Lateral = mean(L_Lateral),
            R_Lateral = mean(R_Lateral),
            Ventral_Throat = mean(Ventral_Throat), 
            Elevation = mean(Elevation), 
            UVB1 = mean(UVB1),
            group6 = mean(group6),
            bio1 = mean(bio1), 
            bio17 = mean(bio17),
            Ventral_Phenotype = mean(Ventral.Lobe),
            Lateral_Phenotype = mean(Lateral.Lobes), 
            Dorsal_Phenotype = mean(Dorsal.Lobes), 
            Identifier = mean(Identifier)) -> mean_quants

mean_quants %>%
  #filter(Population != "WKY", Population != "YVO", Population != "LRD") %>%
  ggplot(aes(x= Ventral_Phenotype, y= UVB1)) + 
  geom_jitter() + 
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson") +
  theme_minimal()


#  mean_quants %>%
#  ggplot(aes(x = Ventral_Phenotype, y = Ventral)) + 
#  geom_jitter() + 
#  geom_smooth(method = lm, se = FALSE) +
#  stat_cor(method = "pearson") +
#  theme_minimal()

mean_quants %>%
  ggplot(aes(x= log(Ventral), y= UVB1)) + 
  geom_jitter() + 
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson") +
  theme_minimal()

pivot_longer(
  mean_quants,
  cols = c(Ventral, Ventral_Throat),
  names_to = "Quantitative Measurements",
  values_to = "Value") -> mean_quants_long

pivot_longer(
  mean_quants,
  cols = c(Identifier),
  #cols = c(Identifier),
  names_to = "Scoring",
  values_to = "Value") -> mean_quals_long

#Ventral.Lobe, Lateral.Lobes, Dorsal.Lobes

mean_quants_long %>%
  #filter(Variables == "avg_ventral", "avg_ldorsal", "avg_rdorsal", "avg_llateral"
  #  , "avg_rlateral" , "avg_throat") %>%
  ggplot(aes(x= log(Value), y = bio17, color = `Quantitative Measurements`)) +
  geom_jitter() +
  scale_color_viridis(discrete=TRUE, option = "viridis", begin = 0, end = 0.5) +
  geom_smooth(method = lm, se = FALSE) +
  stat_cor(method = "pearson")+
  labs(title = "Dry Season Precipitation vs UV Reflectance", 
       x = "Quantitative UV Reflectance (log transformed)", 
       y = "Precipitation in Driest Quarter (mm)") +
  theme_bw()

#UV-B Irradiance (W/m2/mm)

ggsave("bio17_vs_quant.jpg")

mean_quals_long %>%
  ggplot(aes(x= Value, y = bio17, color = `Scoring`)) +
  geom_jitter() +
  scale_color_viridis(discrete=TRUE, begin = 0.75, end = 0) +
  geom_smooth(method = lm, se = FALSE) +
  #stat_cor(method = "pearson")+
  labs(title = "Dry Season Precipitation vs Qualitative Phenotype", 
       x = "Composite Qualitative Phenotype", 
       y = "Precipitation in Driest Quarter (mm)") +
  theme_bw()

ggsave("bio17_vs_qual.jpg")


#Redoing qualitative regression with ordinal logistic reg
UV_complete$Identifier <- factor(UV_complete$Identifier, ordered = TRUE) 

qual_assoc_UVB <- clm(Identifier ~ UVB1, data = UV_complete)

summary(qual_assoc_UVB)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# UVB1 -2.065e-03  2.061e-05  -100.2   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2 -7.59286    0.06593  -115.2
# 2|3 -6.75122    0.06399  -105.5
# (53 observations deleted due to missingness)

newdata <- data.frame(
  UVB1 = seq(min(ordered_qual$UVB1, na.rm = TRUE),
             max(ordered_qual$UVB1, na.rm = TRUE),
             length.out = 100))

preds <- predict(qual_assoc_UVB, newdata = newdata, type = "prob")

pred_df <- cbind(newdata, preds$fit) %>%
  pivot_longer(
    cols = -UVB1,
    names_to = "Category",
    values_to = "Probability"
  )

ggplot(pred_df, aes(x = UVB1, y = Probability, color = Category)) +
  geom_line(size = 1.2) +
  labs(
    title = "Ordinal Regression: Predicted Probabilities",
    x = "UVB1",
    y = "Probability"
  ) +
  theme_minimal()
     

ggplot() +
  geom_jitter(data = ordered_qual,
              aes(x = UVB1, y = as.numeric(Ventral_Phenotype)),
              height = 0.05, alpha = 0.4) +
  geom_line(data = pred_df,
            aes(x = UVB1, y = Probability * 3, color = Category),
            size = 1.2)
