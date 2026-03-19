##UV ORCC Data Visualization 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)
library(ggpmisc)

#Check that all UV measurement files are included and correctly named
setwd("/Users/radha/Downloads/UV_ORCC_CSV")
files_names <- list.files(path = ".", pattern = "*.csv", recursive = T, full.names = TRUE)
files_names

#Create a dataframe with all populations and measurements:
f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ddddd")
  x <- x %>%
     mutate(ID = tools::file_path_sans_ext(basename(f)))
  return(x)
}

dat <- lapply(files_names, f)
dat <- do.call("rbind", dat)

#Create columns for Pop, Maternal_Paternal, Year:
csv_temp <- dat %>%
  mutate(temp = ID) %>%
  rename(Identifier = ...1) %>%
  select(Identifier, Mean, ID, temp) %>%
  separate(temp, into = c("Pop", "Maternal", "Paternal", "Year", "Individual"), sep = '_')

#Assign measurements to areas based on population measurement scheme:

csv_temp %>%
  filter(Pop %in% c("YVO", "BLD", "284", "MEDX", "DKR" )) %>%
  pivot_wider(names_from = Identifier, values_from = Mean, names_prefix = "col") %>%
  rename(Scale_Bar = 'col1', 
         Ventral_Length = 'col2', 
         Ventral = 'col3',
         R_Lateral = 'col4' ,
         L_Lateral = 'col5',
         Dorsal_Length = 'col6' ,
         R_Dorsal = 'col7' ,
         L_Dorsal = 'col8' ,
         Ventral_Throat = 'col9' ) -> scheme1

csv_temp %>%
  filter(Pop == "WKY") %>%
  pivot_wider(names_from = Identifier, values_from = Mean, names_prefix = "col") %>%
  rename(Scale_Bar = 'col1', 
         Ventral_Length = 'col2', 
         Ventral = 'col3',
         R_Lateral = 'col4' ,
         L_Lateral = 'col5',
         Ventral_Throat = 'col6',
         Dorsal_Length = 'col7' ,
         R_Dorsal = 'col8' ,
         L_Dorsal = 'col9' ) -> scheme2

csv_temp %>%
  filter(!Pop %in% c("YVO", "BLD", "284", "MEDX", "DKR", "WKY")) %>%
  pivot_wider(names_from = Identifier, values_from = Mean, names_prefix = "col") %>%
  rename(Scale_Bar = 'col1', 
         Ventral_Length = 'col2', 
         Dorsal_Length = 'col3' ,
         Ventral = 'col4',
         R_Lateral = 'col5' ,
         L_Lateral = 'col6',
         Ventral_Throat = 'col7',
         R_Dorsal = 'col8' ,
         L_Dorsal = 'col9' ) -> scheme3

#Merge the 3 dataframes and save as an RDS for simplicity
full_df <- bind_rows(scheme1, scheme2, scheme3)

saveRDS(full_df, "ORCC_UV_full.rds" )
full_df <- readRDS("ORCC_UV_full.rds")

#Creates scatterplot of R dorsal vs L dorsal lobes with regression line
dorsal_model <- lm(R_Dorsal ~ L_Dorsal, data = full_df)
dorsal_r2 <- summary(dorsal_model)$r.squared

full_df %>%
  ggplot(aes(x = L_Dorsal, y = R_Dorsal, color = Pop)) + 
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
  annotate("text", x = 100, y = 8, label = paste("R² =", round(dorsal_r2, 2)), color = "black")

ggsave("Dorsal_comp1.jpeg")
  
#Creates scatterplot of R lateral vs R dorsal lobes with regression line
dorlat_model <- lm(R_Lateral ~ R_Dorsal, data = full_df)
dorlat_r2 <- summary(dorlat_model)$r.squared

full_df %>%
  ggplot(aes(x = R_Dorsal, y = R_Lateral, color = Pop)) + 
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
  annotate("text", x = 100, y = 8, label = paste("R² =", round(dorlat_r2, 2)), color = "black")

ggsave("Dorsolateral_comp.jpeg")
  
dorlat_model2 <- lm(L_Lateral ~ L_Dorsal, data = full_df)
dorlat2_r2 <- summary(dorlat_model2)$r.squared

full_df %>%
  ggplot(aes(x = L_Dorsal, y = L_Lateral, color = Pop)) + 
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
  annotate("text", x = 100, y = 8, label = paste("R² =", round(dorlat2_r2, 2)), color = "black") 
  
#Creates scatterplot of R lateral vs L lateral lobes with regression line
lateral_model <- lm(L_Lateral ~ R_Lateral, data = full_df)
lateral_r2 <- summary(lateral_model)$r.squared

full_df %>%
  ggplot(aes(x = R_Lateral, y = L_Lateral, color = Pop)) + 
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE) +
  annotate("text", x = 100, y = 8, label = paste("R² =", round(lateral_r2, 2)), color = "black")
ggsave("Lateral_comp.jpeg")

#Import qualitative data, create frequency plots w/ dorsal, ventral, and lateral lobes

UV_qual <- read.csv("/Users/radha/Documents/Blackman Lab/UV_ORCC_Qualitative Phenotyping.csv")
  
# Function to generate a unique identifier for a given (V, D, L) combination
  generate_identifier <- function(Ventral.Lobe, Dorsal.Lobes, Lateral.Lobes) {
    # Adjust values to start from 0 (0, 1, 2) instead of 1 (1, 2, 3)
    v_adj <- (Ventral.Lobe) - 1
    d_adj <- Dorsal.Lobes - 1
    l_adj <- Lateral.Lobes - 1
    
    # Calculate the unique identifier using base-3
    # The weights are 3^2 for V, 3^1 for D, and 3^0 for L
    identifier <- v_adj * (3^2) + d_adj * (3^1) + l_adj * (3^0)
    
    return(identifier)
  }

# Test the function with various combinations
UV_qual <- UV_qual %>%
  arrange(FULL.NAME) %>%
  mutate (Identifier = generate_identifier (Ventral.Lobe, Dorsal.Lobes, Lateral.Lobes)) %>%
  select(Population, FULL.NAME, Ventral.Lobe, Dorsal.Lobes, Lateral.Lobes, Identifier) %>%
  drop_na() 

#Plot frequencies of UV qual data 

UV_qual |>
  group_by(Identifier) |>
  summarize(Frequency = n()) |>
  mutate(Freq = (Frequency/(sum(Frequency)))) |>
  ggplot(aes(x = factor(Identifier), y = Freq)) +
  geom_col(fill = "navyblue") + 
  labs(x = "Identifier", y = "Total Frequency") + 
  annotate(geom = "text", x = 12, y = 0.56, label = "3 2 2") + 
  annotate(geom = "text", x = 3, y = 0.2, label = "1 2 2") +
  annotate(geom = "text", x = 6, y = 0.12, label = "2 2 2") +
  annotate(geom = "text", x = 1, y = 0.06, label = "1 1 1") + 
  annotate(geom = "text", x = 14, y = 0.06, label = "3 3 3")

#Create scatterplots of qualitative vs quantitative analysis: 

UV_qual_quant <- full_df %>% 
  arrange(ID) %>%
  mutate(FULL.NAME = ID) %>%
  merge(UV_qual, by = "FULL.NAME") %>%
  distinct(FULL.NAME, .keep_all = TRUE) %>%
  drop_na(Ventral.Lobe)


UV_qual_quant %>%
  ggplot(aes(x= factor(Ventral.Lobe), y= Ventral)) +
  geom_jitter(color = "navyblue") + 
  labs(x = "Ventral Identifier", y = "Ventral Lobe Reflectance", title = "Comparison of Ventral Qualitative and Quantitative Measurements")

UV_qual_quant %>%
  mutate(dorsal_avg = (R_Dorsal + L_Dorsal)/2) %>%
  ggplot(aes(x= factor(Dorsal.Lobes), y= dorsal_avg)) +
  geom_jitter(color = "navyblue") + 
  labs(x = "Dorsal Identifier", y = "Average Dorsal Lobe Reflectance", title = "Comparison of Dorsal Qualtitative and Quantitative Measurements")

UV_qual_quant %>%
  mutate(lateral_avg = (R_Lateral + L_Lateral)/2) %>%
  ggplot(aes(x= factor(Lateral.Lobes), y= lateral_avg)) +
  geom_jitter(color = "navyblue") + 
  labs(x = "Lateral Identifier", y = "Average Lateral Lobe Reflectance", title = "Comparison of Dorsal Qualtitative and Quantitative Measurements")

saveRDS(UV_qual, "UV_qual.rds")


