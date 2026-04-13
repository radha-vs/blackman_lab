##Exploratory Initial PCA of UV Quantitative Data 

#load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggfortify)

#Read in UV data 
UV_ORCC <- readRDS("/users/radha/Documents/Blackman Lab/Scripts/ORCC_UV_full.rds")
UV_qual <- readRDS("/users/radha/Documents/blackman_lab/UV_qual.rds")

#Scale each of the variables to have a mean of 0 and a standard deviation of 1.
#Calculate the covariance matrix for the scaled variables.
#Calculate the eigenvalues of the covariance matrix.

#First, isolate data to only vars of interest
UV_ORCC %>%
  dplyr::select(Pop, Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat,ID) %>%
  drop_na() -> dat_PCA

dat_PCA2 <- 
  merge(UV_ORCC, UV_qual, by.x = "ID", by.y = "FULL.NAME") %>%
  dplyr::select(Ventral.Lobe, Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat) %>%
  mutate(Ventral.Lobe = factor(Ventral.Lobe)) %>%
  drop_na() 

pca <- prcomp(1/(dat_PCA[,-c(1,8)]), center = TRUE, scale = TRUE ) 
pca

pca_out <- pca$x %>% 
  as.data.frame()
pca_out$Pop <- dat_PCA$Pop
pca_out$ID <- dat_PCA$ID

ggplot(data=pca_out, aes(x=PC1,y=PC2,color=Pop)) +
  geom_point() 
  #remove legend

biplot(pca, scale = 0)

autoplot(pca, data = pca_out, colour = 'Pop',
         loadings = F, loadings.label = T, loadings.label.size = 3,
         frame = FALSE, frame.type = 'norm', scale = 0)


pca2 <- prcomp(log(dat_PCA2[,-1]), center = TRUE, scale = TRUE ) 
pca2


  
ggplot(data = pca2$x, aes(x=PC1,y=PC2)) +
  geom_point()

autoplot(pca2, data = dat_PCA2, colour = 'Ventral.Lobe',
         loadings = F, loadings.label = T, loadings.label.size = 3, 
         frame = F, frame.type = 'norm', scale = 0)

ggsave("PCA_ventral.jpeg")




