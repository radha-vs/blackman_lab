##Further analysis with environmental variables


install.packages("~/Downloads/Matrix_1.6-5.tar.gz",repos = NULL, type="source")

library(tidyverse)
library(lme4)
#library(e1071)
# List of packages to be used in the R session
.packages = c("lme4", "AICcmodavg", "MuMIn", "pbkrtest",
              "parallel", "data.table", "blmeco", "lsmeans")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Attach packages
sapply(.packages, require, character.only=TRUE)



UV_ORCC <- readRDS("/users/radha/Documents/Blackman Lab/Scripts/ORCC_UV_full.rds")
full_env <- read.csv("/users/radha/Documents/Blackman Lab/data/All_Monthly.csv")
ungrouped_env <- 
  read.csv("/users/radha/Documents/Blackman Lab/data/Environmental Data/USE_env-330_bioclim_elevation_230411_sorted.csv")
ORCC_env <- read.csv("/users/radha/Documents/Blackman Lab/data/bcbcm327_7099_PCG.csv")

full_env %>%
  filter(Cal_Year <= 2017, Cal_Year >= 2011) %>%
  group_by(BasinName, Cal_Year) %>%
  summarize(tmx_C = mean(tmx_C), 
            tmn_C = mean(tmn_C),
            tav_C = mean(tav_C),
            snw_mm = mean(snw_mm),
            mlt_mm = mean(mlt_mm),
            pck_mm = mean(pck_mm),
            sbl_mm = mean(sbl_mm),
            ppt_mm = mean(ppt_mm),
            exc_mm = mean(exc_mm))-> full_env2



#(1- summer drought): tmx_C
#(2- temp in rainy szn): tmn_C,tav_C
#(4- snow): snw_mm, mlt_mm, pck_mm, sbl_mm
#(6- precipitation): pt_mm, exc_mm
  



#Testing for normality of UV quant data

current_skewness <- skewness(UV_ORCC$Ventral)
print(paste("Current Skewness:", current_skewness))

# Logarithmic Transformation
log_transformed_response <- log(UV_ORCC$Ventral + 1)

# Square Root Transformation
sqrt_transformed_response <- sqrt(UV_ORCC$Ventral)

# Inverse Transformation
inv_transformed_response <- (1 / UV_ORCC$Ventral)

log_skewness <- skewness(log_transformed_response)
sqrt_skewness <- skewness(sqrt_transformed_response)
inv_skewness <- skewness(inv_transformed_response)

print(paste("Log Transformation Skewness:", log_skewness))
print(paste("Square Root Transformation Skewness:", sqrt_skewness))
print(paste("Inverse Transformation Skewness:", inv_skewness))

ggplot(UV_ORCC, aes(Ventral)) + geom_histogram()


ggplot(UV_ORCC, aes(1/(Ventral))) + geom_histogram() 
ggplot(UV_ORCC, aes(log(Ventral))) + geom_histogram() 

ORCC_env %>%
  distinct(POP, .keep_all = TRUE) -> env2

UV_ORCC %>%
  left_join(env2, by = c("Pop" = "POP"), keep= FALSE) %>%
  #filter(Pop != c("DKR", "YVO")) %>%
  dplyr::select(Ventral, R_Lateral, L_Lateral, R_Dorsal, L_Dorsal, Ventral_Throat, 
         Elevation, group1, group2, group3, group4, group5, group6, ID, Pop) -> UV_env


UV_env %>%
  mutate(Pop = as.factor(Pop)) -> UV_env
#linear mixed model
lmm <- lmer(group6 ~ Ventral + (1|Pop), UV_env)
lmm2 <- lmer(Ventral ~ (1|Pop), UV_env)
lmm
AIC(lmm)
AIC(lmm2)

#Proportion of varaicne explained by random effect 
varcomp <- VarCorr(lmm)
pop_var <- as.numeric(attr(varcomp$Pop, "stddev"))^2
res_var <- attr(varcomp, "sc")^2

totalvar = pop_var + res_var
popvar2 <- pop_var / totalvar
popvar2
pop_var
varcomp
totalvar
