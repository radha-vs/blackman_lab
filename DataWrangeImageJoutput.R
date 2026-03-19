library(ggplot2)
# Set working directory
setwd("/Users/radha/Downloads/UV_ORCC_CSV")

# List files in the directory
files_names <- list.files(path = ".",
                          recursive = T)


# Initialize dataframe

combined_data <- data.frame("unique_ID" = character(),
                            "population" = character(),
                            "year" = numeric(),
                            "materinal_paternal_line" = numeric(),
                            "individual"= character(),
                            "scale_bar" = numeric(),
                            "length_ventral_lobe" = numeric(),
                            "length_dorsal_lobe" = numeric(),
                            "ventral_lobe" = numeric(),
                            "R_lateral" = numeric(),
                            "L_lateral" = numeric(),
                            "ventral_throat"= numeric(),
                            "R_dorsal" = numeric(),
                            "L_dorsal" = numeric())

remove_1 <- grep(x = files_names,
                       pattern = "MUG_1_4_2011_3.csv",
                 value = T)
remove_2 <- grep(x = files_names,
                 pattern = "MUG_10_9_2017_1.csv",
                 value = T)
remove_3 <- grep(x = files_names,
                 pattern = "MUG_3_2_2017_1.csv",
                 value = T)
remove_4 <- grep(x = files_names,
                 pattern = "MUG_4_3_2017_2",
                 value = T)
remove_5 <- grep(x = files_names,
                 pattern = "MUG_9_8_2017_1.csv",
                 value = T)
remove <- c(remove_1,remove_2,remove_3,remove_4,remove_5)

files_names_dw <- files_names[!(files_names %in% remove)]


for(i in files_names_dw){
  csv_tmp <- read.csv(i)

  # Removes the extention from the name
  unique_ID <- gsub(x = i,
                     pattern = ".*/(.*)\\.csv",
                     replacement = "\\1")
  pop <- gsub(x = i,
              pattern = "(.*)/.*\\.csv",
              replacement = "\\1")
  
   year <- gsub(x = i,
                pattern = ".*/.*_(.*)_([1-9])\\.csv",
                replacement = "\\1") %>%
     as.numeric()
   
   materinal_paternal_line <- gsub(x = i,
                                   pattern = ".*/.*_([0-9]+_[0-9]+)_.*_([1-9])\\.csv",
                                   replacement = "\\1")
   individual <- gsub(x = i,
                      pattern = ".*/.*([1-9])\\.csv",
                      replacement = "\\1") %>%
     as.numeric()
  
  if(pop %in% c("YVO", "BLD", "284", "MEDX", "DKR" )){
    scale_bar_tmp <- csv_tmp$Mean[1]
    length_ventral_lobe_tmp <- csv_tmp$Mean[2]
    ventral_lobe_tmp <- csv_tmp$Mean[3]
    R_lateral_tmp <- csv_tmp$Mean[4]
    L_lateral_tmp <- csv_tmp$Mean[5]
    length_dorsal_lobe <- csv_tmp$Mean[6]
    R_dorsal <- csv_tmp$Mean[7]
    L_dorsal <- csv_tmp$Mean[8]
    ventral_throat <- csv_tmp$Mean[9]
    
  } 
  
  if(pop %in% c("WKY")){
    scale_bar_tmp <- csv_tmp$Mean[1]
    length_ventral_lobe_tmp <- csv_tmp$Mean[2]
    ventral_lobe_tmp <- csv_tmp$Mean[3]
    R_lateral_tmp <- csv_tmp$Mean[4]
    L_lateral_tmp <- csv_tmp$Mean[5]
    ventral_throat <- csv_tmp$Mean[6]
    length_dorsal_lobe <- csv_tmp$Mean[7]
    R_dorsal <- csv_tmp$Mean[8]
    L_dorsal <- csv_tmp$Mean[9]

  }
  
  if(!(pop %in% c("YVO", "BLD", "284", "MEDX", "DKR","WKY"))){
    scale_bar_tmp <- csv_tmp$Mean[1]
    length_ventral_lobe_tmp <- csv_tmp$Mean[2]
    length_dorsal_lobe <- csv_tmp$Mean[3]
    ventral_lobe_tmp <- csv_tmp$Mean[4]
    R_lateral_tmp <- csv_tmp$Mean[5]
    L_lateral_tmp <- csv_tmp$Mean[6]
    ventral_throat <- csv_tmp$Mean[7]
    R_dorsal <- csv_tmp$Mean[8]
    L_dorsal <- csv_tmp$Mean[9]
  }
  
  combined_data_tmp <- data.frame("unique_ID" = unique_ID,
                              "population" = pop,
                              "year" = year,
                              "materinal_paternal_line" = materinal_paternal_line,
                              "individual"= individual,
                              "scale_bar" = scale_bar_tmp,
                              "length_ventral_lobe" = length_ventral_lobe_tmp,
                              "length_dorsal_lobe" = length_dorsal_lobe,
                              "ventral_lobe" = ventral_lobe_tmp,
                              "R_lateral" = R_lateral_tmp,
                              "L_lateral" = L_lateral_tmp,
                              "ventral_throat"= ventral_throat,
                              "R_dorsal" = R_dorsal,
                              "L_dorsal" = L_dorsal)

  # combines the data into a single dataframe 
  combined_data <- rbind(combined_data, combined_data_tmp)
  
}
combined_data_melt <- reshape2::melt(combined_data,
                                     id = c("unique_ID","population","year",
                                            "materinal_paternal_line","individual"))

# export a combined CSV
write.csv (combined_data_melt,"~/Desktop/combined_melt.csv",quote=F,row.names=F)

write.csv (combined_data,"~/Desktop/combined.csv",quote=F,row.names=F)


dorsal_comp <- ggplot(combined_data, aes(y = R_dorsal, x = L_dorsal,
                                         color = population)) +
  geom_point() +
  theme_bw() +
  xlab("L_dorsal") +
  ylab("R_dorsal") 

lateral_comp <- ggplot(combined_data, aes(y = R_lateral, x = L_lateral,
                                          color = population)) +
  geom_point() +
  theme_bw() +
  xlab("L_dorsal") +
  ylab("R_dorsal") 

R_dorsal_Rlateral_comp <- ggplot(combined_data, aes(y = R_lateral, x = R_dorsal,
                                                    color = population)) +
  geom_point() +
  theme_bw() +
  xlab("R_dorsal") +
  ylab("R_lateral") 




ventral_throat_plot <- combined_data_melt %>%
  subset(.,variable == "ventral_throat") %>%
  ggplot(., aes(y = value, x = population)) +
  geom_violin() +
  theme_bw() +
  xlab("Populations") +
  ylab("Reflectance") +
  ggtitle("Ventral throat")
  

ventral_lobe_plot <- combined_data_melt %>%
  subset(.,variable == "ventral_lobe") %>%
  ggplot(., aes(y = value, x = population)) +
  geom_violin() +
  theme_bw() +
  xlab("Populations") +
  ylab("Reflectance") +
  ggtitle("Ventral lobe")

R_lateral_plot <- combined_data_melt %>%
  subset(.,variable == "R_lateral") %>%
  ggplot(., aes(y = value, x = population)) +
  geom_violin() +
  theme_bw() +
  xlab("Populations") +
  ylab("Reflectance") +
  ggtitle("Right lateral Lobe")

R_dorsal_plot <- combined_data_melt %>%
  subset(.,variable == "R_dorsal") %>%
  ggplot(., aes(y = value, x = population)) +
  geom_violin() +
  theme_bw() +
  xlab("Populations") +
  ylab("Reflectance") +
  ggtitle("Right dorsal Lobe")


pdf("~/Desktop/plot_test.pdf", height = 10 , width = 15)
R_dorsal_Rlateral_comp
dorsal_comp
lateral_comp
lateral_comp
R_lateral_plot
R_dorsal_plot
ventral_throat_plot
ventral_lobe_plot
dev.off()






