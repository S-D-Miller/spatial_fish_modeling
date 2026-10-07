rm(list=ls())

library(tidyverse)

#Change to your folder
#setwd('~\\spatial_fish_modeling')

source("scripts\\functions\\fish_sum_stats_partition.R")

scales_to_use <- c(50, 150, 250, 500, 1000)

dataSheet <- data.frame("year" = integer(),
                        "scale" = integer(),
                        "area" = numeric())

for(scale_to_use in scales_to_use){
  
  ######################
  ######################
  #LOADS IN 2018 DATA
  ######################
  ######################
  #Loads in predictor data
  predictorDat_2018 <- read.csv('data\\predictors\\predictor_data.csv', stringsAsFactors = F) %>%
    dplyr::filter(Year == 2018,
                  Scale == scale_to_use)
  #Loads in partition data
  partitions_2018 <- read.csv("data\\partitions\\partition_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2018,
                  Scale == scale_to_use)
  #Loads in fish data and attaches to partitions, then summarizes at partition level
  fishDat_2018 <- read.csv("data\\fish\\fish_survey_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2018)
  fishDat_2018$allCode <- paste(fishDat_2018$UniqueCode, fishDat_2018$Minute, sep = "_")
  fishDat_2018 <- fishDat_2018 %>%
    inner_join(partitions_2018, by = 'allCode') %>%
    mutate('partCode' = paste(UniqueCode, partition, sep = "_"))
  fishDat_2018 <- fish_sum_stats_partition(fishDat_2018, fishDat_2018)
  
  #Combines fish data (with area) to predictors to eliminate segments without associated predictor data
  allDat_2018 <- fishDat_2018 %>%
    inner_join(predictorDat_2018, by = 'partCode')
  
  temp_frame_2018 <- data.frame("year" = rep(2018, nrow(allDat_2018)),
                                "scale" = rep(scale_to_use, nrow(allDat_2018)),
                                "area" = allDat_2018$area)
  
  ######################
  ######################
  #LOADS IN 2019 DATA TO BUILD MODELS
  ######################
  ######################
  #Loads in predictor data
  predictorDat_2019 <- read.csv('data\\predictors\\predictor_data.csv', stringsAsFactors = F) %>%
    dplyr::filter(Year == 2019,
                  Scale == scale_to_use)
  #Loads in partition data
  partitions_2019 <- read.csv("data\\partitions\\partition_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2019,
                  Scale == scale_to_use)
  
  fishDat_2019 <- read.csv("data\\fish\\fish_survey_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2019)
  
  fishDat_2019$allCode <- paste(fishDat_2019$UniqueCode, fishDat_2019$Minute, sep = "_")
  fishDat_2019 <- fishDat_2019 %>%
    inner_join(partitions_2019, by = 'allCode') %>%
    mutate('partCode' = paste(UniqueCode, partition, sep = "_"))
  
  fishDat_2019 <- fish_sum_stats_partition(fishDat_2019, fishDat_2019)
  
  allDat_2019 <- fishDat_2019 %>%
    inner_join(predictorDat_2019, by = 'partCode')
  
  temp_frame_2019 <- data.frame("year" = rep(2019, nrow(allDat_2019)),
                                "scale" = rep(scale_to_use, nrow(allDat_2019)),
                                "area" = allDat_2019$area)
  
  dataSheet <- rbind(dataSheet, temp_frame_2018)
  dataSheet <- rbind(dataSheet, temp_frame_2019)
  
}

#Plots 2018
dev.new()
par(oma = c(0,1,0,0))
boxplot(area ~ scale, data = dataSheet[dataSheet$year == 2018,],
        ylim = c(0, 1100), ylab = "",
        xlab = "",
        cex.lab = 1.5, cex.axis = 1.35)
mtext(expression(paste('Target transect area (m'^'2', ")", sep = "")), side = 1, cex = 1.5, line = 3)
mtext(expression(paste('Actual transect area (m'^'2', ")", sep = "")), side = 2, cex = 1.75, line = 3)

segments(-100, 0, 2000, 0, lty = 2, col = 'firebrick')
segments(-100, 100, 2000, 100, lty = 2, col = 'firebrick')
segments(-100, 200, 2000, 200, lty = 2, col = 'firebrick')
segments(-100, 300, 2000, 300, lty = 2, col = 'firebrick')
segments(-100, 450, 2000, 450, lty = 2, col = 'firebrick')
segments(-100, 550, 2000, 550, lty = 2, col = 'firebrick')
segments(-100, 950, 2000, 950, lty = 2, col = 'firebrick')
segments(-100, 1050, 2000, 1050, lty = 2, col = 'firebrick')

text(1, 150, nrow(dataSheet[dataSheet$year == 2018 & 
                              dataSheet$scale == 50,]))
text(2, 250, nrow(dataSheet[dataSheet$year == 2018 & 
                              dataSheet$scale == 150,]))
text(3, 350, nrow(dataSheet[dataSheet$year == 2018 & 
                              dataSheet$scale == 250,]))
text(4, 600, nrow(dataSheet[dataSheet$year == 2018 & 
                              dataSheet$scale == 500,]))
text(5, 1100, nrow(dataSheet[dataSheet$year == 2018 & 
                              dataSheet$scale == 1000,]))
text(0.5, 1100, labels = "(a)", cex = 1.75)

#Plots 2019
dev.new()
par(oma = c(0,1,0,0))
boxplot(area ~ scale, data = dataSheet[dataSheet$year == 2019,],
        ylim = c(0, 1100), ylab = "",
        xlab = "",
        cex.lab = 1.5, cex.axis = 1.35)
mtext(expression(paste('Target transect area (m'^'2', ")", sep = "")), side = 1, cex = 1.5, line = 3)
mtext(expression(paste('Actual transect area (m'^'2', ")", sep = "")), side = 2, cex = 1.75, line = 3)

segments(-100, 0, 2000, 0, lty = 2, col = 'firebrick')
segments(-100, 100, 2000, 100, lty = 2, col = 'firebrick')
segments(-100, 200, 2000, 200, lty = 2, col = 'firebrick')
segments(-100, 300, 2000, 300, lty = 2, col = 'firebrick')
segments(-100, 450, 2000, 450, lty = 2, col = 'firebrick')
segments(-100, 550, 2000, 550, lty = 2, col = 'firebrick')
segments(-100, 950, 2000, 950, lty = 2, col = 'firebrick')
segments(-100, 1050, 2000, 1050, lty = 2, col = 'firebrick')

text(1, 150, nrow(dataSheet[dataSheet$year == 2019 & 
                              dataSheet$scale == 50,]))
text(2, 250, nrow(dataSheet[dataSheet$year == 2019 & 
                              dataSheet$scale == 150,]))
text(3, 350, nrow(dataSheet[dataSheet$year == 2019 & 
                              dataSheet$scale == 250,]))
text(4, 600, nrow(dataSheet[dataSheet$year == 2019 & 
                              dataSheet$scale == 500,]))
text(5, 1100, nrow(dataSheet[dataSheet$year == 2019 & 
                               dataSheet$scale == 1000,]))
text(0.5, 1100, labels = "(b)", cex = 1.75)