rm(list=ls())

#Change to your folder
#setwd('~\\spatial_fish_modeling')

#Loads in packages and the fish_sum_stats_partition() function
library(tidyverse)
library(gbm)
library(dismo)
source("scripts\\functions\\fish_sum_stats_partition.R")

scales_to_use <- c(50, 150, 250, 500, 1000)

#####################
#####################
#SETS UP DATASHEET AND RUNS THE MODEL OVER ALL SPECIES
#####################
#####################
dataSheet <- data.frame('Species' = character(), 'scale' = integer(), 
                        'D2' = numeric(), 'cvD2' = numeric(), 'predD2' = numeric())

for(scale_to_use in scales_to_use){
  
  ######################
  ######################
  #LOADS IN 2018 DATA TO BUILD MODELS
  ######################
  ######################
  
  #Loads in predictor data
  predictorDat_2018 <- read.csv('data\\predictors\\predictor_data.csv', stringsAsFactors = F) %>%
    dplyr::filter(Year == 2018,
                  Scale == scale_to_use)
  partitions_2018 <- read.csv("data\\partitions\\partition_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2018,
                  Scale == scale_to_use)
  fishable <- read.csv("data\\fish\\fishable_species.csv", stringsAsFactors = F)
  
  #Loads in fish data and merges information about whether they are fished or not
  fishDat <- read.csv("data\\fish\\fish_survey_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2018)
  fishDat <- merge(fishDat, fishable, by = "taxonomy")
  
  #Merges fish data with partition data
  fishDat$allCode <- paste(fishDat$UniqueCode, fishDat$Minute, sep = "_")
  fishDat <- fishDat %>%
    inner_join(partitions_2018, by = 'allCode') %>%
    mutate('partCode' = paste(UniqueCode, partition, sep = "_"))
  
  #FILTERS BIG SCHOOLS OF TRIOSTEGUS
  noTrio <- fishDat[!(fishDat$taxonomy == 'Acanthurus triostegus' & fishDat$Number > 50),]
  
  #Condenses data for different herbivore groups into minutes
  grazerDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Herb_Type", species = "Grazer")
  browserDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Browser")
  exDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Excavator")
  detDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Detritivore")
  scrapeDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Scraper")
  fishableDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fishable", species = "Y")
  herbDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fine_Trophic", species = "Herbivore/Detritivore")
  
  #Saves the different fish dataframes into a list
  data_list_2018 <- list(grazer = grazerDat, browser = browserDat, 
                         excavator = exDat, detritivore = detDat, 
                         scraper = scrapeDat, fishable = fishableDat, 
                         all_herb = herbDat)
  
  #####################
  #####################
  #LOADS IN 2019 DATA TO MAKE PREDICTIONS
  #####################
  #####################
  
  #Loads in predictor data
  predictorDat_2019 <- read.csv('data\\predictors\\predictor_data.csv', stringsAsFactors = F) %>%
    dplyr::filter(Year == 2019,
                  Scale == scale_to_use)
  partitions_2019 <- read.csv("data\\partitions\\partition_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2019,
                  Scale == scale_to_use)
  
  #Loads in fish data and merges information about whether they are fished or not
  fishDat <- read.csv("data\\fish\\fish_survey_data.csv", stringsAsFactors = F) %>%
    dplyr::filter(Year == 2019)
  fishDat <- merge(fishDat, fishable, by = "taxonomy")
  
  #Merges fish data with partition data
  fishDat$allCode <- paste(fishDat$UniqueCode, fishDat$Minute, sep = "_")
  fishDat <- fishDat %>%
    inner_join(partitions_2019, by = 'allCode') %>%
    mutate('partCode' = paste(UniqueCode, partition, sep = "_"))
  
  #FILTERS BIG SCHOOLS OF TRIOSTEGUS
  noTrio <- fishDat[!(fishDat$taxonomy == 'Acanthurus triostegus' & fishDat$Number > 50),]
  
  #Condenses data for different herbivore groups into minutes
  grazerDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Herb_Type", species = "Grazer")
  browserDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Browser")
  exDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Excavator")
  detDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Detritivore")
  scrapeDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Scraper")
  fishableDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fishable", species = "Y")
  herbDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fine_Trophic", species = "Herbivore/Detritivore")
  
  #Saves the different fish dataframes into a list
  data_list_2019 <- list(grazer = grazerDat, browser = browserDat, 
                         excavator = exDat, detritivore = detDat, 
                         scraper = scrapeDat, fishable = fishableDat, 
                         all_herb = herbDat)
  
  set.seed(42)
  
  for (j in 1:length(data_list_2018)){
    
    #Combines all data for 2018
    allDat <- data_list_2018[[j]] %>%
      inner_join(predictorDat_2018, by = 'partCode')
    
    allDat$mpa <- as.factor(allDat$mpa)
    
    allDat <- as.data.frame(allDat)
    allDat$CoralStd[is.na(allDat$CoralStd)] <- 0
    allDat$AlgaeStd[is.na(allDat$AlgaeStd)] <- 0
    allDat$HardStd[is.na(allDat$HardStd)] <- 0
    
    allDat$mpa <- as.factor(allDat$mpa)
    allDat <- as.data.frame(allDat)
    
    allDat_2018 <- allDat
    
    #Combines all data for 2019
    allDat <- data_list_2019[[j]] %>%
      inner_join(predictorDat_2019, by = 'partCode')
    
    allDat$mpa <- as.factor(allDat$mpa)
    
    allDat <- as.data.frame(allDat)
    allDat$CoralStd[is.na(allDat$CoralStd)] <- 0
    allDat$AlgaeStd[is.na(allDat$AlgaeStd)] <- 0
    allDat$HardStd[is.na(allDat$HardStd)] <- 0
    
    allDat$mpa <- as.factor(allDat$mpa)
    allDat <- as.data.frame(allDat)
    
    allDat_2019 <- allDat
    
    #Runs the gbm using 2018 data
    gbm_output <- gbm.step(data = allDat_2018, gbm.x = c("CoralStd", "AlgaeStd", 
                                                         "HardStd", "SoftSub", 
                                                         "Offshore", "Pass", "mpa",
                                                         "MeanDepth",
                                                         "vrm",
                                                         "nutrientMay"),
                           gbm.y = "Biomass",  bag.fraction = .5, n.trees = 100, learning.rate = 0.0005, 
                           tree.complexity = 5, family = "gaussian", prev.stratify = F, step.size = 100,
                           max.trees = 50000, silent = F, plot.main = F)
    
    #Makes predictions using 2019 data
    pred2018 <- predict.gbm(gbm_output, allDat_2018, n.trees = gbm_output$n.trees)
    pred2019 <- predict.gbm(gbm_output, allDat_2019, n.trees = gbm_output$n.trees)
    compDat <- data.frame('expected' = allDat_2019$Biomass, 'predicted' = pred2019)
    
    #Calculates raw D2 in 2018
    D2 <- 1 - (gbm_output$self.statistics$mean.resid / gbm_output$self.statistics$mean.null)
    
    #Calculates CV d^2
    totDev <- gbm_output$self.statistics$mean.null
    cvDev <- gbm_output$cv.statistics$deviance.mean
    cvD2 <- 1 - (cvDev / totDev)
    
    #Calculates d^2 from 2019
    totPredDev <- calc.deviance(allDat_2019$Biomass, rep(mean(allDat_2019$Biomass), length(allDat_2019$Biomass)), 
                                family = "gaussian")
    predResidDev <- mean((compDat$expected - compDat$predicted)^2)
    predR2 <- 1 - (predResidDev / totPredDev)
    
    
    temp <- data.frame('Species' = names(data_list_2018)[j], 'scale' = scale_to_use, 
                       'D2' = D2, 'cvD2' = cvD2, 'predR2' = predR2)
    dataSheet <- rbind(dataSheet, temp)
    
  }
}


#Saves the datasheet as csv
#write.csv(dataSheet, file = paste("outputs\\comparison\\comparison_2018_2019_",scale_to_use,"m.csv", sep = ""), row.names = F)