rm(list=ls())

#Change to your folder
#setwd('~\\spatial_fish_modeling')

#Loads in required packages and functions
library(tidyverse)
library(gbm)
library(dismo)
source("scripts\\functions\\fish_sum_stats_partition.R")
source("scripts\\functions\\spatial_autocorrelation_residual_calculator.R")

#Loads in predictor data
predictorDat <- read.csv('data\\predictors\\predictor_data.csv', stringsAsFactors = F) %>%
  dplyr::filter(Year == 2018,
                Scale == 500)
partitions <- read.csv('data\\partitions\\partition_data.csv', stringsAsFactors = F) %>%
  dplyr::filter(Year == 2018,
                Scale == 500)
fishable <- read.csv("data\\fish\\fishable_species.csv", stringsAsFactors = F)

#Loads in fish data and attaches it to partitions
fishDat <- read.csv("data\\fish\\fish_survey_data.csv", header = T, stringsAsFactors = F) %>%
  dplyr::filter(Year == 2018)
fishDat <- merge(fishDat, fishable, by = "taxonomy")

fishDat$allCode <- paste(fishDat$UniqueCode, fishDat$Minute, sep = "_")
fishDat <- fishDat %>%
  inner_join(partitions, by = 'allCode') %>%
  mutate('partCode' = paste(UniqueCode, partition, sep = "_"))

#FILTERS BIG SCHOOLS OF TRIOSTEGUS
noTrio <- fishDat[!(fishDat$taxonomy == 'Acanthurus triostegus' & fishDat$Number > 50),]

#Condenses data for different herbivore groups into minutes
grazerDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Herb_Type", species = "Grazer")
browserDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Browser")
exDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Excavator")
detDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Detritivore")
scrapeDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Scraper")
goatDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "family", species = "Mullidae")
fishableDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fishable", species = "Y")
herbDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fine_Trophic", species = "Herbivore/Detritivore")

#Saves individual data frames into a list
data_list <- list(grazer = grazerDat, browser = browserDat, 
                  excavator = exDat, detritivore = detDat, 
                  scraper = scrapeDat, fishable = fishableDat, 
                  all_herb = herbDat)

#Creates empty dataframes to save metrics
dataSheet1 <- data.frame('species' = character(), 'scale' = integer(), 'var' = character(), 'rel.inf' = character())
dataSheet2 <- data.frame('var1.index' = integer(), 'var1.names' = character(), 'var2.index' = integer(),
                         'var2.names' = character(), 'int.size' = numeric())
dataSheet3 <- data.frame('species' = character(), 'scale' = integer(), 'raw_obs' = numeric(), 'resid_obs' = numeric(),
                         'expected' = numeric(), 'sd' = numeric(), 'p_raw' = numeric(), 'p_resid' = numeric())

set.seed(42) #for reproducibility

#Loops through the different fish groups and generates metrics
for (j in 1:length(data_list)){
  
  #Creates a temporary variable for each group that combines fish and predictors
  allDat <- data_list[[j]] %>%
    inner_join(predictorDat)
  
  allDat$mpa <- as.factor(allDat$mpa)
  
  allDat <- as.data.frame(allDat)
  allDat$CoralStd[is.na(allDat$CoralStd)] <- 0
  allDat$AlgaeStd[is.na(allDat$AlgaeStd)] <- 0
  allDat$HardStd[is.na(allDat$HardStd)] <- 0
  
  allDat$mpa <- as.factor(allDat$mpa)
  allDat <- as.data.frame(allDat)

  #Runs the GBM model
  gbm_output <- gbm.step(data = allDat, gbm.x = c("CoralStd", "AlgaeStd", 
                                                  "HardStd", "SoftSub", 
                                                  "Offshore", "Pass", "mpa",
                                                  "MeanDepth",
                                                  "vrm",
                                                  "nutrientMay"),
                         gbm.y = "Biomass",  bag.fraction = .5, n.trees = 100, learning.rate = 0.0005, 
                         tree.complexity = 5, family = "gaussian", prev.stratify = F, step.size = 100,
                         max.trees = 50000, silent = F, plot.main = F)
  
  #Saves relative importance values
  temp1 <- data.frame('species' = names(data_list)[j], 'scale' = 500, 
                      'var' = summary(gbm_output)$var, 'rel.inf' = summary(gbm_output)$rel.inf)
  
  #Calculates and saves interaction effects
  temp_int <- gbm.interactions(gbm_output)
  temp_frame <- as.data.frame(temp_int$rank.list)
  temp_frame$scale <- 500
  temp_frame$species <- names(data_list[j])
  
  #Calculates autocorrelation in the model residuals and saves them
  autocorr_output <- residual_autocorrelation_calculator(allDat, fishDat, gbm_output, partitions)
  temp_autocorr <- data.frame('species' = names(data_list)[j], 'scale' = 500, 
                              'raw_obs' = autocorr_output[[1]]$observed, 
                              'resid_obs' = autocorr_output[[2]]$observed,
                              'expected' = autocorr_output[[1]]$expected,
                              'sd' = autocorr_output[[1]]$sd,
                              'p_raw' = autocorr_output[[1]]$p.value,
                              'p_resid' = autocorr_output[[2]]$p.value)
  
  #Binds the different metrics for each group into a master dataframe for each suite of metrics
  dataSheet1 <- rbind(dataSheet1, temp1)
  dataSheet2 <- rbind(dataSheet2, temp_frame)
  dataSheet3 <- rbind(dataSheet3, temp_autocorr)

}

#Writes outputs of the different metrics
# write.csv(dataSheet1, file = "outputs\\500m_model\\relative_importance_500m.csv", row.names = F)
# write.csv(dataSheet2, file = "outputs\\500m_model\\interactions_500m.csv", row.names = F)
# write.csv(dataSheet3, file = "outputs\\500m_model\\autocorr_500m.csv", row.names = F)