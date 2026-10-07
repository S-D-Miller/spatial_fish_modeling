rm(list=ls())

#Change to your folder
#setwd('~\\spatial_fish_modeling')

library(tidyverse)
library(gbm)
library(dismo)
library(corrplot)
library(vegan)

source("scripts\\functions\\fish_sum_stats_partition.R")
source("scripts\\functions\\spatial_autocorrelation_residual_calculator.R")
source("scripts\\functions\\ggbrt_functions.R")

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

fish_sum <- noTrio %>%
  group_by(taxonomy) %>%
  summarize(herbType = first(Herb_Type),
            fishable = first(Fishable),
            biomass = sum(mass))

#Calculates the diversity metrics mentioned in the Discussion
allherb <- fish_sum %>%
  filter(herbType != 'na')
herb_dive <- diversity(allherb$biomass)

fishable <- fish_sum %>%
  filter(fishable == 'Y')
fishable_dive <- diversity(fishable$biomass)

grazer <- fish_sum %>%
  filter(herbType == "Grazer")
grazer_dive <- diversity(grazer$biomass)

detritivore <- fish_sum %>%
  filter(herbType == 'Detritivore')
detritivore_dive <- diversity(detritivore$biomass)

excavator <- fish_sum %>%
  filter(herbType == 'Excavator')
excavator_dive <- diversity(excavator$biomass)

scraper <- fish_sum %>%
  filter(herbType == 'Scraper')
scraper_dive <- diversity(scraper$biomass)

browser <- fish_sum %>%
  filter(herbType == 'Browser')
browser_dive <- diversity(browser$biomass)

#Condenses data for different herbivore groups into minutes
grazerDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Herb_Type", species = "Grazer")
browserDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Browser")
exDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Excavator")
detDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Detritivore")
scrapeDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "Herb_Type", species = "Scraper")
goatDat <- fish_sum_stats_partition(fishDat, fishDat, taxonomy = "family", species = "Mullidae")
fishableDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fishable", species = "Y")
herbDat <- fish_sum_stats_partition(fishDat, noTrio, taxonomy = "Fine_Trophic", species = "Herbivore/Detritivore")

#Saves data in a list
data_list <- list(grazer = grazerDat, browser = browserDat, 
                  excavator = exDat, detritivore = detDat, 
                  scraper = scrapeDat, fishable = fishableDat, 
                  all_herb = herbDat)

#Creates the output list
output_list <- list()

#For reproducibility
set.seed(42)

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
                         gbm.y = "Biomass",  bag.fraction = 0.5, n.trees = 100, learning.rate = 0.0005, 
                         tree.complexity = 5, family = "gaussian", prev.stratify = F, step.size = 100,
                         max.trees = 50000, silent = F, plot.main = F)

  output_list[[j]] <- gbm_output
        

}

#################################
#################################
#RUN BOOTSTRAPPING
#################################
#################################
#All herbivores
herb_prerun <- plot.gbm.4list(output_list[[7]])
herb_boot <- gbm.bootstrap.functions(output_list[[7]], list.predictors = herb_prerun, n.reps = 100)
#write_rds(herb_boot, file = "outputs\\pdp_boot\\herb_boot.rds")

#Fishable
fishable_prerun <- plot.gbm.4list(output_list[[6]])
fishable_boot <- gbm.bootstrap.functions(output_list[[6]], list.predictors = fishable_prerun, n.reps = 100)
#write_rds(fishable_boot, file = "outputs\\pdp_boot\\fishable_boot.rds")

#Excavators
excavator_prerun <- plot.gbm.4list(output_list[[3]])
excavator_boot <- gbm.bootstrap.functions(output_list[[3]], list.predictors = excavator_prerun, n.reps = 100)
#write_rds(excavator_boot, file = "outputs\\pdp_boot\\excavator_boot.rds")

#Scrapers
scraper_prerun <- plot.gbm.4list(output_list[[5]])
scraper_boot <- gbm.bootstrap.functions(output_list[[5]], list.predictors = scraper_prerun, n.reps = 100)
#write_rds(scraper_boot, file = "outputs\\pdp_boot\\scraper_boot.rds")

#Detritivores
detritivore_prerun <- plot.gbm.4list(output_list[[4]])
detritivore_boot <- gbm.bootstrap.functions(output_list[[4]], list.predictors = detritivore_prerun, n.reps = 100)
#write_rds(detritivore_boot, file = "outputs\\pdp_boot\\detritivore_boot.rds")

pdp_list <- list(excavator_boot,
                 detritivore_boot,
                 scraper_boot,
                 fishable_boot,
                 herb_boot)

col_list  <- c("#000000", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#########################
#########################
#All herbivores

herb_prerun <- plot.gbm.4list(output_list[[7]])

#Panel 1
ggPD_boot(output_list[[7]], list.4.preds = herb_prerun, predictor = 1, booted.preds = pdp_list[[5]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[1], x.label = "Soft bottom", max.y = 15)
ggsave("herb1.tiff", units = "in", height = 2, width = 2)

#Panel 2
ggPD_boot(output_list[[7]], list.4.preds = herb_prerun, predictor = 2, booted.preds = pdp_list[[5]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[1], x.label = "Live coral", max.y = 10)
ggsave("herb2.tiff", units = "in", height = 2, width = 2)

#Panel 3
ggPD_boot(output_list[[7]], list.4.preds = herb_prerun, predictor = 3, booted.preds = pdp_list[[5]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[1], x.label = "Dist. offshore", max.y = 10)
ggsave("herb3.tiff", units = "in", height = 2, width = 2)

#Panel 4
ggPD_boot(output_list[[7]], list.4.preds = herb_prerun, predictor = 4, booted.preds = pdp_list[[5]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[1], x.label = "Rugosity", max.y = 10)
ggsave("herb4.tiff", units = "in", height = 2, width = 2)


#########################
#########################
#Fishable

fishable_prerun <- plot.gbm.4list(output_list[[6]])

#Panel 1
ggPD_boot(output_list[[6]], list.4.preds = fishable_prerun, predictor = 1, booted.preds = pdp_list[[4]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[7], x.label = "Soft bottom", max.y = 10)
ggsave("fishable1.tiff", units = "in", height = 2, width = 2)

#Panel 2
ggPD_boot(output_list[[6]], list.4.preds = fishable_prerun, predictor = 2, booted.preds = pdp_list[[4]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[7], x.label = "Dist. offshore", max.y = 10)
ggsave("fishable2.tiff", units = "in", height = 2, width = 2)

#Panel 3
ggPD_boot(output_list[[6]], list.4.preds = fishable_prerun, predictor = 3, booted.preds = pdp_list[[4]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[7], x.label = "Dist. pass", max.y = 8)
ggsave("fishable3.tiff", units = "in", height = 2, width = 2)

#Panel 4
ggPD_boot(output_list[[6]], list.4.preds = fishable_prerun, predictor = 4, booted.preds = pdp_list[[4]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[7], x.label = "Turf/CCA", max.y = 8)
ggsave("fishable4.tiff", units = "in", height = 2, width = 2)


#########################
#########################
#Excavators

excavator_prerun <- plot.gbm.4list(output_list[[3]])

#Panel 1
ggPD_boot(output_list[[3]], list.4.preds = excavator_prerun, predictor = 1, booted.preds = pdp_list[[1]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[6], x.label = "Soft bottom", max.y = 5)
ggsave("excavator1.tiff", units = "in", height = 2, width = 2)

#Panel 2
ggPD_boot(output_list[[3]], list.4.preds = excavator_prerun, predictor = 2, booted.preds = pdp_list[[1]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[6], x.label = "Mean depth", max.y = 4)
ggsave("excavator2.tiff", units = "in", height = 2, width = 2)

#Panel 3
ggPD_boot(output_list[[3]], list.4.preds = excavator_prerun, predictor = 3, booted.preds = pdp_list[[1]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[6], x.label = "Dist. pass", max.y = 5)
ggsave("excavator3.tiff", units = "in", height = 2, width = 2)

#Panel 4
ggPD_boot(output_list[[3]], list.4.preds = excavator_prerun, predictor = 4, booted.preds = pdp_list[[1]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[6], x.label = "Nutrients", max.y = 5)
ggsave("excavator4.tiff", units = "in", height = 2, width = 2)


#########################
#########################
#Scrapers

scraper_prerun <- plot.gbm.4list(output_list[[5]])

#Panel 1
ggPD_boot(output_list[[5]], list.4.preds = scraper_prerun, predictor = 1, booted.preds = pdp_list[[3]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[5], x.label = "Dist. offshore", max.y = 8)
ggsave("scraper1.tiff", units = "in", height = 2, width = 2)

#Panel 2
ggPD_boot(output_list[[5]], list.4.preds = scraper_prerun, predictor = 2, booted.preds = pdp_list[[3]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[5], x.label = "Rugosity", max.y = 5)
ggsave("scraper2.tiff", units = "in", height = 2, width = 2)

#Panel 3
ggPD_boot(output_list[[5]], list.4.preds = scraper_prerun, predictor = 3, booted.preds = pdp_list[[3]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[5], x.label = "Dist. pass", max.y = 5)
ggsave("scraper3.tiff", units = "in", height = 2, width = 2)

#Panel 4
ggPD_boot(output_list[[5]], list.4.preds = scraper_prerun, predictor = 4, booted.preds = pdp_list[[3]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[5], x.label = "Nutrients", max.y = 5)
ggsave("scraper4.tiff", units = "in", height = 2, width = 2)


#########################
#########################
#Detritivores

detritivore_prerun <- plot.gbm.4list(output_list[[4]])

#Panel 1
ggPD_boot(output_list[[4]], list.4.preds = detritivore_prerun, predictor = 1, booted.preds = pdp_list[[2]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[3], x.label = "Live coral", max.y = 5)
ggsave("det1.tiff", units = "in", height = 2, width = 2)

#Panel 2
ggPD_boot(output_list[[4]], list.4.preds = detritivore_prerun, predictor = 2, booted.preds = pdp_list[[2]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[3], x.label = "Macroalgae", max.y = 5)
ggsave("det2.tiff", units = "in", height = 2, width = 2)

#Panel 3
ggPD_boot(output_list[[4]], list.4.preds = detritivore_prerun, predictor = 3, booted.preds = pdp_list[[2]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[3], x.label = "Rugosity", max.y = 5)
ggsave("det3.tiff", units = "in", height = 2, width = 2)

#Panel 4
ggPD_boot(output_list[[4]], list.4.preds = detritivore_prerun, predictor = 4, booted.preds = pdp_list[[2]]$function.preds, type.ci = "ribbon", rug = T, common.scale = T,
          cex.line = 1, col.line = col_list[3], x.label = "Dist. offshore", max.y = 5)
ggsave("det4.tiff", units = "in", height = 2, width = 2)
