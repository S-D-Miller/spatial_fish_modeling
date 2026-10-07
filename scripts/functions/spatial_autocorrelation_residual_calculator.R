#Loads in dependencies
library(ape)
library(sp)
library(tidyverse)

residual_autocorrelation_calculator <- function(allDat, fishDat, gbm_output, coords, resid_type = "Biomass"){
  #Used to calculate Moran's I in raw biomass values and model residuals
  #allDat is the dataframe with all predictors and response (used to create model)
  #fishDat is the dataframe with fish surveys
  #gbm_output is the output from the BRT model
  #coords is the partitions dataframe with coordinates for each partition
  
  #Pulls model residuals from gbm_output and creates a dataframe to hold data
  mod_resids <- gbm_output$residuals
  
  if(resid_type == "Biomass"){
    
    tempFrame <- data.frame('partCode' = allDat$partCode, 'resids' = mod_resids, 'raw_bio' = allDat$Biomass)
    
  }else{
    
    tempFrame <- data.frame('partCode' = allDat$partCode, 'resids' = mod_resids, 'raw_bio' = allDat$log_bio)
    
  }
  
  
  #crs for our two different data types
  bathyProj <- CRS("+proj=utm +zone=6 +south +ellps=GRS80")
  fishProj <- CRS("+proj=longlat +ellps=WGS84")
  
  #Creates coordinate data by merging coords and fishDat
  coorDat <- fishDat %>%
    inner_join(coords, by = 'allCode') %>%
    mutate('partCode' = paste(UniqueCode, partition.x, sep = "_"))
  coorDat <- coorDat %>%
    group_by(partCode) %>%
    summarize(UniqueCode = first(UniqueCode),
              partition = first(partition.x),
              avgLat = mean(avgLat.y),
              avgLon = mean(avgLon.y))
  
  #Merges tempFrame with coorDat and makes spatial.  Then transforms to be in a project crs where units are meters
  tempFrame <- merge(tempFrame, coorDat, by = 'partCode')
  tempFrame_sp <- SpatialPointsDataFrame(as.matrix(tempFrame[c('avgLon','avgLat')]), data = tempFrame, proj4string = fishProj)
  tempFrame_sp <- spTransform(tempFrame_sp, bathyProj)
  
  #Calculates distances between each survey partition from the spatial dataframe
  survey.dists <- as.matrix(dist(cbind(tempFrame_sp$avgLon, tempFrame_sp$avgLat)))
  row.names(survey.dists) <- tempFrame$partCode
  colnames(survey.dists) <- tempFrame$partCode
  
  #Calculates the inverse Euclidean distance matrix for the Moran's I calculation
  survey.dists.inv <- 1/survey.dists
  diag(survey.dists.inv) <- 0
  
  survey.dists.inv_og <- survey.dists.inv
  
  #Calculates the Moran's I on the raw observations and the residuals from the model
  moran_raw <- Moran.I(tempFrame$raw_bio, survey.dists.inv_og)
  moran_og <- Moran.I(tempFrame$resids, survey.dists.inv_og)
  
  #Returns outputs
  return(list(moran_raw, moran_og))
  
}

