library(tidyverse)

fish_sum_stats_partition <- function(allDat, dat, taxonomy = "all", species) {
  #Generates summary stats from fish counts
  #allDat is the UNALTERED dataframe of ALL fish observations -- used to generate areas for each minute
  #dat is the dataframe of fish counts -- can be filtered based on what you want to subset by -- if you're not subsetting, just use the same as allDat
  #taxonomy is the column title to be used to filter the fish: takes "all", "family", "genus", "taxonomy", "Coarse_Trophic", "Fine_Trophic", "Herb_Type"
  #Species is then the value of the taxonomy field you want to filter
  #ex, if you used "taxonomy" as the value for the taxonomy field, then this is a scientific name, but if you used "Herb_Type" this would be "Browser" or "Scraper" or whatever
  
  #Creates an empty dataframe using all the fish counts to fill in filtered-out transects.  
  #Sums density and biomass at the minute level (lowest unit of replication), but takes mean of area (because it doesn't change across a minute)
  #Then sums density and biomass at partition level and divides by the total area of the partition
  all_fish <- allDat %>%
    group_by(partCode, Minute) %>%
    summarize(
      UniqueCode = first(UniqueCode),
      partition = first(partition),
      area = mean(area),
      Biomass = sum(mass, na.rm=TRUE),
      Density = sum(Number, na.rm=TRUE)
    ) %>%
    group_by(partCode) %>%
    summarize(
      UniqueCode = first(UniqueCode),
      partition = first(partition),
      area = sum(area),
      Biomass = (sum(Biomass) / sum(area)),
      Density = (sum(Density) / sum(area))
    )
  
  #Creates the empty_fish dataframe where the areas are preserved, but biomass and density are na
  empty_fish <- all_fish
  empty_fish[, c("Biomass", "Density")] <- NA
  
  site_areas <- empty_fish[,c("partCode", "area")]
  
  #If 'all' fishes are selected, then the dataframe is returned (no need to continue)
  if (taxonomy == 'all') {
    
    return_fish <- dat %>%
      group_by(partCode, Minute) %>%
      summarize(
        area = mean(area),
        Biomass = sum(mass) / mean(area),
        Density = sum(Number) / mean(area),
        partCode = first(partCode)
      ) 
    
  } else {
    
    #If taxonomy is something else, it moves to this step where the data are filtered and merged onto the empty frame
    return_fish <- dat[dat[,taxonomy] == species,] %>%
      group_by(partCode, Minute) %>%
      summarize(
        area = mean(area),
        Biomass = sum(mass, na.rm=TRUE),
        Density = sum(Number, na.rm=TRUE)
      )
  }
  
  #Merges the biomass values from return_fish onto the empty_fish dataframe
  #Saves the area from empty_fish and the biomass/density from return_fish
  #If biomass or density is NA, they go to 0 (NA in return_fish means those values were filtered out by taxonomy)
  return_fish <- merge(empty_fish, return_fish, by = 'partCode', all.x= TRUE) %>%
    dplyr::select(partCode, UniqueCode, partition, Minute, area.x, Biomass.y, Density.y)
  colnames(return_fish) <- c('partCode', 'UniqueCode', 'Partition', 'Minute', 'area', 'Biomass', 'Density')
  return_fish$Biomass[is.na(return_fish$Biomass)] <- 0
  return_fish$Density[is.na(return_fish$Density)] <- 0
  
  #Recalculates the biomass and density scaled to the area of the entire partition (even values that had no fish due to filtering)
  return_fish <- return_fish %>%
    group_by(partCode) %>%
    summarize(
      UniqueCode = first(UniqueCode),
      Partition = first(Partition),
      area = mean(area),
      Biomass = (sum(Biomass) / mean(area)),
      Density = (sum(Density) / mean(area))
    )
  
  return(return_fish)
  
}
