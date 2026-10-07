rm(list=ls())

library(tidyverse)

#Change to your folder
#setwd('~\\spatial_fish_modeling')

fish_dat <- read_csv("data\\fish\\fish_survey_data.csv") %>%
  filter(Year == 2018) #Filter for 2018 because that's the year we made the models
notrio <- fish_dat[!(fish_dat$taxonomy == 'Acanthurus triostegus' & fish_dat$Number > 10),]

scrapers <- fish_dat %>%
  filter(Herb_Type == "Scraper") %>%
  group_by(taxonomy) %>%
  summarize(tot_biomass = sum(mass)) %>%
  mutate(relative_bio = tot_biomass / sum(tot_biomass) * 100) %>%
  arrange(desc(relative_bio))

excavators <- fish_dat %>%
  filter(Herb_Type == "Excavator") %>%
  group_by(taxonomy) %>%
  summarize(tot_biomass = sum(mass)) %>%
  mutate(relative_bio = tot_biomass / sum(tot_biomass) * 100) %>%
  arrange(desc(relative_bio))

browsers <- fish_dat %>%
  filter(Herb_Type == "Browser") %>%
  group_by(taxonomy) %>%
  summarize(tot_biomass = sum(mass)) %>%
  mutate(relative_bio = tot_biomass / sum(tot_biomass) * 100) %>%
  arrange(desc(relative_bio))

detritivore <- fish_dat %>%
  filter(Herb_Type == "Detritivore") %>%
  group_by(taxonomy) %>%
  summarize(tot_biomass = sum(mass)) %>%
  mutate(relative_bio = tot_biomass / sum(tot_biomass) * 100) %>%
  arrange(desc(relative_bio))

grazers <- notrio %>%
  filter(Herb_Type == "Grazer") %>%
  group_by(taxonomy) %>%
  summarize(tot_biomass = sum(mass)) %>%
  mutate(relative_bio = tot_biomass / sum(tot_biomass) * 100) %>%
  arrange(desc(relative_bio))

#SCRAPER GRAPH
dev.new()
par(oma = c(7,2,0,0), xpd = NA)
barplot(scrapers$relative_bio, 
        ylab = "", xlab = "", las = 2,
        names.arg = scrapers$taxonomy, ylim = c(0,100),
        cex.lab = 2)
mtext("Species", 1, line = 10, cex = 2)
mtext("Percent of biomass", 2, line = 3, cex = 2)

#EXCAVATOR GRAPH
dev.new()
par(oma = c(7,2,0,0), xpd = NA)
barplot(excavators$relative_bio, 
        ylab = "", xlab = "", las = 2,
        names.arg = c("Chlorurus spilurus", excavators$taxonomy[2:3]), ylim = c(0,100),
        cex.lab = 2)
mtext("Species", 1, line = 10, cex = 2)
mtext("Percent of biomass", 2, line = 3, cex = 2)

#BROWSER GRAPH
dev.new()
par(oma = c(7,2,0,0), xpd = NA)
barplot(browsers$relative_bio, 
        ylab = "", xlab = "", las = 2,
        names.arg = browsers$taxonomy, ylim = c(0,80),
        cex.lab = 2)
mtext("Species", 1, line = 10, cex = 2)
mtext("Percent of biomass", 2, line = 3, cex = 2)

#DETRITIVORE GRAPH
dev.new()
par(oma = c(7,2,0,0), xpd = NA)
barplot(detritivore$relative_bio, 
        ylab = "", xlab = "", las = 2,
        names.arg = detritivore$taxonomy, ylim = c(0,100),
        cex.lab = 2)
mtext("Species", 1, line = 10, cex = 2)
mtext("Percent of biomass", 2, line = 3, cex = 2)

#GRAZER GRAPH
dev.new()
par(oma = c(7,2,0,0), xpd = NA)
barplot(grazers$relative_bio, 
        ylab = "", xlab = "", las = 2,
        names.arg = grazers$taxonomy, 
        ylim = c(0,40),
        cex.lab = 2)
mtext("Species", 1, line = 10, cex = 2)
mtext("Percent of biomass", 2, line = 3, cex = 2)