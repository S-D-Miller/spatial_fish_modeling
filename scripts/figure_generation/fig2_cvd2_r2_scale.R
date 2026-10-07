rm(list=ls())

#Change to your folder
#setwd('~\\spatial_fish_modeling')

#Loads dependencies
library(tidyverse)

#Loads in data
dat <- read.csv("outputs\\comparison\\comparison_2018_2019.csv")

#Summarizes data 
datSum <- dat %>%
  group_by(Species, scale) %>%
  summarize(D2 = mean(D2),
            cvD2 = mean(cvD2),
            predR2 = mean(predR2))

spp_list <- c("all_herb", "fishable", "detritivore", "excavator", "scraper", "browser", "grazer")

dev_diff <- data.frame('Species' = character(), 'cv_diff' = numeric())

for(spp in spp_list){
  
  temp <- datSum %>%
    filter(Species == spp)
  
  cvdiff <- temp$cvD2[temp$scale == 1000] - temp$cvD2[temp$scale == 500]
  
  temp_frame <- data.frame('Species' = spp, 'cv_diff' = cvdiff)
  dev_diff <- rbind(dev_diff, temp_frame)
  
}

#Sets up color, pch, and line type lists for plotting
col_list  <- c("#000000", "#CC79A7", "#009E73", "#D55E00", "#0072B2", "tan", "#F0E442")
#pch_list <- c(16, 15, 17, 18, 25, 8, 4)
pch_list <- c(16, 23, 17, 25, 24, 15, 18)
lty_list <- c(1,2,3,4,5,6,7)

################
#Creates figure for model performance in 2018
################
dev.new()
par(oma = c(0,1,0,0), xpd = NA)
plot(datSum$scale[datSum$Species == spp_list[1]], 
     datSum$cvD2[datSum$Species == spp_list[1]]*100, pch = pch_list[1], cex = 2.5, lwd = 3, type = 'o', lty = lty_list[1],
     xlab = '', ylab = '', ylim = c(-10,60), xlim = c(0, 1000), col = col_list[1], cex.axis = 1.35, xaxt='n', yaxt = 'n')
text(25, 58, labels = "(a)", cex = 2.5)
axis(1, at = c(0, 50, 150, 250, 500, 1000), tick = T, labels = c(0, "", "", 250, 500, 1000), cex.axis = 1.35)
axis(2, at = c(0, 15, 30, 45, 60), tick = T, labels = c(0, 15, 30, 45, 60), cex.axis = 1.35)

for(i in 2:length(spp_list)){
  
  points(datSum$scale[datSum$Species == spp_list[i]], 
         datSum$cvD2[datSum$Species == spp_list[i]]*100, 
         pch = pch_list[i], cex = 2.5, lwd = 3, type = 'o', col = col_list[i], lty = lty_list[i], )
  
}
mtext("% CV deviance explained", side = 2, cex = 1.75, line = 3)
mtext(expression(paste('Transect area (m'^'2', ")", sep = "")), side = 1, cex = 1.75, line = 3)

################
#Creates figure for predictive performance (R^2)
################
#Filters out 1000m^2 due to having few replicates in 2019
datSum_pred <- datSum %>%
  filter(scale != 1000)

dev.new()
par(oma = c(0,1,0,0), xpd = NA)
plot(datSum_pred$scale[datSum_pred$Species == spp_list[1]], 
     datSum_pred$predR2[datSum_pred$Species == spp_list[1]], 
     pch = pch_list[1], cex = 2.5, lwd = 3, type = 'o', xlab = '', ylab = '', lty = lty_list[1],
     ylim = c(-0.1,.6), xlim = c(0, 1000), col = col_list[1], cex.axis = 1.35, xaxt='n', yaxt = 'n')
text(25, .58, labels = "(b)", cex = 2.5)
axis(1, at = c(0, 50, 150, 250, 500, 1000), tick = T, labels = c(0, "", "", 250, 500, 1000), cex.axis = 1.35)
axis(2, at = c(0, .15, 0.3, 0.45, 0.6), tick = T, labels = c(0, 0.15, 0.30, 0.45, 0.60), cex.axis = 1.35)

for(i in 2:length(spp_list)){
  
  points(datSum_pred$scale[datSum_pred$Species == spp_list[i]], 
         datSum_pred$predR2[datSum_pred$Species == spp_list[i]], pch = pch_list[i], cex = 2.5, 
         lwd = 3, type = 'o', col = col_list[i], lty = lty_list[i])
  
  
}
mtext(expression('R'^'2'), side = 2, cex = 1.75, line = 3)
mtext(expression(paste('Transect area (m'^'2', ")", sep = "")), side = 1, cex = 1.75, line = 3)

spp_list_legend <- c("All Herbivores", "Fished", "Detritivore", "Excavator", "Scraper", "Browser", "Grazer")
legend('topright', spp_list_legend, col = col_list, lty = lty_list, pch = pch_list, cex = 1.35)