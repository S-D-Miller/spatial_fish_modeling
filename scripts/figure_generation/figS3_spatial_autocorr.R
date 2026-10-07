rm(list=ls())

library(tidyverse)

#Change to your folder
#setwd('~\\spatial_fish_modeling')

#Read in data
dat <- read.csv("outputs\\500m_model\\autocorr_500m.csv", stringsAsFactors = F)

plot_dat <- dat %>%
  dplyr::select(species, raw_obs, resid_obs, expected)

x <- barplot(t(as.matrix(cbind(plot_dat$raw_obs, plot_dat$resid_obs))), beside = T, ylim = c(-0.01, 0.4),
        names = c("Grazer", "Browser", "Excavator", "Detritivore", "Scraper", "Fishable", "All"),
        xlab = "Herbivore type", ylab = "Observed Moran's I", col = c("gray30", "gray90"))
segments(-10, plot_dat$expected[1], 100, plot_dat$expected[1], lty = 2)
legend('topright', legend = c("Observations", "Residuals"), fill = c("gray30", "gray90"))
text(x[1,], t(as.matrix(cbind(plot_dat$raw_obs, plot_dat$resid_obs)))[1,] + 0.01,
     labels = c("*", "*", "*", "*", "*", "*", "*"), cex = 1.5)
text(x[2,], t(as.matrix(cbind(plot_dat$raw_obs, plot_dat$resid_obs)))[2,] + 0.01,
     labels = c("", "", "", "*", "", "", ""), cex = 1.5)
