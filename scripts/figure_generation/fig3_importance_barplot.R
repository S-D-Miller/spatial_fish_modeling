rm(list=ls())

#loads necessary packages and sets working directory to repository parent
library(tidyverse)

#Change to your folder
#setwd('~\\spatial_fish_modeling')

#reads in data for the relative importance at 500m2 scale
imp_500 <- read_csv('outputs\\500m_model\\relative_importance_500m.csv')

var_list <- data.frame('var' = c('SoftSub', 'Offshore', 'Pass', 'MeanDepth', 'vrm',
                                 'CoralStd', 'AlgaeStd', 'HardStd', 'NutrientMay', 'mpa'),
                       'plot' = c('Unconsolidated Substrate', 'Offshore', 'Pass', 'Depth', 'Rugosity',
                                  'Coral', 'Algae', 'Hard Substrate', 'Nutrients', 'MPA'),
                       'group' = c('Static', 'Static', 'Static', 'Static', 'Static', 'Dynamic', 'Dynamic',
                                   'Dynamic', 'Dynamic', 'Dynamic'))


#Creates a list of the different fish group names
spp_list <- c('browser', 'grazer', 'detritivore', 'excavator', 'scraper', 'fishable', 'all_herb')

#Casts the dataframe to wide format for plotting
dat1 <- pivot_wider(imp_500,
                    id_cols = var,
                    names_from = species,
                    values_from = rel.inf)
dat1 <- dat1[,c('var', 'all_herb', 'fishable', 'detritivore', 'excavator', 'scraper')]

##################
##################
#HUMAN
##################
##################
var_list_human <- data.frame('var' = c('SoftSub', 'Offshore', 'Pass', 'MeanDepth', 'vrm',
                                       'CoralStd', 'AlgaeStd', 'HardStd', 'NutrientMay', 'mpa'),
                             'plot' = c('MPA', 'Nutrients', 'Turf/CCA', 'Macroalgae', 'Coral',
                                        'Soft bottom', 'Depth', 'Rugosity', 'Pass', 'Offshore'),
                             'group' = c('Environment', 'Seascape', 'Seascape', 'Seascape', 'Seascape', 
                                         'Environment', 'Environment',
                                         'Environment', 'Human', 'Human'))
desired_order <- c("Offshore", "Pass", "MeanDepth", "vrm",
                   "SoftSub", "CoralStd", "AlgaeStd", "HardStd", "nutrientMay",
                   "mpa")
#Reorders the dataframe so static and dynamic are together for better plotting aesthetics
plot_seascape <- dat1[match(desired_order, dat1$var),]

col_list_human <- c(colorRampPalette(c("#612B00", "yellow1"))(4),
                    'gray70',
                    colorRampPalette(c("#000075", "cyan1"))(3),
                    "orchid2","magenta")

dev.new()
par(oma = c(4,3,0,0))
barplot(as.matrix(plot_seascape[,2:6]), beside = T, horiz = F, 
        names.arg = c("","","","",""),
        las = 2, cex.names = 1.25, 
        col = (col_list_human), cex.axis = 2,
        ylim = c(0,30), main = "")
segments(-100,10,100,10, col = 'black', lty = 2, lwd = 1.5)
mtext('Relative Importance (%)', side = 4, cex = 1.5, line = 3)

#A plot for just the legend
dev.new()
plot(0,0,xlim = c(-1,10))
legend('topright', rev(var_list_human$plot), fill = col_list_human, cex = 1.25)


###################
###################
#BARPLOT
###################
###################

var_list_bar <- data.frame('var' = c('SoftSub', 'Offshore', 'Pass', 'MeanDepth', 'vrm',
                                     'CoralStd', 'AlgaeStd', 'HardStd', 'nutrientMay', 'mpa'),
                           'plot' = c('Unconsolidated Substrate', 'Offshore', 'Pass', 'Depth', 'Rugosity',
                                      'Coral', 'Algae', 'Hard Substrate', 'Nutrients', 'MPA'),
                           'group' = c('Substrate', 'Seascape', 'Seascape', 'Seascape', 
                                       'Seascape', 'Environment', 'Environment',
                                       'Environment', 'Human', 'Human'))

bar_dat <- merge(imp_500, var_list_bar, by = 'var') %>%
  group_by(species, group) %>%
  summarize(tot_inf = sum(rel.inf))

bar_plot <- matrix(bar_dat$tot_inf, nrow = 4, ncol = 7)
rownames(bar_plot) <- c("Environment", "Human", "Seascape", "Substrate")
colnames(bar_plot) <- c("Herbivore", "Browser", "Detritivore", "Excavator",
                        "Fished", "Grazer", "Scraper")

bar_plot <- bar_plot[c("Seascape", "Substrate", "Environment", "Human"),]
bar_plot <- bar_plot[,c("Herbivore", "Fished", "Scraper", "Excavator",
                        "Detritivore", "Browser", "Grazer")]

col_list <- c("goldenrod2", "gray70", "deepskyblue4", "darkorchid3")

#Filter for just the groups predicted well
bar_plot2 <- bar_plot[,c("Herbivore", "Fished", "Detritivore", "Excavator", "Scraper")]

#Make the group barplot
dev.new()
par(oma = c(7,3,0,0))
barplot(bar_plot2, beside = F, col = col_list, las = 2,
        cex.axis = 2, cex.names = 2)
mtext("Group", 1, line = 10, cex = 2.5)

#Plot for just the legend
dev.new()
plot(0,0, xlim = c(5,15))
legend('right', fill = rev(col_list), legend = rev(c("Seascape", "Habitat", 
                                                     "Reef state", "Anthropogenic")),
       cex = 2)


#mtext("Relative importance (%)", 2, line = 4.5, cex = 2.5)
