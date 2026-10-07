# Distribution of reef fish biomass is better predicted by seascape characteristics than by benthic community state

## Authors:
Scott D. Miller, Andrew Rassweiler

## Citation:
Miller SD, Rassweiler A (2026) Distribution of reef fish biomass is better predicted by seascape characteristics than by benthic community state. Oecologia. Accepted pending minor revision.

## Study abstract: 
The distribution of species is determined by the joint effects of numerous aspects of the environment. For example, coral reef fish assemblages are simultaneously influenced by the composition of the benthic community, by physical aspects of the environment, and by human activities. Understanding the relative strengths of these associations is necessary for predicting spatial distributions and developing management strategies. To determine the relative importance of seascape context, availability of physical reef habitat, benthic reef state, and anthropogenic impacts in describing the distribution of reef herbivores and species targeted by fishing, we collected extensive spatially-explicit data on fish and benthic organisms in the shallow lagoons of Moorea, French Polynesia. We modeled the biomass of different functional groups of herbivorous fishes and species targeted in the local fishery, testing the optimal scale for predictability by repeating our analysis on different spatial scales. To assess the out-of-sample generalizability of our models, we tested their predictive power on fish survey data collected one year later in new locations. We found that most groups were more associated with variables that describe the seascape context and habitat availability rather than with variables describing the reef state or anthropogenic impacts. Herbivore and fished species biomass tended to be highest in areas further offshore, closer to reef passes, and with more physical habitat. Our results suggest that herbivores and fished species may be resilient to changes in the benthic reef state and indicate the importance of considering seascape context in addition to the benthic reef state.

## Repository README: 

### Downloading associated data
This repository contains all the necessary scripts to replicate results in the associated text. First, the user must download the up-to-date data files from the Environmental Data Initiative (EDI) at this link (DOI: ). Metadata for all data files can be found in the data repository. To make the scripts here work out-of-the-box, the user should move the data files into the following folders:

- fish_survey_data.csv and fishable_species into data --> fish
- partition_data.csv into data --> partitions
- predictor_data.csv into data --> predictors
- autocorr_500m.csv, interactions_500m.csv, and relative_importance_500m.csv into outputs --> 500m_model. Alternatively, these can be generated using the "brt_500m.R" script.
- comparison_2018_2019.csv into outputs --> comparison. Alternatively, this can be generated using the "comparison_analysis_2018_2019.R" script.

### Replicating results and figures
To replicate results, the reader can navigate to the scripts --> analysis folder and run the contained scripts after setting the working directory to the repository parent location on your machine and putting the necessary data from above into the correct locations.  The script “brt_500m.R” is used to generate data on the relative importance of predictor variables, the spatial autocorrelation, and model interactions for the different fish groups at the scale of 500 m2.  This is used to generate Figure 3, and these outputs can be found in outputs --> 500m_model.  The scripts “comparison_analysis_2018_2019.R”, are used to create a boosted regression tree model on fish data from 2018, then use that model to make predictions on fish data from 2019 and save the results of model performance at each spatial scale. These outputs are used to generate Figures 2 and 4 in the text and can be found in outputs --> comparison.

To generate figures in the text, the reader can navigate to scripts --> figure_generation.  Each figure in the text, except the maps in Figures 1 and S1, has an accompanying script to replicate it, although some minor formatting (e.g., combining panels) may have been done in another program. The coordinates for the survey sites shown on Figure 1 can be found in the fish data file.

### Navigating the repository
The three highest level folders are: “data”, “outputs”, and “scripts”.  Broadly, the “data” folder contains the input data used for analysis, the “outputs” folder contains the outputs from analyses that are used in the manuscript, and the “scripts” folder contains scripts to replicate the outputs and generate figures in the text. The subfolders all have .gitkeep files in them to preserve the repository structure in the absence of data files which should be accessed from the EDI repository.

#### “data” folder contains input data for analysis and survey coordinates
- “fish” subfolder contains the fish counts for 2018 and 2019 and a metadata file signaling which species are fished and thus included as fishable biomass.  Each row in the fish count data represent a single observation (species of a given size within a transect segment) and these are processed using the function in “fish_sum_stats_partition.R” script.
- “predictors” contains predictor variables subset by the scale and year at which they were calculated. The EDI repository has full metadata for the columns.
- “partitions” contains information on which partition within a transect that each individual transect segment is assigned.  This allows transect segments to be aggregated into larger spatial units that vary in the spatial scale and serve as the units of replication for other analyses.  Also includes coordinates for the calculation of spatial autocorrelation.

#### “outputs” folder contains outputs from model runs and are used to generate figures and are presented in the text.  These can be replicated by the user using the R scripts contained in the “scripts” folder as described elsewhere.
- “500m_model” subfolder contains outputs from the “brt_500m.R” script and provides model performance metrics, interaction strengths, and spatial autocorrelation from the model trained on data collected in 2018 at the scale of 500m^2, determined to be the scale with optimal predictability.
- "comparison” subfolder contains outputs from the “comparison_analysis_2018_2019.R” series of script.  These provide information on model performance of the 2018 models and the performance of using the model trained on data collected in 2018 to predict fish biomass of different groups using data collected in 2019 at the scales described in the text.

#### “scripts” folder contains scripts for running analyses and generating figures
- “analysis” subfolder contains scripts to run 1) the code to calculate relative importance, interactions, and spatial autocorrelation at the 500m scale from 2018 (brt_500m), and 2) scripts to calculate model performance in 2018 and the predictive performance in 2019 using the 2018 model at the different spatial scales (comparison_2018_2019).  These generate the files that are saved in the outputs folder and used for the figures.
- “figure_generation” subfolder contains scripts to generate figures found in the text.  Each script is named to correspond to a figure in the text.
- “functions” subfolder contains two scripts that contain custom functions to calculate the biomass of fish at different spatial scales “fish_sum_stats_partition.R” and to calculate Moran’s I in the model residuals at different survey scales “spatial_autocorrelation_residual_calculator.R”.  The third script, "ggbrt_functions.R" contains functions from the ggBRT package by Jean-Baptiste Jouffray (https://github.com/JBjouffray/ggBRT). These functions are sourced into other scripts and do not need to be called directly by the user to replicate results.
