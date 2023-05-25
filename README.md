# Global methane emissions from rivers and streams


In this project we quantify and undertsand global CH4 emissions from rivers. The analysis in this repository is associated to a manuscript currently in minor revisions.
This repository contains the code to process raw files, model CH4 concentrations and estimate CH4 emissions from rivers, as well as to produce the acompanying figures of the manuscript.
First and corresponding author is me so you can contact for questions (g.rocher.ros at gmail dot com).

# Setup
This is all in R so you need ot know some R and have it installed, together with Rstudio. Specific packages are installed within each script if needed.

First you need to download the data files and datasets used in this study. For the `main analysis`section, the files are deposited in [ZENODO](https://doi.org/10.5281/zenodo.7733604)
You should place those files in the folder `data/processed` for the main analysis. 
If you want to run all the steps from scratch, that is the pre-processing of GIS files, you will need to ask me for all the raw files as they are quite heavy. 
You will also need the Global River Methane Database (Stanley et al. 2023). The data paper is currently accepted in [ESSD](https://essd.copernicus.org/preprints/essd-2022-346/) and the database is publicly available at [EDI](https://doi.org/10.6073/pasta/b7d1fba4f9a3e365c9861ac3b58b4a90).

Code to run this analysis is in the `code/` folder, placed in two major directories: 
- Folder `preprocessing/` contains all the scripts for the GIS processing to obtain all spatial predictors for the GRADES River network. 
- Folder `main analysis/` contains all the scripts to reproduce the random-forest modelling, upscaling procedures, GIS post-processing, main results and figures in the paper.

Hardware used: I tested this on windows 10 and macOS Catalina (10.15). Most parts run fine in a decent laptop, but the scripts `4_CH4_model_predict.R`, `5_CH4_upscaling.R`, `6_CH4_upscaling_uncertainty.R` and `7_GIS_processing.R` have been on the limit with a MacBook pro M1max and 64 GB of RAM, so beware on the memory side.

## Preprocessing
The folder `preprocessing/` contains four scripts that do the following:
- Script `1_join_to_GRADES.R` downloads the GRADES river network database and attaches the sites to the corresponding river reach.
- Script `2_new_preds_to_GRADES.R` assigns other predictors to their corresponding GRADES dataset.
- Script `3_hydro_and_k.R` calculates average monthly discharge for GRADS, estimates velocity and the gas transfer velocity.
- Script `4_all_attributes_to_GRADES.R` puts all the attributes of each GRADES reach into one file for later use in the modelling.

## Main analysis
The folder `main analysis/`  contains the following scripts:
- `1_data_preparation.R`, which downloads the datasets needed for the analysis, and attaches all the attributes to the Methane database (GRiMeDB).
- `2_CH4_model_selection.R`, assesses the data, tries different ML models for the task.
- `3_CH4_model_tuning.R`, tunes the random forest modeland predicts CH4 concentrations monthly. This script also assesses the model performance and produces the figures related to the model.
- `4_CH4_model_predict.R`, uses the random forest model to predict CH4 concentrations to all river reaches in GRADES.
- `5_CH4_upscaling.R`, uses the predicted CH4 concentrations and the hydrological model to upscale CH4 emissions globally.
- `6_CH4_upscaling_uncertainty.R`, repeats the same procedure as the previous script but pushing a montecarlo to perform an uncertainty and sensitivity analysis on the model.
- `7_GIS_processing.R`, processess the output files onto shapefiles for further outputs as well as for visualization.
- `8_figures.R` uses the model outputs and shapefiles produced to do the main figures of the manuscript.

