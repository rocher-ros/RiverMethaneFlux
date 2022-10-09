<<<<<<< HEAD
# Global estimate of Riverine Methane Flux

Project that quantifies CH4 emissions from rivers globally and monthly. This repository contains the code to process raw file, model CH4 concentrations and estimate CH4 emissions from rivers, as well as to produce the acompanying figures. The manuscript associated to this project is currently in preparation.
Main and corresponding author is Gerard Rocher-Ros (g.rocher.ros@gmail.com), which you can contact for questions.

## Setup
Currently the datasets are private in a google drive, so you need access to run the code in this repository. Code is in the `code/` folder, with two categories: 
- Folder `preprocessing/` for all the GIS processing to obtain all spatial predictors for the GRADES River network. This analysis requires many large files and will be available by request.
- Folder `main analysis/` to reproduce the modelling, main results an figures in the paper.

## Preprocessing
The folder `preprocessing/` contains four scripts that do the following:
- Script `1_join_to_GRADES.R` downloads the GRADES river network database and attaches the sites to the corresponding river reach.
- Script `2_new_preds_to_GRADES.R` assigns other predictors to their corresponding GRADES dataset.
- Script `3_hydro_and_k.R` calculates average monthly discharge for GRADS, estimates velocity and the gas transfer velocity.
- Script `4_all_attributes_to_GRADES.R` puts all the attributes of each GRADES reach into one file for later use in the modelling.

## Main analysis
The folder `main analysis/`  contains the following scripts:
- `1_data_preparation.R`, which downloads the datasets needed for the analysis, and attaches all the attributes to the Methane database (GRiMeDB).
- `2_CH4_model_assessment.R`, prepares the data and models CH4 concentrations using random forets models. This script assesses the model performance and produces the figures related to the model.
- `3_CH4_model_predict.R`, uses the random forets model to predict CH4 concentrations to all river reaches in GRADES.
- `4_CH4_upscaling.R`, uses the predicted CH4 concentrations and the hydrological model to upscale CH4 emissions globally.
- `5_GIS_processing.R`, processess the output files onto shapefiles for further outputs as well as for visualization.
- `6_figures.R` uses the model outputs and shapefiles produced to do the main figures of the manuscript.






=======
# Global estimate of Riverine Methane Flux

Project that quantifies CH4 emissions from rivers globally and monthly. This repository contains the code to process raw file, model CH4 concentrations and estimate CH4 emissions from rivers, as well as to produce the acompanying figures. The manuscript associated to this project is currently in preparation.
Main and corresponding author is Gerard Rocher-Ros (g.rocher.ros@gmail.com), which you can contact for questions.

## Setup
Currently the datasets are private, so you need access to run the code in this repository. If you have access to the datasets, they should be placed in `data/processed` for the main analysis, if you want to run all the steps from scratch you will need to ask me for all the raw files as they are quite heavy. In the `data/` folder, there should also be the Global River Methane Database currently in review in ESSD and publicly available at [EDI](https://doi.org/10.6073/pasta/b7d1fba4f9a3e365c9861ac3b58b4a90).

Code is in the `code/` folder, with two categories: 
- Folder `preprocessing/` for all the GIS processing to obtain all spatial predictors for the GRADES River network. This analysis requires many large files and will be available by request.
- Folder `main analysis/` to reproduce the modelling, main results an figures in the paper.

## Preprocessing
The folder `preprocessing/` contains four scripts that do the following:
- Script `1_join_to_GRADES.R` downloads the GRADES river network database and attaches the sites to the corresponding river reach.
- Script `2_new_preds_to_GRADES.R` assigns other predictors to their corresponding GRADES dataset.
- Script `3_hydro_and_k.R` calculates average monthly discharge for GRADS, estimates velocity and the gas transfer velocity.
- Script `4_all_attributes_to_GRADES.R` puts all the attributes of each GRADES reach into one file for later use in the modelling.

## Main analysis
The folder `main analysis/`  contains the following scripts:
- `1_data_preparation.R`, which downloads the datasets needed for the analysis, and attaches all the attributes to the Methane database (GRiMeDB).
- `2_CH4_model_assessment.R`, prepares the data and models CH4 concentrations using random forets models. This script assesses the model performance and produces the figures related to the model.
- `3_CH4_model_predict.R`, uses the random forets model to predict CH4 concentrations to all river reaches in GRADES.
- `4_CH4_upscaling.R`, uses the predicted CH4 concentrations and the hydrological model to upscale CH4 emissions globally.
- `5_GIS_processing.R`, processess the output files onto shapefiles for further outputs as well as for visualization.
- `6_figures.R` uses the model outputs and shapefiles produced to do the main figures of the manuscript.






>>>>>>> 0e776afcaf4903ff153c976d406cd23417b4a631
