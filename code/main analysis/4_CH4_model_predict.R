########################################.
#### R script to model CH4 concentrations in rivers and upscale globally.
#### Author: Gerard Rocher-Ros
#### Last edit: 2022-04-22
########################################.


# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'tidymodels', 'lubridate')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

## Read files ----
## Check if the file is downloaded 
if(file.exists("data/processed/grimeDB_concs_with_grade_attributes.csv") == TRUE) {
  print("files already downloaded")
} else {
  print("go to script 1_data_preparation.R and run to produce the needed file for this script")
}

# Data filtering ----
# First clean the methDB for the useful sites 
# Steps done:
# 1. remove aggregated sites
# 2. remove Downstream of a Dam, Permafrost influenced, Glacier Terminus, downstream of a Point Source,
#    Thermogenically affected, Ditches
grimeDB_attributes <- read_csv("data/processed/grimeDB_concs_with_grade_attributes.csv") %>% 
  filter(Aggregated == "No", distance_snapped < 10000,
         !str_detect(Channel_type,"DD|PI|GT|PS|TH|Th|DIT")) %>%
  mutate( month = month(date)) %>% 
  dplyr::select( COMID, Site_ID, CH4mean, month, GPP_yr:month, 
                 -c(Channel_type, Aggregated, date_end, date, area, T_ECE, S_ECE, slope,
                    temp_month, WaterTemp_degC, WaterTemp_degC_estimated, discharge_measured)) 

colnames(grimeDB_attributes)

#read data of all predictors globally for all COMIDS, to do the predictions
if(file.exists("data/processed/grade_attributes.csv") == TRUE) {
  print("files already downloaded")
} else {
  print("error, file not found. go to repository and download the data")
}


#read the lobal predictors df
global_preds <- read_csv( "data/processed/grade_attributes.csv", lazy=FALSE) 

# we will log transform those to start with
vars_to_log <- c('CH4mean','uparea','popdens','slop' ,'T_OC','S_OC', 'T_CACO3', 'T_CASO4', 'k_month', 'gw_month', 'wetland', 
                 'T_ESP', "N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture", "q_month",
                 "P_gnpp", "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "N_retention_subgrid")

# Select useful variables for the model, some variables were removed due to a high correlation with other ones 
variables_to_remove <- c('Site_ID','COMID','GPP_yr', 'Log_S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH",
                         'npp_month', 'forest', 'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY', "lon", "lat",
                         'S_CASO4', 'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY",
                         "N_aquaculture", "N_gnpp", "N_load", "N_point",  "N_surface_runoff_agri", "N_surface_runoff_nat" )



vars_to_log_glob <-  global_preds %>% 
  select(contains(c("uparea", "popdens", "slop",  "T_OC" ,"T_CACO3", "T_CASO4", "k_", "q_", "gw_", "wetland",   "T_ESP",
                    "N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture", "P_gnpp", 
                    "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "N_retention_subgrid"),
                  ignore.case = FALSE), -wetland_cover, -slope) %>%
  colnames(.)


vars_to_remove_glob <-  global_preds %>% 
  select(contains(c('GPP_yr', 'S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH", "T_ECE", "S_ECE", 
                    'npp_', 'forest',  'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY',
                    'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY", "temp",
                    "N_aquaculture", "N_gnpp", "N_load", "N_point", "N_surface_runoff_agri", "N_surface_runoff_nat"),
                  ignore.case = FALSE), lat, lon, slope, area, -temp_yr) %>%
  colnames(.)


#dataset with some variables log transformed
grimeDB_attr_trans <- grimeDB_attributes %>%
  mutate(across(.cols=all_of(vars_to_log), ~log(.x + .1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", vars_to_log), .cols = all_of(vars_to_log) ) #rename the log transformed variables 


#do same transformations to the global dataset
global_preds_trans <- global_preds %>%
  select(-all_of(vars_to_remove_glob)) %>% 
  mutate(across(.cols=all_of(vars_to_log_glob), ~log(.x + .1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log_glob)), .cols = all_of(vars_to_log_glob) )  %>% #rename the log transformed variables 
  drop_na()

colnames(global_preds_trans)




#prepare a dataset, nesting by month
data_model_monthly <- grimeDB_attr_trans %>% 
  group_by(COMID, month) %>% 
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>% 
  dplyr::select(-all_of(variables_to_remove), COMID) %>% 
  drop_na() %>% 
  left_join(dplyr::select(grimeDB_attr_trans, COMID, biome_label)) %>% 
  dplyr::select(-COMID)

rm(global_preds, grimeDB_attributes, grimeDB_attr_trans)
gc()

#Setup the RF models ----
#with the tuned parameters obtained  in the previous script

n.cores = parallel::detectCores()-1

rf_mod <-
  rand_forest(
    mtry = 13,
    trees = 1200,
    min_n = 8 ) %>%
  set_mode("regression") %>%
  set_engine("ranger",  num.threads = n.cores , keep.inbag = TRUE)

#prepare a workflow with it to feed into the function
wf <-
  workflow() %>%
  add_model(rf_mod)


#Function to predict values to the whole world
predict_methane <- function(data_model, month, global_predictors) {
  
  #select adjacent months for each month
  months_selected <- c(month - 1, month, month + 1)
  if(month == 1){ months_selected <- c(12, 1, 2) }
  if(month == 12){ months_selected <- c(11, 12, 1) }
  
  #now keep those data from the dataframe
  df <- data_model %>%
    filter(month %in% all_of(months_selected)) %>% 
    dplyr::select(-month)
  
  all_months <- tolower(month.abb)
  
  month_selected <- all_months[month]
  
  split <- initial_split(df, prop = .8, strata = biome_label) 
  
  train_sub <- training(split) %>% 
    dplyr::select(-biome_label)
  
  #create recipe
  recipe_train <- train_sub %>% 
    recipe(Log_CH4mean ~.) %>% 
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) 
  
  #fit workflow on train data
  fit_wf <-
    wf %>%
    add_recipe(recipe_train) %>%
    fit(data = train_sub) 

  breaks_df <-  head( round(seq(1, nrow(global_predictors), length.out= 9), 0), -1)
  
  preds_all <- tibble(
    COMID = NULL,
    mean = NULL,
    se = NULL)
  
  for(i in seq_along(breaks_df)){
    
    first_slice = breaks_df[i]
    if(i < length(breaks_df))    
      last_slice = breaks_df[i+1]
    
    if(i == length(breaks_df))
      last_slice = nrow(global_predictors)
    
    df_predictors <- global_predictors %>%
      slice(first_slice:last_slice) %>% 
      dplyr::select(-ends_with(setdiff(all_months, month_selected))) %>%
      rename_all(~ str_replace(., month_selected, "month"))
    
    # preds <- predict(fit_wf$fit$fit$fit, 
    #                  extract_recipe(fit_wf) %>% bake(df_predictors),
    #                  type = "quantiles",
    #                  quantiles = c(0.125, 0.5, 0.875)) %>% 
    #   with(predictions) %>% 
    #   as_tibble() %>% 
    #   set_names(paste0(".pred", c("_lower", "", "_upper"))) 
    # preds_1 <- tibble(COMID = df_predictors$COMID,
    #                   mean = exp(preds$.pred) - .1,
    #                   lower = exp(preds$.pred_lower) - .1,
    #                   upper = exp(preds$.pred_upper) - .1) %>% 
    #   rename_at(vars(mean, lower, upper),  ~ paste0(month.abb[month], "_" , . ) ) 
    
    preds_mod <- predict(fit_wf$fit$fit$fit, 
                         extract_recipe(fit_wf) %>% bake(df_predictors),
                         type = "se",
                         se.method = "infjack") 
    
    
    preds_1 <- tibble(COMID = df_predictors$COMID,
                      mean = exp(preds_mod$predictions) - .1,
                      se = exp(preds_mod$se) - .1) %>% 
      rename_at(vars(mean, se),  ~ paste0(month.abb[month], "_" , . ) ) 
    
    preds_all <- bind_rows(preds_all, preds_1)
    print(paste("done", i))
  }
  
  preds_all %>% 
    write_csv(paste0("data/processed/meth_preds/ch4_preds_uncertainty_", month.abb[month], ".csv" ))
  
  return(preds_all)
}


# Upscale methane globally by month, with uncertainity ----


set.seed(123)

gc()
 
ch4_jan <- predict_methane(data_model_monthly, 1, global_preds_trans )
ch4_jan

ch4_feb <- predict_methane(data_model_monthly, 2, global_preds_trans)
ch4_mar <- predict_methane(data_model_monthly, 3, global_preds_trans)
ch4_apr <- predict_methane(data_model_monthly, 4, global_preds_trans)
ch4_may <- predict_methane(data_model_monthly, 5, global_preds_trans)
ch4_jun <- predict_methane(data_model_monthly, 6, global_preds_trans)
ch4_jul <- predict_methane(data_model_monthly, 7, global_preds_trans)
ch4_aug <- predict_methane(data_model_monthly, 8, global_preds_trans)
ch4_sep <- predict_methane(data_model_monthly, 9, global_preds_trans)
ch4_oct <- predict_methane(data_model_monthly, 10, global_preds_trans)
ch4_nov <- predict_methane(data_model_monthly, 11, global_preds_trans)
ch4_dec <- predict_methane(data_model_monthly, 12, global_preds_trans)

#the function already exports the monthly file so nothing else to do here

