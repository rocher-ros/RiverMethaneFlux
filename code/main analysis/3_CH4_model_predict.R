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
  filter(`Aggregated?` == "No",
         !str_detect(Channel_type,"DD|PI|GT|PS|TH|Th|DIT")) %>%
  mutate( month=month(date)) %>% 
  dplyr::select( COMID, Site_Nid, CH4mean, month, GPP_yr:month, 
                 -c(Channel_type, `Aggregated?`, date_end, date, area, T_ECE, S_ECE, slope,
                    temp_month, WaterTemp_actual, WaterTemp_est, discharge_measured)) 

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
variables_to_remove <- c('Site_Nid','COMID','GPP_yr', 'Log_S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH",
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
  mutate(across(.cols=all_of(vars_to_log), ~log(.x+.1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log)), .cols = all_of(vars_to_log) ) #rename the log transformed variables 


#do same transformations to the global dataset
global_preds_trans <- global_preds %>%
  select(-all_of(vars_to_remove_glob)) %>% 
  mutate(across(.cols=all_of(vars_to_log_glob), ~log(.x + .1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log_glob)), .cols = all_of(vars_to_log_glob) )  %>% #rename the log transformed variables 
  drop_na()

colnames(global_preds_trans)

rm(global_preds, grimeDB_attributes)
gc()




#Setup the RF models ----
#with the tuned parameters obtained  in the previous script

n.cores = parallel::detectCores()-1

rf_mod <-
  rand_forest(
    mtry = 8,
    trees = 1000,
    min_n = 8 ) %>%
  set_mode("regression") %>%
  set_engine("ranger", num.threads = n.cores)

#prepare a workflow with it to feed into the function
wf <-
  workflow() %>%
  add_model(rf_mod)


#Function to predict values to the whole world
predict_methane <- function(data_model, month, global_predictors, m_res) {
  
  #select adjacent months for each month
  months_selected <- c(month - 1, month, month + 1)
  if(month == 1){ months_selected <- c(12, 1, 2) }
  if(month == 12){ months_selected <- c(11, 12, 1) }
  
  #now keep those data from the dataframe
  df <- data_model %>%
    filter(month %in% all_of(months_selected)) %>% 
    dplyr::select(-month)
  
  preds_all <- tibble( COMID = global_predictors$COMID)
  
  for (i in 1:m_res ){
    print(i)
  
  split <- initial_split(df, prop = .7, strata = biome_label) 
  
  train_sub <- training(split) %>% 
    select(-biome_label)
  
  #create recipe
  recipe_train <- train_sub %>% 
    recipe(Log_CH4mean ~ . )
  
  #fit workflow on train data
  fit_wf <- wf %>%
    add_recipe(recipe_train) %>%
    fit(data = train_sub) 

  all_months <- tolower(month.abb)
  
  month_selected <- all_months[month]
  
  df_predictors <- global_predictors %>%
    select(-ends_with(setdiff(all_months, month_selected))) %>%
    rename_all(~ str_replace(., month_selected, "month"))
  
  preds <- predict(fit_wf, df_predictors)

  preds_all <- preds_all %>% 
    add_column( !! paste0("rep_", i) := preds$.pred)

  }
  
  data_out <- preds_all %>%
    mutate(COMID= as.character(COMID)) %>%
    mutate(across(where(is.numeric),  ~ exp(.x) - .1 )) %>% 
    pivot_longer(-COMID, names_to = "rep", values_to = "values") %>% 
    group_by(COMID) %>% 
    summarise(mean = mean(values, na.rm = TRUE),
              sd = sd(values, na.rm = TRUE))  %>% 
    rename_at(vars(mean, sd),  ~ paste0(month.abb[month], "_" , . ) ) 
  
  data_out %>% 
  write_csv(paste0("data/processed/meth_preds/ch4_preds_uncertainty_", month.abb[month], ".csv" ))
  
  yay <- "done"
  return(yay)
}


# Upscale methane globally by month, with uncertainity ----
n_boot = 100

#prepare a dataset, nesting by month
data_model_monthly <- grimeDB_attr_trans %>% 
  group_by(COMID, month) %>% 
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>% 
  dplyr::select(-all_of(variables_to_remove), COMID) %>% 
  drop_na() %>% 
  left_join(dplyr::select(grimeDB_attr_trans, COMID, biome_label)) %>% 
  dplyr::select(-COMID)


set.seed(123)


ch4_jan <- predict_methane(data_model_monthly, 1, global_preds_trans, n_boot)
ch4_feb <- predict_methane(data_model_monthly, 2, global_preds_trans, n_boot)
ch4_mar <- predict_methane(data_model_monthly, 3, global_preds_trans, n_boot)
ch4_apr <- predict_methane(data_model_monthly, 4, global_preds_trans, n_boot)
ch4_may <- predict_methane(data_model_monthly, 5, global_preds_trans, n_boot)
ch4_jun <- predict_methane(data_model_monthly, 6, global_preds_trans, n_boot)
ch4_jul <- predict_methane(data_model_monthly, 7, global_preds_trans, n_boot)
ch4_aug <- predict_methane(data_model_monthly, 8, global_preds_trans, n_boot)
ch4_sep <- predict_methane(data_model_monthly, 9, global_preds_trans, n_boot)
ch4_oct <- predict_methane(data_model_monthly, 10, global_preds_trans, n_boot)
ch4_nov <- predict_methane(data_model_monthly, 11, global_preds_trans, n_boot)
ch4_dec <- predict_methane(data_model_monthly, 12, global_preds_trans, n_boot)

#the function already exports the monthly file so nothing else to do here

