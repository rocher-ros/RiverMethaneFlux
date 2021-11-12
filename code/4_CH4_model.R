# Info ------
# Author: Gerard Rocher-Ros
# Script to model CH4 concentrations in rivers globally.
# Part 1 is to clean and transform data needed for the model


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'tidymodels', 'googledrive' ,  'lubridate', 'vip', 'corrr', 'ggpubr', 'DALEXtra')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)



## Read files ----
## Download the GRiMeDB with GRADES 
if(file.exists("data/processed/grimeDB_concs_with_grade_attributes.csv") == TRUE) {
  print("files already downloaded")
} else {
  drive_download(
    "SCIENCE/PROJECTS/RiverMethaneFlux/processed/grimeDB_concs_with_grade_attributes.csv",
    path = "data/processed/grimeDB_concs_with_grade_attributes.csv",
    overwrite = TRUE
  )
}

# 1. Data filtering ----
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
          -c(Channel_type, `Aggregated?`, date_end, date, area, T_ECE, S_ECE, 
             temp_month, WaterTemp_actual, WaterTemp_est, discharge_measured)) 

colnames(grimeDB_attributes)
  
ggplot(grimeDB_attributes)+
  geom_point(aes(wetland, wetland_class))

# Explore the raw data and process before the modelling

#histograms of all variables, some will need logtansformation
grimeDB_attributes %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value" ) %>% 
  ggplot(aes(value))+
  geom_histogram(bins = 80) +
  theme_classic()+
  facet_wrap(~variable, scales='free')

#ggsave("figures/histograms_rawdata.png", width=16, height = 12)



# we will log transform those to start with
vars_to_log <- c('CH4mean','uparea','popdens','slop' ,'T_OC','S_OC', 'T_CACO3', 'T_CASO4', 'k_month', 'gw_month', 'wetland', 
                 'T_ESP', "N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture",
                 "P_gnpp", "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "P_retention_subgrid")

# Select useful variables for the model, some variables were removed due to a high correlation with other ones 
variables_to_remove <- c('Site_Nid','COMID','GPP_yr', 'Log_S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH",
                         'npp_month', 'forest', 'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY',
                         'S_CASO4', 'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY",
                         "N_aquaculture", "N_gnpp", "N_load", "N_point", "N_retention_subgrid",  "N_surface_runoff_agri", "N_surface_runoff_nat" )


#dataset with some variables log transformed
grimeDB_attr_trans <- grimeDB_attributes %>%
  #group_by(Site_Nid) %>% # this was to check if grouping by sites (collapsing temporal variability) changes things. It does
  #summarise(across(everything(), mean, na.rm=TRUE)) %>% 
  mutate(across(.cols=all_of(vars_to_log), ~log(.x+.1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log)), .cols = all_of(vars_to_log) ) #rename the log transformed variables 

grimeDB_attr_trans %>% 
  select(-all_of(variables_to_remove)) %>% 
  colnames(.)

labeller_vars <-  c("Log_CH4mean" = "Log CH4 (umol/L)", "month"= "month", "NPP_yr" = "NPP (yearly)", "elev" = "elevation (m)",
                    "Log_slop" = "Log slope (unitless)", "Log_popdens" = "Log population density (people km2)", "temp_yr" = "Avg yearly temperature (°C)",
                    "prec_yr" = "Average yearly precipitation (mm)", "T_GRAVEL" = "Sil gravel (%)", "T_SAND"='Soil sand (%w)',
                    "T_SILT" ='Soil silt (%w)', "T_CLAY"='Soil clay (%w)', "pyearRS" = "Total yearly soil respiration",
                    "Log_T_OC" = "Log Total organic Carbon", "Log_T_CACO3" = "Log CACO3 (%w)" , "Log_T_CASO4" = "Log CASO4 (%w)",
                    "Log_T_ESP" = "Sodicity (%)", "S_PH_H2O" = "Soil pH", "T_CEC_SOIL" = "Soil cation exchange capacity",
                    "S_BS" = "Base saturation", "S_TEB" = "Soil TEB", "T_BULK_DENSITY" = "Soil bulk density", "Log_uparea" ="Log Catchment area",
                    "Log_wetland" ="Log wetland cover (%)", "trees" = "Tree cover (%)", "Log_gw_month" ="Log groundwater table depth (m)" ,
                    "Log_k_month" = "Log gas transfer velocity (m/d)", "gpp_month" = "Monthly GPP", "precip_month" = "Monthly precipitation",
                    "tavg_month" = "Monthly temperature", "sresp_month" = "Soil respiration", "other_nat_veg" = "Other land cover (yes/no)",
                    "cropland" = "Cropland land cover (yes/no)", "urban" = "Urban land cover (yes/no)",
                    "sparse_veg"= "Sparse vegetation land cover (yes/no)" , "wetland_class"= "Wetland land cover (yes/no)",
                    "ice"= "Ice land cover (yes/no)", "water_class" = "Water land cover (yes/no)" )



#Do again some histograms with the log transformed variables
# some variables are still very skewed, usually the anthropogenic predictors that have lots of 0s
grimeDB_attr_trans %>% 
  select(-all_of(variables_to_remove)) %>% 
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value" ) %>% 
  ggplot(aes(value))+
    geom_histogram(bins = 80) +
    theme_classic()+
    facet_wrap(~predictor, scales='free')

#ggsave("figures/histograms_transformed.png", width=16, height = 12)



#pearson correlations for CH4 with the predictors
corr_ch4 <- grimeDB_attr_trans %>% 
  correlate() %>% 
  focus(Log_CH4mean)

grimeDB_attr_trans %>% 
  correlate() %>% 
  rplot(shape = 20, colors = c("red", "green"), print_cor = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/correlations_predictors.png", width=16, height = 12)

#check the coefficients in the console
corr_ch4 %>% 
  arrange(desc(abs(Log_CH4mean))) %>% 
  print(n=50)


#get the r coefficients from the df for later plotting
vars_for_plot <- corr_ch4 %>% 
  arrange(desc(abs(Log_CH4mean))) %>% 
  mutate(predictor=fct_reorder(term, desc(abs(Log_CH4mean)))) %>% 
  select(-term) %>% 
  filter(!predictor %in% c("COMID", "Site_Nid", "month"))


#scatter plots of all variables, sorted from highest to lower r. 
# Showing a loess fit to capture potential nonlinearities that might be useful to assess. don't run it often as it takes a while...
 plot_correlations <- grimeDB_attr_trans %>% 
  pivot_longer(all_of(vars_for_plot$predictor), names_to = "predictor", values_to = "value" ) %>% 
  mutate(predictor=fct_relevel(predictor, levels(vars_for_plot$predictor))) %>% 
  ggplot(aes(value, Log_CH4mean))+
  geom_hex(bins = 30) +
  geom_smooth(method = "loess", se=FALSE, color="red3")+
  geom_text( data = vars_for_plot, 
             mapping = aes(x = Inf, y = Inf, label = paste("r=",round(Log_CH4mean,2))), hjust   = 3.5, vjust   = 1.5, color="red")+
  theme_classic()+
  scale_fill_continuous(type = "viridis", trans = "log10")+
  facet_wrap(~predictor, scales='free')

 
ggsave(filename= "figures/correlations.png", plot=plot_correlations, width = 18, height = 12)

# 2.  RF model using tidymodels ----
 
#Links I have been looking at for this: 
# https://www.tidymodels.org/start/recipes/
# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/


##2.1 parameter tuning  for the RF, takes several hours so welcome to skip it ----
## First run will be with the model with average values by site, to see the broad performance and tune RF parameters
 #remove variables that are highly correlated
 data_for_model <-  grimeDB_attr_trans %>% 
   drop_na() %>% 
   group_by(Site_Nid) %>% 
   summarise(across(everything(), mean)) %>% 
   select(-all_of(variables_to_remove))
 

#prep the dataset into a training and testing one
grime_split <- initial_split(data_for_model) 

#have a look
grime_split %>% 
  training() %>%
  glimpse()

#Model recipe, predict CH4, center and scale all predictors
grime_recipe <-  training(grime_split) %>%
  recipe(Log_CH4mean ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) 

#prepare the testing df by passing on the recipe
grime_prep <- prep(grime_recipe) 

grime_prep$steps

#prepare the training dataset
grime_training <- juice(prep(grime_recipe))

# we will tune the hyperparameters of the RF, so we prepare the model for that and put it in a workflow
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune() ) %>%
  set_mode("regression") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(grime_recipe) %>%
  add_model(tune_spec)

#prepare the folds for the tuning
set.seed(234)
trees_folds <- vfold_cv(training(grime_split))


# Now run a bunch of models in parallel with different combinations of parameters to find what works best
doParallel::registerDoParallel()
set.seed(345)

tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res

# lets have a look...
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

#Do a more targeted tuning with a new grid
rf_grid <- grid_regular(
  mtry(range = c(20, 40)),
  min_n(range = c(25, 40)),
  levels = 5
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)


#select the best one and finish the model 
best_auc <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Log_CH4mean ~ .,
      data = juice(grime_prep)
  ) %>%
  vip(geom = "point")


final_wf <- workflow() %>%
  add_recipe(grime_recipe) %>%
  add_model(final_rf)

#get the model from the last fit
final_res <- final_wf %>%
  last_fit(grime_split)

#perf. metrics
final_res %>%
  collect_metrics()


## 2.2  Run the models ----
#with the tuned parameters obtained with thw whole dataset
# ref: https://stackoverflow.com/questions/62687664/map-tidymodels-process-to-a-list-group-by-or-nest

#Custom function to map it over month, with the parameters obtained from the tuning
#setup of the RF model
rf_mod <-
  rand_forest(
    mtry = 20,
    trees = 1000,
    min_n = 25 ) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance="permutation")

#prepare a workflow with it to feed into the function
wf <-
  workflow() %>%
  add_model(rf_mod)


## big function of model fitting and predicting
predict_RF_grime <- function(df) {
  split <- initial_split(df )
  train_df <- training(split)
  test_df <- testing(split)
  
  #create recipe
  recipe_train <- train_df %>% 
    recipe(Log_CH4mean ~.) 
    # %>%
   # step_corr(all_predictors()) %>%
  #  step_center(all_predictors(), -all_outcomes()) %>%
  #  step_scale(all_predictors(), -all_outcomes()) 
  
  #fit workflow on train data
  fit_wf <-
    wf %>%
    add_recipe(recipe_train) %>%
    fit(data = train_df)
  
  #predict on test data
 preds <- predict(fit_wf, test_df) %>% 
   bind_cols(test_df)
 
model <- fit_wf  %>%
  extract_fit_parsnip()

return(list(preds, fit_wf, train_df))

}

## Run the model via map for each month ----

#prepare a dataset, nesting by month
data_model_nested <- grimeDB_attr_trans %>% 
  select(-all_of(variables_to_remove)) %>% 
  drop_na() %>% 
  group_by(month) %>% 
  nest() %>% 
  arrange(month)

set.seed(123)

#Run the model for each month in a map
monthly_models <- data_model_nested %>%
  mutate(month_label = tolower(month.abb[month]),
         model_out = map(data, possibly(predict_RF_grime, otherwise = NA))) %>% 
  rowwise() %>%
  mutate( preds = model_out[1],
    model_fit = model_out[2]) %>% 
  select(-data, -model_out)


#plot them  
monthly_models %>% 
  unnest(preds) %>% 
ggplot( aes(.pred, Log_CH4mean))+
  geom_point(alpha=.6)+
  geom_abline(slope=1, intercept = 0)+
  stat_cor(aes(label = ..rr.label..), label.y.npc = 0.9)+ #put R2 and label
  labs(x="CH4 predictions", y="CH4 observations", title="one model for each month")+
  theme_bw()+
  facet_wrap(~month)

ggsave(filename= "figures/model_perf_monthly.png", width = 12, height = 8)


## run the data on the whole dataset, not nesting by month ----

data_model <- grimeDB_attr_trans %>%
  select(-all_of(variables_to_remove)) %>% 
  drop_na()

set.seed(123)

#Run the model for each month in a map
yearly_model <- predict_RF_grime(data_model)

yearly_preds <- yearly_model[1] %>% as.data.frame()

yearly_model[[2]] %>%
  extract_fit_parsnip() %>% 
  vi() %>% 
  filter(Importance >.13) %>% 
  ggplot( aes(x=Importance, 
           y= reorder(Variable, Importance, FUN = stats::median)))+
  geom_col( color = "gray80", fill="red4")+
  theme_bw()+
  labs(y="")

ggsave(filename = "figures/VIP_scores_mean.png")

#observation vs predictions 
yearly_preds %>% 
  ggplot( aes(.pred, Log_CH4mean))+
  geom_point(alpha=.6)+
  geom_abline(slope=1, intercept = 0)+
  stat_cor(aes(label = ..rr.label..), label.y.npc = 0.9)+ #put R2 and label
  labs(x="CH4 predictions", y="CH4 observations", title="one model for all data")+
  theme_bw()

ggsave(filename= "figures/model_perf_yearly.png", width = 12, height = 8)

#partial dependence plots 
rf_fit <- yearly_model[[2]] %>% 
  fit(data = yearly_model[[3]])

explainer_rf <- explain_tidymodels(
  rf_fit, 
  data = dplyr::select(yearly_train, -Log_CH4mean), 
  y = yearly_train$Log_CH4mean,
  label = "random forest"
)

pdp_rf <- model_profile(explainer_rf, N = 10000)

plot(pdp_rf)

ggsave(filename= "figures/partial_depend.png", width = 12, height = 8)

#residuals
yearly_preds %>% 
  ggplot( aes(.pred, Log_CH4mean-.pred))+
  geom_point()+
  geom_hline(yintercept = 0, lineetyoe=2)+
  labs(x="CH4 predictions", y="Residuals")+
  theme_bw()

# 3. Predict values to the whole world ----




#read data of all predictors globally for all COMIDS, to do the predictions

drive_download( file="SCIENCE/PROJECTS/RiverMethaneFlux/processed/grade_attributes.csv",
                path="data/processed/grade_attributes.csv",
             overwrite = TRUE)

global_preds <- read_csv( "data/processed/grade_attributes.csv") 

vars_to_log_glob <-  global_preds %>% 
  select(contains(c("uparea", "popdens", "slop",  "T_OC" ,"T_CACO3", "T_CASO4", "k_", "gw_", "wetland",   "T_ESP",
                    "N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture", "P_gnpp", 
                    "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "P_retention_subgrid"),
                  ignore.case = FALSE), -wetland_cover) %>%
  colnames(.)
 

vars_to_remove <-  global_preds %>% 
  select(contains(c('GPP_yr', 'S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH",
                     'npp_', 'forest',  'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY',
                     'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY",
                    "N_aquaculture", "N_gnpp", "N_load", "N_point", "N_retention_subgrid",  "N_surface_runoff_agri", "N_surface_runoff_nat"),
                  ignore.case = FALSE)) %>%
  colnames(.)



#do same transformations to the global dataset
global_preds_trans <- global_preds %>%
  mutate(across(.cols=all_of(vars_to_log_glob), ~log(.x+.1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log_glob)), .cols = all_of(vars_to_log_glob) )  %>% #rename the log transformed variables 
  select(-all_of(vars_to_remove)) %>% 
  drop_na()

colnames(global_preds_trans)

predict_methane <- function(all_models, month, global_predictors) {
  all_months <- tolower(month.abb)
  
  month_selected <- all_months[month]
  
  model_month <- all_models$model_fit[[month]]

  df_predictors <- global_predictors %>%
    select(-ends_with(setdiff(all_months, month_selected))) %>%
    rename_all(~ str_replace(., month_selected, "month"))
  
  out <- predict(model_month, df_predictors)
  
  colnames(out) <- paste("ch4", month_selected, sep="_")
  out
}

ch4_jan <- predict_methane(monthly_models, 1, global_preds_trans)
ch4_feb <- predict_methane(monthly_models, 2, global_preds_trans)
ch4_mar <- predict_methane(monthly_models, 3, global_preds_trans)
ch4_apr <- predict_methane(monthly_models, 4, global_preds_trans)
ch4_may <- predict_methane(monthly_models, 5, global_preds_trans)
ch4_jun <- predict_methane(monthly_models, 6, global_preds_trans)
ch4_jul <- predict_methane(monthly_models, 7, global_preds_trans)
ch4_aug <- predict_methane(monthly_models, 8, global_preds_trans)
ch4_sep <- predict_methane(monthly_models, 9, global_preds_trans)
ch4_oct <- predict_methane(monthly_models, 10, global_preds_trans)
ch4_nov <- predict_methane(monthly_models, 11, global_preds_trans)
ch4_dec <- predict_methane(monthly_models, 12, global_preds_trans)


dat_out <- global_preds_trans %>% select(COMID) %>% 
  bind_cols(ch4_jan, ch4_feb, ch4_mar, ch4_apr, ch4_may, ch4_jun,
            ch4_jul, ch4_aug, ch4_sep, ch4_oct, ch4_nov, ch4_dec) %>% 
  mutate(across(starts_with("ch4"), exp))

write_csv(dat_out, "data/processed/meth_predictions.csv")

drive_upload(media = "data/processed/meth_predictions.csv",
             path="SCIENCE/PROJECTS/RiverMethaneFlux/processed/meth_predictions.csv",
             overwrite = TRUE)
