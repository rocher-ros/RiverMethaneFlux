# Info ------
# Author: Gerard Rocher-Ros
# Script to model CH4 concentrations in rivers globally.
# Part 1 is to clean and transform data needed for the model


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'tidymodels', 'googledrive' ,  'lubridate', 'vip', 'corrr', 'ggpubr')

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
  mutate(month=month(date)) %>% 
  select( COMID, Site_Nid,CH4mean, month, GPP_yr:sresp_month,
         -c(Channel_type, `Aggregated?`, date_end, date, area, T_ECE, S_ECE, temp_month, WaterTemp_actual, WaterTemp_est, discharge_measured)) 


colnames(grimeDB_attributes)

# Explore the raw data and process before the modelling

#histograms of all variables, some will need logtansformation
grimeDB_attributes %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value" ) %>% 
  ggplot(aes(value))+
  geom_histogram(bins = 80) +
  theme_classic()+
  facet_wrap(~variable, scales='free')

ggsave("figures/histograms_rawdata.png", width=16, height = 12)

#check variables individually if needed
grimeDB_attributes %>% 
  select(val = gw_month) %>% 
  ggplot(aes(val))+
  geom_histogram(bins = 80) +
  theme_classic()


#get the variables that have negative values, and get the lowest value above 0. I will use this value to shift the data a bit prior log transform 
vars_to_shift <- grimeDB_attributes  %>% 
  dplyr::select_if(~any(. < 0)) %>% 
 summarise(across(everything(), ~min(.x, na.rm = TRUE))) %>% 
  pivot_longer( everything(), names_to = "variable", values_to = "minimum") %>% 
  mutate(minimum=abs(minimum))

# we will log transform those to start with
vars_to_log <- c('CH4mean','uparea','popdens','slop','S_CACO3','S_CASO4' ,'T_OC','S_OC', 'T_CACO3', 'T_CASO4', 
                  'k_month', 'gw_month', 'wetland', 'elev', 'precip_month', 'S_ESP', 'T_ESP' )

#dataset with some variables log transformed
grimeDB_attr_trans <- grimeDB_attributes %>%
  mutate(npp_month = npp_month + vars_to_shift$minimum[vars_to_shift$variable == "npp_month"],
         elev = elev + vars_to_shift$minimum[vars_to_shift$variable == "elev"],
         temp_yr = temp_yr + vars_to_shift$minimum[vars_to_shift$variable == "temp_yr"],
         tavg_month = tavg_month + vars_to_shift$minimum[vars_to_shift$variable == "tavg_month"]) %>%
  #group_by(Site_Nid) %>% # this was to check if grouping by sites (collapsing temporal variability) changes things. It does
  #summarise(across(everything(), mean, na.rm=TRUE)) %>% 
  mutate(across(.cols=all_of(vars_to_log), ~log(.x+.01))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log)), .cols = all_of(vars_to_log) ) #rename the log transformed variables 

#Do again some histograms with the log transformed variables
# some variables are still very skewed, usually the anthropogenic predictors that have lots of 0s
grimeDB_attr_trans %>% 
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value" ) %>% 
  ggplot(aes(value))+
    geom_histogram(bins = 80) +
    theme_classic()+
    facet_wrap(~predictor, scales='free')

ggsave("figures/histograms_transformed.png", width=16, height = 12)



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
  select(-term)


#scatter plots of all variables, sorted from highest to lower r. 
# Showing a loess fit to capture potential nonlinearities that might be useful to assess
 plot_correlations <- grimeDB_attr_trans %>% 
  pivot_longer(-LogCH4mean, names_to = "predictor", values_to = "value" ) %>% 
  mutate(predictor=fct_relevel(predictor, levels(vars_for_plot$predictor))) %>% 
  ggplot(aes(value, LogCH4mean))+
  geom_hex(bins = 30) +
  geom_smooth(method = "loess", se=FALSE, color="red3")+
  geom_text( data = vars_for_plot, 
             mapping = aes(x = Inf, y = Inf, label = paste("r=",round(LogCH4mean,2))), hjust   = 3.5, vjust   = 1.5, color="red")+
  theme_classic()+
  scale_fill_continuous(type = "viridis", trans = "log10")+
  facet_wrap(~predictor, scales='free')

 
#ggsave(filename= "figures/correlations.png", plot=plot_correlations, width = 18, height = 12)

# 2.  RF model using tidymodels ----
 
# Select useful variables for the model, some variables were removed due to a high correlation with other ones 
predictors_selected <- c( 'Log_elev', 'S_SAND', 'Log_T_OC', 'Log_slop', 'Log_gw_month', 'npp_month','gpp_month', 'Log_S_ESP', 'pyearRS',
                     'Log_precip_month', 'sresp_month', 'tavg_month', 'Log_popdens', 'Log_wetland', 'temp_yr', 'prec_yr' , 'Log_uparea', 'GPP_yr')


#Links I have been looking at for this: 
# https://www.tidymodels.org/start/recipes/
# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/


## parameter tuning  for the RF, takes several hours so welcome to skip it ----
## First run will be with the model with average values by site, to see the broad performance and tune RF parameters
 #remove variables that are highly correlated
 data_for_model <-  grimeDB_attr_trans %>% 
   drop_na() %>% 
   # select(month, COMID, Log_CH4mean, predictors_selected) %>%
   group_by(Site_Nid) %>% 
   summarise(across(everything(), mean)) %>% 
   select(-c('Site_Nid','COMID','GPP_yr', 'Log_T_OC', 'T_PH_H2O', 'T_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', 'pyearRS', 'gpp_month', 'tavg_month'))
 

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

grime_recipe

#prepare the testing df by passing on the recipe
grime_prep <- prep(grime_recipe) 

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


##  Run the models ----
#with the tuned parameters obtained with thw whole dataset
# ref: https://stackoverflow.com/questions/62687664/map-tidymodels-process-to-a-list-group-by-or-nest

#Custom function to map it over month, wwith the paramaters obtained from the tuning
#setup of the RF model
rf_mod <-
  rand_forest(
    mtry = 20,
    trees = 1000,
    min_n = 25 ) %>%
  set_mode("regression") %>%
  set_engine("ranger")

#prepare a workflow with it to feed into the function
wf <-
  workflow() %>%
  add_model(rf_mod)


## big function of model fitting and predicting
predict_RF_grime <- function(df) {
  split <- initial_split(df)
  train_df <- training(split)
  test_df <- testing(split)
  
  #create recipe
  recipe_train <- train_df %>% 
    recipe(Log_CH4mean ~.) %>%
    step_corr(all_predictors()) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) 
  
  #fit workflow on train data
  fit_wf <-
    wf %>%
    add_recipe(recipe_train) %>%
    fit(data = train_df)
  
  #predict on test data
  predict(fit_wf, test_df) %>% 
    bind_cols(test_df)
}

## Run the model via map for each month ----

#prepare a dataset, nesting by month
data_model_nested <- grimeDB_attr_trans %>% 
  select(-c('Site_Nid','COMID','GPP_yr', 'Log_T_OC', 'T_PH_H2O', 'T_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', 'pyearRS', 'gpp_month', 'tavg_month')) %>% 
  drop_na() %>% 
  group_by(month) %>% 
  nest() %>% 
  arrange(month)

set.seed(123)

#Run the model for each month in a map
monthly_models <- data_model_nested %>%
  mutate(predictions = map(data, possibly(predict_RF_grime, otherwise = NA)))

#get the predictions
preds_obs <- monthly_models %>% 
  unnest(predictions)
  

#plot them  
ggplot(preds_obs, aes(.pred, Log_CH4mean))+
  geom_point(alpha=.6)+
  geom_abline(slope=1, intercept = 0)+
  stat_cor(aes(label = ..rr.label..), label.y.npc = 0.9)+ #put R2 and label
  labs(x="CH4 predictions", y="CH4 observations", title="one model for each month")+
  theme_bw()+
  facet_wrap(~month)

ggsave(filename= "figures/model_perf_monthly.png", width = 12, height = 8)


## run the data on the whole dataset, not nesting by month ----
data_model <- grimeDB_attr_trans %>%
  select(-c('Site_Nid','COMID','GPP_yr', 'Log_T_OC', 'T_PH_H2O', 'T_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', 'pyearRS', 'gpp_month', 'tavg_month')) %>% 
  drop_na()

set.seed(123)

#Run the model for each month in a map
yearly_model <- predict_RF_grime(data_model)


#plot them 
yearly_model %>% mutate(month=round(month,0)) %>% 
ggplot( aes(.pred, Log_CH4mean))+
  geom_point(alpha=.6)+
  geom_abline(slope=1, intercept = 0)+
  stat_cor(aes(label = ..rr.label..), label.y.npc = 0.9)+ #put R2 and label
  labs(x="CH4 predictions", y="CH4 observations", title="one model for all data")+
  theme_bw()+
  facet_wrap(~month)

ggsave(filename= "figures/model_perf_yearly.png", width = 12, height = 8)
