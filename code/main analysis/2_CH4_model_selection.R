########################################.
#### R script to select a good ML model for CH4 concentrations in rivers
#### Here I first roughly assess the structure and distribution of the data, then tune different ML models
#### (Neural network, XGboost, Random forest), and evaluate the performance among them to select the best one
#### Last edit: 2023-03-10
########################################.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'tidymodels', 'ranger', 'lubridate', 'corrr', 'ggpubr')

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
  print("go to script 1_data_preparation.R and run to produce the needed file for this script")
}

# 1. Data filtering ----
# First clean the methDB for the useful sites 
# Steps done:
# 1. remove aggregated sites
# 2. remove sites snapped far away to a river
# 3. remove Downstream of a Dam, Permafrost influenced, Glacier Terminus, downstream of a Point Source,
#    Thermogenically affected, Ditches
grimeDB_attributes <- read_csv("data/processed/grimeDB_concs_with_grade_attributes.csv") %>% 
  filter(Aggregated == "No", distance_snapped < 5000,
         !str_detect(Channel_type,"DD|PI|GT|PS|TH|Th|DIT")) %>%
  mutate( month = month(date)) %>% 
  dplyr::select( COMID, Site_ID, CH4mean, month, GPP_yr:month, 
                 -c(Channel_type, Aggregated, date_end, date, area, T_ECE, S_ECE, slope,
                    temp_month, WaterTemp_degC, WaterTemp_degC_estimated, discharge_measured)) 

colnames(grimeDB_attributes)

# file to have nicer names in the variables, to use later
labeller_vars <- read_csv("data/processed/variables_names.csv") 

# 2. Explore the raw data and process before the modelling ----

# we will log transform those to start with
vars_to_log <- c('CH4mean','uparea','popdens','slop' ,'T_OC','S_OC', 'T_CACO3', 'T_CASO4', 'k_month', 'gw_month', 'wetland', 
                 'T_ESP', "N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture", "q_month",
                 "P_gnpp", "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "N_retention_subgrid")

# Select useful variables for the model, some variables were removed due to a high correlation with other ones 
variables_to_remove <- c('Site_ID','COMID','GPP_yr', 'Log_S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH",
                         'npp_month', 'forest', 'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY', "lon", "lat",
                         'S_CASO4', 'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY",
                         "N_aquaculture", "N_gnpp", "N_load", "N_point",  "N_surface_runoff_agri", "N_surface_runoff_nat" )

#dataset with some variables log transformed
grimeDB_attr_trans <- grimeDB_attributes %>%
  mutate(across(.cols = all_of(vars_to_log), ~log(.x + .1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~ str_c("Log_", vars_to_log), .cols = all_of(vars_to_log) ) #rename the log transformed variable

# 2. Assess a bit the variables and their correlations ----

#Do  some histograms with the log transformed variables
# some variables are still very skewed, usually the anthropogenic predictors that have lots of 0s
grimeDB_attr_trans %>% 
  select(-all_of(variables_to_remove), -biome_label) %>% 
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value" ) %>% 
  left_join(labeller_vars, by=c("predictor"="var")  )  %>%
  ggplot(aes(value))+
    geom_histogram(bins = 80) +
    theme_classic()+
    facet_wrap(~label, scales='free')+
  theme(strip.text = ggtext::element_markdown())



#pearson correlations for CH4 with the predictors
corr_ch4 <- grimeDB_attr_trans %>% 
  select(where(is.numeric)) %>%  
  correlate() %>% 
  focus(Log_CH4mean)

grimeDB_attr_trans %>% 
  select(where(is.numeric)) %>%  
  correlate() %>% 
  rplot(shape = 20, colors = c("red", "green"), print_cor = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#check the coefficients in the console
corr_ch4 %>% 
  arrange(desc(abs(Log_CH4mean))) %>% 
  print(n = 50)

#get the r coefficients from the df for later plotting
vars_for_plot <- corr_ch4 %>% 
  arrange(desc(abs(Log_CH4mean))) %>% 
  mutate(predictor = fct_reorder(term, desc(abs(Log_CH4mean)))) %>% 
  select(-term) %>% 
  filter(!predictor %in% c("COMID", "Site_Nid", "month"))


#scatter plots of all variables, sorted from highest to lower r. 
# Showing a loess fit to capture potential nonlinearities that might be useful to assess. don't run it often as it takes a while...
 plot_correlations <- grimeDB_attr_trans %>% 
  pivot_longer(all_of(vars_for_plot$predictor), names_to = "predictor", values_to = "value" ) %>% 
  mutate(predictor = fct_relevel(predictor, levels(vars_for_plot$predictor))) %>% 
  ggplot(aes(value, Log_CH4mean))+
  geom_hex(bins = 30) +
  geom_smooth(method = "loess", se=FALSE, color="red3")+
  geom_text( data = vars_for_plot, 
             mapping = aes(x = Inf, y = Inf, label = paste("r=", round(Log_CH4mean,2))), hjust = 3.5, vjust = 1.5, color="red")+
  theme_classic()+
  scale_fill_continuous(type = "viridis", trans = "log10")+
  facet_wrap(~predictor, scales='free')


# 3.  Ml models using tidymodels ----

#Links I have been looking at for this: 
# https://www.tidymodels.org/start/recipes/
# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/



#Prepapre data for model, removing some variables highly correlated from above
#remove variables that are highly correlated
data_for_model <-  grimeDB_attr_trans %>%
  select(-biome_label) %>% 
  group_by(COMID) %>% 
  summarise(across(everything(), mean)) %>%
  ungroup() %>% 
  select(-all_of(variables_to_remove)) %>% 
  drop_na()

#prep the dataset into a training and testing one
grime_split <- initial_split(data_for_model) 

#Model recipe, predict CH4
grime_recipe <-  recipe(Log_CH4mean ~ ., 
                        data = training(grime_split)) %>% 
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>% 
  prep()

# Firs tune a gradient boosting model
xgboost_model <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")

# grid specification
xgboost_params <- 
  dials::parameters(
    trees(),
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )
#prepare the grid for the tuning
xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = 300
  )

#make a workflow that will be applied in each fold
xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(Log_CH4mean ~ .)

# and prepare the folds for the tuning
set.seed(234)
meth_folds <- vfold_cv(training(grime_split), v= 5)


# hyperparameter tuning
xgboost_tuned <- tune_grid(
  object = xgboost_wf,
  resamples = meth_folds,
  grid = xgboost_grid,
  control = tune::control_grid(verbose = TRUE)
)

# look at the results
xgboost_tuned %>% 
  collect_metrics() %>%
  filter(.metric == "rsq", loss_reduction< .01, learn_rate > 0.00001) %>%
  select(mean, trees:loss_reduction) %>%
  pivot_longer(trees:loss_reduction,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "R2")


#let's do a new grid with a more narrow range
new_grid <- grid_regular(
  min_n(range = c(20, 40)),
  tree_depth(range = c(10, 20)),
  trees(range = c(500, 2000)),
  levels = 5
)


#tune the grid again
set.seed(456)
#doParallel::registerDoParallel()

regular_res <- tune_grid(
  xgboost_wf,
  resamples = meth_folds,
  grid = new_grid,
  control = tune::control_grid(verbose = TRUE)
)


regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, trees, tree_depth) %>%
  pivot_longer(min_n:trees,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, colour = tree_depth)) +
  geom_point() +
  scale_color_viridis_c()+
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")


xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("rmse")

#this is the best model using xgboost.
xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

#check perfomance with training data
train_processed <- bake(grime_recipe,  new_data = training(grime_split))

train_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = Log_CH4mean ~ ., 
    data    = train_processed ) %>%
  predict(new_data = train_processed) %>%
  bind_cols(training(grime_split))

xgboost_score_train <- 
  train_prediction %>%
  yardstick::metrics(Log_CH4mean, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(xgboost_score_train)

#and test data 
test_processed  <- bake(grime_recipe, new_data = testing(grime_split))
test_prediction <- xgboost_model_final %>%
  # fit the model on all the training data
  fit(
    formula = Log_CH4mean ~ ., 
    data    = train_processed
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(grime_split))



# measure the accuracy of our model using `yardstick`
xgboost_score <- 
  test_prediction %>%
  yardstick::metrics(Log_CH4mean, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(xgboost_score)


### now we try a neural network. let's do some tuning
tune_spec <- mlp(epochs = tune(), 
                 hidden_units = tune(),  
                 penalty = tune()) %>%
  set_mode("regression") %>% 
  set_engine("nnet", verbose = 0) 

tune_wf <- workflow() %>%
  add_recipe(grime_recipe) %>%
  add_model(tune_spec)

#prepare the folds for the tuning
set.seed(234)
trees_folds <- vfold_cv(training(grime_split), v = 5)


# Now run a bunch of models in parallel with different combinations of parameters to find what works best
doParallel::registerDoParallel()
set.seed(345)

tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20,
  control = tune::control_grid(verbose = TRUE)
)

tune_res

# lets have a look...
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, hidden_units, penalty, epochs) %>%
  pivot_longer(hidden_units:epochs,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, exp(mean), color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

#and we get the best


# Compare performance of the three models ----
# the tuning of the RF is performed in the next script

#prepare some training and test data that will be used in all models
train_processed <- bake(grime_recipe,  new_data = training(grime_split))

test_processed  <- bake(grime_recipe, new_data = testing(grime_split))

set.seed(123)

nnet_fit <-
  mlp(epochs = 2000, 
      hidden_units = 10,  
      penalty = 10) %>%
  set_mode("regression") %>% 
  set_engine("nnet", verbose = 0) %>%
  fit(Log_CH4mean ~ ., data = train_processed)

xgboost_fit <-  boost_tree(
  mode = "regression",
  trees = 1200,
  min_n = 8,
  tree_depth = 13,
  learn_rate = 0.025,
  loss_reduction = 0.0001 ) %>%
  set_engine("xgboost") %>%
  fit(formula = Log_CH4mean ~ ., data = train_processed) 

rf_fit <-
  rand_forest(
    mtry = 13,
    trees = 1200,
    min_n = 8 ) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance="permutation") %>% 
  fit(Log_CH4mean ~ ., data = train_processed)


# use the training model fits to predict the test data
preds_comp <- tibble(
  `Random Forest` = predict(rf_fit, new_data = test_processed) %>% pull ,
  `Neural Network` = predict(nnet_fit, new_data = test_processed) %>% pull(),
  `Gradient boosting` = predict(xgboost_fit, new_data = test_processed) %>% pull,
  observed = testing(grime_split) %>% pull(Log_CH4mean)) %>% 
  pivot_longer(-observed, values_to = "prediction", names_to = "model")

rms_text <- preds_comp %>% 
  group_by(model) %>% 
  summarise(rmse = exp(sqrt(mean((observed - prediction)^2))) - 0.1 ) %>% 
  mutate(rms_label = paste("RMSE = ", round(rmse, 2) ))

#do some plots
ggplot(data= preds_comp, aes(exp(observed), exp(prediction)))+
  geom_point()+
  geom_abline(slope=1, intercept = 0)+
  geom_text( data = rms_text,
             aes(x = .4, y = 11, label = rms_label))+
  stat_cor(aes(label = ..rr.label..), label.y.npc = 0.93, label.x.npc = 0.1)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~model)+
  labs(x="Withheld observations", y= "Predictions")+
  theme_bw()


## Now we pack everything in a function, which will split the dataset for traiing/testing, 
# fit the NN, RF and XGboost models, and evaluate their performance with the testing data
predict_ML_grime <- function(df) {
  
  df <- df %>% dplyr::select(-month)
  
  split <- initial_split(df, prop = .8)
  train_df <- training(split)
  test_df <- testing(split)
  
  #create recipe
  recipe_train <- train_df %>% 
    recipe(Log_CH4mean ~.) %>% 
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) 
  
  #fit workflow on train data
  nnet_fit <-
    mlp(epochs = 2000, 
        hidden_units = 10,  
        penalty = 10) %>%
    set_mode("regression") %>% 
    set_engine("nnet", verbose = 0) %>%
    fit(Log_CH4mean ~ ., data = train_df)
  
  xgboost_fit <-  boost_tree(
    mode = "regression",
    trees = 1200,
    min_n = 8,
    tree_depth = 13,
    learn_rate = 0.025,
    loss_reduction = 0.0001 ) %>%
    set_engine("xgboost") %>%
    fit(formula = Log_CH4mean ~ ., data = train_df) 
  
  rf_fit <-
    rand_forest(
      mtry = 13,
      trees = 1200,
      min_n = 8 ) %>%
    set_mode("regression") %>%
    set_engine("ranger", importance="permutation") %>% 
    fit(Log_CH4mean ~ ., data = train_df)
  
  
  preds <- tibble(
    rf_pred = predict(rf_fit, new_data = test_df) %>% pull ,
    nnet_pred = predict(nnet_fit, new_data = test_df) %>% pull(),
    xgboost_pred = predict(xgboost_fit, new_data = test_df) %>% pull,
    observed = test_df %>% pull(Log_CH4mean))
  
  
  return(list(preds))
  
}

## Run the model via map for each month ----

#prepare a dataset, nesting by month
data_model_monthly <- grimeDB_attr_trans %>% 
  select(-biome_label) %>% 
  group_by(COMID, month) %>% 
  summarise(across(everything(), mean)) %>%
  ungroup() %>% 
  dplyr::select(-all_of(variables_to_remove)) %>% 
  drop_na()


set.seed(123)


# Now finally run the model for each month, using data of adjacent months for each month
# First make a pointer of the data to use for each months, overlapping, 
# Second nest them by month,  third run the model with a map,
# Fourth extract the model output and predicted values
# the lst chunk adapted from: https://community.rstudio.com/t/many-models-with-overlapping-groups/1629/4
monthly_models <- lst(
  jan = . %>% filter(month %in% c(12,1,2)),
  feb = . %>% filter(month %in% 1:3),
  mar = . %>% filter(month %in% 2:4),
  apr = . %>% filter(month %in% 3:5),
  may = . %>% filter(month %in% 4:6),
  jun = . %>% filter(month %in% 5:7),
  jul = . %>% filter(month %in% 6:8),
  aug = . %>% filter(month %in% 7:9),
  sep = . %>% filter(month %in% 8:10),
  oct = . %>% filter(month %in% 9:11),
  nov = . %>% filter(month %in% 10:12),
  dec = . %>% filter(month %in% c(11,12,1))) %>% 
  map_dfr(~ tidyr::nest(.x(data_model_monthly )), .id = "month_label") %>% 
  mutate( model_out = map(data, possibly(predict_ML_grime, otherwise = NA))) %>% 
  rowwise() %>%
  mutate( preds = model_out[1]) %>% 
  dplyr::select(-data, -model_out)


# first we do nicer labels for the months
labelled_months <- tibble(month_label = tolower(month.abb), labels = month.name)

#extract the model prediction data for plotting model performance
monthly_models_unnested <- monthly_models %>% 
  unnest(preds)  %>% 
  pivot_longer(rf_pred:xgboost_pred, values_to = "prediction", names_to = "model") %>% 
  left_join(labelled_months, by = "month_label") %>% 
  mutate(labels = fct_relevel(labels, month.name)) 

#calculate the parameters of the mdel fit and RMSE each month and model, we will do a table with this later
lm_out <- monthly_models_unnested %>% 
  nest_by(labels, model) %>% 
  mutate(fit_lm = list(lm(prediction ~observed, data = data))) %>% 
  summarize(glance(fit_lm))


rms_text <- monthly_models_unnested %>% 
  group_by(labels, model) %>% 
  summarise(rmse = exp(sqrt(mean((observed - prediction)^2))) - 0.1) %>% 
  left_join(lm_out %>% select(labels, model, r.squared) ) %>% 
  mutate( text = paste0(round(r.squared, 2), " [", round(rmse,2), "]"),
          algorithm = case_when(model == "nnet_pred" ~ "Neural network",
                                model == "rf_pred" ~ "Random Forest",
                                model == "xgboost_pred" ~ "Gradient boosting"))


table_out <- rms_text %>% 
  select(labels, algorithm, text) %>% 
  pivot_wider(names_from = labels, values_from = text) 

# let's look at the table of model comparison, which is table S4 in the SM
table_out

table_out %>% 
  write_csv("figures/supplementary/table_models.csv")
