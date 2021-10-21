# Info ------
# Script to model CH4 concentrations in rivers globally.
# First part is to clean and transform data needed for the model


# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'tidymodels', 'googledrive' ,  'lubridate', 'vip')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

# Add some custom functions ----



# Read files ----
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

# Data filtering ----
# First clean the methDB for the useful sites 
# Steps done:
# 1. remove aggregated sites
# 2. remove Downstream of a Dam, Permafrost influenced, Glacier Terminus, downstream of a Point Source,
#    Thermogenically affected, Ditches
grimeDB_attributes <- read_csv("data/processed/grimeDB_concs_with_grade_attributes.csv") %>% 
  filter(`Aggregated?` == "No",
         !str_detect(Channel_type,"DD|PI|GT|PS|TH|Th|DIT")) %>%
  mutate(month=month(date), year= year(date)) %>% 
  select(-c(Channel_type, `Aggregated?`, date_end, date)) 


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

#check variable sindividually if needed
grimeDB_attributes %>% 
  select(val = gw_month) %>% 
  ggplot(aes(val))+
  geom_histogram(bins = 80) +
  theme_classic()


#remove some extreme values for some variables
grimeDB_attributes <- grimeDB_attributes %>% 
  mutate(CH4mean= ifelse(CH4mean > 500, NA, CH4mean),
         popdens= ifelse(popdens > 4000, NA, popdens),
         gw_month= ifelse(gw_month < .1, .1, gw_month))


#histograms of all variables   
grimeDB_attributes %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value" ) %>% 
  ggplot(aes(value))+
  geom_histogram(bins = 80) +
  theme_classic()+
  facet_wrap(~variable, scales='free')

# we will log transform those to start with
vars_to_log <- c('CH4mean','uparea','popdens','slop','S_CACO3','S_CASO4' ,'T_OC','S_OC', 'T_CACO3', 'T_CASO4', 
                  'k_month', 'gw_month', 'wetland' )

#dataset with some variables log transformed
grimeDB_attr_trans <- grimeDB_attributes %>%
  mutate(across(where(is.numeric), zero_to_min )) %>%
  #filter(distance_snapped < 1000) %>%  #I was playing if excluding sites that has been snapped far away changes things
  #group_by(Site_Nid) %>% # this was to check if grouping by sites (collapsing temporal variability) changes things. It does
  #summarise(across(everything(), mean, na.rm=TRUE)) %>% 
  select( CH4mean, month, COMID, Site_Nid,  GPP_yr:sresp_month, -area, -T_ECE, -S_ECE) %>% 
  mutate(across(.cols=all_of(vars_to_log), log)) %>%  #log transform those variables
  rename_with( ~str_c("Log", all_of(vars_to_log)), .cols = all_of(vars_to_log) ) #rename the log transformed variables 

#Do again some histograms with the log transformed variables
grimeDB_attr_trans %>% 
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value" ) %>% 
  ggplot(aes(value))+
    geom_histogram(bins = 80) +
    theme_classic()+
    facet_wrap(~predictor, scales='free')

ggsave("figures/histograms_transformed.png", width=16, height = 12)

summary(grimeDB_attr_trans$LogCH4mean)

#pearson correlations for CH4 
corr_ch4 <- grimeDB_attr_trans %>% 
  correlate() %>% 
  focus(LogCH4mean)

grimeDB_attr_trans %>% 
  correlate() %>% 
  rplot(shape = 20, colors = c("red", "green"), print_cor = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/correlations_predictors.png", width=16, height = 12)

#check the coefficients in the console
corr_ch4 %>% 
  arrange(desc(abs(LogCH4mean))) %>% 
  print(n=50)

vars_for_plot <- corr_ch4 %>% 
  arrange(desc(abs(LogCH4mean))) %>% 
  mutate(predictor=fct_reorder(term, desc(abs(LogCH4mean)))) %>% 
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

# Start the modelling ----
# Select useful variables for the model 
predictors_selected <- c( 'elev', 'S_SAND', 'LogT_OC', 'Logslop', 'Loggw_month', 'npp_month','gpp_month', 'S_ESP', 'pyearRS',
                     'precip_month', 'sresp_month', 'temp_month', 'Logpopdens', 'Logwetland', 'temp_yr', 'prec_yr' , 'Loguparea', 'GPP_yr')

#check correlations between them
grimeDB_attr_trans %>% 
  select( LogCH4mean, predictors_selected) %>%
  correlate() %>% 
  rplot(shape = 20, colors = c("red", "green"), print_cor = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#remove variables that are highly correlated
data_for_model <-  grimeDB_attr_trans %>% 
  drop_na() %>% 
  #select(month, COMID, LogCH4mean, predictors_selected) %>%
  group_by(Site_Nid) %>% 
  summarise(across(everything(), mean)) %>% 
  select(-c('Site_Nid','COMID','GPP_yr', 'LogT_OC', 'T_PH_H2O', 'T_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', 'pyearRS', 'gpp_month', 'temp_month'))




# RF model using tidymodels ----
#Links I ahve been looking at for this: 
# https://www.tidymodels.org/start/recipes/
# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/


####
#### tuning parameter stuff

#prep the dataset into a training and testing one
grime_split <- initial_split(data_for_model) 


grime_split %>% 
  training() %>%
  glimpse()

#Model recipe, predict CH4, center and scale all predictors
grime_recipe <-  training(grime_split) %>%
  recipe(LogCH4mean ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) 

grime_recipe

#prepare the testing ds by passing on the recipe
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
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

#Do a more targeted tuning with a new grid
rf_grid <- grid_regular(
  mtry(range = c(20, 40)),
  min_n(range = c(30, 40)),
  levels = 5
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)


#select the best one and finish the model 
best_auc <- select_best(tune_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)


final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(LogCH4mean ~ .,
      data = juice(grime_prep)
  ) %>%
  vip(geom = "point")


final_wf <- workflow() %>%
  add_recipe(grime_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(grime_split)

final_res %>%
  collect_metrics()





##### other trials, not good.

# Create data frames for the two sets:
train_data <- training(grime_split)
test_data  <- testing(grime_split)

# resample the data with 10-fold cross-validation 
folds  <- vfold_cv(grime_training, v=10)

#Model recipe, predict CH4, center and scale all predictors
grime_recipe <-  training(grime_split) %>%
  recipe(LogCH4mean ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) 

grime_recipe


grime_ranger <- rand_forest( mode = "regression") %>%
  set_engine("ranger") 

model_default <-
  grime_ranger %>%
  fit(LogCH4mean~., data = train_data)

model_default

model_default %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  metrics(LogCH4mean, .pred)  



rf_spec_new <-
  rand_forest(
    mode = "regression",
    mtry = tune(),
    trees = tune() ) %>%
  set_engine("ranger")

# Create a workflow
rf_workflow <-
  workflow() %>%
  add_variables(
    outcomes = LogCH4mean, predictors = everything()
  ) %>%
  add_model(rf_spec_new)

set.seed(300)
grid_tune <-
  rf_workflow %>%
  tune_grid(
    resamples = folds, 
    grid =  5
  )
)

collect_metrics(grid_tune)

show_best(grid_tune, n = 1)


mod_final <-
  finalize_workflow(rf_workflow, select_best(grid_tune)) %>%
  fit(train_data)

mod_final %>% 
  predict(test_data) %>% 
  bind_cols(test_data) %>% 
  metrics(LogCH4mean, .pred) 


manual_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(LogCH4mean ~ .,
      data = juice(prep(grime_recipe))
  ) %>%
  vip(geom = "point")



rf_wf <- 
  workflow() %>%
  add_model(grime_ranger) %>%
  add_formula(LogCH4mean ~ .)

set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds,
                control = control_resamples(save_pred = TRUE))

collect_metrics(rf_fit_rs)

grime_probs <- rf_fit_rs %>%
  collect_predictions()

rsq(truth=LogCH4mean, estimate=.pred, data=grime_probs)

ggplot(grime_probs)+
  geom_point(aes(.pred, LogCH4mean))+
  geom_abline(slope = 1, intercept = 0)


rf_fit_rs %>%
  vip()


