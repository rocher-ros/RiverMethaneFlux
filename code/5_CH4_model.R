# Info ------
# Author: Gerard Rocher-Ros
# Script to model CH4 concentrations in rivers globally.
# Part 1 is to clean and transform data needed for the model


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'tidymodels', 'googledrive' ,  'lubridate', 'vip', 'corrr', 'ggpubr', 'DALEXtra', 'ggtext', 'patchwork')

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
          -c(Channel_type, `Aggregated?`, date_end, date, area, T_ECE, S_ECE, slope,
             temp_month, WaterTemp_actual, WaterTemp_est, discharge_measured)) 

colnames(grimeDB_attributes)
  
labeller_vars <- read_csv("data/processed/variables_names.csv") %>% 
  mutate(label=str_replace(label, "9", ";"))

# Explore the raw data and process before the modelling

# we will log transform those to start with
vars_to_log <- c('CH4mean','uparea','popdens','slop' ,'T_OC','S_OC', 'T_CACO3', 'T_CASO4', 'k_month', 'gw_month', 'wetland', 
                 'T_ESP', "N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture", "q_month",
                 "P_gnpp", "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "N_retention_subgrid")

# Select useful variables for the model, some variables were removed due to a high correlation with other ones 
variables_to_remove <- c('Site_Nid','COMID','GPP_yr', 'Log_S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH",
                         'npp_month', 'forest', 'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY', "lon", "lat",
                         'S_CASO4', 'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY",
                         "N_aquaculture", "N_gnpp", "N_load", "N_point",  "N_surface_runoff_agri", "N_surface_runoff_nat" )


#dataset with some variables log transformed
grimeDB_attr_trans <- grimeDB_attributes %>%
  mutate(across(.cols=all_of(vars_to_log), ~log(.x+.1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log)), .cols = all_of(vars_to_log) ) #rename the log transformed variables 


#Do again some histograms with the log transformed variables
# some variables are still very skewed, usually the anthropogenic predictors that have lots of 0s
grimeDB_attr_trans %>% 
  select(-all_of(variables_to_remove)) %>% 
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value" ) %>% 
  left_join(labeller_vars, by=c("predictor"="var")  )  %>%
  ggplot(aes(value))+
    geom_histogram(bins = 80) +
    theme_classic()+
    facet_wrap(~label, scales='free')+
  theme(strip.text = ggtext::element_markdown())

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
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")

rm(tune_res)
#Do a more targeted tuning with a new grid
rf_grid <- grid_regular(
  mtry(range = c(10,20)),
  min_n(range = c(15, 30)),
  levels = 10
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

n.cores= parallel::detectCores()-1

rf_mod <-
  rand_forest(
    mtry = 10,
    trees = 1200,
    min_n = 21 ) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance="impurity", num.threads =n.cores)#, quantreg = TRUE) #if getting quantiles

#prepare a workflow with it to feed into the function
wf <-
  workflow() %>%
  add_model(rf_mod)


## big function of model fitting and predicting
predict_RF_grime <- function(df) {
  
  df <- df %>% dplyr::select(-month)
  
  split <- initial_split(df)
  train_df <- training(split)
  test_df <- testing(split)
 
  #create recipe
  recipe_train <- train_df %>% 
    recipe(Log_CH4mean ~.)

 #fit workflow on train data
  fit_wf <-
    wf %>%
    add_recipe(recipe_train) %>%
    fit(data = train_df) 
  
  #predict on test data
 preds <- predict(fit_wf, test_df) %>% 
   bind_cols(test_df)

return(list(preds, fit_wf, train_df))

}

## Run the model via map for each month ----

#prepare a dataset, nesting by month
data_model_monthly <- grimeDB_attr_trans %>% 
  group_by(COMID, month) %>% 
  summarise(across(everything(), mean)) %>%
  ungroup() %>% 
  dplyr::select(-all_of(variables_to_remove)) %>% 
  drop_na()


set.seed(123)

#Run the model for each month in a map
# monthly_models <- data_model_monthly %>%
#   mutate(month_label = tolower(month.abb[month]),
#          model_out = map(data, possibly(predict_RF_grime, otherwise = NA))) %>% 
#   rowwise() %>%
#   mutate( preds = model_out[1],
#     model_fit = model_out[2]) %>% 
#   select(-data, -model_out)

# Now finally run the model for each month, using data of adjacent months for each month
# First make a pointer of the data to use for each months, overlapping, 
# Second nest them by month,  third run the model with a map,
# Fourth extract the model output and predicted values
# the lst chunk adapted from: https://community.rstudio.com/t/many-models-with-overlapping-groups/1629/4
# those are the most efficient lines of code I have ever written, 
# 3 years ago it would have taken me about 900 lines of code!! 
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
  mutate( model_out = map(data, possibly(predict_RF_grime, otherwise = NA))) %>% 
  rowwise() %>%
  mutate( preds = model_out[1],
          model_fit = model_out[2]) %>% 
  select(-data, -model_out)

#plot them  
monthly_models %>% 
  mutate(month_label= fct_relevel(month_label, levels = tolower(month.abb))) %>% 
  unnest(preds) %>% 
ggplot( aes(.pred, Log_CH4mean))+
  geom_point(alpha=.6)+
  geom_abline(slope=1, intercept = 0)+
  stat_cor(aes(label = ..rr.label..), label.y.npc = 0.9)+ #put R2 and label
  labs(x="CH4 predictions", y="CH4 observations", title="one model for each month")+
  theme_bw()+
  facet_wrap(~month_label)

#ggsave(filename= "figures/model_perf_monthly_adjacent.png", width = 12, height = 8)

#residuals
monthly_models %>% 
  unnest(preds) %>%  
  ggplot( aes(.pred, Log_CH4mean-.pred))+
  geom_point()+
  #geom_smooth(method="lm")+
  geom_hline(yintercept = 0)+
  labs(x="CH4 predictions", y="Residuals")+
  theme_bw()


#variable importance 
get_vi_vals <- function(data){
  data %>% 
    extract_fit_parsnip() %>% 
    vi()
}


vi_monthly <- map(monthly_models[[3]], get_vi_vals)  %>% 
  set_names(monthly_models[[1]]) %>% 
  map_df( ~as.data.frame(.x), .id="month") %>% 
  left_join(labeller_vars, by=c("Variable" = "var")  )  %>%
  group_by(Variable) %>% 
  filter(median(Importance)> 21.5) %>%
  ungroup() 

vi_monthly_mean <- vi_monthly %>% 
  group_by(Variable) %>% 
  summarise(Importance=median(Importance),
            label=first(label),
            type=first(type)) %>% 
  ungroup() %>% 
  arrange(desc(Importance)) %>% 
  mutate(label= factor(label, unique(label)))

median_se <- function(x) {
  x <- stats::na.omit(x)
  se <- sqrt(stats::var(x)/length(x))
  median <- median(x)
 data.frame(y = median, 
            ymin = median - se, 
            ymax = median + se)
}

vi_plot <- vi_monthly %>% 
ggplot( aes(x=Importance, 
              y=  reorder(label, Importance, FUN = median)))+
  stat_summary( aes(fill = type), color=NA, geom="bar", fun="median", alpha=.8)+
  stat_summary(fun.data = median_se, geom = "linerange", size=1.5, alpha=.6)+
  scale_x_continuous(expand = c(0,0))+
  #scale_y_discrete(position = "right")+
  scale_fill_manual(values=c("forestgreen", "dodgerblue3", "brown3", "darkgoldenrod3", "gray60", "chocolate"), name="Category")+
  theme_classic()+
  labs(y="", fill="Category")+
  theme(legend.position = c(.8,.15), axis.text.y =ggtext::element_markdown(), legend.title =element_text(face="bold") )

ggsave(vi_plot, filename = "figures/VIP_scores_monthly.png", height = 8, width = 6, dpi=500)


## run the data on the whole dataset, not nesting by month ----

set.seed(123)

#Run the model for each month in a map
yearly_model <- grimeDB_attr_trans %>%
  group_by(COMID, month) %>% 
  summarise(across(everything(), mean)) %>%
  ungroup() %>% 
  select(-all_of(variables_to_remove)) %>% 
  drop_na() %>% 
  predict_RF_grime()

yearly_preds <- yearly_model[1] %>% as.data.frame()

#partial dependence plots 
rf_fit <- yearly_model[[2]] %>% 
  fit(data = yearly_model[[3]])

explainer_rf <- explain_tidymodels(
  rf_fit, 
  data = dplyr::select(yearly_model[[3]], -Log_CH4mean), 
  y = yearly_model[[3]]$Log_CH4mean,
  label = "random forest"
)

pdp_rf <- model_profile(explainer_rf, N = 5000)

pdp_data_plot <- pdp_rf$agr_profiles %>% 
  dplyr::select(Variable = `_vname_`, x=`_x_`, y_hat = `_yhat_`) %>%
  right_join(vi_monthly_mean, by=c("Variable")) %>%
  mutate(label=fct_relevel(label, levels(vi_monthly_mean$label))) %>% 
  group_by(Variable) %>% 
  mutate(x = (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)),
         y_hat = (y_hat - min(y_hat, na.rm = T))/(max(y_hat, na.rm = T) - min(y_hat, na.rm = T))) %>% 
  drop_na(y_hat) 

pdp_plot <- pdp_data_plot %>% 
  ggplot(aes(x, y_hat, color=type))+
  geom_line(size=1)+
  scale_color_manual(values=c("darkgreen", "dodgerblue4", "brown4", "darkgoldenrod4", "gray40", "chocolate4"))+
  facet_grid( scales = "free", rows=vars(label))+
  theme_classic()+
  labs(x="", y="") +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_line(color="transparent"),
    axis.line = element_line(color="transparent"),
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
  )

pdp_plot

vi_plot + inset_element(pdp_plot, left = -.088, bottom = -.034, right = .25, top = 1)

ggsave("figures/rf_figure.png",  height = 7, width = 6, dpi=500)




#partial dependence plots, for each month
for (i in 1:12) {
  
  print(i) 
  
  this_month <- lapply(monthly_models, `[[`, i)  
  
  rf_fit <- this_month[[3]] %>% 
    fit(data = this_month[[2]])
  
  explainer_rf <- explain_tidymodels(
    rf_fit, 
    data = dplyr::select(this_month[[2]], -Log_CH4mean), 
    y = this_month[[2]]$Log_CH4mean,
    label = "random forest")
  
  pdp_rf <- model_profile(explainer_rf, N = 5000)
  
  data_out <- cbind(pdp_rf$agr_profiles, this_month[[1]] )
  
  if(i == 1){ pdp_months <- data_out } else{  pdp_months <- rbind(pdp_months, data_out)}


}

#remove some extreme values and sort by importance from the VI
pdp_months_plot <- pdp_months %>% 
  dplyr::select(Variable = `_vname_`, x=`_x_`, y_hat = `_yhat_`, month=`this_month[[1]]`) %>%
  right_join(vi_monthly, by=c("Variable", "month")) %>%
  mutate(y_hat = ifelse(Variable == "T_CEC_SOIL" & x > 60, NA, y_hat),
         y_hat = ifelse(Variable == "Log_k_month" & y_hat > -.6, NA, y_hat),
         y_hat = ifelse(Variable == "sresp_month" & y_hat > -.7, NA, y_hat),
         month= fct_relevel(month, levels = tolower(month.abb)),
         label=fct_relevel(label, levels(vi_monthly_mean$label))) %>% 
  drop_na(y_hat) 

pdp_months_plot %>% 
  ggplot(aes(x, y_hat))+
  geom_line(alpha=.7, aes(color=month))+
  geom_smooth(method="loess", color="black", se=FALSE)+
  scale_color_viridis_d()+
  facet_wrap(~label, scales = "free")+
  theme_classic()+
  guides(color = guide_legend(override.aes = list(size = 1, alpha=1) ) )+
  theme(strip.text = ggtext::element_markdown())


ggsave(filename= "figures/partial_depend_monthly.png", width = 12, height = 8, dpi=600)


# 3. Predict values to the whole world ----

#read data of all predictors globally for all COMIDS, to do the predictions

drive_download( file="SCIENCE/PROJECTS/RiverMethaneFlux/processed/grade_attributes.csv",
                path="data/processed/grade_attributes.csv",
             overwrite = TRUE)

global_preds <- read_csv( "data/processed/grade_attributes.csv", lazy=FALSE) 


vars_to_log_glob <-  global_preds %>% 
  select(contains(c("uparea", "popdens", "slop",  "T_OC" ,"T_CACO3", "T_CASO4", "k_", "q_", "gw_", "wetland",   "T_ESP",
                    "N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture", "P_gnpp", 
                    "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "N_retention_subgrid"),
                  ignore.case = FALSE), -wetland_cover, -slope) %>%
  colnames(.)
 

vars_to_remove <-  global_preds %>% 
  select(contains(c('GPP_yr', 'S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH", "T_ECE", "S_ECE", 
                     'npp_', 'forest',  'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY',
                     'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY", "temp",
                    "N_aquaculture", "N_gnpp", "N_load", "N_point", "N_surface_runoff_agri", "N_surface_runoff_nat"),
                  ignore.case = FALSE), lat, lon, slope, area) %>%
  colnames(.)



#do same transformations to the global dataset
global_preds_trans <- global_preds %>%
  select(-all_of(vars_to_remove)) %>% 
  mutate(across(.cols=all_of(vars_to_log_glob), ~log(.x+.1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log_glob)), .cols = all_of(vars_to_log_glob) )  %>% #rename the log transformed variables 
  drop_na()

colnames(global_preds_trans)

rm(global_preds, grimeDB_attributes)
gc()

# ASSESSMENT OF EXTRAPOLATION ----

names_glob <- global_preds_trans %>% 
  dplyr::select(-ends_with(c("jan", "feb", "mar", "apr", "may", "jun", "aug", "sep", "oct", "nov", "dec")),
                -COMID) %>% 
  rename_all(~ str_replace(., "jul", "month")) %>% colnames() 

names_train <- data_model_monthly %>% dplyr::select(-c(month, Log_CH4mean)) %>% colnames()


setdiff(names_glob, names_train)

# PCA of the training data
pca_training <- prcomp(data_model_monthly %>% dplyr::select(-c(month, Log_CH4mean)),
             center = FALSE,
             scale. = FALSE)


#25 axis to get to 90% of variation
summary(pca_training)

# PCA of the global data
pca_world <- prcomp(global_preds_trans %>% 
                      dplyr::select(-ends_with(c("jan", "feb", "mar", "apr", "may", "jun", "aug", "sep", "oct", "nov", "dec")),
                                    -COMID),
                       center = FALSE,
                       scale. = FALSE)

summary(pca_world)

df_pca_training = data.frame(PCA1 = pca_training$x[,"PC1"],
                             PCA2 = pca_training$x[,"PC2"],
                             PCA3 = pca_training$x[,"PC3"])

df_pca_world = data.frame(PCA1 = pca_world$x[,"PC1"],
                          PCA2 = pca_world$x[,"PC2"],
                          PCA3 = pca_world$x[,"PC3"])

ggplot()+
  stat_density_2d(data=df_pca_world, aes(PCA1, PCA2, alpha = ..level..), geom = "polygon", bins = 4)+
  geom_point(data=df_pca_training, aes(PCA1, PCA2), alpha=.5, color= "red3")





predict_methane <- function(all_models, month, global_predictors) {
  all_months <- tolower(month.abb)
  
  month_selected <- all_months[month]
  
  model_month <- all_models$model_fit[[month]]

   df_predictors <- global_predictors %>%
     select(-ends_with(setdiff(all_months, month_selected))) %>%
     rename_all(~ str_replace(., month_selected, "month"))
   
   #df_baked <- workflows::extract_recipe(model_month) %>% 
    # bake(df_predictors) 
   
   out <- predict(model_month, df_predictors)

  colnames(out) <- paste( month.abb[month],  "ch4", sep="_")
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

ggplot(ch4_jan)+
  geom_density(aes((Jan_ch4)))

dat_out <- global_preds_trans %>% select(COMID) %>% 
  bind_cols(ch4_jan, ch4_feb, ch4_mar, ch4_apr, ch4_may, ch4_jun,
            ch4_jul, ch4_aug, ch4_sep, ch4_oct, ch4_nov, ch4_dec) %>% 
  mutate(across(ends_with("ch4"), ~exp(.x)-.1))

dat_out %>% summarise(across(everything(), median))

dat_out %>% 
  pivot_longer(-COMID, values_to = "ch4", names_to = "month") %>% 
ggplot()+
  geom_density(aes(ch4))+
  facet_wrap(~month)+
  scale_x_log10(labels=scales::number)


write_csv(dat_out, "data/processed/meth_predictions.csv")

drive_upload(media = "data/processed/meth_predictions.csv",
             path="SCIENCE/PROJECTS/RiverMethaneFlux/processed/meth_predictions.csv",
             overwrite = TRUE)
