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
  filter(`Aggregated?` == "No", distance_snapped < 50000,
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
                 'T_ESP',"N_groundwater_agri", "N_groundwater_nat", "N_deposition_water", "P_aquaculture", "q_month",
                 "P_gnpp", "P_background", "P_load", "P_point", "P_surface_runoff_agri", "P_surface_runoff_nat", "N_retention_subgrid")

# Select useful variables for the model, some variables were removed due to a high correlation with other ones 
variables_to_remove <- c('Site_Nid','COMID','GPP_yr', 'Log_S_OC', 'T_PH_H2O', 'S_CEC_SOIL', 'T_BS', 'T_TEB', 'pyearRA', "pyearRH",  "S_PH_H2O",
                         'npp_month', 'forest', 'S_SILT', 'S_CLAY', 'S_CEC_CLAY', 'S_REF_BULK_DENSITY', 'S_BULK_DENSITY', "lon", "lat",  "S_TEB" ,
                         'S_CASO4', 'S_CASO4', "S_GRAVEL", "S_CACO3" , "S_ESP", "S_SAND", "T_REF_BULK_DENSITY", "T_CEC_CLAY", "T_CEC_SOIL",
                         "N_aquaculture", "N_gnpp", "N_load", "N_point",  "N_surface_runoff_agri", "N_surface_runoff_nat" )


#dataset with some variables log transformed
grimeDB_attr_trans <- grimeDB_attributes %>%
  mutate(across(.cols=all_of(vars_to_log), ~log(.x+.1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log)), .cols = all_of(vars_to_log) ) #rename the log transformed variables 


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

ggsave("figures/supplementary/histograms_variables.png", width=16, height = 12)



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

ggsave("figures/supplementary/correlations_predictors.png", width=16, height = 12)

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

 
ggsave(filename= "figures/supplementary/correlations.png", plot=plot_correlations, width = 18, height = 12)

# 2.  RF model using tidymodels ----
 
#Links I have been looking at for this: 
# https://www.tidymodels.org/start/recipes/
# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/


##2.1 parameter tuning  for the RF, takes several hours so welcome to skip it ----
## First run will be with the model with average values by site, to see the broad performance and tune RF parameters
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



#Model recipe, predict CH4, center and scale all predictors
grime_recipe <-  training(grime_split) %>%
  recipe(Log_CH4mean ~.) 

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
  grid = 5
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
  set_engine("ranger", importance="permutation", num.threads = n.cores)

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
    recipe(Log_CH4mean ~.) %>% 
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) 
  

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
labelled_months <- tibble(month_label = tolower(month.abb), labels =month.name)

monthly_models_unnested <- monthly_models %>% 
  mutate(month_label= fct_relevel(month_label, levels = toupper(month.abb))) %>% 
  unnest(preds)  %>% 
  left_join(labelled_months, by = "month_label") %>% 
  mutate(labels = fct_relevel(labels, month.name)) 

rms_text <- monthly_models_unnested %>% 
  group_by(labels) %>% 
  summarise(rmse = exp(sqrt(mean((Log_CH4mean - .pred)^2))) - 0.1 ) %>% 
  mutate(rms_label= paste("RMSE = ", round(rmse, 2) ))
  
ggplot(monthly_models_unnested, aes(exp(.pred), exp(Log_CH4mean)))+
  geom_point(alpha=.6)+
  geom_abline(slope=1, intercept = 0)+
  geom_text( data = rms_text,
    mapping = aes(x = 20, y = .6, label =rms_label ))+
  stat_cor(aes(label = ..rr.label..), label.y.npc = 0.1, label.x = 0.8)+ #put R2 and label
  labs(x = "**CH<sub>4</sub> predictions**<br>(mmol m<sup>-3</sup>)", 
       y = "**CH<sub>4</sub> observations** <br>(mmol m<sup>-3</sup>)")+
  scale_y_log10(labels=scales::number, limits=c(0.1, 200))+
  scale_x_log10(labels=scales::number, limits=c(0.1, 200))+
  facet_wrap(~labels, ncol = 3)+
  theme_bw()+
  theme(axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=12))


ggsave(filename= "figures/supplementary/model_perf_monthly.png", width = 9, height = 8)

#residuals vs predictions
ggplot(monthly_models_unnested, aes(.pred, Log_CH4mean-.pred))+
  geom_point(alpha = .6)+
  geom_hline(yintercept = 0, linetype = 1)+
  geom_smooth( se = FALSE, linetype = 2, color = "red")+
  labs(x = "**CH<sub>4</sub> predictions**<br>log(mmol m<sup>-3</sup>)", 
       y = "**Residuals** <br>log(mmol m<sup>-3</sup>)")+
  facet_wrap(~labels, ncol = 3)+
  theme_bw()+
  theme(axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=12))


ggsave(filename= "figures/supplementary/model_residuals_monthly.png", width = 9, height = 8)

#residuals
ggplot(monthly_models_unnested, aes( x=Log_CH4mean-.pred))+
  geom_density(alpha = .6, fill= "gray60")+
  labs( x = "**Residuals** <br>log(mmol m<sup>-3</sup>)")+
  facet_wrap(~labels, ncol = 3)+
  theme_bw()+
  theme(axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=12))

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
  filter(median(Importance)> .04657) %>%
  ungroup() %>% 
  mutate(type = str_replace(type, "Biogeochemical", "Biological"))

vi_monthly_mean <- vi_monthly %>% 
  group_by(Variable) %>% 
  summarise(Importance=mean(sqrt(Importance)),
            label=first(label),
            type=first(type)) %>% 
  ungroup() %>% 
  arrange(desc(Importance)) %>% 
  mutate(label= factor(label, unique(label)))


vi_plot <- vi_monthly %>% 
ggplot( aes(x=Importance, 
              y=  reorder(label, sqrt(Importance), FUN = mean)))+
  stat_summary( aes(fill = type), color=NA, geom="bar", fun="mean", alpha=.8)+
  stat_summary(fun.data = mean_sd, geom = "linerange", size=1.5, alpha=.6)+
  scale_x_continuous(expand = c(0,0), trans = "sqrt")+
  #scale_y_discrete(position = "right")+
  scale_fill_manual(values=c("forestgreen", "dodgerblue3","brown3", "darkgoldenrod3", "gray60", "chocolate"), name="Category")+ #
  theme_classic()+
  labs(y="", fill="Category")+
  theme(legend.position = c(.9,.15), axis.text.y =ggtext::element_markdown(), legend.title =element_text(face="bold") )



## run the data on the whole dataset, not nesting by month. This is to do PDPs ----

set.seed(123)


#Run the model for each month in a map
yearly_model <- grimeDB_attr_trans %>%
  select(-biome_label) %>% 
  group_by(COMID) %>% 
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

pdp_rf <- model_profile(explainer_rf, N = NULL)

pdp_data_plot <- pdp_rf$agr_profiles %>% 
  dplyr::select(Variable = `_vname_`, x=`_x_`, y_hat = `_yhat_`) %>%
  right_join(vi_monthly_mean, by=c("Variable")) %>%
  mutate(label=fct_relevel(label, levels(vi_monthly_mean$label))) %>% 
  group_by(Variable) %>% 
  mutate(x = (x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T))#,
         #y_hat = (y_hat - min(y_hat, na.rm = T))/(max(y_hat, na.rm = T) - min(y_hat, na.rm = T))
         ) %>% 
  drop_na(y_hat) 



### Figure VIP+ partial dependence
pdp_plot <- pdp_data_plot %>% 
  ggplot(aes(x, y_hat, color=type))+
  geom_line(size=1)+
  scale_color_manual(values = c("darkgreen", "dodgerblue4", "brown4", "darkgoldenrod4", "gray40", "chocolate4"))+ #c("forestgreen", "dodgerblue3","brown3", "darkgoldenrod3",   "gray60", "chocolate")
  facet_grid( scales = "free", rows = vars(label))+
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



#ggsave("figures/rf_figure.png",  height = 7, width = 6, dpi=500)


# combine with jitter plot of the sites targeted
load(file.path("data", "MethDB_tables_converted.rda"))

concs_forplot <- conc_df %>% 
  left_join( sites_df, by="Site_Nid") %>% 
  filter(CH4mean >= 0.001) %>%
  mutate(Channel_type = case_when(Channel_type == "DD" ~ "**Downstream <br> of a dam**",
                                  Channel_type == "PS" ~ "**Downstream of<br>  a point source**",
                                  Channel_type == "TH" ~ "**Thermogenically<br>  influenced**",
                                  Channel_type == "CAN" ~ "**Canals**",
                                  Channel_type == "DIT" ~ "**Ditches**",
                                  Channel_type == "PI" ~ "**Permafrost <br>  influenced**",
                                  TRUE ~ "**Other data**"),
         Channel_type = fct_relevel(Channel_type, "**Other data**", after = Inf)) 

othersites_plot <- 
  ggplot(concs_forplot, aes(y = Channel_type,  x = CH4mean, color= Channel_type))+
  geom_jitter(size = 2, alpha = ifelse(concs_forplot$Channel_type == "**Other data**", 0, 0.7))+
  stat_summary(fun.data = "mean_sd", size = 1, color="black")+
  geom_vline(xintercept =.18, linetype=2 )+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(trans="log10", labels = scales::number, breaks = c(.01, .1, 1, 10, 100))+
  labs(y="", x= "CH<sub>4</sub> (mmol m^-3)")+
  theme_classic()+
  theme(legend.position = "none", 
        axis.title.x = ggtext::element_markdown(),
        axis.text.y = ggtext::element_markdown(),
        #axis.ticks.y = element_line(color="transparent"),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA)) # bg of the plot

alldata_plot <- 
  concs_forplot %>% 
  mutate(all = "all") %>% 
  ggplot( aes(x = CH4mean, y=all))+
  geom_jitter(size = 1, alpha = .1, height = .58)+
  scale_x_continuous(trans="log10")+
  labs(y="", x= "")+
  theme_classic()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_line(color="transparent"),
    axis.line = element_line(color="transparent"),
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.margin = unit(c(0,0,0,3), "cm")
  )

vi_pdp_plot <- vi_plot + 
  inset_element(pdp_plot, left = -.1, bottom = -.027, right = .43, top = 1)

plot_jittered <- alldata_plot + 
  inset_element(othersites_plot, left = -.48, bottom = -.057, right = 1, top = 1)

plots_combined <- vi_pdp_plot + 
  plot_jittered +
  plot_layout(ncol=2, tag_level = 'keep') + 
  plot_annotation(tag_levels = list(c('a', '', '', 'b'))) & 
  theme(plot.tag.position = c(0.1, 1), plot.tag = element_text(size=16))

ggsave("figures/variables_jitter.png", plot = plots_combined, height = 9, width = 9.5, dpi=500)  


##Figure only of the partial dependence 
pdp_data_plot %>% 
  ggplot(aes(x, y_hat))+
  geom_line(size = 1)+
  facet_wrap( ~label, ncol = 3, scales = "free")+
  theme_classic()+
  labs(x = "", y = expression( hat(y) ))+
  theme(strip.text =  ggtext::element_markdown() )

ggsave(filename = "figures/supplementary/PDP_individual.png", height = 9, width = 8, dpi = 800)

# 3. ASSESSMENT OF SPATIAL EXTRAPOLATION ----
# This is done by looking at the coverage of sampled sites relative to the high-dimensional space of spatial predictors


#First read data of all predictors globally for all COMIDS, And transform it as the observations df

if(file.exists("data/processed/grade_attributes.csv") == TRUE) {
  print("files already downloaded")
} else {
  drive_download( file="SCIENCE/PROJECTS/RiverMethaneFlux/processed/grade_attributes.csv",
                  path="data/processed/grade_attributes.csv",
               overwrite = TRUE)
  }

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
                  ignore.case = FALSE), lat, lon, slope, area, -temp_yr, -biome_label) %>%
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


# 
assess_spatial_extrapolation <- function(df_sampled, df_global, month, plot_the_space){

    
  #select adjacent months for each month
  months_selected <- c(month - 1, month, month + 1)
  if(month == 1){ months_selected <- c(12, 1, 2) }
  if(month == 12){ months_selected <- c(11, 12, 1) }
  
  #now keep those data from the dataframe
  df <- df_sampled %>%
    filter(month %in% all_of(months_selected)) 
  
  all_months <- tolower(month.abb)
  
  month_selected <- all_months[month]
  
  #select only the month we need and make new columns for where we have observations or not
  df_together <- df_global %>%
    select(-ends_with(setdiff(all_months, month_selected))) %>%
    rename_all(~ str_replace(., month_selected, "month")) %>% 
    mutate(type = ifelse(COMID %in% df$COMID, "sampled", "world")) 
  
  
  # We do a PCA of the global dataset
  pca_all <- df_together %>% 
    dplyr::select(-COMID, -type) %>% 
    scale() %>% 
    prcomp()
  
  
  #25 axis to get to 90% of variation
  summary(pca_all)
  
  #List of the PCs
  ev_pca <- pca_all %>%
    tidy(matrix = "eigenvalues")
  
  #recover the original database, but only the columns we care, COMIDS and whether locations are sampled or not.
  pca_df <- pca_all %>%
    augment(df_together) %>%
    dplyr::select(COMID, type, starts_with(".fitted"))
  
  #Save the PCs that we want, the ones that give us 90% of the variation
  selected_pcas <- ev_pca %>% 
    filter(cumulative < 0.9) %>% 
    mutate(PC_rep = PC) 
  
  #make a vector of all possible combinations
  combinations <- selected_pcas %>% 
    expand(crossing(PC, PC_rep)) %>% 
    filter(PC != PC_rep)
  
  #prepare an empty df to save results
  dat_out <- tibble(COMID = df_together$COMID,
                    obs = 0)
  
  
  
  #now we do a loop to run it for all combinations of PC axis, (600)
  for(i in 1:length(combinations$PC)){
    
  PC_to_select <- c(paste0(".fittedPC", combinations$PC[i]), paste0(".fittedPC", combinations$PC_rep[i]) )
  
  pca_df_selected <- pca_df %>% 
    dplyr::select(COMID, type, all_of(PC_to_select)) 
  
  colnames(pca_df_selected) <- c("COMID", "type", "PC_x", "PC_y")
  
  #make a hull of the sampled locations, to characterize the space represented in the samples
  hull <- pca_df_selected %>% 
    filter(type == "sampled") %>%
    slice(chull(PC_x, PC_y))
  
  # mark which sites are inside or outside the hull, in this given plane
  points_in_hull <- sp::point.in.polygon(pca_df_selected$PC_x, pca_df_selected$PC_y, hull$PC_x, hull$PC_y )
  points_in_hull <- if_else(points_in_hull >= 1, 1, 0)
  
  #make a plot of the space, optional parameter
  if(plot_the_space == TRUE){
    
   ggplot()+
     geom_hex(data = pca_df_selected %>% filter(type == "world"), 
              aes(PC_x, PC_y))+
     geom_polygon(data = hull, aes(PC_x, PC_y), alpha = 0.4, , color= "red3")+
     geom_point(data = pca_df_selected %>% filter(type == "sampled"), 
                aes(PC_x, PC_y), alpha=.5, color= "red3")+
     scale_fill_viridis_c()+
     theme_classic()+
     labs(x = paste(PC_to_select[1]), y = paste(PC_to_select[2]),
          title= paste(PC_to_select[1], "x", PC_to_select[2]))
    ggsave(paste0( "figures/PCAs_examples/", PC_to_select[1], "x", PC_to_select[2], ".png") )
  }
  
  #update the file
  dat_out <- dat_out %>%
    mutate(obs = obs + points_in_hull)
  
  print(paste("done ",i ,"of", length(combinations$PC) ))
  
  
  }
  
  dat_out <- dat_out %>% 
    mutate(interp_month = obs/length(combinations$PC)) %>% 
    dplyr::select(-obs) %>% 
    rename_at(vars(interp_month),  ~ str_replace(. , "month", month.abb[month] ) ) 
  
 return(dat_out)

}


jan_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 1, plot_the_space = FALSE)
feb_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 2, plot_the_space = FALSE)
mar_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 3, plot_the_space = FALSE)
apr_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 4, plot_the_space = FALSE)
may_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 5, plot_the_space = FALSE)
jun_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 6, plot_the_space = FALSE)
jul_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 7, plot_the_space = FALSE)
aug_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 8, plot_the_space = FALSE)
sep_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 9, plot_the_space = FALSE)
oct_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 10, plot_the_space = FALSE)
nov_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 11, plot_the_space = FALSE)
dec_extrap <- assess_spatial_extrapolation(grimeDB_attr_trans, global_preds_trans, month = 12, plot_the_space = FALSE)

jan_extrap %>% 
  summarise(sum(interp_Jan > .8)/n())

extrap_monthly <- jan_extrap %>% 
  left_join(feb_extrap, by = "COMID") %>% 
  left_join(mar_extrap, by = "COMID") %>% 
  left_join(apr_extrap, by = "COMID") %>% 
  left_join(may_extrap, by = "COMID") %>% 
  left_join(jun_extrap, by = "COMID") %>% 
  left_join(jul_extrap, by = "COMID") %>% 
  left_join(aug_extrap, by = "COMID") %>% 
  left_join(sep_extrap, by = "COMID") %>% 
  left_join(oct_extrap, by = "COMID") %>% 
  left_join(nov_extrap, by = "COMID") %>% 
  left_join(dec_extrap, by = "COMID")  


  ggplot( aes(x=frac_int, fill=month))+
  geom_density() 

  extrap_monthly %>% 
    pivot_longer(-COMID, values_to = "frac_int", names_to = "month") %>% 
    group_by(month) %>% 
    summarise(per_above.8 = sum(frac_int >= .9) / n() )
  
  
  
write_csv(extrap_monthly, "data/processed/interpolated_COMIDS.csv")



