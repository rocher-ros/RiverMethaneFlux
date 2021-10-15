# Info ------
# Script to model CH4 concentrations in rivers globally.
# First part is to clean and transform data needed for the model


# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'randomForest', 'nlme', 'googledrive' , 'corrr', 'car', 'caret' )

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

# Add some custom functions ----
# Function to replace 0s to the lowest value, to avoid problems in log trans
zero_to_min <- function(x){
  min_nozero <- min( x[ x > 0 ], na.rm = TRUE )
  x[ x <= 0 ] <- min_nozero
  x
}



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
  dplyr::select(where(is.numeric))

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
  select( CH4mean,  GPP_yr:sresp_month, -area, -T_ECE, -S_ECE) %>% 
  mutate(across(.cols=vars_to_log, log)) %>%  #log transform those variables
  rename_with( ~str_c("Log", vars_to_log), .cols = all_of(vars_to_log) ) #rename the log transformed variables 

#Do again some histograms with the log transformed variables
grimeDB_attr_trans %>% 
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value" ) %>% 
  ggplot(aes(value))+
    geom_histogram(bins = 80) +
    theme_classic()+
    facet_wrap(~predictor, scales='free')

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

 
ggsave(filename= "figures/correlations.png", plot=plot_correlations, width = 18, height = 12)

# Start the modelling ----
# Select useful variables for the model 
cols_to_include <- c('LogCH4mean', 'elev', 'S_SAND', 'LogT_OC', 'Logslop', 'Loggw_month', 'npp_month','gpp_month', 'S_ESP', 'pyearRS',
                     'precip_month', 'sresp_month', 'temp_month', 'Logpopdens', 'Logwetland', 'temp_yr', 'prec_yr' , 'Loguparea', 'GPP_yr')

#check correlations between them
grimeDB_attr_trans %>% 
  select( cols_to_include) %>%
  correlate() %>% 
  rplot(shape = 20, colors = c("red", "green"), print_cor = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#select the potentially useful variables
data_for_model <-  grimeDB_attr_trans %>% 
  select(cols_to_include) %>% 
  drop_na()


## Step-wise regression ----
#1
m_min <- lm(LogCH4mean ~ 1, data = data_for_model)
m_max <-lm(
    LogCH4mean ~ elev + S_SAND + LogT_OC + Logslop + Loggw_month + npp_month +
      gpp_month + S_ESP + GPP_yr + pyearRS + precip_month + sresp_month + temp_month +
      Logpopdens + Logwetland + temp_yr + prec_yr,
    data = data_for_model )
m_red <- step(m_max,
              scope = c(upper = m_max, lower = m_min),
              trace = 1)
summary(m_red) #0.4237

vif(m_red)
print(paste("AIC =",round(AIC(m_red),0)))
print(paste("N =",length(m_red$residuals)))

