########################################.
#### R script to extract all predictor variables from the GRADES dataset, prior the modelling in the next scripts
#### Author: Gerard Rocher-Ros
#### Last edit: 2022-04-22
########################################.

# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'lubridate')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# Download required datasets form different sources:
#1 Global River Methane Database (GRiMeDB) from doi: xxxx 
#2 GRADES river network with a wide array of predictors, and ancillary variables for upscaling from doi: xxxx
#3 Optional: Output of this project to skip the processing and reproduce the figures, from doi: xxxx


## Load the GRiMeDB ----
load(file.path("data", "MethDB_tables_converted.rda"))

# fix NAs in channel type
sites_df <- sites_df %>% 
  mutate(Channel_type = ifelse(is.na(Channel_type) == TRUE, "normal", Channel_type)) 

#read the GRADES id (COMID) that belongs to each site in GRiMeDB
grime_comids <- read_csv("data/processed/sites_meth_comid.csv") %>% 
  mutate(Site_Nid= as.character(Site_Nid))


sites_clean <- sites_df %>% 
  left_join(grime_comids, by="Site_Nid") %>% 
  drop_na(COMID)



## attach the COMID to the concentration df and keep useful variables. we also fix NAs from CH4mean to close to equilibrium
conc_df_comids <- conc_df %>% 
  filter(Site_Nid %in% sites_clean$Site_Nid) %>% 
  left_join(sites_clean, by="Site_Nid") %>% 
  dplyr::select(Site_Nid, `Aggregated?`, Channel_type, COMID, distance_snapped, slope_m_m, CH4mean, CO2mean,
                date= Date_start, date_end= Date_end, discharge_measured= Q, WaterTemp_actual, WaterTemp_est  ) %>% 
  mutate(CH4mean =ifelse(CH4mean < 0.0001, 0.0001, CH4mean)) %>% 
  drop_na(CH4mean)

# Load GRADES with attributes ----
grades_attributes <- read_csv( "data/processed/grade_attributes.csv", lazy=FALSE) 

#Now attach all the annual variables to each pair of site_obs in the conc df 
grimeDB_attributes <- conc_df_comids %>% 
  left_join(grades_attributes, by="COMID")

# Now we do the monthly resolved variables, needs some thinking...
# Ok the way is to check the month of each observation and then match the value of a given variable
grimeDB_attributes_mon <- grimeDB_attributes %>% 
  mutate( gw_month = case_when(month(date) == 1 ~ gw_jan,
                               month(date) == 2 ~ gw_feb,
                               month(date) == 3 ~ gw_mar,
                               month(date) == 4 ~ gw_apr,
                               month(date) == 5 ~ gw_may,
                               month(date) == 6 ~ gw_jun,
                               month(date) == 7 ~ gw_jul,
                               month(date) == 8 ~ gw_aug,
                               month(date) == 9 ~ gw_sep,
                               month(date) == 10 ~ gw_oct,
                               month(date) == 11 ~ gw_nov,
                               month(date) == 12 ~ gw_dec),
          k_month =  case_when(month(date) == 1 ~  k_jan,
                               month(date) == 2 ~  k_feb,
                               month(date) == 3 ~  k_mar,
                               month(date) == 4 ~  k_apr,
                               month(date) == 5 ~  k_may,
                               month(date) == 6 ~  k_jun,
                               month(date) == 7 ~  k_jul,
                               month(date) == 8 ~  k_aug,
                               month(date) == 9 ~  k_sep,
                               month(date) == 10 ~ k_oct,
                               month(date) == 11 ~ k_nov,
                               month(date) == 12 ~ k_dec),
          q_month =  case_when(month(date) == 1 ~  q_jan,
                               month(date) == 2 ~  q_feb,
                               month(date) == 3 ~  q_mar,
                               month(date) == 4 ~  q_apr,
                               month(date) == 5 ~  q_may,
                               month(date) == 6 ~  q_jun,
                               month(date) == 7 ~  q_jul,
                               month(date) == 8 ~  q_aug,
                               month(date) == 9 ~  q_sep,
                               month(date) == 10 ~ q_oct,
                               month(date) == 11 ~ q_nov,
                               month(date) == 12 ~ q_dec),
          gpp_month = case_when(month(date) == 1 ~ gpp_jan,
                                month(date) == 2 ~ gpp_feb,
                                month(date) == 3 ~ gpp_mar,
                                month(date) == 4 ~ gpp_apr,
                                month(date) == 5 ~ gpp_may,
                                month(date) == 6 ~ gpp_jun,
                                month(date) == 7 ~ gpp_jul,
                                month(date) == 8 ~ gpp_aug,
                                month(date) == 9 ~ gpp_sep,
                                month(date) == 10 ~ gpp_oct,
                                month(date) == 11 ~ gpp_nov,
                                month(date) == 12 ~ gpp_dec),
          npp_month = case_when(month(date) == 1 ~ npp_jan,
                                month(date) == 2 ~ npp_feb,
                                month(date) == 3 ~ npp_mar,
                                month(date) == 4 ~ npp_apr,
                                month(date) == 5 ~ npp_may,
                                month(date) == 6 ~ npp_jun,
                                month(date) == 7 ~ npp_jul,
                                month(date) == 8 ~ npp_aug,
                                month(date) == 9 ~ npp_sep,
                                month(date) == 10 ~ npp_oct,
                                month(date) == 11 ~ npp_nov,
                                month(date) == 12 ~ npp_dec),
          temp_month = case_when(month(date) == 1 ~ temp_jan,
                                 month(date) == 2 ~ temp_feb,
                                 month(date) == 3 ~ temp_mar,
                                 month(date) == 4 ~ temp_apr,
                                 month(date) == 5 ~ temp_may,
                                 month(date) == 6 ~ temp_jun,
                                 month(date) == 7 ~ temp_jul,
                                 month(date) == 8 ~ temp_aug,
                                 month(date) == 9 ~ temp_sep,
                                 month(date) == 10 ~ temp_oct,
                                 month(date) == 11 ~ temp_nov,
                                 month(date) == 12 ~ temp_dec),
          precip_month = case_when(month(date) == 1 ~ precip_jan/31*365,
                                   month(date) == 2 ~ precip_feb/28*365,
                                   month(date) == 3 ~ precip_mar/31*365,
                                   month(date) == 4 ~ precip_apr/30*365,
                                   month(date) == 5 ~ precip_may/31*365,
                                   month(date) == 6 ~ precip_jun/30*365,
                                   month(date) == 7 ~ precip_jul/31*365,
                                   month(date) == 8 ~ precip_aug/31*365,
                                   month(date) == 9 ~ precip_sep/30*365,
                                   month(date) == 10 ~ precip_oct/31*365,
                                   month(date) == 11 ~ precip_nov/30*365,
                                   month(date) == 12 ~ precip_dec/31*365),
          tavg_month = case_when(month(date) == 1 ~ tavg_jan,
                                 month(date) == 2 ~ tavg_feb,
                                 month(date) == 3 ~ tavg_mar,
                                 month(date) == 4 ~ tavg_apr,
                                 month(date) == 5 ~ tavg_may,
                                 month(date) == 6 ~ tavg_jun,
                                 month(date) == 7 ~ tavg_jul,
                                 month(date) == 8 ~ tavg_aug,
                                 month(date) == 9 ~ tavg_sep,
                                 month(date) == 10 ~ tavg_oct,
                                 month(date) == 11 ~ tavg_nov,
                                 month(date) == 12 ~ tavg_dec),
          runoff_month = case_when(month(date) == 1 ~ runoff_jan,
                                 month(date) == 2 ~ runoff_feb,
                                 month(date) == 3 ~ runoff_mar,
                                 month(date) == 4 ~ runoff_apr,
                                 month(date) == 5 ~ runoff_may,
                                 month(date) == 6 ~ runoff_jun,
                                 month(date) == 7 ~ runoff_jul,
                                 month(date) == 8 ~ runoff_aug,
                                 month(date) == 9 ~ runoff_sep,
                                 month(date) == 10 ~ runoff_oct,
                                 month(date) == 11 ~ runoff_nov,
                                 month(date) == 12 ~ runoff_dec),
          sresp_month = case_when(month(date) == 1 ~ sresp_jan*365, #soil resp is in daily values
                                  month(date) == 2 ~ sresp_feb*365,
                                  month(date) == 3 ~ sresp_mar*365,
                                  month(date) == 4 ~ sresp_apr*365,
                                  month(date) == 5 ~ sresp_may*365,
                                  month(date) == 6 ~ sresp_jun*365,
                                  month(date) == 7 ~ sresp_jul*365,
                                  month(date) == 8 ~ sresp_aug*365,
                                  month(date) == 9 ~ sresp_sep*365,
                                  month(date) == 10 ~ sresp_oct*365,
                                  month(date) == 11 ~ sresp_nov*365,
                                  month(date) == 12 ~ sresp_dec*365) ) %>%
  dplyr::select(!ends_with(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

# quickly check one site
grimeDB_attributes_mon %>% filter(Site_Nid == "2597") %>% 
  ggplot(aes(date, gw_month))+
  geom_point()

#save to file for further use in this project
grimeDB_attributes_mon %>% 
  write_csv("data/processed/grimeDB_concs_with_grade_attributes.csv")  

