# Info ------
# Author: Gerard Rocher-Ros
# Last edit: 2022-04-25
# Script to process files into GIS layers for further visualization. 
# This script uses the modelled methane concentrations in scripts #3, and upscaled fluxes from scripts #4 to
# produce GIS shapefiles for visualization. Data is aggregated with larger hexes to represent broad patterns.
# 


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'sf',  'rnaturalearth')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# 1. Load files ----

#upscaled methane fluxes
meth_fluxes <- read_csv("data/processed/grades_ch4_fluxes.csv", lazy = FALSE) %>% 
  dplyr::select(-runoffFL, -contains("area"))

#upscaled methane concentrations
#mean estimates are "month_ch4", while SD are "month_ch4_sd"
meth_concs <- lapply(list.files(path = "data/processed/meth_preds/", pattern = "ch4_preds_uncertainty", full.names = TRUE), read_csv) %>% 
  purrr::reduce(left_join, by = "COMID") %>% 
  rename_with( ~str_replace(.x, "mean", "ch4")) %>% 
  rename_with( ~str_replace(.x, "sd", "ch4_sd")) %>% 
  dplyr::select(COMID, paste0(month.abb, "_ch4"), paste0(month.abb, "_ch4_sd"))

# load file with discharge and k data 
hydro_k <- read_csv("data/processed/q_and_k.csv") %>% 
  dplyr::select(COMID, contains("k"), runoff_yr) %>% 
  rowwise() %>% 
  mutate( k_mean = mean(Jan_k:Dec_k, na.rm = TRUE)) %>% 
  dplyr::select(COMID, k_mean, runoff_yr)

#read coordinates from grades
grades <- read_csv("data/processed/grades_coords.csv") 

#read extrapolated areas
extrap_areas <- read_csv("data/processed/interpolated_COMIDS.csv") %>% 
  left_join(grades %>% dplyr::select(COMID, lat, lon)) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

#join it into one file 
meth_gis <- meth_concs %>% 
  left_join( grades, by = "COMID") %>%
  left_join( meth_fluxes, by = "COMID") %>% 
  left_join(hydro_k, by = "COMID") 

#download world map
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

rm(grades, meth_concs, meth_fluxes)
gc()



# 2. GIS processing of files ----

#First we calculate average yearly values, takes some time 
#also turn it into a sf object
meth_avg <- meth_gis %>% 
  drop_na(Jan_ch4F) %>% 
  rowwise() %>% 
  mutate( ch4_mean = mean(Jan_ch4:Dec_ch4, na.rm = TRUE),
          Fch4_mean = mean(Jan_ch4F:Dec_ch4F, na.rm = TRUE),
          ch4_sd = mean(Jan_ch4_sd:Dec_ch4_sd, na.rm = TRUE),
          ch4_cv = ch4_sd / ch4_mean * 100,
          Ech4_cv =mean(Jan_ch4F_cv:Dec_ch4F_cv, na.rm = TRUE) * 100 ) %>% 
  dplyr::select(COMID:lon, ch4_mean, Fch4_mean, ch4_cv, Ech4_cv, Jan_ch4E_reach:Dec_ch4E_reach,
                Jan_ch4F:Dec_ch4F, Jan_iceCov:Dec_iceCov, k_mean, runoff = runoff_yr) %>%
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 



#make a hex grid for the world and keep only terrestrial masses
grid <- st_make_grid(
  world,
  n = c(250, 250), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>% 
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid) 

rm(meth_gis)

#join the meth data to the grid
meth_hexes <- st_join(meth_avg, grid, join = st_intersects)

#now aggregate all the meth data for each hex, do means and sum for area.
meth_hexes_avg <- meth_hexes %>% 
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(ch4_mean = mean(ch4_mean, na.rm = TRUE),
            Fch4_mean = mean(Fch4_mean, na.rm = TRUE),
            ch4_cv = mean(ch4_cv, na.rm = TRUE),
            Ech4_cv = mean(Ech4_cv, na.rm = TRUE),
            Jan_ch4 = mean(Jan_ch4, na.rm = TRUE),
            Feb_ch4 = mean(Feb_ch4, na.rm = TRUE),
            Mar_ch4 = mean(Mar_ch4, na.rm = TRUE),
            Apr_ch4 = mean(Apr_ch4, na.rm = TRUE),
            May_ch4 = mean(May_ch4, na.rm = TRUE),
            Jun_ch4 = mean(Jun_ch4, na.rm = TRUE),
            Jul_ch4 = mean(Jul_ch4, na.rm = TRUE),
            Aug_ch4 = mean(Aug_ch4, na.rm = TRUE),
            Sep_ch4 = mean(Sep_ch4, na.rm = TRUE),
            Oct_ch4 = mean(Oct_ch4, na.rm = TRUE),
            Nov_ch4 = mean(Nov_ch4, na.rm = TRUE),
            Dec_ch4 = mean(Dec_ch4, na.rm = TRUE),
            Jan_ch4F = mean(Jan_ch4F, na.rm = TRUE),
            Feb_ch4F = mean(Feb_ch4F, na.rm = TRUE),
            Mar_ch4F = mean(Mar_ch4F, na.rm = TRUE),
            Apr_ch4F = mean(Apr_ch4F, na.rm = TRUE),
            May_ch4F = mean(May_ch4F, na.rm = TRUE),
            Jun_ch4F = mean(Jun_ch4F, na.rm = TRUE),
            Jul_ch4F = mean(Jul_ch4F, na.rm = TRUE),
            Aug_ch4F = mean(Aug_ch4F, na.rm = TRUE),
            Sep_ch4F = mean(Sep_ch4F, na.rm = TRUE),
            Oct_ch4F = mean(Oct_ch4F, na.rm = TRUE),
            Nov_ch4F = mean(Nov_ch4F, na.rm = TRUE),
            Dec_ch4F = mean(Dec_ch4F, na.rm = TRUE),
            Jan_ch4E = sum(Jan_ch4E_reach, na.rm = TRUE)/1000,
            Feb_ch4E = sum(Feb_ch4E_reach, na.rm = TRUE)/1000,
            Mar_ch4E = sum(Mar_ch4E_reach, na.rm = TRUE)/1000,
            Apr_ch4E = sum(Apr_ch4E_reach, na.rm = TRUE)/1000,
            May_ch4E = sum(May_ch4E_reach, na.rm = TRUE)/1000,
            Jun_ch4E = sum(Jun_ch4E_reach, na.rm = TRUE)/1000,
            Jul_ch4E = sum(Jul_ch4E_reach, na.rm = TRUE)/1000,
            Aug_ch4E = sum(Aug_ch4E_reach, na.rm = TRUE)/1000,
            Sep_ch4E = sum(Sep_ch4E_reach, na.rm = TRUE)/1000,
            Oct_ch4E = sum(Oct_ch4E_reach, na.rm = TRUE)/1000,
            Nov_ch4E = sum(Nov_ch4E_reach, na.rm = TRUE)/1000,
            Dec_ch4E = sum(Dec_ch4E_reach, na.rm = TRUE)/1000,
            Jan_icecov = mean(Jan_iceCov, na.rm = TRUE),
            Feb_icecov = mean(Feb_iceCov, na.rm = TRUE),
            Mar_icecov = mean(Mar_iceCov, na.rm = TRUE),
            Apr_icecov = mean(Apr_iceCov, na.rm = TRUE),
            May_icecov = mean(May_iceCov, na.rm = TRUE),
            Jun_icecov = mean(Jun_iceCov, na.rm = TRUE),
            Jul_icecov = mean(Jul_iceCov, na.rm = TRUE),
            Aug_icecov = mean(Aug_iceCov, na.rm = TRUE),
            Sep_icecov = mean(Sep_iceCov, na.rm = TRUE),
            Oct_icecov = mean(Oct_iceCov, na.rm = TRUE),
            Nov_icecov = mean(Nov_iceCov, na.rm = TRUE),
            Dec_icecov = mean(Dec_iceCov, na.rm = TRUE),
            runoff = mean(runoff, na.rm = TRUE)) %>%
  right_join(grid, by="index") %>%
  st_sf() 



#export the files
meth_hexes_avg %>% 
  select(-contains("icecov")) %>% 
  st_transform(crs = 4326 ) %>% 
  st_write( "data/processed/GIS/meth_hexes.shp", delete_layer = TRUE)

meth_hexes_avg %>% 
  select(-contains("ch4")) %>% 
  st_transform(crs = 4326 ) %>% 
  st_write( "data/processed/GIS/icecov_hexes.shp", delete_layer = TRUE)


#now we do the extrapolated areas, aggregated by hex
#join the meth data to the grid
extrap_hexes <- st_join(extrap_areas, grid, join = st_intersects)

#now aggregate all the meth data for each hex, do means and sum for area.
extrap_hexes_avg <- extrap_hexes %>% 
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(interp_Jan = mean(interp_Jan, na.rm = TRUE),
            interp_Feb = mean(interp_Feb, na.rm = TRUE),
            interp_Mar = mean(interp_Mar, na.rm = TRUE),
            interp_Apr = mean(interp_Apr, na.rm = TRUE),
            interp_May = mean(interp_May, na.rm = TRUE),
            interp_Jun = mean(interp_Jun, na.rm = TRUE),
            interp_Jul = mean(interp_Jul, na.rm = TRUE),
            interp_Aug = mean(interp_Aug, na.rm = TRUE),
            interp_Sep = mean(interp_Sep, na.rm = TRUE),
            interp_Oct = mean(interp_Oct, na.rm = TRUE),
            interp_Nov = mean(interp_Nov, na.rm = TRUE),
            interp_Dec = mean(interp_Dec, na.rm = TRUE)) %>%
  right_join(grid, by="index") %>%
  st_sf() 


extrap_hexes_avg %>% #extrap_pols %>% 
  st_transform(crs = 4326 ) %>% 
  st_write( "data/processed/GIS/extrap_sites.shp", delete_layer = TRUE)


## Aggregated maps of important variables ----


#read the global predictors df
global_preds <- read_csv( "data/processed/grade_attributes.csv", lazy=FALSE) %>% 
  select(starts_with(c("COMID" ,"slop", "elev", "pyearRS", "gw_", "temp_yr", "k_", "T_OC" ,"peatland_cover", "GPP_yr", "prec_yr", 
                     "popdens", "P_load", "S_BS", "S_SILt", "T_REF_BULK_DENSITY", "S_GRAVEL" )))


names(global_preds) %>% sort()

vars_to_log_glob <-  c("slop", "gw_mean", "k_mean", "T_OC", "P_load" , "popdens")


#do same transformations to the global dataset
features_important <- global_preds %>%
  rowwise() %>% 
  mutate(
         k_mean = mean(k_jan:k_dec),
         gw_mean = mean(gw_jan:gw_dec)) %>% 
  dplyr::select(-ends_with(month.abb)) %>% 
  mutate(across(.cols = all_of(vars_to_log_glob),
                ~log(.x + .1))) %>%  #log transform those variables, shift a bit from 0 as well
  rename_with( ~str_c("Log_", all_of(vars_to_log_glob)), .cols = all_of(vars_to_log_glob) )  %>% #rename the log transformed variables 
  drop_na() %>% 
  left_join(grades) %>% 
  dplyr::select(-COMID)

rm(grades, global_preds)
gc()

#download world map
world <-  read_sf("data/raw/gis/ne_110m_land/ne_110m_land.shp") %>% 
  st_transform("+proj=eqearth +wktext") 


#make a hex grid for the world and keep only terrestrial masses
grid <- st_make_grid(
  world,
  n = c(400, 400), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>% 
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid) 


#join the meth data to the grid
features_hexes <- features_important %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") %>% 
  st_join( grid, join = st_intersects)

#now aggregate all the meth data for each hex, do means and sum for area.
features_hexes_avg <- features_hexes %>% 
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  right_join(grid, by="index") %>%
  st_sf() 

features_hexes_avg %>% 
  drop_na(everything()) %>% 
  st_sf() %>% 
  st_transform(crs = 4326) %>% 
  write_sf( "data/processed/GIS/important_features.shp", delete_layer = TRUE)
