########################################.
#### Script to process files into GIS layers for further visualization. 
#### This script uses the modelled methane concentrations in scripts #4, and upscaled fluxes from scripts #5 to
#### produce GIS shapefiles for visualization. Data is aggregated with larger hexes to represent broad patterns.
#### Author:  Gerard Rocher-Ros
#### Last edit: 2023-03-10
########################################.


# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'sf',  'rnaturalearth', 'terra', 'stars')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

sf_use_s2(FALSE)

# Load files ----

#upscaled methane fluxes
meth_fluxes <- read_csv("data/processed/grades_ch4_fluxes.csv", lazy = FALSE) %>% 
  mutate(Fch4_mean = rowMeans(pick(ends_with("_ch4F")), na.rm = TRUE),
         ch4_cv = rowMeans(pick(ends_with("ch4F_cv")), na.rm = TRUE) * 100,
         Ech4_reach = rowMeans(pick(ends_with("reach")), na.rm = TRUE),
         Ech4_extrap = rowMeans(pick(ends_with("extrap")), na.rm = TRUE),
         dry_mean = rowMeans(pick(ends_with("dryout")), na.rm = TRUE)) %>% 
  dplyr::select(-runoffFL, -contains("area"), -ends_with(c("dryout", "extrap")))

#upscaled methane concentrations
#mean estimates are "month_ch4", while SD are "month_ch4_sd"
meth_concs <- lapply(list.files(path = "data/processed/meth_preds/", pattern = "ch4_preds_uncertainty", full.names = TRUE), read_csv) %>% 
  purrr::reduce(left_join, by = "COMID") %>% 
  dplyr::select(COMID, ends_with("_mean")) %>% 
  rename_with( ~str_replace(.x, "mean", "ch4")) %>% 
  dplyr::select(COMID, paste0(month.abb, "_ch4"))


# load file with discharge and k data 
hydro_k <- read_csv("data/processed/q_and_k.csv") %>% 
  dplyr::select(COMID, contains("k"), runoff_yr) %>% 
  mutate( k_mean = rowMeans(pick(ends_with("_k")), na.rm = TRUE)) %>% 
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

#world <-  ne_download(scale = 110, type = 'land', category = 'physical', 
#                      destdir = "data/processed/upscaling_extra_data/", returnclass = "sf") %>%
#  st_transform("+proj=eqearth +wktext") 

world <- read_sf("data/processed/upscaling_extra_data/ne_110m_land.shp") %>%
  st_transform("+proj=eqearth +wktext") 

rm(grades, meth_concs, meth_fluxes)
gc()



# GIS processing of files ----

#First we calculate average yearly values, takes some time 
#also turn it into a sf object
  

meth_avg <- meth_gis %>% 
  mutate( ch4_mean = rowMeans(pick(ends_with("_ch4")), na.rm = TRUE)) %>% 
  dplyr::select(COMID:lon, ch4_mean, Fch4_mean, dry_mean, ch4_cv, Jan_ch4E_reach:Dec_ch4E_reach,
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
            dry_mean = mean(dry_mean, na.rm = TRUE),
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
  st_write( "data/processed/GIS/meth_hexes_footprint_corr.shp", delete_layer = TRUE)
  #st_write( "data/processed/GIS/meth_hexes_k_corr.shp", delete_layer = TRUE)
  #st_write( "data/processed/GIS/meth_hexes_flux_corr.shp", delete_layer = TRUE)
  #st_write( "data/processed/GIS/meth_hexes_uncorrected.shp", delete_layer = TRUE)

meth_hexes_avg %>% 
  select(-contains("ch4")) %>% 
  st_transform(crs = 4326 ) %>% 
  st_write( "data/processed/GIS/icecov_hexes.shp", delete_layer = TRUE)


#now we do the extrapolated areas, aggregated by hex
#join the  data to the grid
extrap_hexes <- st_join(extrap_areas, grid, join = st_intersects)

#now aggregate all the interp data for each hex, do means and sum for area.
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


# Produce  gridded maps as  tiffs for flux rates and total emissions ----

rm(list = ls())

#upscaled methane fluxes
meth_fluxes <- read_csv("data/processed/grades_ch4_fluxes.csv", lazy = FALSE) %>% 
  dplyr::select(-runoffFL, -contains("area"))

#upscaled methane concentrations
#mean estimates are "month_ch4", while SD are "month_ch4_sd"
meth_concs <- lapply(list.files(path = "data/processed/meth_preds/", pattern = "ch4_preds_uncertainty", full.names = TRUE), read_csv) %>% 
  purrr::reduce(left_join, by = "COMID") %>% 
  rename_with( ~str_replace(.x, "mean", "ch4")) %>% 
  rename_with( ~str_replace(.x, "se", "ch4_sd")) %>% 
  dplyr::select(COMID, paste0(month.abb, "_ch4"), paste0(month.abb, "_ch4_sd"))

#read coordinates from grades
grades <- read_csv("data/processed/grades_coords.csv") 


#join it into one file 
meth_gis <- meth_concs %>% 
  left_join( grades, by = "COMID") %>%
  left_join( meth_fluxes, by = "COMID") 


rm(grades, meth_concs, meth_fluxes)
gc()


#download world map
world <-  read_sf("data/raw/gis/ne_110m_land/ne_110m_land.shp") 



#summarise some values 

meth_avg <- meth_gis %>% 
  drop_na(Jan_ch4F) %>% 
  mutate( ch4_mean = rowMeans(pick(ends_with("_ch4"))),
          Fch4_mean = rowMeans(pick(ends_with("_ch4F"))),
          Jan_ch4E = Jan_ch4E_reach + Jan_ch4E_extrap,
          Feb_ch4E = Feb_ch4E_reach + Feb_ch4E_extrap,
          Mar_ch4E = Mar_ch4E_reach + Mar_ch4E_extrap,
          Apr_ch4E = Apr_ch4E_reach + Apr_ch4E_extrap,
          May_ch4E = May_ch4E_reach + May_ch4E_extrap,
          Jun_ch4E = Jun_ch4E_reach + Jun_ch4E_extrap,
          Jul_ch4E = Jul_ch4E_reach + Jul_ch4E_extrap,
          Aug_ch4E = Aug_ch4E_reach + Aug_ch4E_extrap,
          Sep_ch4E = Sep_ch4E_reach + Sep_ch4E_extrap,
          Oct_ch4E = Oct_ch4E_reach + Oct_ch4E_extrap,
          Nov_ch4E = Nov_ch4E_reach + Nov_ch4E_extrap,
          Dec_ch4E = Dec_ch4E_reach + Dec_ch4E_extrap,
          ch4E_year = rowSums(pick(ends_with("reach"))) + rowSums(pick(ends_with("extrap")))
          ) %>% 
  dplyr::select(COMID, lat, lon, ch4_mean,  Fch4_mean, ch4E_year, Jan_ch4:Dec_ch4, Jan_ch4F:Dec_ch4F, Jan_ch4E:Dec_ch4E) %>%
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)



#make a hex grid for the world and keep only terrestrial masses
grid <- st_make_grid(
  world,
  cellsize = 0.25, #10x10km
  crs = st_crs(world),
  what = "polygons") %>% 
  st_intersection(world)

grid <- st_sf(index = 1:length(lengths(grid)), grid) 


#join the meth data to the grid
meth_hexes <- st_join(meth_avg, grid, join = st_intersects)

  #now aggregate all the meth data for each hex, do means and sum for area.
meth_hexes_avg <- meth_hexes %>% 
  st_drop_geometry() %>%
  group_by(index) %>%
  summarise(ch4_conc_avg = mean(ch4_mean, na.rm = TRUE),
            ch4_flux_avg = mean(Fch4_mean, na.rm = TRUE),
            ch4_emissions_year = sum(ch4E_year, na.rm = TRUE)/1e+6,
            Jan_ch4_conc = mean(Jan_ch4, na.rm = TRUE),
            Feb_ch4_conc = mean(Feb_ch4, na.rm = TRUE),
            Mar_ch4_conc = mean(Mar_ch4, na.rm = TRUE),
            Apr_ch4_conc = mean(Apr_ch4, na.rm = TRUE),
            May_ch4_conc = mean(May_ch4, na.rm = TRUE),
            Jun_ch4_conc = mean(Jun_ch4, na.rm = TRUE),
            Jul_ch4_conc = mean(Jul_ch4, na.rm = TRUE),
            Aug_ch4_conc = mean(Aug_ch4, na.rm = TRUE),
            Sep_ch4_conc = mean(Sep_ch4, na.rm = TRUE),
            Oct_ch4_conc = mean(Oct_ch4, na.rm = TRUE),
            Nov_ch4_conc = mean(Nov_ch4, na.rm = TRUE),
            Dec_ch4_conc = mean(Dec_ch4, na.rm = TRUE),
            Jan_ch4_flux = mean(Jan_ch4F, na.rm = TRUE),
            Feb_ch4_flux = mean(Feb_ch4F, na.rm = TRUE),
            Mar_ch4_flux = mean(Mar_ch4F, na.rm = TRUE),
            Apr_ch4_flux = mean(Apr_ch4F, na.rm = TRUE),
            May_ch4_flux = mean(May_ch4F, na.rm = TRUE),
            Jun_ch4_flux = mean(Jun_ch4F, na.rm = TRUE),
            Jul_ch4_flux = mean(Jul_ch4F, na.rm = TRUE),
            Aug_ch4_flux = mean(Aug_ch4F, na.rm = TRUE),
            Sep_ch4_flux = mean(Sep_ch4F, na.rm = TRUE),
            Oct_ch4_flux = mean(Oct_ch4F, na.rm = TRUE),
            Nov_ch4_flux = mean(Nov_ch4F, na.rm = TRUE),
            Dec_ch4_flux = mean(Dec_ch4F, na.rm = TRUE),
            Jan_ch4_emissions = sum(Jan_ch4E, na.rm = TRUE)/1e+6,
            Feb_ch4_emissions = sum(Feb_ch4E, na.rm = TRUE)/1e+6,
            Mar_ch4_emissions = sum(Mar_ch4E, na.rm = TRUE)/1e+6,
            Apr_ch4_emissions = sum(Apr_ch4E, na.rm = TRUE)/1e+6,
            May_ch4_emissions = sum(May_ch4E, na.rm = TRUE)/1e+6,
            Jun_ch4_emissions = sum(Jun_ch4E, na.rm = TRUE)/1e+6,
            Jul_ch4_emissions = sum(Jul_ch4E, na.rm = TRUE)/1e+6,
            Aug_ch4_emissions = sum(Aug_ch4E, na.rm = TRUE)/1e+6,
            Sep_ch4_emissions = sum(Sep_ch4E, na.rm = TRUE)/1e+6,
            Oct_ch4_emissions = sum(Oct_ch4E, na.rm = TRUE)/1e+6,
            Nov_ch4_emissions = sum(Nov_ch4E, na.rm = TRUE)/1e+6,
            Dec_ch4_emissions = sum(Dec_ch4E, na.rm = TRUE)/1e+6) %>%
  right_join(grid, by="index") %>%
  st_sf() 



names_for_tiffs <- names(meth_hexes_avg %>% st_drop_geometry %>% select(-index))

for(i in 1:length(names_for_tiffs)){

  st_rasterize(sf = meth_hexes_avg %>% select(names_for_tiffs[i]), 
               template = st_as_stars(st_bbox(grid), nx = 1440, ny = 695, values = NA_real_)) %>% 
    write_stars(., paste0("data/processed/GIS/tiffs_out/",names_for_tiffs[i],".tif"))
  
  print(paste("done", i))
}


filelist <- list.files("data/processed/GIS/tiffs_out", full.names = TRUE)

all_stacked <- rast(filelist)
all_stacked


#export all files
rast(filelist[grepl("_ch4_conc", filelist)]) %>%  
  writeRaster( "data/processed/GIS/final_product/river_methane_concs_monthly.tiff", overwrite=TRUE)

rast(filelist[grepl("_ch4_emissions", filelist)]) %>% 
  writeRaster( "data/processed/GIS/final_product/river_methane_emissions_monthly.tiff", overwrite=TRUE)

rast(filelist[grepl("_ch4_flux", filelist)]) %>% 
  writeRaster( "data/processed/GIS/final_product/river_methane_flux_rates_monthly.tiff", overwrite=TRUE)

rast(filelist[grepl("avg|year", filelist)]) %>% 
  writeRaster( "data/processed/GIS/final_product/river_methane_yearly.tiff", overwrite=TRUE)




