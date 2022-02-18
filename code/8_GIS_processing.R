# Info ------
# Author: Gerard Rocher-Ros
# Script make some figures of CH4 emissions in rivers globally.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'rnaturalearth', 'igraph')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# 1. Load files ----

#upscaled methane fluxes
meth_fluxes <- read_csv("data/processed/grades_ch4_fluxes.csv", lazy = FALSE) 

#upscaled methane concentrations
#mean estimates are "month_ch4", while SD are "month_ch4_sd"
meth_concs <- lapply(list.files(path = "data/processed/meth_preds/", pattern = "ch4_preds_uncertainty", full.names = TRUE), read_csv) %>% 
  purrr::reduce(left_join, by = "COMID") %>% 
  rename_with( ~str_replace(.x, "mean", "ch4")) %>% 
  rename_with( ~str_replace(.x, "sd", "ch4_sd")) %>% 
  dplyr::select(COMID, paste0(month.abb, "_ch4"), paste0(month.abb, "_ch4_sd"))

# load file with discharge and k data 
hydro_k <- read_csv("data/processed/q_and_k.csv") %>% 
  dplyr::select(COMID, Slope, contains("k")) %>% 
  rowwise() %>% 
  mutate( k_mean = mean(Jan_k:Dec_k, na.rm = TRUE)) %>% 
  dplyr::select(COMID, Slope, k_mean)

#read coordinates from grades
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  dplyr::select(COMID, lon=lon_mid, lat = lat_mid, subarea)

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
  dplyr::select(COMID:lat, ch4_mean, Fch4_mean, ch4_cv, Ech4_cv, runoff = runoffFL, 
                Jan_ch4F:Dec_ch4F, Jan_iceCov:Dec_iceCov) %>%
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



# now do the polygons with extrapolated areas 
for(Mon in month.abb){

extrap_mon <- extrap_areas %>% 
  select(int = contains(all_of(Mon))) %>% 
  filter( int < 0.9) %>% 
  st_cast("MULTIPOINT") %>% 
  st_buffer( dist = 30000 ) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_intersection(world)

#find which polygons are intersecting, to simplify geometries
g = st_intersects(extrap_mon, extrap_mon)

#get the adjancency list of all polygons
G = graph_from_adj_list(g)

clusters_id =  components(G)

#st_intersection
pols_merged <- extrap_mon %>% 
  mutate(cluster_id = clusters_id$membership) %>% 
  group_by(cluster_id) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(month = Mon, 
         size =clusters_id$csize) %>% 
  filter(size > 2)


#combining results
if( Mon == 'Jan'){extrap_pols  <- pols_merged} else {extrap_pols  <- rbind(extrap_pols ,pols_merged)}

print(paste("done", Mon))

}





extrap_hexes_avg %>% #extrap_pols %>% 
  st_transform(crs = 4326 ) %>% 
  st_write( "data/processed/GIS/extrap_sites.shp", delete_layer = TRUE)



