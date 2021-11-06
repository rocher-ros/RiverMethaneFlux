# Info ------
# Author: Gerard Rocher-Ros
# Script upscale CH4 emissions in rivers globally.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'ggpubr', 'rnaturalearth')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)
# 1. Load files ----

#upscaled methane concentrations
meth_concs <- read_csv("data/processed/meth_predictions.csv")

#read coordinates from grades
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_lat_lon.csv")

#turn it an sf object
meth_coords <- left_join(meth_concs, grades, by = "COMID") %>% 
  st_as_sf( coords = c("lat", "lon"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

rm(grades, meth_concs)


#calculate average methane for each site
meth_avg <- meth_coords %>% 
  drop_na(ch4_jan:ch4_dec) %>% 
  mutate( ch4_mean = rowMeans(select(., ch4_jan:ch4_dec), na.rm = TRUE)) %>% 
  select(COMID,ch4_mean, temp_yr, geometry) %>% 
  st_sf()


# Map
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = "sf")

grid <- st_make_grid(
  world,
  n = c(250, 250), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>% 
  st_intersection( world)

grid <- st_sf(index = 1:length(lengths(grid)), grid) 

ggplot() +
  geom_sf(data = grid, fill = "white", color = "black", size = 0.0725) +
  coord_sf(datum = NA) +
  theme_minimal()


meth_hexes <- st_join(meth_avg, grid, join = st_intersects)

meth_hexes_avg <- meth_hexes %>% 
  group_by(index) %>%
  summarise(methane = mean(ch4_mean, na.rm = TRUE),
            temp= mean(temp_yr, na.rm = TRUE)) %>% 
  mutate(buffer_change = case_when(
    temp < 0 ~ -100,
    temp >=0 & temp < 20~ -6000,
    temp >= 20 ~ -12000)) %>% 
  drop_na(temp) %>%
  st_sf() %>% 
  st_buffer( dist= meth_hexes_avg$buffer_change)


meth_hexes_avg2 <- meth_hexes_avg %>%
  st_drop_geometry() %>%
  right_join(grid, by="index") %>% 
  st_sf()

ggplot() +
  geom_sf(
    data = meth_hexes_avg2, color = NA,
    aes( fill = methane) )+
  geom_sf(data=lakes, fill="white", color="white")+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    trans = "log10", 
    name = "Methane concentration (umol/L)")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_map()+
  theme(legend.position = c(0.4, 0.05),
        legend.direction = "horizontal")

ggsave(filename = "figures/map_ch4.png", dpi=500, height = 10, width = 16)

tmap(land)
land <- land %>% st_as_sf()



tmap_mode("plot")
tm_shape(land)+
  tm_raster("trees", palette = terrain.colors(10))
