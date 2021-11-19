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
meth_concs <- read_csv("output/grades_ch4_k_q.csv", lazy = FALSE)

#read coordinates from grades
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv", lazy = FALSE)


#turn it an sf object
meth_gis <- left_join(meth_concs, grades, by = "COMID") %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

rm(grades, meth_concs)



#calculate average methane for each site
meth_avg <- meth_gis %>% 
  rowwise() %>% 
  mutate( ch4_mean = mean(Jan_ch4:Dec_ch4)) %>% 
  select(COMID, ch4_mean, runoff, runoffFL, geometry) %>% 
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
            runoff= mean(runoff, na.rm = TRUE) ) %>%
  st_sf()  

buffers <- meth_hexes_avg %>% 
  st_drop_geometry() %>%
  right_join(grid, by="index") %>%
  mutate(buffer_change = case_when(
    runoff < 25 ~ -30000,
    runoff >= 25 & runoff < 50 ~ -20000,
    runoff >=50 & runoff < 100 ~ -15000,
    runoff >=100 & runoff < 200 ~ -10000,
    runoff >= 200 & runoff < 400 ~ -5000,
    runoff >= 400 & runoff < 700 ~ -2000,
    runoff >= 700 & runoff < 1000 ~ -1000,
    runoff >= 1000  ~ 0,
    is.na(methane) == TRUE ~ 0))
 


meth_hexes_avg2 <- meth_hexes_avg %>%
  st_drop_geometry() %>%
  right_join(grid, by="index") %>%
  st_sf() %>% 
  st_buffer( dist=  buffers$buffer_change)

ggplot() +
  geom_sf(
    data = meth_hexes_avg2, color = NA,
    aes( fill = methane) )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    #trans = "log10", 
    name = "Methane concentration (umol/L)")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.15),
        legend.direction = "horizontal")

ggsave(filename = "figures/map_ch4.png", dpi=600, height = 10, width = 16)


ch4E_hybas <- read_csv("output/ch4E_hybas.csv")

ch4E_hybas %>% 
  summarise(across(ch4E_Jan:ch4E_Dec, sum)) %>% 
  pivot_longer( everything(), names_to = "month", values_to = "flux") %>% 
  mutate(per_day=flux/12) %>% 
  summarise(sum=sum(per_day))
  


min(ch4E_hybas$ch4_Apr)

