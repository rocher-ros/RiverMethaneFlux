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
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv", lazy = FALSE) %>% 
  dplyr::select(-Length, -slope)


#turn it an sf object
meth_gis <- left_join(meth_concs, grades, by = "COMID") %>% 
  mutate(Jan_fch4 = (Jan_ch4 - Jan_ch4eq)*Jan_k,
         Feb_fch4 = (Feb_ch4 - Feb_ch4eq)*Feb_k,
         Mar_fch4 = (Mar_ch4 - Mar_ch4eq)*Mar_k,
         Apr_fch4 = (Apr_ch4 - Apr_ch4eq)*Apr_k,
         May_fch4 = (May_ch4 - May_ch4eq)*May_k,
         Jun_fch4 = (Jun_ch4 - Jun_ch4eq)*Jun_k,
         Jul_fch4 = (Jul_ch4 - Jul_ch4eq)*Jul_k,
         Aug_fch4 = (Aug_ch4 - Aug_ch4eq)*Aug_k,
         Sep_fch4 = (Sep_ch4 - Sep_ch4eq)*Sep_k,
         Oct_fch4 = (Oct_ch4 - Oct_ch4eq)*Oct_k,
         Nov_fch4 = (Nov_ch4 - Nov_ch4eq)*Nov_k,
         Dec_fch4 = (Dec_ch4 - Dec_ch4eq)*Dec_k
         ) %>%
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

rm(grades, meth_concs)


gc()

#calculate average methane for each site
meth_avg <- meth_gis %>% 
  rowwise() %>% 
  mutate( ch4_mean = mean(Jan_ch4:Dec_ch4),
          Fch4_mean = mean(Jan_fch4:Dec_fch4),
          k_mean = mean(Jan_k:Dec_k)) %>% 
  select(COMID, ch4_mean, Fch4_mean, k_mean, runoff, runoffFL, geometry) %>% 
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



meth_hexes <- st_join(meth_avg, grid, join = st_intersects)

meth_hexes_avg <- meth_hexes %>% 
  group_by(index) %>%
  summarise(methane = mean(ch4_mean, na.rm = TRUE),
            methane.flux = sum(Fch4_mean, na.rm = TRUE),
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
    direction = -1,
    #trans = "log10", 
    name = "Methane concentration (umol/L)")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.15),
        legend.direction = "horizontal")

ggsave(filename = "figures/map_ch4.png", dpi=600, height = 10, width = 16)


ggplot() +
  geom_sf(
    data = meth_hexes_avg2 %>%  mutate(methane.flux= ifelse(methane.flux > 5000,5000, methane.flux)), 
                                       color = NA, aes( fill = methane.flux) )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    #trans = "log10", 
    direction = -1,
    name = "Methane flux")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.15),
        legend.direction = "horizontal")

ggsave(filename = "figures/map_ch4_flux.png", dpi=600, height = 10, width = 16)



ch4E_hybas <- read_csv("output/ch4E_hybas.csv")

ch4E_hybas %>% 
  summarise(across(ch4E_Jan:ch4E_Dec, sum)) %>% 
  pivot_longer( everything(), names_to = "month", values_to = "flux") %>% 
  mutate(per_day=flux/12) %>% 
  summarise(sum=sum(per_day))
  


median(meth_avg$Fch4_mean, na.rm=TRUE)


ggplot(meth_avg, aes(ch4_mean, Fch4_mean))+
  geom_hex(bins=50)+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()

grimeDB_attributes %>% 
  select(CH4mean) %>% 
  mutate(type="observations") %>% 
  bind_rows(meth_avg %>% 
              st_drop_geometry() %>% 
              select(CH4mean= ch4_mean) %>% 
              mutate(type="predictions")) %>% 
ggplot()+
  geom_density(aes(CH4mean), alpha=.6)+
  scale_x_log10(labels=scales::number)+
  facet_wrap(~type, ncol = 1, scales = "free_y")+
  theme_bw()

min(grimeDB_attributes$CH4mean)

meth_data <- meth_gis %>% 
  st_drop_geometry() %>% 
  select(COMID, Jan_ch4:Dec_ch4) %>% 
  pivot_longer(-COMID, values_to = "CH4mean", names_to = "month")

meth_data %>% 
  ggplot()+
  geom_density(aes(CH4mean))+
  facet_wrap(~month)
