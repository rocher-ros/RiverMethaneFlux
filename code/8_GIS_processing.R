# Info ------
# Author: Gerard Rocher-Ros
# Script make some figures of CH4 emissions in rivers globally.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'ggpubr', 'rnaturalearth', 'ggtext', 'lubridate', 'patchwork')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# 1. Load files ----
#upscaled methane fluxes
meth_fluxes <- read_csv("data/processed/grades_ch4_fluxes.csv", lazy = FALSE) %>% 
  select(-contains("sd"))

#upscaled methane concentrations
#mean estimates are "month_ch4", while SD are "month_ch4_sd"
meth_concs <- lapply(list.files(path = "data/processed/meth_preds/", pattern = "ch4_preds_uncertainty", full.names = TRUE), read_csv) %>% 
  purrr::reduce(left_join, by = "COMID") %>% 
  rename_with( ~str_replace(.x, "mean", "ch4")) %>% 
  rename_with( ~str_replace(.x, "sd", "ch4_sd"))



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
  left_join( meth_fluxes, by = "COMID") 

rm(grades, meth_concs, meth_fluxes)
gc()

# 2. Maps ----
# Firs we need some data processing

#calculate average yearly values, takes some time 
meth_avg <- meth_gis %>% 
  drop_na(Jan_ch4F) %>% 
  rowwise() %>% 
  mutate( ch4_mean = mean(Jan_ch4:Dec_ch4, na.rm = TRUE),
          Fch4_mean = mean(Jan_ch4F:Dec_ch4F, na.rm = TRUE),
          area_mean = mean(Jan_area_m2:Dec_area_m2, na.rm = TRUE)/1e+6) %>% 
  select(COMID:lat, ch4_mean, Fch4_mean, area_mean, runoff = runoffFL, Jan_ch4F:Dec_ch4F) %>%
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 


#download world map
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

#and lake layer
lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = "sf")

#and mountains
mountains <- read_sf("data/raw/gis/global_mountains/CMEC_Mountains_Enh2018.shp") %>% 
  st_transform(4326)

#make a hex grid for the world and keep only terrestrial masses
grid <- st_make_grid(
  world,
  n = c(250, 250), # grid granularity
  crs = st_crs(world),
  what = "polygons",
  square = FALSE) %>% 
  st_intersection( world)

grid <- st_sf(index = 1:length(lengths(grid)), grid) 


#join the meth data to the grid
meth_hexes <- st_join(meth_avg, grid, join = st_intersects)

#now aggregate all the meth data for each hex, do means and sum for area.
meth_hexes_avg <- meth_hexes %>% 
  group_by(index) %>%
  summarise(ch4_mean = mean(ch4_mean, na.rm = TRUE),
            Fch4_mean = mean(Fch4_mean, na.rm = TRUE),
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
            runoff = mean(runoff, na.rm = TRUE),
            area = sum(area_mean, na.rm = TRUE)) %>%
  st_sf()  



#buffer df to shrink each hex according to runoff
buffers <- meth_hexes_avg %>% 
  st_drop_geometry() %>%
  right_join(grid, by="index") %>%
  mutate(
      buffer_change = case_when(
        runoff < 25 ~ -30000,
        runoff >= 25 & runoff < 50 ~ -20000,
        runoff >= 50 & runoff < 100 ~ -15000,
        runoff >= 100 & runoff < 200 ~ -10000,
        runoff >= 200 & runoff < 400 ~ -5000,
        runoff >= 400 & runoff < 700 ~ -2000,
        runoff >= 700 & runoff < 1000 ~ -1000,
        runoff >= 1000  ~ 0,
        is.na(ch4_mean) == TRUE ~ 0),
      buffer_change_area = case_when(
        area < 25 ~ -30000,
        area >= 25 & area < 50 ~ -20000,
        area >= 50 & area < 75 ~ -15000,
        area >= 75 & area < 100 ~ -10000,
        area >= 100 & area < 150 ~ -5000,
        area >= 150 & area < 300 ~ -1000,
        area >= 300  ~ 0,
        is.na(ch4_mean) == TRUE ~ 0)
      )
 

#shrink the hexes by river area or runoff
meth_hexes_avg <- meth_hexes_avg %>%
  st_drop_geometry() %>%
  right_join(grid, by="index") %>%
  st_sf() %>% 
  st_buffer( dist = buffers$buffer_change)


for(Mon in month.abb){

extrap_mon <- extrap_areas %>%
  select(int = contains(all_of(Mon))) %>% 
  filter( int < 0.9) %>% 
  st_cast("MULTIPOINT") %>% 
  st_buffer( dist = 10000 ) %>% 
  st_cast("MULTIPOLYGON")

#find which polygons are intersecting, to simplify geometries
g = st_intersects(extrap_mon, extrap_mon)

G = graph_from_adj_list(g)

clusters_id =  components(G)

pols_merged <- extrap_mon %>% 
  mutate(cluster_id = clusters_id$membership) %>% 
  group_by(cluster_id) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(Month = Mon) 


#combining results
if( Mon == 'Jan'){extrap_pols  <- pols_merged} else {extrap_pols  <- rbind(extrap_pols ,pols_merged)}

print(paste("done", Mon))

}






meth_hexes_avg %>% 
  st_transform(crs = 4326 ) %>% 
  st_write( "data/processed/GIS/meth_hexes.shp", delete_layer = TRUE)

extrap_pols %>% 
  st_transform(crs = 4326 ) %>% 
  st_write( "data/processed/GIS/extrap_sites.shp", delete_layer = TRUE)

## map of concentrations ----
map_ch4 <- ggplot() +
  geom_sf(
    data = meth_hexes_avg, color = NA,
    aes( fill = ch4_mean) )+
  geom_sf(data = pols_merged, fill="blue3", color = NA, alpha=.5)+
 # geom_sf(data=mountains, fill="gray20", size=.08, alpha=.1, color="gray20" )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    #trans = "pseudo_log", 
    name = "**CH<sub>4</sub> conc.** <br>(mmol m<sup>-3</sup>)")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.2, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(map_ch4, filename = "figures/map_ch4_extrapJul.png", dpi=600, height = 10, width = 16)


## map of fluxes ----
map_fch4 <- 
  ggplot()+
  geom_sf(
    data = meth_hexes_avg, 
    aes(fill = Fch4_mean*16/12), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  #geom_sf(data=mountains, fill="gray20", size=.08, alpha=.1, color="gray20" )+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
   # trans = "pseudo_log", 
    #breaks= c(0,1,5,10,30),
    direction = -1,
    name = "**CH<sub>4</sub> flux** <br> (g CH<sub>4</sub> m^-2 d<sup>-1</sup>)")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.2, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

#ggsave(filename = "figures/map_ch4_flux_mountains2.png", dpi= 600, height = 10, width = 12)

maps_combined <- map_ch4 / map_fch4

ggsave(maps_combined, filename = "figures/fig_maps_w_mountains.png", dpi= 1000)
# Figure of seasonal and latitudinal patterns 

colnames(meth_gis)


seasonal_df <- meth_gis %>% 
  #dplyr::select( !ends_with("ch4")) %>% 
  mutate(lat_label = case_when(lat >= 70 ~ " > 70 \u00B0N",
                               lat >= 60 & lat < 70 ~ "60 - 70 \u00B0N",
                               lat >= 50 & lat < 60 ~ "50 - 60 \u00B0N",
                               lat >= 40 & lat < 50 ~ "40 - 50 \u00B0N",
                               lat >= 30 & lat < 40 ~ "30 - 40 \u00B0N",
                               lat >= 20 & lat < 30 ~ "20 - 30 \u00B0N",
                               lat >= 10 & lat < 20 ~ "10 - 20 \u00B0N",
                               lat >=  0 & lat < 10 ~ "0 - 10 \u00B0N",
                               lat >= -10 & lat < 0 ~ "0 - 10 \u00B0S",
                               lat >= -20 & lat < -10 ~ "10 - 20 \u00B0S",
                               lat >= -30 & lat < -20 ~ "20 - 30 \u00B0S",
                               lat >= -40 & lat < -30 ~ "30 - 40 \u00B0S",
                               lat >= -30 & lat < -20 ~ "20 - 30 \u00B0S",
                               lat >= -40 & lat < -30 ~ "30 - 40 \u00B0S",
                               lat >= -50 & lat < -40 ~ "40 - 50 \u00B0S",
                               lat < -50 ~ " > 50 \u00B0S") ) %>% 
  group_by(lat_label) %>% 
  summarise(lat = mean(lat),
            Jan_ch4E = sum(Jan_ch4E_reach + Jan_ch4E_extrap, na.rm = TRUE)/1e+12*16/12, #in Mg Ch4
            Feb_ch4E = sum(Feb_ch4E_reach + Feb_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Mar_ch4E = sum(Mar_ch4E_reach + Mar_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Apr_ch4E = sum(Apr_ch4E_reach + Apr_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            May_ch4E = sum(May_ch4E_reach + May_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Jun_ch4E = sum(Jun_ch4E_reach + Jun_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Jul_ch4E = sum(Jul_ch4E_reach + Jul_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Aug_ch4E = sum(Aug_ch4E_reach + Aug_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Sep_ch4E = sum(Sep_ch4E_reach + Sep_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Oct_ch4E = sum(Oct_ch4E_reach + Oct_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Nov_ch4E = sum(Nov_ch4E_reach + Nov_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
            Dec_ch4E = sum(Dec_ch4E_reach + Dec_ch4E_extrap, na.rm = TRUE)/1e+12*16/12,
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
            Jan_areakm2 = sum(Jan_area_m2, na.rm = TRUE)/1e+6,
            Feb_areakm2 = sum(Feb_area_m2, na.rm = TRUE)/1e+6,
            Mar_areakm2 = sum(Mar_area_m2, na.rm = TRUE)/1e+6,
            Apr_areakm2 = sum(Apr_area_m2, na.rm = TRUE)/1e+6,
            May_areakm2 = sum(May_area_m2, na.rm = TRUE)/1e+6,
            Jun_areakm2 = sum(Jun_area_m2, na.rm = TRUE)/1e+6,
            Jul_areakm2 = sum(Jul_area_m2, na.rm = TRUE)/1e+6,
            Aug_areakm2 = sum(Aug_area_m2, na.rm = TRUE)/1e+6,
            Sep_areakm2 = sum(Sep_area_m2, na.rm = TRUE)/1e+6,
            Oct_areakm2 = sum(Oct_area_m2, na.rm = TRUE)/1e+6,
            Nov_areakm2 = sum(Nov_area_m2, na.rm = TRUE)/1e+6,
            Dec_areakm2 = sum(Dec_area_m2, na.rm = TRUE)/1e+6)

seasonal_df_long <- seasonal_df %>% 
  pivot_longer(-c(lat, lat_label), names_to = c("month", "parameter"), names_pattern = "(.*)_(.*)",
               values_to = "value") %>% 
  mutate(lat_label= fct_reorder(as_factor(lat_label), lat,  .desc = TRUE),
         month = fct_relevel(as_factor(month), month.abb)) %>% 
  pivot_wider(names_from = parameter, values_from = value)

season_lat_plot <- 
  seasonal_df_long %>% 
  ggplot(aes(month, ch4E, fill=areakm2))+
  geom_col()+
  scale_y_continuous(trans = "sqrt", breaks = c(0, 0.04, .2, 0.4), 
                     limits = c(0, NA) , expand=c(0,0))+
  scale_fill_viridis_c(trans="log10", direction = -1)+
  facet_wrap(~lat_label, ncol = 1, strip.position = "right")+
  theme_bw()+
  labs(x = "", y = "**CH<sub>4</sub> emissions** <br> (Tg CH<sub>4</sub>)" , fill="**River area** <br> (km^2 )")+
  theme(axis.ticks.length.x = unit(0, "cm"),
        strip.background = element_blank(),#element_rect(color=NA, fill = "gray80"),
        strip.text.y = element_blank(), #element_text(size = 12, angle=0),
        axis.text.x = element_text(size = 12, color = "gray5"),
        axis.text.y = element_text(color = "gray40"),
        panel.border =  element_rect(color="gray70"), 
        panel.grid = element_blank(),
        axis.line.y = element_line(color="gray70"),
        axis.ticks.y = element_line(color="gray70"),
        axis.title.y = ggtext::element_markdown(size = 12),
        legend.title = ggtext::element_markdown(size = 12),
        legend.position = c(.5,-.065), legend.direction = "horizontal",
        legend.key.width = unit(1.3, "cm"), legend.key.height = unit( .3, "cm"),
        plot.margin = unit(c(0,180,40,0), "pt")
  )


total_lats <- 
  seasonal_df_long %>% 
  group_by(lat_label) %>% 
  summarise(lat= mean(lat), 
            total_flux= sum(ch4E)) %>% 
  mutate(lat_label = fct_reorder(as_factor(lat_label), lat,  .desc = FALSE) ) %>% 
  ggplot(aes( x= total_flux, y= lat_label))+
  geom_col(fill="gray80")+
  #geom_text(aes(x=2, label=lat_label))+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "**CH<sub>4</sub> emissions** <br> (Tg CH<sub>4</sub>)", y="")+
  theme_classic()+
  theme(axis.ticks.length.y = unit(0, "cm"),
        #axis.text.y = element_blank(),
        axis.text.y = element_text(size=12, color="black"),
        axis.text.x = element_text(size=12),
        axis.line.x = element_line(color="gray70"),
        axis.line.y = element_blank(),
        axis.title.x = ggtext::element_markdown(size = 12, color="black"),
        legend.title = ggtext::element_markdown(size = 12),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
  )



season_lat_plot +  inset_element(total_lats, left = .97, right = 1.4, bottom = -.118, top = 1.02)

ggsave("figures/emissions_lat_season.png", width =8, height = 8)




#meth relationships with k
ggplot(meth_avg, aes(k_mean, ch4_mean))+
  geom_hex(bins=50)+
  scale_fill_viridis_c()+
  theme_bw()


ggsave("figures/k_ch4_predicted.png")

ggplot(meth_avg, aes(k_mean, Fch4_mean))+
  geom_hex(bins=50)+
  scale_fill_viridis_c()+
  theme_bw()

ggsave("figures/k_flux_predicted.png")


meth_avg %>% 
  group_by(mountains) %>% 
  summarise(fch4= mean(Fch4_mean), 
            k = mean(k_mean))



meth_avg %>% 
  ggplot(aes(mountains, ch4_mean))+
  geom_boxplot()+
  scale_y_log10()


# Flux comparison with grime data ----
# Load grime DB
load(file.path("data", "raw", "MethDB_tables_converted.rda"))


# and the fiel with the COMID from GRADES and SiteNId from GRIME
grime_comids <- read_csv("data/processed/sites_meth_comid.csv") %>% 
  mutate(Site_Nid= as.character(Site_Nid))

#merge the files
sites_clean <- sites_df %>% 
  left_join(grime_comids, by="Site_Nid") %>% 
  drop_na(COMID)

#get the flux df and fix some issues, also merge with conc_Df
flux_comid <-  flux_df %>% 
  mutate(Diffusive_CH4_Flux_Mean =ifelse(Diffusive_CH4_Flux_Mean < 0, 0, Diffusive_CH4_Flux_Mean)) %>% 
  filter(Site_Nid %in% sites_clean$Site_Nid) %>% 
  left_join(sites_clean, by="Site_Nid") %>% 
  left_join(conc_df, by=c("Site_Nid", "Date_start")) %>% 
  mutate(WaterTemp_best = if_else(is.na(WaterTemp_actual) == FALSE,WaterTemp_actual, WaterTemp_est ),
         k_method= str_replace(k_method, "additon", "addition"),
         k_method = str_replace(k_method, "Physical" ,"physical")) 

#compare mean fluxes observed and predicted
mean(meth_avg$Fch4_mean, na.rm=TRUE)
mean(flux_comid$Diffusive_CH4_Flux_Mean, na.rm=TRUE)

#now join the observed fluxes and predicted into one df. I will filter the sites that are not well snapped (>100m)
flux_comp <- flux_comid %>% 
  select(COMID, Site_Nid, date= Date_start, distance_snapped, CH4mean, WaterTemp_best, elevation_m_new, CO2mean,
         Diffusive_CH4_Flux_Mean, Diff_Method, k_method ) %>% 
  filter(distance_snapped < 100) %>% 
  left_join( meth_gis %>% st_drop_geometry(), by="COMID") %>% 
  drop_na(Diffusive_CH4_Flux_Mean) %>% 
  mutate(CH4mean_ex = CH4mean - get_Ch4eq(WaterTemp_best+ 273.15, elev),
         CH4mean_ex = ifelse(is.na(CH4mean_ex) == TRUE,
                             CH4mean - get_Ch4eq(20+ 273.15, elev),
                             CH4mean_ex),
         k_obs =Diffusive_CH4_Flux_Mean/CH4mean,  # estimate kch4 (m d-1) as the quotient of flux (mmol m-2 d-1) and concentration (mmol m-3)
         month_obs= month(date),
         k_pred =  case_when(month_obs == 1 ~  Jan_k,
                             month_obs == 2 ~  Feb_k,
                             month_obs == 3 ~  Mar_k,
                             month_obs == 4 ~  Apr_k,
                             month_obs == 5 ~  May_k,
                             month_obs == 6 ~  Jun_k,
                             month_obs == 7 ~  Jul_k,
                             month_obs == 8 ~  Aug_k,
                             month_obs == 9 ~  Sep_k,
                             month_obs == 10 ~ Oct_k,
                             month_obs == 11 ~ Nov_k,
                             month_obs == 12 ~ Dec_k),
         flux_pred = case_when(month_obs == 1 ~  Jan_fch4,
                               month_obs == 2 ~  Feb_fch4,
                               month_obs == 3 ~  Mar_fch4,
                               month_obs == 4 ~  Apr_fch4,
                               month_obs == 5 ~  May_fch4,
                               month_obs == 6 ~  Jun_fch4,
                               month_obs == 7 ~  Jul_fch4,
                               month_obs == 8 ~  Aug_fch4,
                               month_obs == 9 ~  Sep_fch4,
                               month_obs == 10 ~ Oct_fch4,
                               month_obs == 11 ~ Nov_fch4,
                               month_obs == 12 ~ Dec_fch4),
         sc_obs =  1824 - 98.12 * WaterTemp_best + 2.413 * (WaterTemp_best^2) - 0.0241 * (WaterTemp_best^3),
         k600_obs = k_obs*(600/sc_obs)^(-.5),
         #sc_pred = 1824 - 98.12 * x + 2.413 * (x^2) - 0.0241 * (x^3),
         k600_pred = k_pred*(600/sc_obs)^(-.5)
         ) 



flux_comp %>% filter( !k_method %in% c("not determined", "other"),
                       flux_pred > 0, Diffusive_CH4_Flux_Mean > 0, is.na(k_obs) == FALSE) %>%  
  ggplot(aes(Diffusive_CH4_Flux_Mean, flux_pred, color=(k600_pred/k600_obs)*100), alpha=.5, size=2)+
  geom_point()+
  geom_abline(slope=1, intercept = 0)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  scale_color_viridis_c(trans="log10", labels=scales::number, breaks = c(1,100,10000))+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  labs(x="Measured CH4 diffusive flux", y="Modelled CH4 diffusive flux", 
       color="% difference\n predicted \n vs observed k",
       caption="Values close to 100 in the color scale mean that k is similar, 
       values of 1000 means that predicted k is 10 times higher than observed k")+
  theme_bw()+
  theme(legend.key.size = unit(1, 'cm'))

ggsave("figures/flux_comp_allk.png")

ggplot(data= flux_comp %>% filter( !k_method %in% c("not determined", "other"),
                                   #distance_snapped < 10,
                                     k600_pred/k600_obs > .5 &  k600_pred/k600_obs < 1.5, 
                                   flux_pred > 0, Diffusive_CH4_Flux_Mean > 0),
         aes(Diffusive_CH4_Flux_Mean, flux_pred))+
    geom_point(data=flux_comp %>% filter( !k_method %in% c("not determined", "other"),
                                          flux_pred > 0, Diffusive_CH4_Flux_Mean > 0, is.na(k600_obs) == FALSE),
               aes(Diffusive_CH4_Flux_Mean, flux_pred, color=(k600_pred/k600_obs)*100), alpha=.5, size=2)+
  geom_point( color="black", size=2)+
  geom_abline(slope=1, intercept = 0)+
  geom_smooth(method="lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = .9)+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_color_viridis_c(trans="log10", labels=scales::number, breaks = c(1, 50,  150, 10000))+
  labs(x="Measured CH4 diffusive flux", y="Modelled CH4 diffusive flux", 
       color="% difference\n predicted \n vs observed k",
       caption="regression now is done only with the points that have comparable k, +/- 50%")+
  theme_bw()+
  theme(legend.key.size = unit(1, 'cm'))

  ggsave("figures/flux_comp_similark.png")

flux_comp %>% 
  filter(k_pred > .01, k_obs > .01) %>% 
  rename(modelled_flux = flux_pred, measured_flux = Diffusive_CH4_Flux_Mean) %>% 
  pivot_longer(c(modelled_flux, measured_flux), values_to = "ch4_flux", names_to = "method") %>% 
  ggplot(aes(x=method, y=ch4_flux))+
  geom_boxplot(fill="blue3", alpha=.5)+
  scale_y_log10(labels=scales::number)+
  labs(x="", y="CH<sub>4</sub> flux (mmol m^-2 d^-1)")+
  theme_bw()+
  theme(axis.title.y = ggtext::element_markdown())

ggsave("figures/flux_comp_boxplot.png")

flux_comp %>% filter(k_pred > .1, k_obs > .1, !k_method %in% c("not determined", "other")) %>% 
ggplot( aes(k_obs, k_pred))+
  geom_point()+
  geom_abline(slope=1, intercept = 0)+
  geom_smooth(method="lm", se=FALSE)+
  stat_cor()+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  labs(x="Measured k", y="Modelled k")+
  theme_bw()

ggsave("figures/comp_k.png")


#figures to look at high slope/high k fluxes
slope_flux <- ggplot(meth_avg, aes(Slope, Fch4_mean))+
  geom_hex(bins=50)+
  geom_point(data=flux_comp %>%
               filter(Diffusive_CH4_Flux_Mean > 0.0001, Slope>0),
             aes(Slope, Diffusive_CH4_Flux_Mean), alpha=.4)+
  geom_smooth(data=flux_comp %>%
                  filter(Diffusive_CH4_Flux_Mean > 0.0001, Slope>0),
                aes(Slope, Diffusive_CH4_Flux_Mean), color="red3")+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()+
  labs(caption = "black points are observations", x="Slope", y="CH4 flux (mmol/m2/d)")



k_flux <- ggplot(meth_avg, aes(k_mean, Fch4_mean))+
  geom_hex(bins=50)+
  geom_point(data=flux_comp %>%
               filter(Diffusive_CH4_Flux_Mean > 0.0001, k_obs < 500, k_obs > .1),
             aes(k_obs, Diffusive_CH4_Flux_Mean), alpha=.4)+
  geom_smooth(data=flux_comp %>%
                filter(Diffusive_CH4_Flux_Mean > 0.0001, k_obs < 500, k_obs > .1),
              aes(k_obs, Diffusive_CH4_Flux_Mean), color="red3")+
  geom_vline(xintercept = 35, linetype=2, color="gray20")+
  annotate(geom="text", x=43, y=.01, label="k = 35 m/d",angle=90)+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()+
  labs( x="k (m/d)", y="CH4 flux (mmol/m2/d)")

ggarrange(k_flux, slope_flux, nrow = 1, legend = "none", align = "h")

ggsave("figures/k_flux_pred_obs.png", width = 13, height = 6)


slope_conc <- ggplot(meth_avg, aes(Slope, ch4_mean))+
  geom_hex(bins=50)+
  geom_point(data=flux_comp %>%
               filter(CH4mean > 0.0001, Slope>0),
             aes(Slope, CH4mean), alpha=.4)+
  geom_smooth(data=flux_comp %>%
                filter(CH4mean > 0.0001, Slope>0),
              aes(Slope, CH4mean), color="red3")+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()+
  labs(caption = "black points are observations", x="Slope", y="CH4  (mmol/m3)")



k_conc <- ggplot(meth_avg, aes(k_mean, ch4_mean))+
  geom_hex(bins=50)+
  geom_point(data=flux_comp %>%
               filter(CH4mean > 0.0001, k_obs < 500, k_obs > 0.001),
             aes(k_obs, CH4mean), alpha=.4)+
  geom_smooth(data=flux_comp %>%
                filter(CH4mean > 0.0001, k_obs < 500, k_obs > 0.001),
              aes(k_obs, CH4mean), color="red3")+
  geom_vline(xintercept = 35, linetype=2, color="gray20")+
  annotate(geom="text", x=43, y=11, label="k = 35 m/d",angle=90)+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()+
  labs( x="k (m/d)", y="CH4 (mmol/m3)")

ggarrange(k_conc, slope_conc, nrow = 1, legend = "none", align = "h")

ggsave("figures/k_conc_pred_obs.png", width = 13, height = 6)


#ebullition
flux_comid %>% 
  filter(Eb_CH4_Flux_Mean>0.00001) %>% 
  filter(k_method == "chamber + conc") %>% 
  ggplot(aes(Diffusive_CH4_Flux_Mean, Eb_CH4_Flux_Mean ))+
  geom_point( aes( color=Site_Nid), size=4, alpha=.4)+
  geom_smooth( method="lm", se=FALSE, color="red3", size=2, linetype=2)+
  geom_abline(intercept = 0, slope = 1)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = .9)+
  scale_y_log10(labels=scales::number)+
  scale_x_log10(labels=scales::number)+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Diffusive CH4 flux", y="Ebulitive flux", caption="Each color is a unique site in the DB",
       title=" Only ebullitive fluxes were k comes from chamber+conc")

ggsave("figures/ebulliton_diffusion.png")

#look at spatial aggregation of COMID to sites
grimeDB_attributes <- read_csv("data/processed/grimeDB_concs_with_grade_attributes.csv") %>% 
  filter(`Aggregated?` == "No",
         !str_detect(Channel_type,"DD|PI|GT|PS|TH|Th|DIT")) %>%
  mutate( month=month(date)) 

sites_many_obs <- grimeDB_attributes %>% 
  group_by(COMID) %>% 
  summarise(n_obs=n()) %>% 
  arrange(desc(n_obs)) %>% 
  #filter(n_obs > 20) %>% 
  left_join(grimeDB_attributes %>% select(COMID, Site_Nid, date, CH4mean, lat, lon))

# the example of krycklan
grimeDB_attributes %>% 
  filter(COMID == 24006204, CH4mean > .0001) %>% 
  ggplot()+
  geom_boxplot(aes(y=as.factor(Site_Nid), x= CH4mean))+
  scale_x_log10(labels=scales::number)+
  theme_bw()+
  labs(y="Site_Nid")

ggsave("figures/site_krycklan.png", width = 5, height = 10)


ggplot(meth_avg, aes(Slope, k_mean))+
  geom_hex(bins=50)

colnames(flux_comid)

flux_comid %>% 
  left_join(mountains_df) %>% 
  group_by(mountains) %>% 
  summarise(fch4= mean(Diffusive_CH4_Flux_Mean, na.rm =TRUE))
  
meth_avg %>% 
  st_drop_geometry() %>% 
  group_by(mountains) %>% 
  summarise(fch4= mean(Fch4_mean))
  
flux_comid %>% 
    left_join(mountains_df) %>% 
    ggplot(aes(x=as.factor(mountains), Diffusive_CH4_Flux_Mean ))+
  stat_summary(geom = "pointrange", fun.data = "mean_se")



meth_avg %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x=as.factor(mountains), Fch4_mean ))+
  stat_summary(geom = "pointrange", fun.data = "mean_se")
  
