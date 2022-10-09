########################################.
#### Script to generate the main figures in the manuscript, some figures are generated in script #2 though
#### Author:  Gerard Rocher-Ros
#### Last edit: 2022-05-10
########################################.


# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'ggpubr', 'rnaturalearth', 'ggtext', 'lubridate', 'patchwork', 'see', 'broom', 'readxl')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# Load files ----
# aggregated hex layers

meth_hexes_avg <- st_read( "data/processed/GIS/meth_hexes_footprint_corr.shp") %>%
  mutate(Fch4_mean = ifelse(Fch4_mean > .11, NA, Fch4_mean) ) %>% 
  st_transform("+proj=eqearth +wktext") 

meth_hexes_flux_corr <- st_read( "data/processed/GIS/meth_hexes_flux_corr.shp") %>%
  st_transform("+proj=eqearth +wktext") 

meth_hexes_k_corr <- st_read( "data/processed/GIS/meth_hexes_k_corr.shp") %>%
  st_transform("+proj=eqearth +wktext") 

meth_hexes_uncapped <- st_read( "data/processed/GIS/meth_hexes_uncapped.shp") %>%
  st_transform("+proj=eqearth +wktext") 

ice_cover <- st_read( "data/processed/GIS/icecov_hexes.shp") %>%
  st_transform("+proj=eqearth +wktext") 

extrap_pols <- st_read( "data/processed/GIS/extrap_sites.shp") %>%
  st_transform("+proj=eqearth +wktext") 

mountains <- st_read("data/processed/GIS/mountains/CMEC_Mountains_Enh2018.shp") %>%
  st_transform("+proj=eqearth +wktext") 

meth_fluxes <- read_csv("data/processed/grades_ch4_fluxes.csv", lazy = FALSE) %>% 
  dplyr::select(-contains("sd"))

grades <- read_csv("data/processed/grades_coords.csv") 

grimeDB_attributes <- read_csv("data/processed/grimeDB_concs_with_grade_attributes.csv") %>% 
  drop_na(lat) %>% 
  filter(`Aggregated?` == "No",
         !str_detect(Channel_type,"DD|PI|GT|PS|TH|Th|DIT")) %>%
  mutate( month = month.name[month(date)]) %>%
  dplyr::select(COMID, Site_Nid, lat, lon, month) %>% 
  group_by(COMID, month) %>% 
  summarise(lat= mean(lat),
            lon = mean(lon)) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

#join it into one file 
meth_gis <- meth_fluxes %>% 
  left_join( grades, by = "COMID") 

rm(meth_fluxes, grades)

#download world map
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

#and lake layer
lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = "sf")


#buffer df to shrink each hex according to runoff
buffers <- meth_hexes_avg %>% 
  mutate( 
    buffer_change = case_when(
      runoff < .005 ~ -40000,
      runoff >= .005 & runoff < .02 ~ -30000,
      runoff >= .02 & runoff < .04 ~ -20000,
      runoff >= .04 & runoff < .06 ~ -12000,
      runoff >= .06 & runoff < .1 ~ -8000,
      runoff >= .1 & runoff < .5 ~ -6000,
      runoff >= .5 & runoff < 1 ~ -3000,
      runoff >= 1 & runoff < 1.5 ~ -2000,
      runoff >= 1.5 & runoff < 2 ~ -1000,
      runoff >= 2  ~ 0,
      is.na(runoff) == TRUE ~ 0)
  ) %>%
  dplyr::select(buffer_change) %>% 
  mutate( buffer_change = ifelse(is.na(buffer_change) == TRUE, 0, buffer_change)) %>% 
  st_drop_geometry()



# Map figures ----
## map of concentrations 
map_ch4 <- 
  ggplot() +
  geom_sf(
    data = st_buffer(meth_hexes_avg, dist = buffers$buffer_change), color = NA,
    aes( fill = ch4_mean) )+
  geom_sf(data = lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = - 1,
    name = "**CH<sub>4</sub> concentration** <br>(mmol m<sup>-3</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(text = element_text(family = "Helvetica"),
        legend.position = c(0.125, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


## map of fluxes
map_fch4 <- 
  ggplot()+
  geom_sf(
    data = st_buffer(meth_hexes_avg, dist = buffers$buffer_change), 
    aes(fill = Fch4_mean*16/12), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> emission** <br> (g CH<sub>4</sub> m^-2 d<sup>-1</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(
        text = element_text(family = "Helvetica"),
        legend.position = c(0.1, 0.35),
        legend.direction = "vertical",
        legend.title = element_markdown(size = 10),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


#put them together and add labels
maps_combined <- map_ch4 + 
  map_fch4 + 
  plot_annotation(tag_levels = 'a') +
  plot_layout(ncol = 1) & 
  theme(plot.tag.position = c(0.03, 1))

ggsave(maps_combined, filename = "figures/fig_maps.png", dpi= 1000, scale = .8)


# Map of the CV of concentrations ----
#map of CV of concentrations
map_ch4_cv <- 
ggplot() +
  geom_sf(
    data = meth_hexes_avg, color = NA,
    aes( fill = ch4_cv) )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    #trans = "pseudo_log", 
    name = "**CV of <br> predicted CH<sub>4</sub>** <br>(%)")+
  guides( fill = guide_colourbar(title.position = "top"))+
  coord_sf(xlim = c(-16000000, 16000000), ylim = c(-7300000, 8600000), expand = FALSE) +
  theme_void()+
  theme(text = element_text(family = "Helvetica"),
        legend.position = c(0.17, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(),
        legend.key.height  = unit(.6, 'cm'),
        legend.key.width =  unit(.4, 'cm'))


ggsave(map_ch4_cv, filename = "figures/supplementary/fig_maps_uncertainty.png", dpi= 1000, scale = .8)


# Monthly maps ----

#Show the modelled concentrations by month, with ice cover
labelled_months <- tibble(month= month.abb, labels =month.name)

#prepare the files, pivot to longer for faceting and fix the labels
meth_monthly <- meth_hexes_avg %>% 
  st_drop_geometry() %>% 
  dplyr::select(index, Jan_ch4:Dec_ch4) %>% 
  pivot_longer(-index, values_to = "ch4", names_to = "month") %>% 
  mutate(month = str_remove(month, "_ch4")) %>% 
  left_join(labelled_months, by = "month") %>% 
  mutate(labels = fct_relevel(labels, month.name)) %>% 
  left_join(meth_hexes_avg %>% select(index), by = "index") %>% 
  st_sf() 

ice_monthly <- ice_cover %>% 
  select(-runoff) %>% 
  st_drop_geometry() %>% 
  pivot_longer(-index, values_to = "ice_frac", names_to = "month") %>% 
  mutate(month = str_remove(month, "_icecov"),
         ice_frac = ifelse(ice_frac > 0.6, 1, NA)) %>% 
  left_join(labelled_months, by = "month") %>% 
  mutate(labels = fct_relevel(labels, month.name)) %>% 
  left_join(ice_cover %>% select(index), by = "index") %>% 
  drop_na(ice_frac) %>% 
  st_sf()

## plot of monthly concentrations ----
map_monthly <- 
  ggplot( meth_monthly) +
  geom_sf(aes(fill = ch4), color = NA )+
  geom_sf(data = lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  geom_sf(data = ice_monthly, fill= "powderblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> concentration** (mmol m<sup>-3</sup>)")+
    facet_wrap( ~ labels, ncol = 3)+
  guides( fill = guide_colourbar(title.position = "top"))+
  coord_sf(xlim = c(-13000000, 16000000), ylim = c(-7300000, 8300000), expand = FALSE) +
  theme_void()+
  theme(text = element_text(family = "Helvetica"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 16),
        legend.key.width =  unit(2, 'cm'),
        legend.text = element_text(size = 13),
        strip.text = element_text( size = 19))


ggsave(map_monthly, 
       filename = "figures/supplementary/map_ch4_monthly.png", dpi= 1500, height = 10, width = 12)



  ## Map of extrapolated areas monthly, with sampled sites ----
#prepare the file, pivot longer for facet_wrap and fix month labels
extrap_pols_monthly <- extrap_pols %>% 
  st_drop_geometry() %>% 
  pivot_longer(-index, values_to = "extrap", names_to = "month") %>% 
  mutate(month = str_remove(month, "interp_"),
         extrap = ifelse(extrap < 0.9, 1, NA)) %>% 
  left_join(labelled_months, by = "month") %>% 
  mutate(labels = fct_relevel(labels, month.name)) %>% 
  left_join(extrap_pols  %>% select(index), by = "index") %>% 
  drop_na(extrap) %>% 
  st_sf()


map_extrap_areas <- 
  ggplot(extrap_pols_monthly)+
  geom_sf(data = ice_monthly, fill = "powderblue", color = NA)+
  geom_sf(data = world, fill = NA, color = "gray50")+
  geom_sf(fill = "brown4", color = NA)+
  geom_sf(data = grimeDB_attributes, color="black", size = 1)+
  coord_sf(xlim = c(-13000000, 16000000), ylim = c(-7600000, 8300000), expand = FALSE)+
  facet_wrap( ~ labels, ncol = 3)+
  theme_void()+
  theme(strip.text = element_text( size = 19))

ggsave(map_extrap_areas, 
       filename = "figures/supplementary/map_extrap_monthly.png", dpi = 1500, height = 10, width = 12)



# Figure with seasonality emissions along latitudinal bands ----

# Prepare file with latitudinal bands, and sum emissions for that 
seasonal_df <- meth_gis %>% 
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

#pivot to longer for faceting
seasonal_df_long <- seasonal_df %>% 
  pivot_longer(-c(lat, lat_label), names_to = c("month", "parameter"), names_pattern = "(.*)_(.*)",
               values_to = "value") %>% 
  mutate(lat_label= fct_reorder(as_factor(lat_label), lat,  .desc = TRUE),
         month = fct_relevel(as_factor(month), month.abb)) %>% 
  pivot_wider(names_from = parameter, values_from = value)

#make the plot of monthly emissions by latitudinal bands
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

#make a plot of total emissions by latitudanl bands
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

# put them together and save them
season_lat_plot +  inset_element(total_lats, left = .97, right = 1.4, bottom = -.118, top = 1.02)

ggsave("figures/emissions_lat_season.png", width =8, height = 8, dpi = 800)



#Map alternatives of flux estimates, with capping k, capping with fluxes, correcting k by footprint and uncorrected

## map of fluxes
map_footprint <- 
  ggplot()+
  geom_sf(
    data = st_buffer(meth_hexes_avg,  dist = buffers$buffer_change), 
    aes(fill = Fch4_mean*16/12), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  geom_sf(data = mountains, fill = NA, color= "gray50", size= .2)+
  annotate("text", x = 3000000, y = -6000000, label = expression("Total flux = 13.4 Tg "~CH[4]~yr^-1 ), size= 3 )+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> flux** <br> (g CH<sub>4</sub> m^-2 d<sup>-1</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  labs(title = "k corrected for reach footprint")+
  theme_void()+
  theme(legend.position = c(0.125, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'),
        plot.title = element_text(hjust = .5))

map_k_capped <- 
  ggplot()+
  geom_sf(
    data = st_buffer(meth_hexes_k_corr,  dist = buffers$buffer_change), 
    aes(fill = Fch4_mean*16/12), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  geom_sf(data = mountains, fill = NA, color= "gray50", size= .2)+
  annotate("text", x = 3000000, y = -6000000, label = expression("Total flux = 11.8 Tg "~CH[4]~yr^-1 ), size= 3 )+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> flux** <br> (g CH<sub>4</sub> m^-2 d<sup>-1</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  labs(title = "k capped at 35 m/d")+
  theme_void()+
  theme(legend.position = c(0.125, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'),
        plot.title = element_text(hjust = .5))

map_flux_capped <- 
  ggplot()+
  geom_sf(
    data = st_buffer(meth_hexes_flux_corr,  dist = buffers$buffer_change), 
    aes(fill = Fch4_mean*16/12), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  geom_sf(data = mountains, fill = NA, color= "gray50", size= .2)+
  annotate("text", x = 3000000, y = -6000000, label = expression("Total flux = 14.5 Tg "~CH[4]~yr^-1 ), size= 3 )+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> flux** <br> (g CH<sub>4</sub> m^-2 d<sup>-1</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  labs(title= "Fluxes capped at 2 SD")+
  theme_void()+
  theme(legend.position = c(0.125, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'),
        plot.title = element_text(hjust = .5))

map_uncapped <- 
  ggplot()+
  geom_sf(
    data = st_buffer(meth_hexes_uncapped,  dist = buffers$buffer_change), 
    aes(fill = Fch4_mean*16/12), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  geom_sf(data = mountains, fill = NA, color= "gray50", size= .2)+
  annotate("text", x = 3000000, y = -6000000, label = expression("Total flux = 14.7 Tg "~CH[4]~yr^-1 ), size= 3 )+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> flux** <br> (g CH<sub>4</sub> m^-2 d<sup>-1</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  labs(title= "No correction")+
  theme_void()+
  theme(legend.position = c(0.125, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'),
        plot.title = element_text(hjust = .5))


maps_fluxes <- map_uncapped + 
  map_footprint + 
  map_k_capped + 
  map_flux_capped +
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = 'a')

ggsave("figures/supplementary/maps_fluxes_comp.png", maps_fluxes, dpi = 800, width = 9, height = 9)

# Comparison of the modelled fluxes with measured fluxes in GRiMeDB data ----

# Function to calculate gas saturation, 
# from temperature ( in Kelvin) and atm pressure (derived from elevation (in m.a.s.l) for CH4 
# Henry's Law constants from http://www.henrys-law.org
# Temperature correction using van't Hoff equation , temperature is in (Kelvin)
# Kh is in ("Kh, cp") = (mol/L/Atm) at STP
# Concentrations (mole fraction, also ppmv) in the atmosphere are approximate for 2013, should be updated over time
# AtmP is in (atmospheres)
get_Ch4eq <- function(temperature, elevation){
  
  pressure <- (1-(.0000225577*elevation))^5.25588
  Kh <- (1.4*10^-3)*exp(1700*((1/temperature)-(1/298)))
  AtmosphereConc <-  1.83
  EquilSaturation <- AtmosphereConc*Kh*pressure #umol/L, mmol/m3
  
  return(EquilSaturation)
}

# get the modelled k values 
# load file with discharge and k data 
hydro_k <- read_csv("data/processed/q_and_k.csv") %>% 
  dplyr::select(COMID, Slope, ends_with("k")) %>% 
  rowwise() %>% 
  mutate( k_mean = mean(Jan_k:Dec_k, na.rm = TRUE))

#do average fluxes
meth_avg <- meth_gis %>% 
  left_join(hydro_k %>%  dplyr::select(COMID, Slope, k_mean), by = "COMID") %>% 
  drop_na(Jan_ch4F) %>% 
  rowwise() %>% 
  mutate(Fch4_mean = mean(Jan_ch4F:Dec_ch4F, na.rm = TRUE)) %>% 
  dplyr::select(COMID, Slope, k_mean, Fch4_mean )


## Now get GRiMeDB 
load(file.path("data", "GRiMeDB.rda"))

sites_df <- sites_df %>% 
  mutate(Channel_type = ifelse(is.na(Channel_type) == TRUE, "normal", Channel_type)) 

grime_comids <- read_csv("data/processed/sites_meth_comid.csv") %>% 
  mutate(Site_Nid= as.character(Site_Nid))


# join the flux with concentration and the site dataset, 
boltzmann_k <- 8.62e-5

flux_sites <- flux_df %>% 
  left_join( conc_df, by=c("Site_Nid", "Date_start")) %>% 
  left_join( sites_df, by="Site_Nid") %>% 
  filter(`Aggregated?` == "No", WaterTemp_actual < 100, 
         !Channel_type %in% c("DD","GT", "PS", "TH","Th", "CAN")) %>%
  mutate(WaterTemp_best = ifelse(is.na(WaterTemp_actual) == TRUE, WaterTemp_est, WaterTemp_actual) ) %>% 
  drop_na(Diffusive_CH4_Flux_Mean, WaterTemp_best ) %>% 
  mutate(site_lat = paste("Site:",Site_Nid,"\nLat: " , round(Latitude,1), sep=""),
         temp_inv = 1/(boltzmann_k*(WaterTemp_best + 273.15)),
         temp_stand = 1/(boltzmann_k*288.15) - 1/(boltzmann_k*(WaterTemp_best + 273.15))) 

#sort by latitude for later plots
flux_sites$site_lat <- reorder(flux_sites$site_lat, desc(flux_sites$Latitude))

# Process all files ----
sites_clean <- sites_df %>% 
  left_join(grime_comids, by="Site_Nid") %>% 
  drop_na(COMID)



## attach the COMID to the concentration df and keep useful variables 
conc_df_comids <- conc_df %>% 
  filter(Site_Nid %in% sites_clean$Site_Nid) %>% 
  left_join(sites_clean, by="Site_Nid") %>% 
  dplyr::select(Site_Nid, Publication_Nid = Publication_Nid.x , `Aggregated?`, Channel_type, COMID,
                Latitude, distance_snapped, CH4mean, CO2mean,
                date= Date_start, discharge_measured= Q, WaterTemp_actual, WaterTemp_est  ) %>% 
  mutate(CH4mean =ifelse(CH4mean < 0, 0.0001, CH4mean)) %>% 
  drop_na(CH4mean)

#Look at sites with highest concentrations
conc_df_comids %>% 
  group_by(Site_Nid) %>% 
  summarise(CH4mean= median(CH4mean),
            Latitude = first(Latitude),
            Site_Nid = first(Site_Nid),
            COMID = first(COMID)) %>% 
  arrange(desc(CH4mean)) %>% 
  dplyr::select(CH4mean, Latitude, Site_Nid, COMID) %>% 
  print(n=30)


#get the flux df and fix some issues, also merge with conc_Df
flux_comid <-  flux_df %>% 
  mutate(Diffusive_CH4_Flux_Mean =ifelse(Diffusive_CH4_Flux_Mean < 0, 0, Diffusive_CH4_Flux_Mean)) %>% 
  filter(Site_Nid %in% sites_clean$Site_Nid) %>% 
  left_join(sites_clean, by="Site_Nid") %>% 
  left_join(conc_df, by=c("Site_Nid", "Date_start")) %>% 
  mutate(WaterTemp_best = if_else(is.na(WaterTemp_actual) == FALSE,WaterTemp_actual, WaterTemp_est ),
         k_method= str_replace(k_method, "additon", "addition"),
         k_method = str_replace(k_method, "Physical" ,"physical")) 

#now join the observed fluxes and predicted into one df. I will filter the sites that are not well snapped (>100m)
flux_comp <- flux_comid %>% 
  dplyr::select(COMID, Site_Nid, date= Date_start, distance_snapped, CH4mean, WaterTemp_best, elev=Elevation_m, CO2mean,
         Diffusive_CH4_Flux_Mean, Diff_Method, k_method ) %>% 
  filter(distance_snapped < 500) %>% 
  left_join( meth_gis, by="COMID") %>%  
  left_join(hydro_k, by = "COMID") %>% 
  drop_na(Diffusive_CH4_Flux_Mean) %>% 
  mutate(CH4mean_ex = CH4mean - get_Ch4eq(WaterTemp_best + 273.15, elev),
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
         flux_pred = case_when(month_obs == 1 ~  Jan_ch4F,
                               month_obs == 2 ~  Feb_ch4F,
                               month_obs == 3 ~  Mar_ch4F,
                               month_obs == 4 ~  Apr_ch4F,
                               month_obs == 5 ~  May_ch4F,
                               month_obs == 6 ~  Jun_ch4F,
                               month_obs == 7 ~  Jul_ch4F,
                               month_obs == 8 ~  Aug_ch4F,
                               month_obs == 9 ~  Sep_ch4F,
                               month_obs == 10 ~ Oct_ch4F,
                               month_obs == 11 ~ Nov_ch4F,
                               month_obs == 12 ~ Dec_ch4F),
         sc_obs =  1824 - 98.12 * WaterTemp_best + 2.413 * (WaterTemp_best^2) - 0.0241 * (WaterTemp_best^3),
         k600_obs = k_obs*(600/sc_obs)^(-.5),
         flux_pred = flux_pred*1000/12,
         k600_pred = k_pred*(600/sc_obs)^(-.5)
         ) %>% 
  group_by(COMID) %>% 
  summarise(k600_pred = median(k600_pred),
            k600_obs = median(k600_obs),
            flux_pred = median(flux_pred) ,
            flux_obs = median(Diffusive_CH4_Flux_Mean),
            k_method = first(k_method)) %>% 
  filter( !k_method %in% c("not determined", "other"),
          flux_pred > 0, flux_obs > 0)

flux_comp %>% group_by(k_method) %>% summarise(n=n())

plot_overall <- ggplot(data= flux_comp,
       aes(flux_obs, flux_pred))+
  geom_point( color="black", size=2)+
  #geom_point(data = flux_comp %>% 
  #         filter( k600_pred/k600_obs > .5 &  k600_pred/k600_obs < 1.5),
  #       aes(flux_obs, flux_pred), color= "red")+
  geom_abline(slope=1, intercept = 0, linetype=2)+
  geom_smooth(method="lm", se= FALSE, color= "red", linetype = 2)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
 # stat_regline_equation(label.y.npc = .9)+
  scale_x_log10(labels=scales::number, limits = c(0.0001, 1000))+
  scale_y_log10(labels=scales::number, limits = c(0.0001, 1000))+
  labs(x="Measured CH<sub>4</sub> diffusive emissions", y="Modelled CH<sub>4</sub> diffusive emissions", title = "All data")+
  theme_bw()+
  theme(legend.key.size = unit(1, 'cm'), 
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown())

plot_zoomed <- ggplot(data= flux_comp %>% 
         filter( !k_method %in% c("not determined", "other"),
                #distance_snapped < 50,
                  k600_pred/k600_obs > .5 &  k600_pred/k600_obs < 1.5, 
                flux_pred > 0, flux_obs > 0),
         aes(flux_obs, flux_pred))+
    geom_point(data=flux_comp %>% filter( !k_method %in% c("not determined", "other"),
                                          flux_pred > 0, flux_obs > 0, is.na(k600_obs) == FALSE, k600_pred/k600_obs < 100),
               aes(flux_obs, flux_pred, color = (k600_pred/k600_obs)*100), alpha=.5, size=2)+
  geom_point( color="black", size=2)+
  geom_abline(slope=1, intercept = 0, linetype=2)+
  geom_smooth(method="lm", color= "red")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y.npc = .99)+
  stat_regline_equation(label.y.npc = .9)+
  scale_x_log10(labels=scales::number, limits = c(0.01, 100))+
  scale_y_log10(labels=scales::number, limits = c(0.1, 10))+
  scale_color_viridis_c(trans="log10", labels=scales::number, breaks = c(10, 50,  150, 1000, 10000))+
  labs(x = "Measured CH<sub>4</sub> diffusive emissions", y="", 
       color = "% difference\n predicted \n vs observed k",
       title = "Points with comparable k<sub>600</sub> (+/- 50%)")+
  theme_bw()+
  theme(legend.key.size = unit(1, 'cm'),
        axis.title.x = ggtext::element_markdown(),
        plot.title =  ggtext::element_markdown())

plot_overall + 
  plot_zoomed +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag.position = c(0.1, 1), plot.tag = element_text(size=15))

ggsave("figures/supplementary/flux_comp_similark.png", width = 9, height = 4)



# Temperature dependence of diffusive fluxes ----

mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(21)

#plot with all sites with a n > 30
plot_sites <- 
  flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
  ggplot(aes(temp_stand, Diffusive_CH4_Flux_Mean, color = abs(Latitude)))+
  geom_point( size = 2, alpha = .2)+
  geom_smooth(data= flux_sites %>% 
                filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
                add_count(Site_Nid) %>% 
                filter(n > 20) %>% 
                arrange(abs(Latitude)),  
              method= "lm", se= FALSE, size = .8,
              aes(temp_stand, Diffusive_CH4_Flux_Mean, group = Site_Nid), alpha=.6)+
  geom_smooth(method= "lm", color="gray20", se= FALSE, size = 1)+
  geom_abline(slope=.96, intercept = -.39, linetype = 2)+
  annotate("text", x=2.8, y = 360, label="m = 0.96", angle = 33)+
  scale_color_viridis_c(option = "viridis", name = "Latitude")+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT) - frac(1, kT[C]), ")" ))), limits= c(-2.6, 3.1),
                     sec.axis = sec_axis(~ (273.15*boltzmann_k* . + 0.0520562)/(0.00347041 - boltzmann_k* . ), name =expression("Temperature " ( degree*C)), 
                                         breaks = seq(0, 40, by=5)) )+ # solved using wolfram alpha, y= 1/(k*(288.15)) - 1/(k*(x+273.15)), solve x 
  scale_y_continuous(trans= "log10", labels = scales::number, breaks = c(.01, .1, 1, 10, 100),
                     name= "Diffusive CH<sub>4</sub> emissions (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(axis.title.y = ggtext::element_markdown(), legend.position = c(.1,.8), legend.key.height = unit(4, 'mm'),
        legend.background=element_blank())



# Comparison with other aquatic systems ----

yvon_durocher2014 <- read_excel("~/Desktop/yvon-durocher2014.xlsx") %>% 
  filter(!temp == "see notes") %>% 
  mutate(across(c(temp, flux), as.numeric),
         flux_mmol.m2 = flux/16,
         ecosystem.type = recode(ecosystem.type, "Aquatic" = "Lakes", "Rice Paddy" = "Rice Paddies", "Wetland" = "Wetlands")) %>% 
  add_count(site) %>%
  filter(n > 5) %>% 
  dplyr::select(site, latitude, longitude, ecosystem.type, temp_c = temp, flux_mmol.m2)

#reformot our data and put together withthe other aquatic systems
data_for_Em <- flux_sites  %>% 
  filter(Diffusive_CH4_Flux_Mean > 0, WaterTemp_actual > -30) %>% 
  add_count(Site_Nid) %>% 
  filter(n > 5) %>% 
  mutate(ecosystem.type = "Rivers") %>% 
  dplyr::select(site = Site_Nid, latitude = Latitude, longitude = Longitude, ecosystem.type, 
                temp_c = WaterTemp_actual, flux_mmol.m2 = Diffusive_CH4_Flux_Mean) %>% 
  bind_rows(yvon_durocher2014) %>% 
  mutate(temp_inv = 1/(boltzmann_k*(temp_c + 273.15)))
  
#estimate Ems using linear models, need to try with lme
Em_all <- data_for_Em %>% 
  group_by(site) %>% 
  do(fit = lm(log(flux_mmol.m2) ~ temp_inv, data = .)) %>% 
  summarize(site = first(site),
            tidy(fit))  %>% 
  left_join(data_for_Em %>% 
              group_by(site) %>% 
              summarise(across(c(ecosystem.type, latitude, longitude), first))) 

library(lme4)
Em_all <- data_for_Em %>% 
  group_by(site) %>% 
  do(fit = lm(log(flux_mmol.m2) ~ temp_inv, data = .)) %>% 
  summarize(site = first(site),
            tidy(fit)) 

lme_out <- lmer(log(flux_mmol.m2) ~ temp_inv + (1 + temp_inv|site), data = data_for_Em)

plot(lme_out)
qqnorm(resid(lme_out))
qqline(resid(lme_out))

dat_together <- data_for_Em %>% left_join(coef(lme_out))


# do the density plot of Em across systems
comp_systems <- Em_all %>% 
  filter(term == "temp_inv") %>% 
  mutate(ecosystem.type = ecosystem.type %>% 
           fct_relevel("Rivers", after =Inf) %>%
           fct_relevel("Lakes", after= 2)) %>% 
  ggplot()+
  geom_density(aes(-estimate, fill= ecosystem.type, color = ecosystem.type), alpha =.2, size= 1) +
  geom_vline(xintercept = 0, linetype = 2)+ 
  scale_fill_manual(values = c("olivedrab", "mediumseagreen", "cornflowerblue", "darkblue"))+
  scale_color_manual(values = c("olivedrab", "mediumseagreen", "cornflowerblue", "darkblue"))+
  scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position = c(.2, .86))+
  guides(color = guide_legend(ncol = 1),
         fill = guide_legend(ncol = 1))+
  labs(x= "Apparent activation energy (eV)", y = "Density", color= "", fill = "")



plot_sites +
  comp_systems +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a') & 
  theme( plot.tag = element_text(size = 15))


ggsave("figures/temperature_methane_rivers.png", width = 5, height = 7.5)

Em_all %>% 
  filter(term == "temp_inv") %>% 
  group_by(ecosystem.type) %>% 
  summarise(mean(-estimate),
            median(-estimate))
Em_all %>% 
  filter(term == "temp_inv", ecosystem.type == "Rivers") %>% 
  summarise(quantile( -estimate, 0.25),
            quantile(-estimate, 0.75))

em_all_data <-flux_sites %>%
  filter(Diffusive_CH4_Flux_Mean > 0, WaterTemp_actual > -30) %>% 
  do(fit = lm(log(Diffusive_CH4_Flux_Mean) ~ temp_inv, data = .)) 
  
em_all_data$fit

wilcox.test(Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Rivers") %>% 
              pull(estimate), 
            Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Lakes") %>% 
              pull(estimate))

wilcox.test(Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Rivers") %>% 
              pull(estimate), 
            Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Wetlands") %>% 
              pull(estimate))

wilcox.test(Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Rivers") %>% 
              pull(estimate), 
            Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Rice Paddies") %>% 
              pull(estimate))

wilcox.test(Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Wetlands") %>% 
              pull(estimate), 
            Em_all %>% 
              filter(term == "temp_inv", ecosystem.type == "Lakes") %>% 
              pull(estimate))




ebullition_temp <- flux_sites %>% 
  filter(Eb_CH4_Flux_Mean > 0.001) %>% 
  ggplot(aes(temp_stand, (Eb_CH4_Flux_Mean)))+
  geom_point(aes( color=abs(Latitude)),  size = 3, alpha = .6)+
  geom_smooth(method= "lm", color="gray20", se= FALSE, size=2)+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y.npc = .99)+
  stat_regline_equation(label.y.npc = .99)+
  scale_color_viridis_c(name="Latitude")+
  #geom_abline(slope=.96, intercept = -.71, linetype = 2)+
  #annotate("text", x=2.9, y = 75, label="m = 0.96", angle=34)+
  annotate("text", x=-2.1, y = 100, label="n = 245")+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT) - frac(1, kT[C]), ")" ))), limits= c(-2.6, 3.1),
                     sec.axis = sec_axis(~ (273.15*boltzmann_k* . + 0.0520562)/(0.00347041 - boltzmann_k* . ), name =expression("Temperature " ( degree*C)), 
                                         breaks = seq(0, 40, by=5)) )+ # solved using wolfram alpha, y= 1/(k*(288.15)) - 1/(k*(x+273.15)), solve x 
  scale_y_continuous(trans= "log10", labels = scales::number, breaks = c(.01, .1, 1, 10, 100),
                     name= "Ebullitive CH<sub>4</sub> emissions <br> (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(axis.title.y = ggtext::element_markdown(), legend.position = "right")

ggsave("figures/supplementary/ebullition_temp.png", ebullition_temp,  scale = .7)


## Temperature dependence of ebullition and across river size ----
flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
  drop_na(System_size) %>% 
  mutate(System_size = fct_relevel(System_size, "large", after = Inf)) %>% 
  ggplot(aes(temp_stand, Diffusive_CH4_Flux_Mean))+
  geom_point(aes( color=abs(Latitude)),  size = 2, alpha = .6)+
  geom_smooth(method= "lm", color="gray20", se= FALSE, size = 1)+
  stat_regline_equation(label.y.npc = .99)+
  scale_color_viridis_c(name="Latitude", limits= c(0, 71))+
  #geom_abline(slope = .96, intercept = -.39, linetype = 2)+
  #annotate("text", x = 2.7, y = 90, label="m = 0.96", angle = 48)+
  #annotate("text", x = -2, y = 250, label="n = 4,417")+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT) - frac(1, kT[C]), ")" ))), limits= c(-2.6, 3.1),
                     sec.axis = sec_axis(~ (273.15*boltzmann_k* . + 0.0520562)/(0.00347041 - boltzmann_k* . ), name =expression("Temperature " ( degree*C)), 
                                         breaks = seq(0, 40, by=5)) )+ # solved using wolfram alpha, y= 1/(k*(288.15)) - 1/(k*(x+273.15)), solve x 
  scale_y_continuous(trans= "log10", labels = scales::number, breaks = c(.01, .1, 1, 10, 100),
                     name= "Diffusive CH<sub>4</sub> flux <br> (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(axis.title.y = ggtext::element_markdown(), legend.position = "right",
        strip.text = element_text(size=14, face= "bold"))+
  facet_wrap(~System_size)

ggsave("figures/supplementary/river_size_temp.png", width = 12, scale = .7)


# Ebullition assessment ----

mycolors2 <- colorRampPalette(RColorBrewer::brewer.pal(8, "Paired"))(93)
#ecomparison of ebullition and difussive fluxes in GRIMeDB, we remove observations close to 0, virtually 0
reg_compl_plot <- 
  flux_comid %>% 
  filter(Eb_CH4_Flux_Mean > 0.00001,
         Diffusive_CH4_Flux_Mean > 0.0001) %>% 
  filter(k_method %in% c("chamber + conc", "tracer addition")) %>% 
  ggplot(aes(Diffusive_CH4_Flux_Mean, Eb_CH4_Flux_Mean ))+
  geom_point( aes( color=Site_Nid), size = 2, alpha=.7)+
  geom_smooth( method="lm",  color="black", size=1)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y.npc = .99)+
  stat_regline_equation(label.y.npc = .9)+
  scale_color_manual(values = mycolors2)+
  scale_y_log10(labels=scales::number)+
  scale_x_log10(labels=scales::number)+
  labs(x = "Diffusive CH<sub>4</sub> emissions <br>(mmol m^-2 d<sup>-1</sup>)", 
       y = "Ebulitive CH<sub>4</sub> emissions <br>(mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown())


#ebullition and difussion density plots
density_comp_plot <- 
  flux_comid %>% 
  filter(Eb_CH4_Flux_Mean > 0.0001,
         Diffusive_CH4_Flux_Mean > 0.0001) %>% 
  filter(k_method %in% c("chamber + conc", "tracer addition")) %>% 
  pivot_longer(c(Eb_CH4_Flux_Mean, Diffusive_CH4_Flux_Mean ), names_to = "type", values_to = "flux") %>% 
  mutate(type= case_when(type == "Eb_CH4_Flux_Mean" ~ "Ebullition",
                         type == "Diffusive_CH4_Flux_Mean" ~ "Diffusion" )) %>% 
  ggplot(aes(type, flux, fill=type))+
  geom_violindot(fill_dots = "gray30", color_dots="gray30", dots_size = 1, width = 1.5, alpha = .7) +
  stat_summary(fun = "median", geom = "point",  size=4, colour = "black", 
               position = position_nudge(x = 0.1, y = 0), show.legend = FALSE)+
  scale_y_log10(labels=scales::number)+
  scale_fill_manual(values= c("darkblue", "cornflowerblue"))+
  labs(x = "", y = "CH<sub>4</sub> emissions<br> (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = ggtext::element_markdown(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=11, color="black"))

density_comp_plot + 
  reg_compl_plot +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag.position = c(0.1, 1), plot.tag = element_text(size=15))

ggsave("figures/supplementary/ebullition_diffusion.png", width = 9, height = 4)



#read file to compare the two methods for upscaling 
methods_comp <- read_csv("data/processed/methods_flux_comparison.csv") %>% 
  mutate(method =  as_factor(method) %>% 
           fct_recode( `Fluxes capped` = "flux_cap", `k capped` = "k_cap",
                       `Reach footprint correction` = "footprint", Uncorrected = "uncorrected") %>% 
           fct_relevel("Uncorrected", "k capped", "Fluxes capped", "Reach footprint correction" ))


#density plot comparison
ggplot()+
  stat_density_2d(data= flux_sites %>% filter(CH4mean> 0, Diffusive_CH4_Flux_Mean > 0), 
                  geom = "polygon", contour = TRUE,
                  aes(CH4mean, Diffusive_CH4_Flux_Mean, alpha = ..level..), 
                  breaks = c(0.05, 0.1, 0.25, 0.5, 1, 1.5, 2))+
  stat_density_2d(data = slice_sample(methods_comp, n= 5e+5), 
                  geom = "polygon", aes(x=ch4, y=flux*1000/16, alpha = ..level..),
                  breaks = c(0.05, 0.1, 0.25, 0.5, 1, 1.5, 2),
                  fill = "red4", color= "brown", size = .1)+
  scale_x_log10()+
  scale_y_log10(labels = scales::number)+
  facet_wrap(~method)+
  labs(x="CH<sub>4</sub> concentration <br> (mmol m^-3)", 
       y = "CH<sub>4</sub> flux <br> (mmol m^-2 d<sup>-1</sup>)",
       caption = "Grey contours are empirical data from GRiMeDB")+
  theme_bw()+
  theme(legend.position =  "none",
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        strip.background = element_blank(),
        strip.text = element_text(size=12, face = "bold"))

ggsave("figures/supplementary/flux_corrections_density.png", width = 8, height = 7)




  

mean(flux_sites$Diffusive_CH4_Flux_Mean, na.rm=TRUE)

methods_comp %>% 
  group_by(method) %>% 
  summarise(flux = mean(flux*1000/16, na.rm = TRUE))


#plot some other variables fors presentations
imp_features <- read_sf("data/processed/GIS/important_features.shp") %>%
  st_transform("+proj=eqearth +wktext") 

names(imp_features)

ggplot() +
  geom_sf(
    data = imp_features, color = NA,
    aes( fill = exp(Lg_k_mn)) )+
  geom_sf(data = lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "viridis", na.value = "gray",
    direction = - 1, trans= "log10",
    name = "**k<sub>600</sub>** <br>(m d<sup>-1</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


ggsave(filename = "figures/supplementary/map_k.jpg", dpi= 1000, scale = .6)




flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0) %>% 
  mutate(Latitude_label = case_when(Latitude >= 70 ~ -999,
                                    Latitude >= 60 & Latitude < 70 ~ 65,
                                    Latitude >= 50 & Latitude < 60 ~ 55,
                                    Latitude >= 40 & Latitude < 50 ~ 45,
                                    Latitude >= 30 & Latitude < 40 ~ 35,
                                    Latitude >= 20 & Latitude < 30 ~ 25,
                                    Latitude >= 10 & Latitude < 20 ~ 15,
                                    Latitude >=  0 & Latitude < 10 ~ 5,
                                    Latitude >= -10 & Latitude < 0 ~ -999,
                                    Latitude >= -20 & Latitude < -10 ~ -15,
                                    Latitude >= -30 & Latitude < -20 ~ -25,
                                    Latitude >= -40 & Latitude < -30 ~ -35,
                                    Latitude >= -50 & Latitude < -40 ~ -45,
                                    Latitude < -50 ~ 55) ) %>% 
  filter(Latitude_label> -990) %>% 
  group_by(Latitude_label) %>%
  do(fit = lm(log(Diffusive_CH4_Flux_Mean) ~ temp_inv, data = .)) %>% 
  summarize(Latitude_label = first(Latitude_label),
            tidy(fit))  %>% 
  arrange(desc(Latitude_label)) %>% 
  filter(term == "temp_inv") %>% 
  ggplot(aes( x = Latitude_label, y= -estimate,))+
  geom_col()+
  coord_flip()

flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0) %>% 
  mutate(Latitude_label = case_when(Latitude >= 70 ~ 75,
                                    Latitude >= 60 & Latitude < 70 ~ 65,
                                    Latitude >= 50 & Latitude < 60 ~ 55,
                                    Latitude >= 40 & Latitude < 50 ~ 45,
                                    Latitude >= 30 & Latitude < 40 ~ 35,
                                    Latitude >= 20 & Latitude < 30 ~ 25,
                                    Latitude >= 10 & Latitude < 20 ~ 15,
                                    Latitude >=  0 & Latitude < 10 ~ 5,
                                    Latitude >= -10 & Latitude < 0 ~ -5,
                                    Latitude >= -20 & Latitude < -10 ~ -15,
                                    Latitude >= -30 & Latitude < -20 ~ -25,
                                    Latitude >= -40 & Latitude < -30 ~ -35,
                                    Latitude >= -50 & Latitude < -40 ~ -45,
                                    Latitude < -50 ~ 55) ) %>% 
  arrange(desc(Latitude_label)) %>% 
  ggplot(aes(temp_stand, Diffusive_CH4_Flux_Mean ))+
  geom_point()+
  geom_smooth(method= "lm")+
  scale_y_log10()+
  facet_wrap(~Latitude_label, ncol = 1, strip.position = "right")

            

