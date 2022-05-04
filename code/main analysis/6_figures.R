# Info ------
# Author: Gerard Rocher-Ros
# Script make some figures of CH4 emissions in rivers globally.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'ggpubr', 'rnaturalearth', 'ggtext', 'lubridate', 'patchwork', 'see')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# 1. Load files ----
# aggregated hex layers

meth_hexes_avg <- st_read( "data/processed/GIS/meth_hexes.shp") %>%
  st_transform("+proj=eqearth +wktext") 

ice_cover <- st_read( "data/processed/GIS/icecov_hexes.shp") %>%
  st_transform("+proj=eqearth +wktext") 

extrap_pols <- st_read( "data/processed/GIS/extrap_sites.shp") %>%
  st_transform("+proj=eqearth +wktext") 

meth_fluxes <- read_csv("data/processed/grades_ch4_fluxes.csv", lazy = FALSE) %>% 
  select(-contains("sd"))

grades <- read_csv("data/processed/grades_coords.csv") 

grimeDB_attributes <- read_csv("data/processed/grimeDB_concs_with_grade_attributes.csv") %>% 
  filter(`Aggregated?` == "No",
         !str_detect(Channel_type,"DD|PI|GT|PS|TH|Th|DIT")) %>%
  mutate( month=month(date)) %>%
  select(COMID, Site_Nid, lat, lon, month) %>% 
  group_by(COMID, month) %>% 
  summarise(lat= mean(lat),
            lon = mean(lon)) %>% 
  mutate(labels = month.name[month] %>% fct_relevel(month.name)) %>% 
  drop_na(lat) %>% 
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

#and mountains
mountains <- read_sf("data/raw/gis/global_mountains/CMEC_Mountains_Enh2018.shp") %>% 
  st_transform(4326)


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



#shrink the hexes by river area or runoff
meth_hexes_avg2 <- meth_hexes_avg %>%
  st_buffer( dist = buffers$buffer_change)


# Map figures ----
## map of concentrations 
map_ch4 <- 
  ggplot() +
  geom_sf(
    data = meth_hexes_avg2, color = NA,
    aes( fill = ch4_mean) )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> conc.** <br>(mmol m<sup>-3</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.11, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.text = element_text(size=9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

ggsave(map_ch4, filename = "figures/map.try.png", dpi= 1000, scale = 1.2)

## map of fluxes
map_fch4 <- 
  ggplot()+
  geom_sf(
    data = meth_hexes_avg2, 
    aes(fill = Fch4_mean*16/12), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> flux** <br> (g CH<sub>4</sub> m^-2 d<sup>-1</sup>)")+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-8600000, 8600000), expand = FALSE) +
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.125, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 10),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))


#put them together and add labels
maps_combined <- map_ch4 / map_fch4 + plot_annotation(tag_levels = 'a')  & theme(plot.tag.position = c(0, 1))

ggsave(maps_combined, filename = "figures/fig_maps.png", dpi= 1000, scale = 1.2)



ggplot()+
    geom_sf(
      data = meth_hexes_avg2 %>% filter(runoff < 1), 
      aes(fill =runoff), color = NA )+
    scale_fill_viridis_c(
      option = "magma", na.value = "gray", 
      direction = -1)


# maps of the extrapolated areas and CV of concentrations ----

#extrapolated areas and sum by month
#prepare the file 
extrap_pols_avg <- extrap_pols %>% 
  st_drop_geometry() %>% 
  pivot_longer(-index, values_to = "extrap", names_to = "month") %>% 
  mutate(month = str_remove(month, "interp_"),
         extrap = ifelse(extrap < 0.92, 1, 0)) %>% 
  group_by(index) %>% 
  summarise(extrap_months = sum(extrap)) %>% 
  arrange(index)

#shrink according to runoff
buffers <- buffers %>% arrange(index)

extrap_pols_avg <- extrap_pols_avg %>%
  left_join(extrap_pols %>% select(index), by = "index") %>% 
  st_sf() %>% 
  st_buffer( dist = buffers$buffer_change) 

#get the sampled sites data
sites_summary <- grimeDB_attributes %>% 
  st_drop_geometry() %>% 
  group_by(COMID) %>% 
  summarise(n = n()) %>% 
  left_join(grimeDB_attributes  %>% select(COMID), by = "COMID") %>% 
  st_sf()
  
#do the map of which areas are extrapolated
map_extrap <- 
ggplot() +
  geom_sf(
    data = extrap_pols_avg, color = NA,
    aes( fill = extrap_months) )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill = "aliceblue", color = NA)+
  geom_sf(data = sites_summary, aes(color = n ), size = 1, shape = 17)+
  scale_fill_distiller(type = "seq", direction = 1, palette = "Oranges", na.value = "gray",
                       name = "**Model extrapolation**<br>(number of months)")+
  scale_color_viridis_c( option = "viridis", na.value = "gray", direction = -1,
                        name = "**Measurements**<br> (number of months)" )+
  guides( fill = guide_colourbar(title.position = "top"),
          color = guide_colourbar(title.position = "top"))+
  coord_sf(xlim = c(-15000000, 16000000), ylim = c(-7300000, 8600000), expand = FALSE) +
  theme_void()+
  theme(legend.position = c(.65, .1),
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 9),
        legend.text = element_text(size=8),
        legend.key.height  = unit(.3, 'cm'),
        legend.key.width =  unit(.5, 'cm'),
        legend.background = element_rect(fill=NA, color=NA),
        legend.box = "horizontal"
        )


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
  theme(legend.position = c(0.17, 0.35),
        legend.direction = "vertical",
        legend.title = ggtext::element_markdown(size = 9),
        legend.key.height  = unit(.5, 'cm'),
        legend.key.width =  unit(.3, 'cm'))

#put them together and save them
maps_combined_model <-  map_extrap / map_ch4_cv + plot_annotation(tag_levels = 'a')  & theme(plot.tag.position = c(0.1, 1))

ggsave(maps_combined_model, filename = "figures/fig_maps_uncertainity.png", dpi= 1500, scale = 1.2)


## Monthly maps ----
#Show the modelled concentrations by month, with ice cover
labelled_months <- tibble(month= month.abb, labels =month.name)

#prepare the files, pivlot to longer for faceting and fix the labels
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

#plot of monthly concentrations
map_monthly <- 
  ggplot( meth_monthly) +
  geom_sf(aes(fill = ch4), color = NA )+
  geom_sf(data = lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  geom_sf(data = ice_monthly, fill= "powderblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "**CH<sub>4</sub> conc.** (mmol m<sup>-3</sup>)")+
    facet_wrap( ~ labels, ncol = 3)+
  guides( fill = guide_colourbar(title.position = "top"))+
  coord_sf(xlim = c(-13000000, 16000000), ylim = c(-7300000, 8300000), expand = FALSE) +
  theme_void()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 14),
        legend.key.width =  unit(2, 'cm'),
        legend.text = element_text(size = 13),
        strip.text = element_text( size = 19))


ggsave(map_monthly, 
       filename = "figures/supplementary/map_ch4_monthly.png", dpi= 1500, height = 10, width = 12)



#### Map extrapolated areas monthly ----
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



## Figure with seasonality emissions along latitudinal bands ----

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


seasonal_df_long %>% 
  group_by(lat_label) %>% 
  summarise(lat= mean(lat), 
            total_flux= sum(ch4E)) %>% 
  mutate(lat_label = fct_reorder(as_factor(lat_label), lat,  .desc = FALSE), 
         total_flux_per = total_flux/sum(total_flux)*100) 
  
##### month /latitude plot of flux rate and surface area
flux_lat_plot <- 
  seasonal_df_long %>% 
  ggplot(aes(month, ch4F))+
  geom_col()+
  scale_y_continuous(breaks = c(0, 0.03, 0.06))+
  facet_wrap(~lat_label, ncol = 1, strip.position = "right")+
  theme_bw()+
  labs(x = "", y = "mmol m<sup>-3</sup> d<sup>-1</sup>", title="**CH<sub>4</sub> flux**" , fill="**River area** <br> (km^2 )")+
  theme(axis.ticks.length.x = unit(0, "cm"),
        strip.background = element_rect(color=NA, fill = "white"),
        strip.text.y = element_text(size = 12, angle=0),
        axis.text.x = element_text(size = 12, color = "gray5"),
        axis.text.y = element_text(color = "gray40"),
        panel.border =  element_rect(color="gray70"), 
        panel.grid = element_blank(),
        axis.line.y = element_line(color="gray70"),
        axis.ticks.y = element_line(color="gray70"),
        axis.title.y = ggtext::element_markdown(size = 10),
        plot.title = ggtext::element_markdown(size = 14),
        legend.title = ggtext::element_markdown(size = 12)
  )

area_lat_plot <- 
  seasonal_df_long %>% 
  ggplot(aes(month, areakm2))+
  geom_col()+
  scale_y_continuous(position = "right")+
  facet_wrap(~lat_label, ncol = 1, strip.position = "right", scales="free_y")+
  theme_bw()+
  labs(x = "", y = expression(km^2), title = "**River surface area**")+
  theme(axis.ticks.length.x = unit(0, "cm"),
        strip.background = element_blank(),#element_rect(color=NA, fill = "gray80"),
        strip.text.y = element_blank(), #element_text(size = 12, angle=0),
        axis.text.x = element_text(size = 12, color = "gray5"),
        axis.text.y = element_text(color = "gray40"),
        panel.border =  element_rect(color="gray70"), 
        panel.grid = element_blank(),
        axis.line.y = element_line(color="gray70"),
        axis.ticks.y = element_line(color="gray70"),
        plot.title = ggtext::element_markdown(size = 14)
  )

flux_lat_plot +
area_lat_plot

ggsave("figures/supplementary/flux_area_month.png", width =10, height = 8, dpi = 800)

# Flux comparison with grime data ----

# Function to calculate gas saturation, 
# from temperature ( in Kelvin) and atm pressure (derived from elevation (in m.a.s.l) for CH4 
# Henry's Law constants from http://www.henrys-law.org
# Temperature correction using van't Hoff equation , temperature is in (Kelvin)
# Kh is in ("Kh, cp") = (mol/L*Atm) at STP
# Concentrations (mole fraction, also ppmv) in the atmosphere are approximate for 2013, should be updated over time
# AtmP is in (atmospheres)
get_Ch4eq <- function(temperature, elevation){
  
  pressure <- (1-(.0000225577*elevation))^5.25588
  Kh <- (1.4*10^-3)*exp(1700*((1/temperature)-(1/298)))
  AtmosphereConc <-  1.83
  EquilSaturation <- AtmosphereConc*Kh/pressure #umol/L, mmol/m3
  
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
  left_join(hydro_k %>% select(COMID, Slope, k_mean), by = "COMID") %>% 
  drop_na(Jan_ch4F) %>% 
  rowwise() %>% 
  mutate(Fch4_mean = mean(Jan_ch4F:Dec_ch4F, na.rm= TRUE)) %>% 
  select(COMID, Slope, k_mean, Fch4_mean )


## Now get GRiMeDB ----
load(file.path("data", "MethDB_tables_converted.rda"))

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
                Latitude, distance_snapped, slope_m_m, CH4mean, CO2mean,
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

#compare mean fluxes observed and predicted
mean(meth_hexes_avg$Fch4_mean, na.rm=TRUE)
mean(flux_comid$Diffusive_CH4_Flux_Mean/1000*12, na.rm=TRUE)

#now join the observed fluxes and predicted into one df. I will filter the sites that are not well snapped (>100m)
flux_comp <- flux_comid %>% 
  select(COMID, Site_Nid, date= Date_start, distance_snapped, CH4mean, WaterTemp_best, elev=elevation_m_new, CO2mean,
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
  labs(x="Measured CH<sub>4</sub> diffusive flux", y="Modelled CH<sub>4</sub> diffusive flux", title = "All data")+
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
  labs(x = "Measured CH<sub>4</sub> diffusive flux", y="", 
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

flux_comp %>% 
  filter(k600_pred > .01, k600_obs > .01) %>% 
  rename(modelled_flux = flux_pred, measured_flux = flux_obs) %>% 
  pivot_longer(c(modelled_flux, measured_flux), values_to = "ch4_flux", names_to = "method") %>% 
  ggplot(aes(x=method, y=ch4_flux))+
  geom_boxplot(fill="blue3", alpha=.5)+
  scale_y_log10(labels=scales::number)+
  labs(x="", y="CH<sub>4</sub> flux (mmol m^-2 d^-1)")+
  theme_bw()+
  theme(axis.title.y = ggtext::element_markdown())

#ggsave("figures/flux_comp_boxplot.png")


#temperature dependence plots
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(13)

plot_sites <- flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
  ggplot(aes(temp_stand, Diffusive_CH4_Flux_Mean))+
  geom_point( size = 2, alpha = .1)+
  geom_smooth(method= "lm", color="gray20", se= FALSE, size = 1)+
  geom_point(data= flux_sites %>% 
               filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
               add_count(Site_Nid) %>% filter(n > 30),  
             aes(temp_stand, Diffusive_CH4_Flux_Mean, color=Site_Nid), alpha=.6, size= 2)+
  geom_smooth(data= flux_sites %>% 
                filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
                add_count(Site_Nid) %>% filter(n > 30),  
              method= "lm", se= FALSE,
              aes(temp_stand, Diffusive_CH4_Flux_Mean, color=Site_Nid), alpha=.6)+
  #geom_abline(slope=.96, intercept = -.39, linetype = 2)+
  #annotate("text", x=2.8, y = 360, label="m = 0.96", angle = 35)+
  scale_color_manual(values = mycolors)+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT) - frac(1, kT[C]), ")" ))), limits= c(-2.6, 3.1),
                     sec.axis = sec_axis(~ (273.15*boltzmann_k* . + 0.0520562)/(0.00347041 - boltzmann_k* . ), name =expression("Temperature " ( degree*C)), 
                                         breaks = seq(0, 40, by=5)) )+ # solved using wolfram alpha, y= 1/(k*(288.15)) - 1/(k*(x+273.15)), solve x 
  scale_y_continuous(trans= "log10", labels = scales::number, breaks = c(.01, .1, 1, 10, 100),
                     name= "")+#"Diffusive CH<sub>4</sub> flux <br> (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(axis.title.y = ggtext::element_markdown(), legend.position = "none")


flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
  drop_na(System_size) %>% 
  mutate(System_size = fct_relevel(System_size, "large", after = Inf)) %>% 
  ggplot(aes(temp_stand, Diffusive_CH4_Flux_Mean))+
  geom_point(aes( color=abs(Latitude)),  size = 2, alpha = .6)+
  geom_smooth(method= "lm", color="gray20", se= FALSE, size = 1)+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y.npc = .99)+
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
  theme(axis.title.y = ggtext::element_markdown(), legend.position = "right")+
  facet_wrap(~System_size)

ggsave("figures/supplementary/river_size_temp.png", width = 12, scale = .7)

diff_plot <- 
  flux_sites %>% 
  filter(Diffusive_CH4_Flux_Mean > 0.001) %>% 
  ggplot(aes(temp_stand, Diffusive_CH4_Flux_Mean))+
  geom_point(aes( color=abs(Latitude)),  size = 2, alpha = .6)+
  geom_smooth(method= "lm", color="gray20", se= FALSE, size = 1)+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y.npc = .99)+
  stat_regline_equation(label.y.npc = .99)+
  scale_color_viridis_c(name="Latitude", limits= c(0, 71))+
  #geom_abline(slope = .96, intercept = -.39, linetype = 2)+
  #annotate("text", x = 2.8, y = 360, label="m = 0.96", angle = 35)+
  annotate("text", x = -2, y = 250, label="n = 4,417")+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT) - frac(1, kT[C]), ")" ))), limits= c(-2.6, 3.1),
                     sec.axis = sec_axis(~ (273.15*boltzmann_k* . + 0.0520562)/(0.00347041 - boltzmann_k* . ), name =expression("Temperature " ( degree*C)), 
                                         breaks = seq(0, 40, by=5)) )+ # solved using wolfram alpha, y= 1/(k*(288.15)) - 1/(k*(x+273.15)), solve x 
  scale_y_continuous(trans= "log10", labels = scales::number, breaks = c(.01, .1, 1, 10, 100),
                     name= "Diffusive CH<sub>4</sub> flux <br> (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(axis.title.y = ggtext::element_markdown(), legend.position = "right")



flux_sites %>% 
  filter(Eb_CH4_Flux_Mean > 0.001) %>% 
  ggplot(aes(temp_stand, (Eb_CH4_Flux_Mean)))+
  geom_point(aes( color=abs(Latitude)),  size = 3, alpha = .6)+
  geom_smooth(method= "lm", color="gray20", se= FALSE, size=2)+
  #stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y.npc = .99)+
  stat_regline_equation(label.y.npc = .99)+
  scale_color_viridis_c(name="Latitude")+
  #geom_abline(slope=.96, intercept = -.71, linetype = 2)+
  #annotate("text", x=2.9, y = 75, label="m = 0.96", angle=34)+
  annotate("text", x=-2.24, y = 150, label="n = 244")+
  scale_x_continuous(name= expression(paste("Standardized temperature ", bgroup("(", frac(1, kT) - frac(1, kT[C]), ")" ))), limits= c(-2.6, 3.1),
                     sec.axis = sec_axis(~ (273.15*boltzmann_k* . + 0.0520562)/(0.00347041 - boltzmann_k* . ), name =expression("Temperature " ( degree*C)), 
                                         breaks = seq(0, 40, by=5)) )+ # solved using wolfram alpha, y= 1/(k*(288.15)) - 1/(k*(x+273.15)), solve x 
  scale_y_continuous(trans= "log10", labels = scales::number, breaks = c(.01, .1, 1, 10, 100),
                     name= "Ebullitive CH<sub>4</sub> flux <br> (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(axis.title.y = ggtext::element_markdown(), legend.position = "right")

ggsave("figures/supplementary/ebullition_temp.png", scale = .7)

flux_comid %>% 
  filter(Eb_CH4_Flux_Mean > 0.00001,
         Diffusive_CH4_Flux_Mean > 0.0001) %>%
  filter(k_method == "chamber + conc") %>% 
  select(Site_Nid) %>% unique()

flux_comid %>% 
  filter(Eb_CH4_Flux_Mean > 0.00001) %>% 
  count(Site_Nid)

mycolors2 <- colorRampPalette(RColorBrewer::brewer.pal(8, "Paired"))(93)

#ebullition
reg_compl_plot <- 
  flux_comid %>% 
  filter(Eb_CH4_Flux_Mean > 0.00001,
         Diffusive_CH4_Flux_Mean > 0.0001) %>% 
  filter(k_method %in% c("chamber + conc", "tracer addition")) %>% 
  ggplot(aes(Diffusive_CH4_Flux_Mean, Eb_CH4_Flux_Mean ))+
  geom_point( aes( color=Site_Nid), size = 3, alpha=.7)+
  geom_smooth( method="lm",  color="black", size=1)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y.npc = .99)+
  stat_regline_equation(label.y.npc = .9)+
  scale_color_manual(values = mycolors2)+
  scale_y_log10(labels=scales::number)+
  scale_x_log10(labels=scales::number)+
  labs(x = "Diffusive CH<sub>4</sub> flux <br>(mmol m^-2 d<sup>-1</sup>)", 
       y = "Ebulitive CH<sub>4</sub> flux <br>(mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown())


#ebullition
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
   labs(x = "", y = "CH<sub>4</sub> flux<br> (mmol m^-2 d<sup>-1</sup>)")+
  theme_classic()+
  theme(legend.position = "none",
        axis.title.y = ggtext::element_markdown(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=11, color="black"))


diff_plot + 
  plot_sites +
  density_comp_plot + 
  reg_compl_plot + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag.position = c(0.1, 1), plot.tag = element_text(size=15))

ggsave("figures/diff_temperature_ebullition.png", width = 10, height = 7)



flux_comid %>% 
  filter(Eb_CH4_Flux_Mean > 0.0001,
         Diffusive_CH4_Flux_Mean > 0.0001) %>% 
  filter(k_method == "chamber + conc") %>% 
  summarise(eb_median = median(Eb_CH4_Flux_Mean),
            diff_median = median(Diffusive_CH4_Flux_Mean),
            eb_min = min(Eb_CH4_Flux_Mean),
            eb_max = max(Eb_CH4_Flux_Mean))


residuals_ebb <- flux_comid %>% 
  filter(Eb_CH4_Flux_Mean > 0.0001,
         Diffusive_CH4_Flux_Mean > 0.0001, 
         k_method == "chamber + conc") %>% 
  mutate(diff_log = log(Diffusive_CH4_Flux_Mean), 
         ebb_log = log(Eb_CH4_Flux_Mean)) %>% 
  do(augment(lm(ebb_log ~ diff_log, data=.)))

residuals_ebb %>% 
  select(.resid) %>% 
  mutate(residuals = exp(.resid)) %>% 
  summarise(sd = sd(residuals),
            mean =mean(residuals))

ggplot(residuals_ebb)+
  geom_histogram(aes(.resid), bins=60)+
  theme_classic()
  
sites_with_many_obs <- conc_df_comids %>% 
  filter(CH4mean > 0.0005) %>% 
  group_by(COMID = as.factor(COMID)) %>% 
  mutate(n_obs = n(), 
         month= month.abb[month(date)]) %>% 
  filter(n_obs > 150) %>% 
  arrange(n_obs) %>% 
  ungroup() %>% 
  mutate(month = fct_relevel(month, month.abb),
         COMID = fct_reorder(COMID, desc(n_obs))) 

  labs_plot_n_obs <- sites_with_many_obs %>% 
  group_by(COMID) %>% 
  slice_max( Publication_Nid) %>% 
  left_join(papers_df, by = "Publication_Nid" ) %>%  
  group_by(COMID) %>% 
  summarise( Publication_Nid= first(Publication_Nid), 
             n_obs= first(n_obs), Authorlastname = first(Authorlastname)) %>% 
  mutate(label = paste0(COMID," \n(", Authorlastname, ")")) %>% 
  arrange(desc(n_obs))

ggplot(sites_with_many_obs, aes(COMID, CH4mean, color= month ))+
  geom_jitter(alpha = .3)+
  geom_text(data=labs_plot_n_obs, aes(COMID, 0.0001, label = n_obs), color = "black")+
  stat_summary_bin(aes(group= month, fill=month ), fun.data = "mean_sd", color= "black", size = 1, shape =21)+
  scale_y_log10(labels=scales::number, breaks = c(0.001, 0.01, 0.1, 1, 10, 100))+
  scale_color_viridis_d(name = "")+
  scale_fill_viridis_d(name = "")+
  scale_x_discrete(labels = labs_plot_n_obs$label, guide = guide_axis(angle = 45))+
  labs(x = "**GRADES river reach ID** <br> (main author)", y = "**CH<sub>4</sub> concentration** <br> (mmol m^-3)")+
  theme_classic()+
  theme(legend.position = "top", axis.title.y = ggtext::element_markdown(),
        axis.title.x = ggtext::element_markdown())+
  guides(color=guide_legend(nrow=1))
 
ggsave("figures/supplementary/sites_with_many_obs.png", width = 10, height = 5)


time_series <- sites_with_many_obs %>% 
  filter(COMID == 24006204, Publication_Nid == 2320) %>% 
  ggplot(aes(month, CH4mean, color=Site_Nid, group = Site_Nid))+
  geom_point(alpha=.6)+
  geom_smooth( se= FALSE)+
  geom_point(data = . %>% group_by(Site_Nid, month) %>% summarise(CH4mean = mean(CH4mean)),
             aes(month, CH4mean, color=Site_Nid), size=5)+
  scale_color_brewer(palette = "Set2", guide = "none" )+
  scale_y_log10(labels=scales::number, breaks = c( 0.01, 0.1, 1, 10, 100), limits= c(0.01, 100))+
  theme_classic()+
  labs(x="", y= expression(CH[4]~concentration~(mmol~m^-3)), title= "Temporal variability")

spatial_surveys <- sites_with_many_obs %>% 
  filter(COMID == 24006204, Publication_Nid == 2279) %>% 
  mutate(Site_Nid = as.numeric(Site_Nid)) %>% 
  ggplot(aes(COMID, CH4mean, color=Site_Nid))+
  geom_jitter(alpha = .8, size = 3)+
  scale_color_distiller(type = "seq", palette = "Reds",  guide = "none" )+
  scale_y_log10(labels=scales::number, breaks = c( 0.01, 0.1, 1, 10, 100), limits= c(0.01, 100))+
  theme_classic()+
  labs(x = "", y = "", title= "Spatial variability")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

time_series + spatial_surveys +  plot_layout(widths = c(2.5, 1) )

ggsave("figures/supplementary/krycklan_example.png",  width = 7, height = 5)

    
## Maps of important features ----
important_pols <- st_read( "data/processed/GIS/important_features.shp") %>% 
  st_transform("+proj=eqearth +wktext") %>% 
  mutate(prec_yr = log(prec_yr))


ggplot(important_pols, aes(fill=slope))+
  geom_sf(color= NA)+
  scale_fill_viridis_c()

#devtools::install_github("marcosci/layer")
library(layer)

tilt_landscape_1 <- tilt_map(important_pols)
tilt_landscape_2 <- tilt_map(important_pols,  y_shift = -15000000)
tilt_landscape_3 <- tilt_map(important_pols,  y_shift = -15000000*2)
tilt_landscape_4 <- tilt_map(important_pols,  y_shift = -15000000*3)
tilt_landscape_5 <- tilt_map(important_pols,  y_shift = -15000000*4)
tilt_landscape_6 <- tilt_map(important_pols,  y_shift = -15000000*5)


map_list <- list(tilt_landscape_1, tilt_landscape_2, tilt_landscape_3, tilt_landscape_4, tilt_landscape_5,
                 tilt_landscape_6 )

plots_tiled1 <- plot_tiltedmaps(map_list, 
                                layer = c("slope", "elev", "pyearRS", "Lg_gw_m", "Lg_T_OC", "temp_yr" ),
                                palette = c("turku", "batlowW", "lajolla","tofino", "bilbao",  "rocket"),
                                direction = c(-1, 1, 1, 1, 1, -1))

plots_tiled2 <- plot_tiltedmaps(map_list, 
                                layer = c("Lg_k_mn", "ptlnd_c", "GPP_yr", "prec_yr", "Lg_ppdn", "S_SILT"),
                                palette = c("grayC", "inferno" ,"bam", "roma", "tokyo", "cividis"),
                                direction = c(1, 1, 1, 1, 1, 1))

plots_combined <- plots_tiled1 + plots_tiled2

ggsave( "figures/supplementary/tiled_maps.png", plots_combined, dpi = 1000, scale = 4)



