# Info ------
# Author: Gerard Rocher-Ros
# Script make some figures of CH4 emissions in rivers globally.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'ggpubr', 'rnaturalearth', 'ggtext', 'lubridate')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

## And useful custom functions 
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

# 1. Load files ----
if(file.exists("data/processed/grades_ch4_k_q.csv") == TRUE) {
  print("files already downloaded")
} else {
  drive_download(
    "SCIENCE/PROJECTS/RiverMethaneFlux/processed/grades_ch4_k_q.csv",
    path = "data/processed/grades_ch4_k_q.csv",
    overwrite = TRUE
  )
}

if(file.exists("data/processed/grades_coords.csv") == TRUE) {
  print("files already downloaded")
} else {
  drive_download(
    "SCIENCE/PROJECTS/RiverMethaneFlux/processed/grades_coords.csv",
    path = "data/processed/grades_coords.csv",
    overwrite = TRUE
  )
}

#upscaled methane concentrations
meth_concs <- read_csv("data/processed/grades_ch4_k_q.csv", lazy = FALSE)

#read coordinates from grades
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv", lazy = FALSE) %>% 
  dplyr::select(-Length, -slope)

mountains_df <- read_csv("data/raw/gis/GRADES_attributes/mountains.csv")


#turn it an sf object
meth_gis <- left_join(meth_concs, grades, by = "COMID") %>%
  left_join(mountains_df, by = "COMID") %>% 
  #mutate(across(ends_with("_k"), ~ifelse(.x > 50, 50, .x))) %>% 
  mutate(Jan_fch4 = Jan_ch4ex*Jan_k,
         Feb_fch4 = Feb_ch4ex*Feb_k,
         Mar_fch4 = Mar_ch4ex*Mar_k,
         Apr_fch4 = Apr_ch4ex*Apr_k,
         May_fch4 = May_ch4ex*May_k,
         Jun_fch4 = Jun_ch4ex*Jun_k,
         Jul_fch4 = Jul_ch4ex*Jul_k,
         Aug_fch4 = Aug_ch4ex*Aug_k,
         Sep_fch4 = Sep_ch4ex*Sep_k,
         Oct_fch4 = Oct_ch4ex*Oct_k,
         Nov_fch4 = Nov_ch4ex*Nov_k,
         Dec_fch4 = Dec_ch4ex*Dec_k
         ) %>%
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

rm(grades, meth_concs)
gc()

#calculate average methane for each site
meth_avg <- meth_gis %>% 
  drop_na(Jan_ch4) %>% 
  rowwise() %>% 
  mutate( ch4_mean = mean(Jan_ch4:Dec_ch4, na.rm= TRUE),
          Fch4_mean = mean(Jan_fch4:Dec_fch4, na.rm= TRUE),
          k_mean = mean(Jan_k:Dec_k, na.rm= TRUE)) %>% 
  mutate(mountains= as.factor(mountains)) %>% 
  select(COMID, ch4_mean, Fch4_mean, k_mean, runoff, Slope, elev, mountains, strmOrder, geometry) %>% 
  st_sf()

# 2. Maps ----
#download world maps
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

#and lake layer
lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = "sf")

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

#now aggregate all the meth data for each hex, do means.
meth_hexes_avg <- meth_hexes %>% 
  group_by(index) %>%
  summarise(methane = mean(ch4_mean, na.rm = TRUE),
            methane.flux = mean(Fch4_mean, na.rm = TRUE),
            runoff= mean(runoff, na.rm = TRUE) ) %>%
  st_sf()  

#buffer df to shrink each hex according to runoff
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
 

#shrink the hexes with low runoff
meth_hexes_avg2 <- meth_hexes_avg %>%
  st_drop_geometry() %>%
  right_join(grid, by="index") %>%
  st_sf() %>% 
  st_buffer( dist=  buffers$buffer_change)

## map of concentrations ----
ggplot() +
  geom_sf(
    data = meth_hexes_avg2, color = NA,
    aes( fill = methane) )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    #trans = "pseudo_log", 
    name = "**Methane concentration** <br>(mmol m^-3 )")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.18),
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 14),
        legend.text = element_text(size=12),
        legend.key.size = unit(.9, 'cm'))

ggsave(filename = "figures/map_ch4.png", dpi=600, height = 10, width = 16)


## map of fluxes ----
ggplot() +
  geom_sf(
    data = meth_hexes_avg2, aes(fill = methane.flux), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  geom_sf(data=mountains, fill=NA, color="black" )+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    trans = "pseudo_log", 
    breaks= c(0,1,5,10,30),
    direction = -1,
    name = "**Methane flux** <br> (mmol m^-2 d^-1 )")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.18),
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 14),
        legend.text = element_text(size=12),
        legend.key.size = unit(.9, 'cm'))

ggsave(filename = "figures/map_ch4_flux_mountains.png", dpi=600, height = 10, width = 12)


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

#read the file with upscaled fluxes aggregated for hybas basis
ch4E_hybas <- read_csv("data/processed/ch4E_hybas.csv")

#total flux
ch4E_hybas %>% 
  drop_na(ch4E_Jan) %>% 
  summarise(across(ch4E_Jan:ch4E_Dec, sum)) %>% 
  pivot_longer( everything(), names_to = "month", values_to = "flux") %>% 
  mutate(per_day=flux/365,
         n_days=c(31,28,31,30,31,30,31,31,30,31,30,31)) %>% 
  summarise(sum=sum(per_day*n_days)) *16/12/1000 #in TgCH4
  

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
  

meth_concs %>% 
  group_by(strmOrder) %>% 
  summarise(mean(Length))
