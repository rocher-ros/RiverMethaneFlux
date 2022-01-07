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



#upscaled methane fluxes
meth_fluxes <- read_csv("data/processed/meth_flux_predictions.csv", lazy = FALSE) %>% 
  rename_at(vars(ends_with('ch4')), ~ str_replace(., "ch4","fch4")) %>% 
  drop_na(Jan_fch4)

#upscaled methane concentrations
meth_concs <- read_csv("data/processed/grades_ch4_k_q.csv", lazy = FALSE)

#read coordinates from grades
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv", lazy = FALSE) %>% 
  dplyr::select(-Length, -slope)


#turn it an sf object
meth_gis <- left_join(meth_fluxes, grades, by = "COMID")  %>%
  left_join(meth_concs, by="COMID") %>% 
  mutate(Jan_fch4_prod = Jan_ch4ex*Jan_k,
         Feb_fch4_prod = Feb_ch4ex*Feb_k,
         Mar_fch4_prod = Mar_ch4ex*Mar_k,
         Apr_fch4_prod = Apr_ch4ex*Apr_k,
         May_fch4_prod = May_ch4ex*May_k,
         Jun_fch4_prod = Jun_ch4ex*Jun_k,
         Jul_fch4_prod = Jul_ch4ex*Jul_k,
         Aug_fch4_prod = Aug_ch4ex*Aug_k,
         Sep_fch4_prod = Sep_ch4ex*Sep_k,
         Oct_fch4_prod = Oct_ch4ex*Oct_k,
         Nov_fch4_prod = Nov_ch4ex*Nov_k,
         Dec_fch4_prod = Dec_ch4ex*Dec_k ) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

rm(grades, meth_concs)
gc()

meth_gis  %>% 
  filter(is.na(Jan_ch4) == TRUE)

#calculate average methane for each site
meth_avg <- meth_gis %>% 
  drop_na() %>% 
  rowwise() %>% 
  mutate( ch4_mean = mean(Jan_ch4:Dec_ch4),
          k_mean = mean(Jan_k:Dec_k),
          Fch4_mean = mean(Jan_fch4:Dec_fch4),
          Fch4_prod_mean = mean(Jan_fch4_prod:Dec_fch4_prod)) %>% 
  dplyr::select(COMID, ch4_mean, Fch4_mean, k_mean, Fch4_prod_mean, runoff, Slope, elev, strmOrder, geometry) %>% 
  st_sf()

# 2. Maps ----
#download world aps
world <-  ne_download(scale = 110, type = 'land', category = 'physical', returnclass = "sf") %>%
  st_transform("+proj=eqearth +wktext") 

#and lake layer
lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', returnclass = "sf")

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
            methane_flux = mean(Fch4_mean, na.rm = TRUE),
            k_mean = mean(k_mean, na.rm=TRUE),
            methane_flux_prod = mean(Fch4_prod_mean, na.rm = TRUE),
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
    data = meth_hexes_avg2, 
    aes(fill = ifelse(methane_flux_prod > 50, 50, methane_flux_prod )), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    trans = "pseudo_log", 
    breaks= c(0,1,5,10,50),
    direction = -1,
    name = "**Flux with k capped** <br> (mmol m^-2 d^-1 )")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.18),
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 14),
        legend.text = element_text(size=12),
        legend.key.size = unit(.9, 'cm'))

ggsave(filename = "figures/map_ch4_flux_capped.png", dpi=600, height = 10, width = 16)

ggplot() +
  geom_sf(
    data = meth_hexes_avg2 %>% filter(k_mean < 5000), aes(fill = k_mean ), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
  scale_fill_viridis_c(
    option = "magma", na.value = "gray",
    direction = -1,
    name = "Areas with k < 50 m/d")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.18),
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 14),
        legend.text = element_text(size=12),
        legend.key.size = unit(.9, 'cm'))

ggsave(filename = "figures/k_0.png", dpi=600, height = 10, width = 16)


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

#read the file with upscaled fluxes aggregated for hybas basis
ch4E_hybas <- read_csv("data/processed/ch4E_hybas.csv")

#total flux
ch4E_hybas %>% 
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
mean(meth_avg$Fch4_prod_mean, na.rm=TRUE)
mean(flux_comid$Diffusive_CH4_Flux_Mean, na.rm=TRUE)

#now join the observed fluxes and predicted into one df. I will filter the sites that are not well snapped (>100m)
flux_comp <- flux_comid %>% 
  select(COMID, Site_Nid, date= Date_start, distance_snapped, CH4mean, WaterTemp_best, elevation_m_new, CO2mean,
         slope_m_m, Diffusive_CH4_Flux_Mean, Diff_Method, k_method ) %>% 
  filter(distance_snapped < 100) %>% 
  left_join( meth_gis %>% st_drop_geometry(), by="COMID") %>% 
  drop_na(Diffusive_CH4_Flux_Mean) %>% 
  mutate(CH4mean_ex = CH4mean - get_Ch4eq(WaterTemp_best+ 273.15, elev),
         CH4mean_ex = ifelse(is.na(CH4mean_ex) == TRUE,
                             CH4mean - get_Ch4eq(20 + 273.15, elev),
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


kpred_flux_init <- meth_avg %>% 
  ggplot(aes(k_mean, Fch4_prod_mean))+
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
  labs( x="k (m/d)", y="CH4 flux as k*CH4pred \n(mmol/m2/d)", title="Initial approach")


pred_flux <- ggplot(meth_avg, aes(k_mean, Fch4_mean))+
  geom_hex(bins=50)+
  geom_point(data=flux_comp %>%
               filter(Diffusive_CH4_Flux_Mean > 0.0001, k_obs < 500, k_obs > .1),
             aes(k_obs, Diffusive_CH4_Flux_Mean), alpha=.4)+
  geom_smooth(data=flux_comp %>%
                filter(Diffusive_CH4_Flux_Mean > 0.0001, k_obs < 500, k_obs > .1),
              aes(k_obs, Diffusive_CH4_Flux_Mean), color="red3")+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()+
  labs( x="k (m/d)", y="Directly predicting flux \n(mmol/m2/d)", title="Trying with direct flux prediction")



kpred_flux <- meth_avg %>% 
  mutate(Fch4_prod_mean2= ifelse(k_mean > 30, 
                                 rlnorm(1, meanlog = .71, sdlog = 0.52), 
                                 Fch4_prod_mean )) %>% 
ggplot(aes(k_mean, Fch4_prod_mean2))+
  geom_hex(bins=50)+
  geom_point(data=flux_comp %>%
               filter(Diffusive_CH4_Flux_Mean > 0.0001, k_obs < 500, k_obs > .1),
             aes(k_obs, Diffusive_CH4_Flux_Mean), alpha=.4)+
  geom_smooth(data=flux_comp %>%
                filter(Diffusive_CH4_Flux_Mean > 0.0001, k_obs < 500, k_obs > .1),
              aes(k_obs, Diffusive_CH4_Flux_Mean), color="red3")+
  geom_vline(xintercept = 30, linetype=2, color="gray20")+
  annotate(geom="text", x=43, y=.01, label="k = 35 m/d",angle=90)+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()+
  labs( x="k (m/d)", y="CH4 flux as k*CH4pred \n(mmol/m2/d)", title="Bootstrapping the observed \ndistribution above 35 m/d")

ggarrange(kpred_flux_init, pred_flux, kpred_flux,  nrow = 1, legend = "none", align = "h") %>% 
  annotate_figure(  bottom = text_grob("Black points and red smooth line are independent flux observations", color = "black", size = 12))

ggsave("figures/model_flux_comp_capped.png", width = 16, height = 6)


flux_comp %>% filter(k_obs < 500, k_obs > 35) %>%  
  summarise(mean= mean(Diffusive_CH4_Flux_Mean),
          median= median(Diffusive_CH4_Flux_Mean),
          sd= sd(Diffusive_CH4_Flux_Mean),
          n=n())


flux_comp %>% filter(k_obs < 500, k_obs > 35) %>%  
  ggplot(aes(y=Diffusive_CH4_Flux_Mean))+
  geom_boxplot()+
  scale_y_log10()

fluxes_high_k <- flux_comp %>% 
  filter(k_obs < 500, k_obs > 30, Diffusive_CH4_Flux_Mean < 200) %>% 
  select(flux=Diffusive_CH4_Flux_Mean)

ggplot(fluxes_high_k, aes(flux))+
  geom_density()


library(brms)


mdl_1 <- brm(flux ~ 1, data=fluxes_high_k, family="lognormal")  # Using the default priors

plot(mdl_1)

pp_check(mdl_1, ndraws = 50)

rlnorm(10, meanlog = .66, sdlog = 1.49)

flux_comid %>% filter(slope_m_m> 0.05, Diffusive_CH4_Flux_Mean >0) %>%
  group_by(Publication_Nid) %>% 
  summarise(slope_m_m =mean(slope_m_m), 
            mean_flux= mean(Diffusive_CH4_Flux_Mean),
            sd_flux= sd(Diffusive_CH4_Flux_Mean),
            n=n()) %>% 
  arrange(desc(slope_m_m))

flux_comid %>% 
  filter(slope_m_m> 0.05, Diffusive_CH4_Flux_Mean >0,  Diffusive_CH4_Flux_Mean <90, Publication_Nid > 1) %>%
  group_by(Publication_Nid) %>% 
  mutate(slope = mean(slope_m_m, na.rm =TRUE)) %>% 
  ungroup() %>% 
  mutate(Publication_Nid = as_factor(Publication_Nid) %>% 
           fct_reorder(slope)) %>% 
  ggplot()+
  geom_boxplot(aes(x=Diffusive_CH4_Flux_Mean, y=Publication_Nid, fill=slope))+
  scale_x_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()
