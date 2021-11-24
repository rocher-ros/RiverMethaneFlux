# Info ------
# Author: Gerard Rocher-Ros
# Script upscale CH4 emissions in rivers globally.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'ggpubr', 'rnaturalearth', 'ggtext', 'lubridate')

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
  #mutate(across(ends_with("_k"), ~ifelse(.x > 50, 50, .x))) %>% 
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
            methane.flux = mean(Fch4_mean, na.rm = TRUE),
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
    name = "**Methane concentration** <br>(mmol m^-3 )")+
  guides( fill = guide_colourbar(title.position = "top"))+
  theme_void()+
  theme(legend.position = c(0.55, 0.18),
        legend.direction = "horizontal",
        legend.title = ggtext::element_markdown(size = 14),
        legend.text = element_text(size=12),
        legend.key.size = unit(.9, 'cm'))

ggsave(filename = "figures/map_ch4.png", dpi=600, height = 10, width = 16)


ggplot() +
  geom_sf(
    data = meth_hexes_avg2, aes(fill = methane.flux), color = NA )+
  geom_sf(data=lakes %>% filter(scalerank < 2), fill="aliceblue", color=NA)+
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

ggsave(filename = "figures/map_ch4_flux.png", dpi=600, height = 10, width = 16)


ggplot(meth_avg, aes(k_mean, ch4_mean))+
  geom_hex(bins=50)+
 # scale_x_log10(labels=scales::number)+
  #scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()


ggsave("figures/k_ch4_predicted.png")

ggplot(meth_avg, aes(k_mean, Fch4_mean))+
  geom_hex(bins=50)+
  # scale_x_log10(labels=scales::number)+
  #scale_y_log10(labels=scales::number)+
  scale_fill_viridis_c()+
  theme_bw()

ggsave("figures/k_flux_predicted.png")


ch4E_hybas <- read_csv("output/ch4E_hybas.csv")

ch4E_hybas %>% 
  summarise(across(ch4E_Jan:ch4E_Dec, sum)) %>% 
  pivot_longer( everything(), names_to = "month", values_to = "flux") %>% 
  mutate(per_day=flux/12) %>% 
  summarise(sum=sum(per_day))
  

## Flux comparison with grime ----
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
  mutate(k_method= str_replace(k_method, "additon", "addition"),
         k_method = str_replace(k_method, "Physical" ,"physical")) %>% 
  left_join(conc_df, by=c("Site_Nid", "Date_start"))

#compare mean fluxes observed and predicted
mean(meth_avg$Fch4_mean, na.rm=TRUE)
mean(flux_comid$Diffusive_CH4_Flux_Mean, na.rm=TRUE)

#now join the observed fluxes and predicted into one df. I will filter the sites that are not well snapped (>100m)
flux_comp <- flux_comid %>% 
  select(COMID, Site_Nid, date= Date_start, distance_snapped, CH4mean, Diffusive_CH4_Flux_Mean, Diff_Method, k_method ) %>% 
  filter(distance_snapped < 100) %>% 
  left_join( meth_gis %>% st_drop_geometry(), by="COMID") %>% 
  drop_na(Diffusive_CH4_Flux_Mean) %>% 
  mutate(k_obs =Diffusive_CH4_Flux_Mean/CH4mean,  # estimate kch4 (m d-1) as the quotient of flux (mmol m-2 d-1) and concentration (mmol m-3)
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
                               month_obs == 12 ~ Dec_fch4)
         ) 


flux_comp %>% filter( !k_method %in% c("not determined", "other"),
                       flux_pred > 0, Diffusive_CH4_Flux_Mean > 0, is.na(k_obs) == FALSE) %>%  
  ggplot(aes(Diffusive_CH4_Flux_Mean, flux_pred, color=(k_pred/k_obs)*100), alpha=.5, size=2)+
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
                                     k_pred/k_obs > .5 &  k_pred/k_obs < 1.5, flux_pred > 0),
         aes(Diffusive_CH4_Flux_Mean, flux_pred))+
    geom_point(data=flux_comp %>% filter( !k_method %in% c("not determined", "other"),
                                          flux_pred > 0, Diffusive_CH4_Flux_Mean > 0, is.na(k_obs) == FALSE),
               aes(Diffusive_CH4_Flux_Mean, flux_pred, color=(k_pred/k_obs)*100), alpha=.5, size=2)+
  geom_point( color="black", size=2)+
  geom_abline(slope=1, intercept = 0)+
  geom_smooth(method="lm")+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  stat_regline_equation(label.y.npc = .9)+
  scale_x_log10(labels=scales::number)+
  scale_y_log10(labels=scales::number)+
  scale_color_viridis_c(trans="log10", labels=scales::number, breaks = c(1, 50,  150, 10000))+
  labs(x="Measured CH4 diffusive flux", y="Modelled CH4 diffusive flux", 
       color="% difference predicted \n vs observed k",
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

ggsave("figures_comp_k.png")

obs_flux_k <- flux_comp %>% 
  filter( k_obs > .1, k_obs < 500) %>% 
  ggplot(aes(k_obs,Diffusive_CH4_Flux_Mean))+
  geom_point()+
  theme_bw()+
  labs(x="estimated k from the F/conc observations")

obs_flux_slope <- flux_comid %>% 
  ggplot(aes(slope_m_m,Diffusive_CH4_Flux_Mean))+
  geom_point()+
  theme_bw()+
  labs(x="channel slope")

ggarrange(obs_flux_k, obs_flux_slope, nrow = 1)

ggsave("figures/k_flux_observed.png", width = 11, height = 5)

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

grimeDB_attributes %>% 
  filter(COMID == 24006204, CH4mean > .0001) %>% 
  ggplot()+
  geom_boxplot(aes(y=as.factor(Site_Nid), x= CH4mean))+
  scale_x_log10(labels=scales::number)+
  theme_bw()+
  labs(y="Site_Nid")

ggsave("figures/site_krycklan.png", width = 5, height = 10)


