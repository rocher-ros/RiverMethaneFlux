# Load  and install packages ----
#List of all packages needed
package_list <- c('tidyverse', 'lubridate', 'corrr', 'patchwork', 'ggthemes', 'sf')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

#If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

#Now load all the packages
lapply(package_list, require, character.only = TRUE)


# Read and merge files ----
load(file.path("data", "raw", "MethDB_tables_converted.rda"))



#Merge the temperature estimated and measured into one column
conc_df <- conc_df %>% 
  mutate(WaterTemp_best = if_else(is.na(WaterTemp_actual) == FALSE,WaterTemp_actual, WaterTemp_est ))


# join the concentration with the gis datasets
concs_gis <-  left_join(conc_df, 
                        gis_df %>% 
                          mutate(Site_Nid=as.character(Site_Nid)), by="Site_Nid")

# join the concentration with the site dataset, sort by latitude for later plots
conc_sites <- left_join(conc_df, sites_df, by="Site_Nid") %>% 
  mutate(site_lat = paste("Site:",Site_Nid,"\nLat: " , round(Latitude,1), sep=""))

conc_sites$site_lat <- reorder(conc_sites$site_lat, desc(conc_sites$Latitude))



# Exploration in space with the gis data ----

## global map per month ----
map_sc <- map_data('world')

sites_months <- conc_sites %>% group_by(Site_Nid) %>% 
  summarise(Latitude =first(Latitude),
            Longitude = first(Longitude),
            n_CH4=n(),
            n_CO2 = sum(CO2mean > 0),
            n_N2O = sum(N2Omean > 0),
            n_Q = sum(Q > 0),
            n_temp = sum(WaterTemp_actual > -10, na.rm = TRUE) + sum(WaterTemp_est > -10, na.rm = TRUE),
            jan = sum(month(Date_start) == 1),
            feb = sum(month(Date_start) == 2),
            mar = sum(month(Date_start) == 3),
            apr = sum(month(Date_start) == 4),
            may = sum(month(Date_start) == 5),
            jun = sum(month(Date_start) == 6),
            jul = sum(month(Date_start) == 7),
            aug = sum(month(Date_start) == 8),
            sep = sum(month(Date_start) == 9),
            oct = sum(month(Date_start) == 10),
            nov = sum(month(Date_start) == 11),
            dec = sum(month(Date_start) == 12)
  ) %>% 
  pivot_longer(jan:dec, names_to = "month", values_to = "n_month") %>% 
  mutate(n_month = if_else(n_month == 0, NA_integer_, n_month)) %>% 
  drop_na(n_month)


sites_months$month <- factor(sites_months$month, levels= c("jan","feb","mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

dat_text <- sites_months %>% 
  group_by(month) %>% 
  summarise(n_sites = n_distinct(Latitude), 
            label = paste("n_obs = ", n(), ", n_sites =", n_sites))


ggplot()+
  geom_map(data=map_sc, map=map_sc,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="gray90", colour="gray40", size=0.5)+
  geom_point(data=sites_months, 
             aes(x=Longitude, y=Latitude, color= n_month, alpha = 0.5), 
             size=2, alpha=.5)+
  scale_y_continuous(limits=c(-52,80))+
  geom_text( data = dat_text,
             mapping = aes(x = -Inf, y = -Inf, label = label), hjust   = -.1, vjust   = -.8)+
  scale_colour_viridis_b(option = "D", trans= "log", breaks = c(1,2, 5, 10, 50, 100), na.value = NA)+
  coord_sf(crs = 4087)+
  labs(color= "")+
  theme_map()+
  facet_wrap(~month)+
  theme(title = element_text(size=16))

#ggsave("figures/map_sites_months.png", plot = sites_months_map , width = 15)




## Correlations with spatial explicit variables----

#Make the average for each site
concs_gis_avg <- concs_gis  %>%
  select(Site_Nid, CH4mean, q_avg_m3s:human_footprint_up_09) %>%
  filter(CH4mean > 0.01) %>%
  group_by(Site_Nid) %>% 
  summarise(across(everything(), mean)) 

#pearson correlations for CH4
corr_ch4_everything <- concs_gis_avg %>% 
  mutate(CH4mean = log(CH4mean)) %>%
  select_if(is.numeric) %>%
  correlate() %>% 
  focus(CH4mean)

corr_ch4_everything %>% arrange(CH4mean) %>% print(n=20)
corr_ch4_everything %>% arrange(desc(CH4mean)) %>% print(n=20)

#select the variables for plotting, these are with a high r and i remove redundant variables
vars_for_plot <- corr_ch4_everything %>% 
  arrange(desc(abs(CH4mean))) %>% 
  filter(abs(CH4mean) > .12,
         !str_detect(term, "up|s09|s06|s05|average|actual|ecoegion|s01|s02|sub_min|karst|people") ) %>% 
  mutate(term=fct_reorder(term, desc(abs(CH4mean))))


concs_gis_avg %>% 
  select(CH4mean, vars_for_plot$term) %>% 
  pivot_longer(	-CH4mean, names_to = "variable", values_to = "value") %>% 
  filter(CH4mean > 0, value >= 0) %>% 
  mutate(variable=fct_relevel(variable, levels(vars_for_plot$term))) %>% 
  ggplot(aes(value, CH4mean))+
  geom_hex(bins = 30) +
  geom_text( data = vars_for_plot %>% rename(variable=term), 
             mapping = aes(x = Inf, y = Inf, label = paste("r=",round(CH4mean,2))), hjust   = 3.5, vjust   = 1.5)+
  #scale_x_log10()+
  scale_y_log10(breaks=c(.1,1,10,100))+
  scale_fill_continuous(type = "viridis", trans = "log10")+
  facet_wrap(~variable, scales= "free")+
  theme_classic()

#ggsave("figures/correlations.png", width = 12, height = 9)


