# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive', 'lubridate', 'sf')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

# Download needed files ----
# download the grade attributes to the local copy 
dir.create("data/raw/gis/GRADES_attributes") 
grades_in_drive <- drive_ls("SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes")
#get the file paths for drive and locally
names_in_drive <- paste("SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes", grades_in_drive$name, sep="/") 
names_destination <- paste("data/raw/gis/GRADES_attributes", grades_in_drive$name, sep="/") 

#feed them through a map, if needed
if(all(file.exists(names_destination)) == TRUE) {
  print("files already downloaded")
} else {
  map2(names_in_drive, names_destination, drive_download)
}

## Download the GRiMe ----
if(file.exists("data/raw/MethDB_tables_converted.rda") == TRUE) {
  print("files already downloaded")
} else {
  drive_download(
    "SCIENCE/PROJECTS/RiverMethaneFlux/methane/MethDB_tables_converted.rda",
    path = "data/raw/MethDB_tables_converted.rda",
    overwrite = TRUE
  )
}

## Download the sites in GRiMe with the COMID's ----
if(file.exists("data/processed/sites_meth_comid.csv") == TRUE) {
  print("files already downloaded")
} else {
  drive_download("SCIENCE/PROJECTS/RiverMethaneFlux/methane/sites_meth_comid.csv",
                 path = "data/processed/sites_meth_comid.csv" )
}

rm(list=ls())

# Read files into R ----
#get the names
grades_attributes_files <- list.files("data/raw/gis/GRADES_attributes")

#check the file sizes, in MB
sum(file.size(paste("data/raw/gis/GRADES_attributes", grades_attributes_files, sep="/")))/1e+6 

#I will combine each attribute into one file in R

#get the string of unique attributes
attributes <- word(grades_attributes_files,1,sep = "\\_") %>% unique()

attributes

## First read all attributes in GRADES ----
annPP <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "annPP", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(annPP) <- c("COMID", "GPP_yr",   "NPP_yr")

eleSlope <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "eleSlope", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(eleSlope) 

gwTable <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "gwTable", full.names = TRUE), read_csv) %>% 
  bind_rows() %>% 
  mutate(across(starts_with("gw"), abs))

colnames(gwTable)

k <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "k", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(k) <- c("COMID", "k_jan", "k_feb", "k_mar", "k_apr", "k_may", "k_jun", "k_jul", "k_aug", "k_sep", "k_oct", "k_nov", "k_dec")

monPP <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "monPP", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(monPP) <- c("COMID",  "gpp_jan", "npp_jan", "gpp_feb", "npp_feb", "gpp_mar", "npp_mar", "gpp_apr", "npp_apr", "gpp_may", "npp_may", "gpp_jun", 
                     "npp_jun", "gpp_jul", "npp_jul", "gpp_aug", "npp_aug", "gpp_sep", "npp_sep", "gpp_oct", "npp_oct", "gpp_nov", "npp_nov", "gpp_dec", "npp_dec")

monTemp <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "monTemp", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(monTemp) <- c("COMID", "temp_jan", "temp_feb", "temp_mar", "temp_apr", "temp_may", "temp_jun", "temp_jul", "temp_aug", "temp_sep", "temp_oct", "temp_nov", "temp_dec")

popdens <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "popdens", full.names = TRUE), read_csv) %>% 
  bind_rows() %>% 
  mutate(  popdens= rowMeans(select(., `2000`:`2015`))) %>% 
  select(COMID, popdens)

colnames(popdens) 

prectemp <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "prectemp", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(prectemp) <- c("COMID", "temp_yr", "prec_yr", "prec_jan", "prec_feb", "prec_mar", "prec_apr", "prec_may", "prec_jun", "prec_jul", "prec_aug", "prec_sep", "prec_oct", "prec_nov", "prec_dec",
                        "tavg_jan", "tavg_feb", "tavg_mar", "tavg_apr", "tavg_may", "tavg_jun", "tavg_jul", "tavg_aug", "tavg_sep", "tavg_oct", "tavg_nov", "tavg_dec")

soilATT <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "soilATT", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(soilATT)

sresp <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "sresp", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(sresp) <- c("COMID", "pRS_jan", "pRS_feb", "pRS_mar", "pRS_apr", "pRS_may", "pRS_jun", "pRS_jul", "pRS_aug", "pRS_sep", "pRS_oct", "pRS_nov", "pRS_dec", "pyearRA", "pyearRH", "pyearRS")

uparea <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "uparea", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(uparea)

wetland <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "wetland", full.names = TRUE), read_csv) %>% 
  bind_rows() %>% 
  mutate( wetland = (wetland*100))

colnames(wetland)

land <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "landcover", full.names = TRUE), read_csv) %>% 
  bind_rows() %>% 
  select(COMID, cover, cover_cls, trees)

colnames(land)

#read coordinates from grades
grades_latlon <-  read_csv("data/raw/gis/GRADES_attributes/grades_lat_lon.csv") %>% 
  select(COMID, lat, lon)

## Now get GRiMeDB ----
load(file.path("data", "raw", "MethDB_tables_converted.rda"))

sites_df <- sites_df %>% 
  mutate(Channel_type = ifelse(is.na(Channel_type) == TRUE, "normal", Channel_type)) 

grime_comids <- read_csv("data/processed/sites_meth_comid.csv") %>% 
  mutate(Site_Nid= as.character(Site_Nid))



# Process all files ----
sites_clean <- sites_df %>% 
  left_join(grime_comids, by="Site_Nid") %>% 
  drop_na(COMID)



## attach the COMID to the concentration df and keep useful variables 
conc_df_comids <- conc_df %>% 
  filter(Site_Nid %in% sites_clean$Site_Nid) %>% 
  left_join(sites_clean, by="Site_Nid") %>% 
  select(Site_Nid, `Aggregated?`, Channel_type, COMID,  order, distance_snapped, slope_m_m, CH4mean, CO2mean,
         date= Date_start, date_end= Date_end, discharge_measured= Q, WaterTemp_actual, WaterTemp_est  ) %>% 
  mutate(CH4mean =ifelse(CH4mean < 0, 0.0001, CH4mean)) %>% 
  drop_na(CH4mean)

# Gw has some gaps, fix it

grades_gw_sf <- grades_latlon %>% 
  left_join(gwTable, by="COMID") %>% 
  st_as_sf( coords = c("lat", "lon"),  crs = 4326) %>%
  st_transform("+proj=eqearth +wktext") 

#get the sites with gaps and no gaps in separate df
gw_withdata <-  grades_gw_sf %>% 
  drop_na(gw_jan) %>% 
  st_sf()

gw_missing <- grades_gw_sf %>%
  filter_all(any_vars(is.na(.))) %>% 
  select(COMID)

#find nearest point with data
nearest <- st_nearest_feature(gw_missing, gw_withdata)

gw_filled <- cbind(gw_missing, st_drop_geometry(gw_withdata)[nearest,]) %>% 
  select(-COMID.1)


#join with both filled datasets
gw_good <- bind_rows(gw_withdata %>% st_drop_geometry(), 
                     gw_filled %>% st_drop_geometry())

rm( grades_gw_sf, gw_withdata, gw_filled)

#join all GRADES tables into one, and export it as one
grades_attributes <-  grades_latlon %>% 
  left_join(annPP, by="COMID") %>%  
  left_join(eleSlope, by="COMID") %>% 
  left_join(gw_good, by="COMID") %>% 
  left_join(k, by="COMID") %>% 
  left_join(monPP, by="COMID") %>% 
  left_join(monTemp, by="COMID") %>% 
  left_join(popdens, by="COMID") %>%
  left_join(prectemp, by="COMID") %>%
  left_join(soilATT, by ="COMID") %>% 
  left_join(sresp, by ="COMID") %>% 
  left_join(uparea, by ="COMID") %>% 
  left_join(wetland, by ="COMID") %>% 
  mutate(across(where(is.numeric), ~na_if(., -Inf))) %>% 
  mutate(across(where(is.numeric), ~na_if(., Inf))) %>% 
  drop_na(elev)

rm(annPP, eleSlope, gwTable, k, land, monPP, monTemp, popdens, prectemp, soilATT, sresp, 
   uparea, wetland, gw_good)

gc()


# export the file with all attributes
grades_attributes %>% 
  rename_all( ~ str_replace(., "pRS", "sresp")) %>% 
  rename_all( ~ str_replace(., "prec", "precip")) %>% 
  rename(prec_yr = precip_yr) %>% 
write_csv("data/processed/grade_attributes.csv")  

drive_upload(media = "data/processed/grade_attributes.csv",
             path="SCIENCE/PROJECTS/RiverMethaneFlux/processed/grade_attributes.csv",
             overwrite = TRUE)


#Now attach all the annual variables to each pair of site_obs
grimeDB_attributes <- conc_df_comids %>% 
  left_join(grades_attributes, by="COMID")

# Now we do the monthly resolved variables, needs some thinking...
# Ok the way is to check the month of each observation and then match the value of a given variable
grimeDB_attributes_mon <- grimeDB_attributes %>% 
  mutate( gw_month = case_when(month(date) == 1 ~ gw_jan,
                             month(date) == 2 ~ gw_feb,
                             month(date) == 3 ~ gw_mar,
                             month(date) == 4 ~ gw_apr,
                             month(date) == 5 ~ gw_may,
                             month(date) == 6 ~ gw_jun,
                             month(date) == 7 ~ gw_jul,
                             month(date) == 8 ~ gw_aug,
                             month(date) == 9 ~ gw_sep,
                             month(date) == 10 ~ gw_oct,
                             month(date) == 11 ~ gw_nov,
                             month(date) == 12 ~ gw_dec),
          k_month =  case_when(month(date) == 1 ~  k_jan,
                               month(date) == 2 ~  k_feb,
                               month(date) == 3 ~  k_mar,
                               month(date) == 4 ~  k_apr,
                               month(date) == 5 ~  k_may,
                               month(date) == 6 ~  k_jun,
                               month(date) == 7 ~  k_jul,
                               month(date) == 8 ~  k_aug,
                               month(date) == 9 ~  k_sep,
                               month(date) == 10 ~ k_oct,
                               month(date) == 11 ~ k_nov,
                               month(date) == 12 ~ k_dec),
          gpp_month = case_when(month(date) == 1 ~ gpp_jan,
                               month(date) == 2 ~ gpp_feb,
                               month(date) == 3 ~ gpp_mar,
                               month(date) == 4 ~ gpp_apr,
                               month(date) == 5 ~ gpp_may,
                               month(date) == 6 ~ gpp_jun,
                               month(date) == 7 ~ gpp_jul,
                               month(date) == 8 ~ gpp_aug,
                               month(date) == 9 ~ gpp_sep,
                               month(date) == 10 ~ gpp_oct,
                               month(date) == 11 ~ gpp_nov,
                               month(date) == 12 ~ gpp_dec),
          npp_month = case_when(month(date) == 1 ~ npp_jan,
                                month(date) == 2 ~ npp_feb,
                                month(date) == 3 ~ npp_mar,
                                month(date) == 4 ~ npp_apr,
                                month(date) == 5 ~ npp_may,
                                month(date) == 6 ~ npp_jun,
                                month(date) == 7 ~ npp_jul,
                                month(date) == 8 ~ npp_aug,
                                month(date) == 9 ~ npp_sep,
                                month(date) == 10 ~ npp_oct,
                                month(date) == 11 ~ npp_nov,
                                month(date) == 12 ~ npp_dec),
          temp_month = case_when(month(date) == 1 ~ temp_jan,
                                month(date) == 2 ~ temp_feb,
                                month(date) == 3 ~ temp_mar,
                                month(date) == 4 ~ temp_apr,
                                month(date) == 5 ~ temp_may,
                                month(date) == 6 ~ temp_jun,
                                month(date) == 7 ~ temp_jul,
                                month(date) == 8 ~ temp_aug,
                                month(date) == 9 ~ temp_sep,
                                month(date) == 10 ~ temp_oct,
                                month(date) == 11 ~ temp_nov,
                                month(date) == 12 ~ temp_dec),
          precip_month = case_when(month(date) == 1 ~ prec_jan/31*365,
                                month(date) == 2 ~ prec_feb/28*365,
                                month(date) == 3 ~ prec_mar/31*365,
                                month(date) == 4 ~ prec_apr/30*365,
                                month(date) == 5 ~ prec_may/31*365,
                                month(date) == 6 ~ prec_jun/30*365,
                                month(date) == 7 ~ prec_jul/31*365,
                                month(date) == 8 ~ prec_aug/31*365,
                                month(date) == 9 ~ prec_sep/30*365,
                                month(date) == 10 ~ prec_oct/31*365,
                                month(date) == 11 ~ prec_nov/30*365,
                                month(date) == 12 ~ prec_dec/31*365),
          tavg_month = case_when(month(date) == 1 ~ tavg_jan,
                                month(date) == 2 ~ tavg_feb,
                                month(date) == 3 ~ tavg_mar,
                                month(date) == 4 ~ tavg_apr,
                                month(date) == 5 ~ tavg_may,
                                month(date) == 6 ~ tavg_jun,
                                month(date) == 7 ~ tavg_jul,
                                month(date) == 8 ~ tavg_aug,
                                month(date) == 9 ~ tavg_sep,
                                month(date) == 10 ~ tavg_oct,
                                month(date) == 11 ~ tavg_nov,
                                month(date) == 12 ~ tavg_dec),
          sresp_month = case_when(month(date) == 1 ~ pRS_jan*365,
                                month(date) == 2 ~ pRS_feb*365,
                                month(date) == 3 ~ pRS_mar*365,
                                month(date) == 4 ~ pRS_apr*365,
                                month(date) == 5 ~ pRS_may*365,
                                month(date) == 6 ~ pRS_jun*365,
                                month(date) == 7 ~ pRS_jul*365,
                                month(date) == 8 ~ pRS_aug*365,
                                month(date) == 9 ~ pRS_sep*365,
                                month(date) == 10 ~ pRS_oct*365,
                                month(date) == 11 ~ pRS_nov*365,
                                month(date) == 12 ~ pRS_dec*365) ) %>%
  select(!ends_with(c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

# quickly check one site
grimeDB_attributes_mon %>% filter(Site_Nid == "2597") %>% 
  ggplot(aes(date, gw_month))+
  geom_point()

#save to file and upload to google drive
grimeDB_attributes_mon %>% 
  write_csv("data/processed/grimeDB_concs_with_grade_attributes.csv")  

drive_upload(media = "data/processed/grimeDB_concs_with_grade_attributes.csv",
             path="SCIENCE/PROJECTS/RiverMethaneFlux/processed/grimeDB_concs_with_grade_attributes.csv",
             overwrite = TRUE)

