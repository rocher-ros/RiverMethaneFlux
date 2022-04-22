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

annPP %>% 
  summarise(across(everything(), ~sum(is.na(.x))))

eleSlope <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "eleSlope", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(eleSlope) 

eleSlope %>% 
  summarise(across(everything(), ~sum(is.na(.x))))

gwTable <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "gwTable", full.names = TRUE), read_csv) %>% 
  bind_rows() %>% 
  mutate(across(starts_with("gw"), abs))

colnames(gwTable)


gwTable %>% 
  summarise(across(everything(), ~sum(is.na(.x))))

q_k <- read_csv("data/processed/q_and_k.csv") %>% 
  dplyr::select(COMID, ends_with("Qmean"), ends_with("k"), -yeaQmean) #lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "k", full.names = TRUE), read_csv) %>% 
  #bind_rows()

colnames(q_k) <- c("COMID", "q_jan", "q_feb", "q_mar", "q_apr", "q_may", "q_jun", "q_jul", "q_aug", "q_sep", "q_oct", "q_nov", "q_dec",
                 "k_jan", "k_feb", "k_mar", "k_apr", "k_may", "k_jun", "k_jul", "k_aug", "k_sep", "k_oct", "k_nov", "k_dec")

monPP <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "monPP", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(monPP) <- c("COMID",  "gpp_jan", "npp_jan", "gpp_feb", "npp_feb", "gpp_mar", "npp_mar", "gpp_apr", "npp_apr", "gpp_may", "npp_may", "gpp_jun", 
                     "npp_jun", "gpp_jul", "npp_jul", "gpp_aug", "npp_aug", "gpp_sep", "npp_sep", "gpp_oct", "npp_oct", "gpp_nov", "npp_nov", "gpp_dec", "npp_dec")

monTemp <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "monTemp", full.names = TRUE), read_csv) %>% 
  bind_rows()

colnames(monTemp) <- c("COMID", "temp_jan", "temp_feb", "temp_mar", "temp_apr", "temp_may", "temp_jun", "temp_jul", "temp_aug", "temp_sep", "temp_oct", "temp_nov", "temp_dec")

popdens <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "popdens", full.names = TRUE), read_csv) %>% 
  bind_rows() %>% 
  mutate(  popdens= rowMeans(dplyr::select(., `2000`:`2015`))) %>% 
  dplyr::select(COMID, popdens)

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

land <- read_csv("data/raw/gis/GRADES_attributes/land_good.csv") %>%
  mutate(trees=ifelse(is.na(trees) == TRUE, 0, trees)) %>% 
  dplyr::select(-Length, -slope, -uparea)

colnames(land)

nutrients_water <- read_csv("data/raw/gis/GRADES_attributes/nutrients_water.csv") %>% 
  dplyr::select(-P_retention_subgrid)
colnames(nutrients_water)

footprint <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "human_footprint", full.names = TRUE), read_csv) %>% 
  bind_rows() %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))


fertilizers <- read_csv("data/raw/gis/GRADES_attributes/fertilizers.csv")
colnames(fertilizers)

mountains <- read_csv("data/raw/gis/GRADES_attributes/mountains.csv")
colnames(mountains)

peatlands <- read_csv("data/raw/gis/GRADES_attributes/peatlands.csv")
colnames(peatlands)

biomes <- read_csv("data/raw/gis/GRADES_attributes/biomes.csv") %>% dplyr::select(-biome_num)
colnames(biomes)


#read coordinates from grades
grades_latlon <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  dplyr::select(COMID, lat =lat_mid, lon=lon_mid, slope)

nutrients_water %>% 
  summarise(across(everything(), ~sum(is.na(.x)))) %>% 
  pivot_longer(everything(), names_to = "name", values_to = "n_na") %>% 
  arrange(desc(n_na))

# Gw has some gaps, fix it
grades_gw_sf <- grades_latlon %>% 
  dplyr::select(-slope) %>% 
  left_join(gwTable, by="COMID") %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326) 

#get the sites with gaps and no gaps in separate df
gw_withdata <-  grades_gw_sf %>% 
  drop_na(gw_jan:gw_dec) %>% 
  st_sf()

gw_missing <- grades_gw_sf %>%
  filter_all(any_vars(is.na(.))) %>% 
  dplyr::select(COMID)

#find nearest point with data
nearest <- st_nearest_feature(gw_missing, gw_withdata)

gw_filled <- cbind(gw_missing, st_drop_geometry(gw_withdata)[nearest,]) %>% 
  dplyr::select(-COMID.1)


#join with both filled datasets
gw_good <- bind_rows(gw_withdata %>% st_drop_geometry(), 
                     gw_filled %>% st_drop_geometry())

rm( grades_gw_sf, gw_withdata, gw_filled)

#join all GRADES tables into one, and export it as one
grades_attributes <-  annPP %>% 
  left_join(grades_latlon, by="COMID") %>%  
  left_join(eleSlope, by="COMID") %>% 
  left_join(gw_good, by="COMID") %>% 
  left_join(q_k, by="COMID") %>% 
  left_join(monPP, by="COMID") %>% 
  left_join(monTemp, by="COMID") %>% 
  left_join(popdens, by="COMID") %>%
  left_join(prectemp, by="COMID") %>%
  left_join(soilATT, by ="COMID") %>% 
  left_join(sresp, by ="COMID") %>% 
  left_join(uparea, by ="COMID") %>% 
  left_join(wetland, by ="COMID") %>% 
  left_join(land, by ="COMID") %>% 
  left_join(nutrients_water, by ="COMID") %>% 
  left_join(footprint, by ="COMID") %>% 
  left_join(fertilizers, by ="COMID") %>% 
  left_join(mountains, by ="COMID") %>% 
  left_join(peatlands, by ="COMID") %>% 
  left_join(biomes, by ="COMID") %>% 
  drop_na(q_jan) %>% 
  distinct(COMID, .keep_all = TRUE)

colnames(grades_attributes)

grades_attributes %>% 
  summarise(across(everything(), ~sum(is.na(.x)))) %>% 
  pivot_longer(everything(), names_to = "name", values_to = "n_na") %>% 
  arrange(desc(n_na)) %>% print(n=10)

grades_attributes %>%
  filter(duplicated(.[["COMID"]])) %>% 
  arrange(COMID)  %>% print(n=100)

grades_attributes %>% 
  filter(is.na(biome_label )) %>% 
  dplyr::select(COMID, lat, lon, biome_label ) %>% 
  print(n=10)

         
rm(annPP, eleSlope, gwTable, q_k, land, monPP, monTemp, popdens, prectemp, soilATT, sresp, 
   uparea, wetland, gw_good, biomes, peatlands, mountains, fertilizers, nutrients_water)

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


