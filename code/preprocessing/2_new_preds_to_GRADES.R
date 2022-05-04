# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive', 'ncdf4', 'raster', 'sf',  'velox', 'tictoc', 'tmap')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

#The fastest way for raster extractions is the package velox, need to be installed here:
#remotes::install_github("hunzikp/velox")

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# Download needed files ----
#  get the raster files of the new attributes such as  groundwater table, to join with grades
dir.create("data/raw/gis/Groundwater_table")
gw_in_drive <- drive_ls("SCIENCE/PROJECTS/RiverMethaneFlux/gis/Groundwater_table")

#get the file paths for drive and locally
gwpath_in_drive <- paste("SCIENCE/PROJECTS/RiverMethaneFlux/gis/Groundwater_table", gw_in_drive$name, sep="/") 
gw_destination <- paste("data/raw/gis/Groundwater_table", gw_in_drive$name, sep="/") 

#feed them through a map
if(all(file.exists(gw_destination)) == TRUE) {
  print("files already downloaded")
} else {
  map2(gwpath_in_drive, gw_destination, drive_download)
}

rm(list = ls())

# Join the new attributes to the main files ----

#get the files than end in .shp, and separate them in catchments and networks
files <- list.files("data/raw/grades")[grepl(".shp$", list.files("data/raw/grades"))]

shape_files <- paste("data/raw/grades", files[grepl(".shp$", files)], sep="/") 

shapes_catchments <- shape_files[grepl("cat", shape_files)]

gw_files <- list.files("data/raw/gis/Groundwater_table", full.names = TRUE)

gw_files <- gw_files[grepl(".nc$", gw_files)]

## Start with #1, which is Africa ----
# read the GRADES shapefile of catchments and the gwt file 
africa <- read_sf(shapes_catchments[1]) %>% st_set_crs(4326)

# open a gw file into a stack of rasters, one for each month
africa_gwstack <- stack(gw_files[1])

#the velox method is the fastest for this task
vx <- velox(africa_gwstack, extent=extent(africa_gwstack), res=res(africa_gwstack),
            crs=crs(africa_gwstack))

tic()
monthly <- vx$extract(sp=africa, fun=mean)
toc()

africa_df <- as.data.frame(monthly)

colnames(africa_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")

africa_df$COMID <- africa$COMID

write_csv(africa_df, "data/raw/gis/GRADES_attributes/gwTable_01.csv")
 
rm(africa_df, africa, vx, africa_gwstack, monthly)

## Do #2, which is Europe ----
# read the GRADES shapefile of catchments and the gwt file 
europe <- read_sf(shapes_catchments[2]) %>% st_set_crs(4326)

# open a gw file into a stack of rasters, one for each month
europe_gwstack <- stack(gw_files[2])

europe_gwstack <- crop(europe_gwstack, extent(europe))


#the velox method is the fastest for this task
vx <- velox(europe_gwstack, extent=extent(europe_gwstack), res=res(europe_gwstack),
            crs=crs(europe_gwstack))

tic()
monthly <- vx$extract(sp=europe, fun=mean)
toc()

#turn into a df
europe_df <- as.data.frame(monthly)
colnames(europe_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")
europe_df$COMID <- europe$COMID

#iceland is missing from the gw map
write_csv(europe_df, "data/raw/gis/GRADES_attributes/gwTable_02.csv")

rm(europe_df, europe, vx, europe_gwstack, monthly)



## Do #3, which is Asia north ----
# read the GRADES shapefile of catchments and the gwt file 
asia_north <- read_sf(shapes_catchments[3]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
asiaN_gwstack <- stack(gw_files[2])

asiaN_gwstack <- crop(asiaN_gwstack, extent(asia_north))


#the velox method is the fastest for this task
vx <- velox(asiaN_gwstack, extent=extent(asiaN_gwstack), res=res(asiaN_gwstack),
            crs=crs(asiaN_gwstack))

tic()
monthly <- vx$extract(sp=asia_north, fun=mean)
toc()

#turn into a df
asiaN_df <- as.data.frame(monthly)
colnames(asiaN_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")
asiaN_df$COMID <- asia_north$COMID

write_csv(asiaN_df, "data/raw/gis/GRADES_attributes/gwTable_03.csv")

rm(asiaN_df, asia_north, vx, asiaN_gwstack, monthly)


## Do #4, which is Asia south ----
# read the GRADES shapefile of catchments and the gwt file 
asia_south <- read_sf(shapes_catchments[4]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
asiaS_gwstack <- stack(gw_files[2])

asiaS_gwstack <- crop(asiaS_gwstack, extent(asia_south))


#the velox  is the fastest for this task
tic()
vx <- velox(asiaS_gwstack, extent=extent(asiaS_gwstack), res=res(asiaS_gwstack),
            crs=crs(asiaS_gwstack))

monthly <- vx$extract(sp=asia_south, fun=mean)
toc()

#turn into a df
asiaS_df <- as.data.frame(monthly)
colnames(asiaS_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")
asiaS_df$COMID <- asia_south$COMID

write_csv(asiaS_df, "data/raw/gis/GRADES_attributes/gwTable_04.csv")

rm(asiaS_df, asia_south, vx, asiaS_gwstack, monthly)


## Do #5, which is Oceania ----
# read the GRADES shapefile of catchments and the gwt file 
oceania <- read_sf(shapes_catchments[5]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
oceania_gwstack <- stack(gw_files[4])

oceania_gwstack <- crop(oceania_gwstack, extent(oceania))


#the velox  is the fastest for this task
vx <- velox(oceania_gwstack, extent=extent(oceania_gwstack), res=res(oceania_gwstack),
            crs=crs(oceania_gwstack))

tic()
monthly <- vx$extract(sp=oceania, fun=mean)
toc()

#turn into a df
oceania_df <- as.data.frame(monthly)
colnames(oceania_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")
oceania_df$COMID <- oceania$COMID

write_csv(oceania_df, "data/raw/gis/GRADES_attributes/gwTable_05.csv")

rm(oceania_df, oceania, vx, oceania_gwstack, monthly)

## Do #6, which is south america ----
# read the GRADES shapefile of catchments and the gwt file 
south_america <- read_sf(shapes_catchments[6]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
south_america_gwstack <- stack(gw_files[5])

south_america_gwstack <- crop(south_america_gwstack, extent(south_america))


#the velox  is the fastest for this task
vx <- velox(south_america_gwstack, extent=extent(south_america_gwstack), res=res(south_america_gwstack),
            crs=crs(south_america_gwstack))

tic()
monthly <- vx$extract(sp=south_america, fun=mean)
toc()

#turn into a df
south_america_df <- as.data.frame(monthly)
colnames(south_america_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")
south_america_df$COMID <- south_america$COMID

write_csv(south_america_df, "data/raw/gis/GRADES_attributes/gwTable_06.csv")

rm(south_america_df, south_america, vx, south_america_gwstack, monthly)

## Do #7, which is north america 1----
# read the GRADES shapefile of catchments and the gwt file 
north_america <- read_sf(shapes_catchments[7]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
north_america_gwstack <- stack(gw_files[3])

north_america_gwstack <- crop(north_america_gwstack, extent(north_america))


#the velox  is the fastest for this task
vx <- velox(north_america_gwstack, extent=extent(north_america_gwstack), res=res(north_america_gwstack),
            crs=crs(north_america_gwstack))

tic()
monthly <- vx$extract(sp=north_america, fun=mean)
toc()

#turn into a df
north_america_df <- as.data.frame(monthly)
colnames(north_america_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")
north_america_df$COMID <- north_america$COMID

write_csv(north_america_df, "data/raw/gis/GRADES_attributes/gwTable_07.csv")

rm(north_america_df, north_america, vx, north_america_gwstack, monthly)


## Do #8, which is north america 2----
# read the GRADES shapefile of catchments and the gwt file 
north2_america <- read_sf(shapes_catchments[8]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
north2_america_gwstack <- stack(gw_files[3])

north2_america_gwstack <- crop(north2_america_gwstack, extent(north2_america))


#the velox  is the fastest for this task
vx <- velox(north2_america_gwstack, extent=extent(north2_america_gwstack), res=res(north2_america_gwstack),
            crs=crs(north2_america_gwstack))

tic()
monthly <- vx$extract(sp=north2_america, fun=mean)
toc()

#turn into a df
north2_america_df <- as.data.frame(monthly)
colnames(north2_america_df) <- c("gw_jan", "gw_feb", "gw_mar", "gw_apr", "gw_may", "gw_jun", "gw_jul", "gw_aug", "gw_sep", "gw_oct", "gw_nov", "gw_dec")
north2_america_df$COMID <- north2_america$COMID

write_csv(north2_america_df, "data/raw/gis/GRADES_attributes/gwTable_08.csv")

rm(north2_america_df, north2_america, vx, north2_america_gwstack, monthly)


# Upload the processed files into google drive ----

full_files <- list.files("data/raw/gis/GRADES_attributes",full.names = TRUE) 

path_in_drive <- paste("SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes", 
                         list.files("data/raw/gis/GRADES_attributes"), sep="/") 

gw_path_in_drive <-  path_in_drive[grepl("gwTable", path_in_drive)] 

gw_to_upload <-  full_files[grepl("gwTable", full_files)] 


map2(gw_to_upload, gw_path_in_drive, drive_upload)

# Land cover data into grades as well ----
dir.create("data/raw/gis/")
dir.create("data/raw/gis/GRADES_attributes")


#get land cover data
data(land)

land <- land %>% 
  st_as_sf() %>% 
  st_transform(4326)

#get grades back in
files <- list.files("data/raw/grades")[grepl(".shp$", list.files("data/raw/grades"))]

shape_files <- paste("data/raw/grades", files[grepl(".shp$", files)], sep="/") 

shapes_catchments <- shape_files[grepl("cat", shape_files)]

sf::sf_use_s2(FALSE)
for(i in 1:9){
  
  grades <- read_sf(shapes_catchments[i]) %>% st_set_crs(4326)

  grades_land <- st_join(land, grades) %>% 
    st_drop_geometry() %>%
    drop_na(COMID) %>% 
    arrange(COMID)
  
  dat_out <- 
    grades_land %>% 
    group_by(COMID) %>%
    summarise(water=sum(cover_cls == "Water")/n()*100,
              cropland=sum(cover_cls == "Cropland")/n()*100,
              forest=sum(cover_cls == "Forest")/n()*100,
              wetland=sum(cover_cls == "Wetland")/n()*100,
              other_nat_veg=sum(cover_cls == "Other natural vegetation")/n()*100,
              bare_sparse=sum(cover_cls == "Bare area/Sparse vegetation")/n()*100,
              urban=sum(cover_cls == "Urban")/n()*100,
              trees = mean(trees, na.rm=TRUE)) 
  
  dat_out %>% 
    write_csv( file=paste0("data/raw/gis/GRADES_attributes/landcover_0",i,".csv"))
  
  rm(grades_land, grades, dat_out)
  gc()
  print(paste("done", i))

}

#check if there are sites with no values, and fill them with the nearest one

land <- lapply(list.files(path = "data/raw/gis/GRADES_attributes", pattern = "landcover", full.names = TRUE), read_csv) %>% 
  bind_rows()  %>%
  mutate(trees=ifelse(is.na(trees) == TRUE, 0, trees)) %>% 
  dplyr::rename(wetland_cover=wetland)

grades_land <- read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>%
  left_join(land, by="COMID") %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)

grades_land %>% 
  filter(is.na(trees) == TRUE) %>% 
  ggplot()+
  geom_sf()


#get the sites with gaps and no gaps in separate df
dat_withdata <-  grades_land %>% 
  drop_na(urban) %>% 
  st_sf()

dat_missing <- grades_land %>%
  filter_all(any_vars(is.na(.))) %>% 
  dplyr::select(COMID)

#find nearest point with data
nearest <- st_nearest_feature(dat_missing, dat_withdata)

dat_filled <- cbind(dat_missing, st_drop_geometry(dat_withdata)[nearest,]) %>% 
  dplyr::select(-COMID.1)

dat_filled %>% 
  filter(is.na(urban) == TRUE) 

#join with both filled datasets
dat_good <- bind_rows(dat_withdata %>% st_drop_geometry(), 
                      dat_filled %>% st_drop_geometry())

write_csv(dat_good, "data/raw/gis/GRADES_attributes/land_good.csv")

#upload the file to google drive
drive_upload("data/raw/gis/GRADES_attributes/land_good.csv", "SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes/land_good.csv")

# process the nutrient data ----

#find the names of the nitrogen and phosphorus files
files_nitrogen <- list.files("data/raw/gis/nutrients freshwater/nitrogen/2000/", full.names = TRUE)
types_n <-  list.files("data/raw/gis/nutrients freshwater/nitrogen/2000/") %>% str_remove(".asc")

files_phosphorus <- list.files("data/raw/gis/nutrients freshwater/phosphorus/2000/", full.names = TRUE)
types_p <-  list.files("data/raw/gis/nutrients freshwater/phosphorus/2000/") %>% str_remove(".asc")

#read them all into a stack
n_stack = terra::rast(files_nitrogen)
p_stack <- terra::rast(files_phosphorus)


raster::plot(n_stack[[2]])



grades <- read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)
  

n_vals <- terra::extract( n_stack, st_coordinates(grades))

p_vals <- terra::extract( p_stack, st_coordinates(grades)) 

colnames(n_vals) <- c("N_groundwater_agri", "N_groundwater_nat", "N_aquaculture", "N_deposition_water", "N_gnpp",             
 "N_load", "N_point", "N_retention", "N_retention_subgrid" , "N_surface_runoff_agri", "N_surface_runoff_nat")

colnames(p_vals) <- c("P_aquaculture", "P_gnpp", "P_background", "P_load" , "P_point", "P_surface_runoff_agri",
                      "P_surface_runoff_nat",  "P_retention", "P_retention_subgrid")

dat_out <- bind_cols(grades, n_vals, p_vals)

dat_out %>% 
  filter(is.na(N_retention_subgrid) == TRUE | is.na(P_load) == TRUE ) %>% 
  ggplot()+
  geom_sf()


#get the sites with gaps and no gaps in separate df
dat_withdata <-  dat_out %>% 
  drop_na(N_retention_subgrid, P_load) %>% 
  st_sf()

dat_missing <- dat_out %>%
  filter(is.na(N_retention_subgrid) == TRUE | is.na(P_load) == TRUE ) %>% 
  dplyr::select(COMID)

#find nearest point with data
nearest <- st_nearest_feature(dat_missing, dat_withdata)

dat_filled <- cbind(dat_missing, st_drop_geometry(dat_withdata)[nearest,]) %>% 
  dplyr::select(-COMID.1)

dat_filled %>% 
  filter(is.na(N_retention_subgrid) == TRUE) 

#join with both filled datasets
dat_good <- bind_rows(dat_withdata %>% st_drop_geometry(), 
                      dat_filled %>% st_drop_geometry())

dat_good %>% dplyr::select( -slope, -uparea, -Length) %>% 
  write_csv(file="data/raw/gis/GRADES_attributes/nutrients_water.csv")
 
#upload the file to google drive
drive_upload("data/raw/gis/GRADES_attributes/nutrients_water.csv", 
             "SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes/nutrients_water.csv")


# Get the human footprint ----
#load raster file 
human_footprint <-raster("data/raw/gis/HumanFootprint/Maps/HFP2009.tif")

#get grades back in
files <- list.files("data/raw/grades")[grepl(".shp$", list.files("data/raw/grades"))]

shape_files <- paste("data/raw/grades", files[grepl(".shp$", files)], sep="/") 
shapes_catchments <- shape_files[grepl("cat", shape_files)]


for(i in 1:length(shapes_catchments)){
  
catchment <- read_sf(shapes_catchments[i]) %>% 
  st_set_crs(4326)%>% 
  st_transform(crs(human_footprint))

footprint_area <- crop(human_footprint, st_bbox(catchment))

#the velox  is the fastest for this task
vx <- velox(footprint_area, extent=extent(footprint_area), res=res(footprint_area),
            crs=crs(footprint_area))
print("velox done")
tic("extracting")
monthly <- vx$extract(sp=catchment, fun=mean)
toc()

#turn into a df
monthly_df <- as.data.frame(monthly)
colnames(monthly_df) <- c("hfi")
monthly_df$COMID <- catchment$COMID

print(i)
write_csv(monthly_df, paste("data/raw/gis/GRADES_attributes/human_footprint_",i,".csv"))

rm(catchment, vx, footprint_area)
gc()
}


#get the fertilizer data  
nh4 <- terra::rast("data/raw/gis/fertilizer inputs/NH4_input_ver1.nc4")
no3 <- terra::rast("data/raw/gis/fertilizer inputs/NO3_input_ver1.nc4")

grades <- read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  mutate(lon = lon_mid, lat = lat_mid) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)

nh4  <- terra::subst(nh4, c(NA, NaN), 0)
no3  <- terra::subst(no3, c(NA, NaN), 0)


nh4_avg <- sum(nh4[[480:600]], na.rm= TRUE)
no3_avg <- sum(no3[[480:600]], na.rm= TRUE)

nh4_vals <- terra::extract( nh4_avg, st_coordinates(grades))

no3_vals <- terra::extract( no3_avg, st_coordinates(grades)) 

dat_out <- tibble(COMID =grades$COMID, 
                  nh4_input = nh4_vals$sum,
                  no3_input = no3_vals$sum) %>% 
  mutate(nh4_input = ifelse(is.na(nh4_input) == TRUE, 0, nh4_input),
         no3_input = ifelse(is.na(no3_input) == TRUE, 0, no3_input ))

dat_out %>% filter(is.na(nh4_input) == TRUE)

write_csv(dat_out, "data/raw/gis/GRADES_attributes/fertilizers.csv")


# Mountains ----

sf::sf_use_s2(FALSE)

grades <- read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  mutate(lon = lon_mid, lat = lat_mid) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)


mountains <- read_sf("data/raw/gis/global_mountains/CMEC_Mountains_Enh2018.shp") %>% 
  st_transform(4326)

ggplot(mountains)+
  geom_sf(fill="red4")

data_out <- st_join(grades, mountains) %>% 
  mutate(mountains = if_else(is.na(OBJECTID) == FALSE, 1, 0)) %>% 
  dplyr::select(COMID, mountains) %>% 
  st_drop_geometry()



write_csv(data_out, "data/raw/gis/GRADES_attributes/mountains.csv")

# Peatlands ----

sf::sf_use_s2(FALSE)

grades <- read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  mutate(lon = lon_mid, lat = lat_mid) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)


peatlands <- terra::rast("data/raw/gis/peatlands/Peat-ML_global_peatland_extent.nc")

plot(peatlands)

peat_vals <- terra::extract( peatlands, st_coordinates(grades)) 

dat_out <- tibble(COMID = grades$COMID, 
                  peatland_cover = peat_vals$PEATLAND_P) %>% 
  mutate(peatland_cover = ifelse(is.na(peatland_cover) == TRUE, 0, peatland_cover))

summary(dat_out)


write_csv(dat_out, "data/raw/gis/GRADES_attributes/peatlands.csv")

# Peatlands ----
sf::sf_use_s2(FALSE)

grades <- read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  mutate(lon = lon_mid, lat = lat_mid) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)


biomes <- read_sf("data/raw/gis/biomes/wwf_terr_ecos.shp") %>% 
  mutate(biome_label = case_when(
    BIOME == 1 ~ "Tropical & Subtropical Moist Broadleaf Forests",
    BIOME == 2 ~ "Tropical & Subtropical Dry Broadleaf Forests",
    BIOME == 3 ~ "Tropical & Subtropical Coniferous Forests",
    BIOME == 4 ~ "Temperate Broadleaf & Mixed Forests",
    BIOME == 5 ~ "Temperate Conifer Forests",
    BIOME == 6 ~  "Boreal Forests/Taiga",
    BIOME == 7 ~ "Tropical & Subtropical Grasslands, Savannas & Shrublands",
    BIOME == 8 ~  "Temperate Grasslands, Savannas & Shrublands",
    BIOME == 9 ~ "Flooded Grasslands & Savannas",
    BIOME == 10 ~ "Montane Grasslands & Shrublands",
    BIOME == 11 ~ "Tundra",
    BIOME == 12 ~ "Mediterranean Forests, Woodlands & Scrub",
    BIOME == 13 ~ "Deserts & Xeric Shrublands",
    BIOME == 14 ~ "Mangroves",
    TRUE ~ "other"
  ))


unique(biomes$biome_label) %>% sort()

data_out <- st_join(grades, biomes) %>% 
  dplyr::select(COMID, biome_num = BIOME, biome_label ) 

#there are some missing sites 
data_out %>% filter(is.na(biome_label) == TRUE)

#all coastal sites are missing biome, we add the closest one
data_out %>% filter(is.na(biome_label) == TRUE) %>% 
  left_join(grades) %>% 
  st_as_sf() %>% 
ggplot()+
  geom_sf()

#we separate the completed ones with the missing ones
biome_withdata <-  data_out %>% 
  drop_na(biome_label) %>% 
  st_sf()

biome_missing <- data_out %>%
  filter_all(any_vars(is.na(.))) %>% 
  dplyr::select(COMID)  


#find nearest point with data
nearest <- st_nearest_feature(biome_missing, biomes)

biome_filled <- cbind(biome_missing, st_drop_geometry(biomes)[nearest,]) %>% 
  dplyr::select(COMID, biome_num = BIOME, biome_label )


#join with both filled datasets
biome_good <- bind_rows(biome_withdata %>% st_drop_geometry(), 
                        biome_filled %>% st_drop_geometry())


write_csv(biome_good, "data/raw/gis/GRADES_attributes/biomes.csv")



###### 
library(terra)
library(lubridate)

sf::sf_use_s2(FALSE)

grades <- read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  dplyr::select(COMID, lon = lon_mid, lat = lat_mid) %>% 
  st_as_sf( coords = c("lon", "lat"),  crs = 4326)


runoff <- terra::rast("data/raw/gis/runoff/GRUN_v1_GSWP3_WGS84_05_1902_2014.nc")

runoff_index <- tibble(names = names(runoff),
                     date = as.Date(terra::time(runoff)),
                     year= year(date),
                     month = month(date))

dates_to_extract <- runoff_index %>% 
  filter(year > 1984 )



runoff_jan <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 1] ) %>% mean() 
runoff_feb <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 2] ) %>% mean() 
runoff_mar <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 3] ) %>% mean() 
runoff_apr <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 4] ) %>% mean() 
runoff_may <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 5] ) %>% mean() 
runoff_jun <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 6] ) %>% mean() 
runoff_jul <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 7] ) %>% mean() 
runoff_aug <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 8] ) %>% mean() 
runoff_sep <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 9] ) %>% mean() 
runoff_oct <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 10] ) %>% mean() 
runoff_nov <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 11] ) %>% mean() 
runoff_dec <- subset(runoff, dates_to_extract$names[dates_to_extract$month == 12] ) %>% mean() 

year_runoff <- mean(runoff_jan,
            runoff_feb,
            runoff_mar,
            runoff_apr,
            runoff_may,
            runoff_jun,
            runoff_jul,
            runoff_aug,
            runoff_sep,
            runoff_oct,
            runoff_nov,
            runoff_dec)

plot(year_runoff)

dat_out <- tibble(COMID = grades$COMID, 
                  runoff_jan = terra::extract( runoff_jan, st_coordinates(grades))$mean,
                  runoff_feb = terra::extract( runoff_feb, st_coordinates(grades))$mean,
                  runoff_mar = terra::extract( runoff_mar, st_coordinates(grades))$mean,
                  runoff_apr = terra::extract( runoff_apr, st_coordinates(grades))$mean,
                  runoff_may = terra::extract( runoff_may, st_coordinates(grades))$mean,
                  runoff_jun = terra::extract( runoff_jun, st_coordinates(grades))$mean,
                  runoff_jul = terra::extract( runoff_jul, st_coordinates(grades))$mean,
                  runoff_aug = terra::extract( runoff_aug, st_coordinates(grades))$mean,
                  runoff_sep = terra::extract( runoff_sep, st_coordinates(grades))$mean,
                  runoff_oct = terra::extract( runoff_oct, st_coordinates(grades))$mean,
                  runoff_nov = terra::extract( runoff_nov, st_coordinates(grades))$mean,
                  runoff_dec = terra::extract( runoff_dec, st_coordinates(grades))$mean) 

dat_out %>% filter(is.na(runoff_jan) == TRUE)

#we separate the completed ones with the missing ones
runoff_withdata <-  dat_out %>% 
  drop_na(runoff_jul) %>% 
  left_join(grades) %>% 
  st_sf()

runoff_missing <- dat_out %>%
  filter_all(any_vars(is.na(.))) %>% 
  dplyr::select(COMID)  %>% 
  left_join(grades) %>% 
  st_sf()


#find nearest point with data
nearest <- st_nearest_feature(runoff_missing, runoff_withdata)


runoff_filled <- cbind(runoff_missing, st_drop_geometry(runoff_withdata)[nearest,]) %>% 
  dplyr::select(- COMID.1) 

#join with both filled datasets
runoff_good <- bind_rows(runoff_withdata %>% st_drop_geometry(), 
                        runoff_filled %>% st_drop_geometry()) %>% 
  rowwise() %>% 
  mutate(runoff_yr = mean(runoff_jan:runoff_dec))

runoff_good %>% filter(is.na(runoff_jul) == TRUE)


write_csv(runoff_good, "data/raw/gis/GRADES_attributes/runoff.csv")
