# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive', 'ncdf4', 'raster', 'sf',  'tabularaster', 'tictoc')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

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



#### Using tabularastwer function "cellnumbers" sped up the calculataions in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(africa_gwstack, 1), africa)
toc()
head(index)

tic("do extraction")
africa_df <- index %>% 
  mutate( gw_jan = raster::extract(africa_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(africa_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(africa_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(africa_gwstack[[4]], cell_ ),
          gw_may = raster::extract(africa_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(africa_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(africa_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(africa_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(africa_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(africa_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(africa_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(africa_gwstack[[12]], cell_ )
          ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()

africa_df <- africa_df %>% 
  mutate(COMID = africa$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

write_csv(africa_df, "data/raw/gis/GRADES_attributes/gwTable_01.csv")
 
rm(africa_df, africa, index, africa_gwstack)

## Do #2, which is Europe ----
# read the GRADES shapefile of catchments and the gwt file 
europe <- read_sf(shapes_catchments[2]) %>% st_set_crs(4326)

# open a gw file into a stack of rasters, one for each month
europe_gwstack <- stack(gw_files[2])

europe_gwstack <- crop(europe_gwstack, extent(europe))


ggplot()+
  geom_sf(data=sample_n(anti_join(europe,europe_df), 3000), color="blue")

plot(europe_gwstack[[1]])


#### Using tabularaster function "cellnumbers" sped up the calculations in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(europe_gwstack, 1), europe)
toc()
head(index)

tic("do extraction")
europe_df <- index %>% 
  mutate( gw_jan = raster::extract(europe_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(europe_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(europe_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(europe_gwstack[[4]], cell_ ),
          gw_may = raster::extract(europe_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(europe_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(europe_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(europe_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(europe_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(europe_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(europe_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(europe_gwstack[[12]], cell_ )
  ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

europe_df <- europe_df %>% 
  mutate(COMID = europe$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

#iceland is missing from the gw map
write_csv(europe_df, "data/raw/gis/GRADES_attributes/gwTable_02.csv")

rm(europe, index, europe_gwstack)
gc()

## Do #3, which is Asia north ----
# read the GRADES shapefile of catchments and the gwt file 
asia_north <- read_sf(shapes_catchments[3]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
asiaN_gwstack <- stack(gw_files[2])

asiaN_gwstack <- crop(asiaN_gwstack, extent(asia_north))


#### Using tabularaster function "cellnumbers" sped up the calculations in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(asiaN_gwstack, 1), asia_north)
toc()
head(index)

tic("do extraction")
asiaN_df <- index %>% 
  mutate( gw_jan = raster::extract(asiaN_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(asiaN_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(asiaN_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(asiaN_gwstack[[4]], cell_ ),
          gw_may = raster::extract(asiaN_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(asiaN_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(asiaN_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(asiaN_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(asiaN_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(asiaN_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(asiaN_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(asiaN_gwstack[[12]], cell_ )
  ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

asiaN_df <- asiaN_df %>% 
  mutate(COMID = asia_north$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

write_csv(asiaN_df, "data/raw/gis/GRADES_attributes/gwTable_03.csv")

rm(asia_north, index, asiaN_gwstack)


## Do #4, which is Asia south ----
# read the GRADES shapefile of catchments and the gwt file 
asia_south <- read_sf(shapes_catchments[4]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
asiaS_gwstack <- stack(gw_files[2])

asiaS_gwstack <- crop(asiaS_gwstack, extent(asia_south))


#### Using tabularaster function "cellnumbers" sped up the calculations in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(asiaS_gwstack, 1), asia_south)
toc()
head(index)

tic("do extraction")
asiaS_df <- index %>% 
  mutate( gw_jan = raster::extract(asiaS_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(asiaS_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(asiaS_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(asiaS_gwstack[[4]], cell_ ),
          gw_may = raster::extract(asiaS_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(asiaS_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(asiaS_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(asiaS_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(asiaS_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(asiaS_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(asiaS_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(asiaS_gwstack[[12]], cell_ )
  ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

asiaS_df <- asiaS_df %>% 
  mutate(COMID = asia_south$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

write_csv(asiaS_df, "data/raw/gis/GRADES_attributes/gwTable_04.csv")

rm(asia_south, index, asiaS_gwstack)


## Do #5, which is Oceania ----
# read the GRADES shapefile of catchments and the gwt file 
oceania <- read_sf(shapes_catchments[5]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
oceania_gwstack <- stack(gw_files[4])

oceania_gwstack <- crop(oceania_gwstack, extent(oceania))


#### Using tabularaster function "cellnumbers" sped up the calculations in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(oceania_gwstack, 1), oceania)
toc()
head(index)

tic("do extraction")
oceania_df <- index %>% 
  mutate( gw_jan = raster::extract(oceania_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(oceania_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(oceania_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(oceania_gwstack[[4]], cell_ ),
          gw_may = raster::extract(oceania_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(oceania_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(oceania_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(oceania_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(oceania_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(oceania_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(oceania_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(oceania_gwstack[[12]], cell_ )
  ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

oceania_df <- oceania_df %>% 
  mutate(COMID = oceania$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

write_csv(oceania_df, "data/raw/gis/GRADES_attributes/gwTable_05.csv")

rm(oceania, index, oceania_gwstack, oceania_df)

## Do #6, which is south america ----
# read the GRADES shapefile of catchments and the gwt file 
south_america <- read_sf(shapes_catchments[6]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
south_america_gwstack <- stack(gw_files[5])

south_america_gwstack <- crop(south_america_gwstack, extent(south_america))


#### Using tabularaster function "cellnumbers" sped up the calculations in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(south_america_gwstack, 1), south_america)
toc()
head(index)

tic("do extraction")
south_america_df <- index %>% 
  mutate( gw_jan = raster::extract(south_america_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(south_america_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(south_america_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(south_america_gwstack[[4]], cell_ ),
          gw_may = raster::extract(south_america_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(south_america_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(south_america_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(south_america_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(south_america_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(south_america_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(south_america_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(south_america_gwstack[[12]], cell_ )
  ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

south_america_df <- south_america_df %>% 
  mutate(COMID = south_america$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

write_csv(south_america_df, "data/raw/gis/GRADES_attributes/gwTable_06.csv")

rm(south_america_df, south_america, index, south_america_gwstack)

## Do #7, which is north america 1----
# read the GRADES shapefile of catchments and the gwt file 
north_america <- read_sf(shapes_catchments[7]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
north_america_gwstack <- stack(gw_files[3])

north_america_gwstack <- crop(north_america_gwstack, extent(north_america))


#### Using tabularaster function "cellnumbers" sped up the calculations in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(north_america_gwstack, 1), north_america)
toc()
head(index)

tic("do extraction")
north_america_df <- index %>% 
  mutate( gw_jan = raster::extract(north_america_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(north_america_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(north_america_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(north_america_gwstack[[4]], cell_ ),
          gw_may = raster::extract(north_america_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(north_america_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(north_america_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(north_america_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(north_america_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(north_america_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(north_america_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(north_america_gwstack[[12]], cell_ )
  ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

north_america_df <- north_america_df %>% 
  mutate(COMID = north_america$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

write_csv(north_america_df, "data/raw/gis/GRADES_attributes/gwTable_07.csv")

rm(north_america, index, north_america_gwstack)


## Do #8, which is north america 2----
# read the GRADES shapefile of catchments and the gwt file 
north2_america <- read_sf(shapes_catchments[8]) %>% st_set_crs(4326)


# open a gw file into a stack of rasters, one for each month
north2_america_gwstack <- stack(gw_files[3])

north2_america_gwstack <- crop(north2_america_gwstack, extent(north2_america))


#### Using tabularaster function "cellnumbers" sped up the calculations in Africa from 8 days to 20 minutes...so much worth it
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
tic("make index")
index <- cellnumbers(raster::subset(north2_america_gwstack, 1), north2_america)
toc()
head(index)

tic("do extraction")
north2_america_df <- index %>% 
  mutate( gw_jan = raster::extract(north2_america_gwstack[[1]], cell_ ),
          gw_feb = raster::extract(north2_america_gwstack[[2]], cell_ ),
          gw_mar = raster::extract(north2_america_gwstack[[3]], cell_ ),
          gw_apr = raster::extract(north2_america_gwstack[[4]], cell_ ),
          gw_may = raster::extract(north2_america_gwstack[[5]], cell_ ),
          gw_jun = raster::extract(north2_america_gwstack[[6]], cell_ ),
          gw_jul = raster::extract(north2_america_gwstack[[7]], cell_ ),
          gw_aug = raster::extract(north2_america_gwstack[[8]], cell_ ),
          gw_sep = raster::extract(north2_america_gwstack[[9]], cell_ ),
          gw_oct = raster::extract(north2_america_gwstack[[10]], cell_ ),
          gw_nov = raster::extract(north2_america_gwstack[[11]], cell_ ),
          gw_dec = raster::extract(north2_america_gwstack[[12]], cell_ )
  ) %>% 
  group_by(object_) %>%
  summarise(across(gw_jan:gw_dec, ~ median(.x, na.rm = TRUE)))
toc()
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

north2_america_df <- north2_america_df %>% 
  mutate(COMID = north2_america$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)

write_csv(north2_america_df, "data/raw/gis/GRADES_attributes/gwTable_08.csv")

rm(north2_america, index, north2_america_gwstack,north2_america_df)

# Upload the processed files into google drive ----

full_files <- list.files("data/raw/gis/GRADES_attributes",full.names = TRUE) 

path_in_drive <- paste("SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes", 
                         list.files("data/raw/gis/GRADES_attributes"), sep="/") 

gw_path_in_drive <-  path_in_drive[grepl("gwTable", path_in_drive)] 

gw_to_upload <-  gw_destination[grepl("gwTable", gw_destination)] 



map2(gw_to_upload, gw_path_in_drive, drive_upload)


