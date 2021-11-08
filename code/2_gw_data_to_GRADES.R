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

# Try to get land cover data into grades as well

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
  
  gc()
  
  grades_land <- st_join(land, grades)
  
  grades_land %>% 
    st_drop_geometry() %>%
    count(a, b, sort = TRUE) %>%
    group_by(a) %>%
    summarise(first = b[1],second = b[2])
    write_csv( file=paste0("data/raw/gis/GRADES_attributes/land_0",i,".csv"))
  
  rm(grades_land, grades)

}

a <- read_csv("data/raw/gis/GRADES_attributes/land_01.csv") %>% 
  drop_na(COMID)

