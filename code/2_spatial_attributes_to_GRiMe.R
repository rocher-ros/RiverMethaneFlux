# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'leaflet', 'googledrive', 'ncdf4', 'raster', 'sf', 'exactextractr', 'foreach', 'parallel')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

#sites_meth_comid <- read_csv("data/processed/sites_meth_comid.csv")
# Download the needed files from google drive  ----

## download the grade attributes to the local copy ----
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

# Also get the raster files of the new attributes such as  groundwater table, to join with grades
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

# Download the sites in GRiMe with the COMID's
if(file.exists("data/processed/sites_meth_comid.csv") == TRUE) {
  print("files already downloaded")
} else {
drive_download("SCIENCE/PROJECTS/RiverMethaneFlux/methane/sites_meth_comid.csv",
             path = "data/processed/sites_meth_comid.csv" )
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
# read the GRADES shapefile and the gwt file 
africa <- read_sf(shapes_catchments[1]) %>% st_set_crs(4326)

# open a netCDF file
africa_gw <- nc_open(gw_files[1])

print(africa_gw)


get_avg_ncdf <- function(ncdf_file, shape_file){
  extent_catch <- extent(shape_file)
  
  # get longitude and latitude
  lon <- ncvar_get(ncdf_file,"lon")
  nlon <- dim(lon)
  
  lat <- ncvar_get(ncdf_file,"lat")
  nlat <- dim(lat)
  
  # create a bounding box to work with a subset
  LonIdx <- which( ncdf_file$dim$lon$vals > extent_catch@xmin & 
                     ncdf_file$dim$lon$vals < extent_catch@xmax)
  LatIdx <- which( ncdf_file$dim$lat$vals >= extent_catch@ymin & 
                     ncdf_file$dim$lat$vals < extent_catch@ymax)
  
  # read only the part we are intrested
  cropped2 = list()
  cropped2$x = ncvar_get(ncdf_file, "lon", start=c( LonIdx[1]), count=c(length(LonIdx)))
  cropped2$y = ncvar_get(ncdf_file, "lat", start=c(LatIdx[1]), count=c(length(LatIdx)))
  
  
  r<-list()
  
  for(i in 1:12){
    cropped2$z =  ncvar_get(ncdf_file, "WTD", 
                            start=c(LonIdx[1],LatIdx[1], i), 
                            count=c(length(LonIdx),length(LatIdx), 1))
    
    r[i]<-raster(cropped2)
  }
  # create layer stack with time dimension
  cropped2.r<-stack(r)
  
  #monthly[i] <-  raster::extract(x=cropped2.r, y=shape_file, fun = median)
  monthly <-exact_extract(x=cropped2.r, y=shape_file, fun = "median")
  
  data.frame(
    COMID = shape_file$COMID,
    gwt_jan = monthly[1],
    gwt_feb = monthly[2],
    gwt_mar = monthly[3],
    gwt_apr = monthly[4],
    gwt_may = monthly[5],
    gwt_jun = monthly[6],
    gwt_jul = monthly[7],
    gwt_aug = monthly[8],
    gwt_sep = monthly[9],
    gwt_oct = monthly[10],
    gwt_nov = monthly[11],
    gwt_dec = monthly[12]
  )

}



tic()  
f_out <- get_avg_ncdf(ncdf_file =africa_gw, shape_file= africa[1:2,])
toc()

tic()  
x <- foreach(
  i = 1:20, 
  .combine = 'rbind'
) %do% {
  get_avg_ncdf(ncdf_file =africa_gw, shape_file= africa[i,])
}
toc()

africa_sub <-africa[1:20,]

tic() 
dat <- get_avg_ncdf(ncdf_file =africa_gw, shape_file= africa_sub)
toc()


######
africa_gwstack <- stack(gw_files[1])
# https://gis.stackexchange.com/questions/386241/fastest-way-to-perform-focal-operations-using-r
index <- cellnumbers(raster::subset(africa_gwstack, 1), africa_sub)

head(index)

tic()
aps <- index %>% 
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

africa_df <- aps %>% 
  mutate(COMID = africa$COMID[object_] ) %>% 
  drop_na(COMID) %>% 
  dplyr::select(COMID, gw_jan:gw_dec)
  


