# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'RCurl', 'sf', 'XML', 'countrycode', 'leaflet', 'googledrive', 'lwgeom')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)



# Download GRADES river network from the server ----

## Prepare the things for the download ----
dir.create("data/raw/grades")

url = "http://hydrology.princeton.edu/data/mpan/MERIT_Basins/MERIT_Hydro_v07_Basins_v01_bugfix1/pfaf_level_01/"
#check that it is the right folder
browseURL(url)

#get the urls of the files
files <- getHTMLLinks(url)

files <- files[grepl("^cat|^riv.", files)]


## Download GRADES river network to the local file. it takes over 2h.----
if(all(file.exists(paste("data/raw/grades", files, sep="/"))) == TRUE){
  print("files already downloaded") 
} else {
  
  #find which files are missing
  to_download <- which(file.exists(paste("data/raw/grades", files, sep="/")) == FALSE)
  #download them in a loop
  for(i in to_download){
    download.file(url = paste(url, files[i], sep = "/"), 
                  destfile=paste("data/raw/grades", files[i], sep="/"), mode="wb")
    # this took me a day to figure out. downloading the files with the default 
    # mode="w" was corrupting the shapefiles! "wb" writes it in binary
  }
  alarm()
}


# Process the files, PFAF area by PFAF area ----

## Prepare the files for processing ----

# Download the methane database
drive_download(
  "SCIENCE/PROJECTS/RiverMethaneFlux/methane/MethDB_tables_converted.rda",
  path = "data/raw/MethDB_tables_converted.rda",
  overwrite = TRUE
)
#load the methane DB 
load(file.path("data", "raw", "MethDB_tables_converted.rda"))

#select the sites, and make a new column to get the continent name. This will be the way to break the processing in pieces
sites_meth <- gis_df %>% 
  dplyr::select(Site_Nid, lat=lat_new, lon=lon_new, elevation_m =z_m_combined, country=countries_sub) %>% 
  mutate(continent =  countrycode(sourcevar = country,
                                  origin = "country.name",
                                  destination = "continent"),
         #this needed some manual fixing in some cases
         continent = case_when(lat > 45 & lon > 60 ~ "Asia",
                               lat < 4.7 & lon > 108 ~ "Oceania",
                               Site_Nid %in% c(2405, 9299, 9303) ~ "Europe",
                               Site_Nid %in% 12164 ~ "Asia",
                               Site_Nid %in% 7428 ~ "Americas",
                               Site_Nid %in% 8581 ~ "Africa",
                               TRUE ~ continent)) %>% 
  st_as_sf( coords = c("lon", "lat"), crs = 4326)

#check if there are some without continent
sites_meth %>% filter(is.na(continent) == TRUE) 


#get the files than end in .shp, and separate them in catchments and networks
files <- list.files("data/raw/grades")[grepl(".shp$", list.files("data/raw/grades"))]

shape_files <- paste("data/raw/grades", files[grepl(".shp$", files)], sep="/") 

shapes_rivers <- shape_files[grepl("riv", shape_files)]

shapes_catchments <- shape_files[grepl("cat", shape_files)]

#check the file sizes, in MB
file.size(shapes_catchments)/1e+6

#The shp +dbf will go over the RAM capacity, we will need to do it in parts


## Start with #1, which is Africa ----
# read the GRADES shapefile 
africa <- read_sf(shapes_rivers[1]) %>% st_set_crs(4326)

sites_in_africa <- sites_meth %>% filter(continent == "Africa")



#to double check, plot 1000 of the sites randomly selected
sample_n(africa, 1000) %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data=sites_in_africa, color="red")

#find the nearest GRADES network
nearest <- st_nearest_feature(sites_in_africa, africa)
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

#get the distance that has been snapped as a QAQC
distance_snapped <- st_distance(sites_in_africa, africa[nearest,], by_element = TRUE) %>% as.vector()

summary(distance_snapped)

#g et the snapped COMID and ancillary variables 
sites_in_africa <- sites_in_africa %>% 
  mutate(COMID = africa[nearest,]$COMID,
         lengthkm = africa[nearest,]$lengthkm,
         sinuosity = africa[nearest,]$sinuosity,
         slope_grades = africa[nearest,]$slope,
         uparea = africa[nearest,]$uparea,
         order = africa[nearest,]$order,
         NextDownID = africa[nearest,]$NextDownID, 
         distance_snapped = distance_snapped) 



#launch a leaflet with the sites, to see how it went
#there is a measure tool (top right), sou you can look at coordinates, distance and area if needed
leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolylines(data= africa[nearest,], label = ~COMID,
               color= "blue") %>% 
  addCircles(data= sites_in_africa, label = ~Site_Nid,
             color= "red") %>% 
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")

rm(africa)

## #2, is Europe ----
# read the GRADES shapefile 
europe <- read_sf(shapes_rivers[2]) %>% st_set_crs(4326)


sites_in_europe <- sites_meth %>% filter(continent %in% c("Europe"))

#to double check, plot 1000 of the sites randomly selected
sample_n(europe, 2000) %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data=sites_meth, color="blue")+
  geom_sf(data=sites_in_europe, color="red")

#find the nearest GRADES network
nearest <- st_nearest_feature(sites_in_europe, europe)
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

#get the distance that has been snapped as a QAQC
distance_snapped <- st_distance(sites_in_europe, europe[nearest,], by_element = TRUE) %>% as.vector()

summary(distance_snapped)

#get the snapped COMID and ancillary variables 
sites_in_europe <- sites_in_europe %>% 
  mutate(COMID = europe[nearest,]$COMID,
         lengthkm = europe[nearest,]$lengthkm,
         sinuosity = europe[nearest,]$sinuosity,
         slope_grades = europe[nearest,]$slope,
         uparea = europe[nearest,]$uparea,
         order = europe[nearest,]$order,
         NextDownID = europe[nearest,]$NextDownID, 
         distance_snapped = distance_snapped) 

#launch a leaflet with the sites, to see how it went
#there is a measure tool (top right), sou you can look at coordinates, distance and area if needed
leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolylines(data= europe[nearest,],#europe %>%st_crop(xmin=10, xmax=20, ymin=55, ymax=59), 
               label = ~COMID,
               color= "blue") %>% 
  addCircles(data= sites_in_europe, label = ~Site_Nid,
             color= "red") %>% 
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")

rm(europe)

## #3 is Asia ----
# read the GRADES shapefile 
asia_north <- read_sf(shapes_rivers[3]) %>% st_set_crs(4326)
asia_south <- read_sf(shapes_rivers[4]) %>% st_set_crs(4326)

asia <- rbind(asia_north, asia_south)
rm(asia_north, asia_south)

sites_in_asia <- sites_meth %>% filter(continent %in% "Asia")

#to double check, plot 1000 of the sites randomly selected
sample_n(asia, 2000) %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data=sites_meth, color="blue")+
  geom_sf(data=sites_in_asia, color="red")

#find the nearest GRADES network
nearest <- st_nearest_feature(sites_in_asia, asia)
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

#get the distance that has been snapped as a QAQC
distance_snapped <- st_distance(sites_in_asia, asia[nearest,], by_element = TRUE) %>% as.vector()

summary(distance_snapped)

#get the snapped COMID and ancillary variables 
sites_in_asia <- sites_in_asia %>% 
  mutate(COMID = asia[nearest,]$COMID,
         lengthkm = asia[nearest,]$lengthkm,
         sinuosity = asia[nearest,]$sinuosity,
         slope_grades = asia[nearest,]$slope,
         uparea = asia[nearest,]$uparea,
         order = asia[nearest,]$order,
         NextDownID = asia[nearest,]$NextDownID, 
         distance_snapped = distance_snapped) 


#launch a leaflet with the sites, to see how it went
#there is a measure tool (top right), sou you can look at coordinates, distance and area if needed
leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolylines(data= asia[nearest,],#europe %>%st_crop(xmin=10, xmax=20, ymin=55, ymax=59), 
               label = ~COMID,
               color= "blue") %>% 
  addCircles(data= sites_in_asia, label = ~Site_Nid,
             color= "red") %>% 
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")

rm(asia)

## #4 is Oceania ----
# read the GRADES shapefile 
oceania <- read_sf(shapes_rivers[5]) %>% st_set_crs(4326)

sites_in_oceania <- sites_meth %>% filter(continent %in% "Oceania")

#to double check, plot 1000 of the sites randomly selected
sample_n(oceania, 2000) %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data=sites_meth, color="blue")+
  geom_sf(data=sites_in_oceania, color="red")

#find the nearest GRADES network
nearest <- st_nearest_feature(sites_in_oceania, oceania)
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

#get the distance that has been snapped as a QAQC
distance_snapped <- st_distance(sites_in_oceania, oceania[nearest,], by_element = TRUE) %>% as.vector()

summary(distance_snapped)

#get the snapped COMID and ancillary variables 
sites_in_oceania <- sites_in_oceania %>% 
  mutate(COMID = oceania[nearest,]$COMID,
         lengthkm = oceania[nearest,]$lengthkm,
         sinuosity = oceania[nearest,]$sinuosity,
         slope_grades = oceania[nearest,]$slope,
         uparea = oceania[nearest,]$uparea,
         order = oceania[nearest,]$order,
         NextDownID = oceania[nearest,]$NextDownID, 
         distance_snapped = distance_snapped) 


#launch a leaflet with the sites, to see how it went
#there is a measure tool (top right), sou you can look at coordinates, distance and area if needed
leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolylines(data= oceania[nearest,],#europe %>%st_crop(xmin=10, xmax=20, ymin=55, ymax=59), 
               label = ~COMID,
               color= "blue") %>% 
  addCircles(data= sites_in_oceania, label = ~Site_Nid,
             color= "red") %>% 
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")

rm(oceania)

## #5 is America ----
# read the GRADES shapefile 
south_america <- read_sf(shapes_rivers[6]) %>% st_set_crs(4326)
north_america <- read_sf(shapes_rivers[7]) %>% st_set_crs(4326)
north_america2 <- read_sf(shapes_rivers[8]) %>% st_set_crs(4326)
greenland <- read_sf(shapes_rivers[9]) %>% st_set_crs(4326)

america <- rbind(south_america, north_america, north_america2, greenland)
rm(south_america, north_america, north_america2, greenland)

sites_in_america <- sites_meth %>% filter(continent %in% "Americas")

#to double check, plot 1000 of the sites randomly selected
sample_n(america , 2000) %>% 
  ggplot()+
  geom_sf()+
  geom_sf(data=sites_meth, color="blue")+
  geom_sf(data=sites_in_america, color="red")

#find the nearest GRADES network
nearest <- st_nearest_feature(sites_in_america, america)
#blip when done
system("rundll32 user32.dll,MessageBeep -1")

#get the distance that has been snapped as a QAQC
distance_snapped <- st_distance(sites_in_america, america[nearest,], by_element = TRUE) %>% as.vector()

summary(distance_snapped)

#get the snapped COMID and ancillary variables 
sites_in_america <- sites_in_america %>% 
  mutate(COMID = america[nearest,]$COMID,
         lengthkm = america[nearest,]$lengthkm,
         sinuosity = america[nearest,]$sinuosity,
         slope_grades = america[nearest,]$slope,
         uparea = america[nearest,]$uparea,
         order = america[nearest,]$order,
         NextDownID = america[nearest,]$NextDownID, 
         distance_snapped = distance_snapped) 


#launch a leaflet with the sites, to see how it went
#there is a measure tool (top right), sou you can look at coordinates, distance and area if needed
leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addPolylines(data= america[nearest,],#europe %>%st_crop(xmin=10, xmax=20, ymin=55, ymax=59), 
               label = ~COMID,
               color= "blue") %>% 
  addCircles(data= sites_in_america, label = ~Site_Nid,
             color= "red") %>% 
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")

rm(america)


## Combine all files and write output ----
sites_meth_comid <- rbind(sites_in_africa, sites_in_america, sites_in_asia, sites_in_europe, sites_in_oceania) %>% 
  mutate(lat = sf::st_coordinates(.)[,1],
         lon = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

write_csv(sites_meth_comid, "data/processed/sites_meth_comid.csv")

# upload the csv file to google drive
  drive_upload(media = "data/processed/sites_meth_comid.csv" ,
               path = "SCIENCE/PROJECTS/RiverMethaneFlux/methane/sites_meth_comid.csv")

sites_meth_comid %>% 
  ggplot()+
  geom_histogram(aes(distance_snapped), bins=60)+
  facet_wrap(~continent, scales = "free")+
  theme_bw()


## last thing,  get point coordinates for all segments in grades 
files <- list.files("data/raw/grades")[grepl(".shp$", list.files("data/raw/grades"))]

shape_files <- paste("data/raw/grades", files[grepl(".shp$", files)], sep="/") 

shapes_rivers <- shape_files[grepl("riv", shape_files)]

list_shapes <- lapply(shapes_rivers, st_read)

grades <-  do.call(what = sf:::rbind.sf, args=list_shapes) %>% st_set_crs(4326)

grades_properties <- grades %>% 
  mutate(start_point = st_startpoint(.),
         lon = sf::st_coordinates(start_point)[,1],
         lat = sf::st_coordinates(start_point)[,2]) %>% 
  st_drop_geometry() %>% 
  select(COMID, lengthkm, slope, uparea, NextDownID, lat, lon)

write_csv(grades_properties, "data/raw/gis/GRADES_attributes/grades_lat_lon.csv")

# upload the csv file to google drive
drive_upload(media = "data/raw/gis/GRADES_attributes/grades_lat_lon.csv" ,
             path = "SCIENCE/PROJECTS/RiverMethaneFlux/gis/grades_coords.csv")
