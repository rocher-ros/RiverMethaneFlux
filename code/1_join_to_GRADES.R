# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'downloader', 'RCurl', 'sf', 'XML')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)



#### Download GRADES river network from the server

dir.create("data/raw/grades")

url = "http://hydrology.princeton.edu/data/mpan/MERIT_Basins/MERIT_Hydro_v07_Basins_v01_bugfix1/pfaf_level_01/"
#check that it is the right folder
browseURL(url)

#get the urls of the files
files <- getHTMLLinks(url)

files <- files[grepl("^riv.", files)]


#### Download GRADES river network to the local file. it takes over 2h. 
if(all(file.exists(paste("data/raw/grades", files, sep="/"))) == TRUE){
  print("files already downloaded") 
  } else {
      #download them in a loop
      for(i in 1:length(files)){
        download.file(url = paste(url, files[i], sep = "/"), 
                 destfile=paste("data/raw/grades", files[i], sep="/"), mode="wb")
        # this took me a day to figure out. downloading the files with the default 
        # mode="w" was corrupting the shapefiles! "wb" writes it in binary
      }
    }


#get the files than end in .shp
shape_files <- paste("data/raw/grades", files[grepl(".shp$", files)], sep="/") 


system.file(shape_files[15], package="sf")

site <- read_sf("data/raw/grades/riv_pfaf_9_MERIT_Hydro_v07_Basins_v01_bugfix1.shp")




