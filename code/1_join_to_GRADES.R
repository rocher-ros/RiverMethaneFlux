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

url = "http://hydrology.princeton.edu/data/mpan/GRADES/MERIT_Hydro_v00_Basins_v01/level_01/"
#check that it is the right folder
browseURL(url)

#get the urls of the files
files <- getHTMLLinks(url)

files <- files[grepl("^pfaf.", files)]


#### Download GRADES river network to the local file. it takes over 1h. 
if(file.exists(paste("data/raw/grades", files, sep="/")) == TRUE){
  print("files already downloaded")
  }
  else
    {
      #download them in a loop
      for(i in 1:length(files)){
        download(url = paste(url, files[i], sep = "/"), 
                 destfile=paste("data/raw/grades", files[i], sep="/"))
      }
    }


#get the files than end in .shp
shape_files <- files[grepl(".shp$", files)] 


site <- here::here(paste("data/raw/grades", shape_files[11], sep="/")) %>% 
  st_read()




