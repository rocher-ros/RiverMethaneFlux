# Info ------
# Author: Gerard Rocher-Ros
# Script upscale CH4 emissions in rivers globally.


# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' , 'sf',  'ggpubr')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)
# 1. Load files ----

#upscaled methane concentrations
meth_concs <- read_csv("data/processed/meth_predictions.csv")

#load the grades polygons
files <- list.files("data/raw/grades")[grepl(".shp$", list.files("data/raw/grades"))]

grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_lat_lon.csv")


