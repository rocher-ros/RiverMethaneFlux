# Load  and install packages ----
# List of all packages needed
package_list <- c('googledrive', 'purrr')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)



# Download data files from google drive ----

# Download the methane database
drive_download(
  "SCIENCE/PROJECTS/RiverMethaneFlux/methane/MethDB_tables_converted.rda",
  path = "data/raw/MethDB_tables_converted.rda",
  overwrite = TRUE
)

# download the grade attributes to the local copy
grades_in_drive <- drive_ls("SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes")

#get the file paths for drive and locally
names_in_drive <- paste("SCIENCE/PROJECTS/RiverMethaneFlux/gis/GRADES flowline attributes", grades_in_drive$name, sep="/") 
names_destination <- paste("data/raw/gis/GRADES_attributes", grades_in_drive$name, sep="/") 

#feed them through a map
map2(names_in_drive, names_destination, drive_download)



