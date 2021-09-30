# Load  and install packages ----
# List of all packages needed
package_list <- c('googledrive')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)



# Download data files from goole drive ----

# Download the methane database
drive_download(
  "SCIENCE/PROJECTS/RiverMethaneFlux/methane/MethDB_tables_converted.rda",
  path = "data/raw/MethDB_tables_converted.rda",
  overwrite = TRUE
)

# We should add the other files here for the modelling


