################################################################################
#
#  Compile Ecology marine monitoring location info into one data file.
#
#  Input data must be in subfolder 'downloads' within home directory.
#
#  June 2024,  PD
#
################################################################################

library(tidyverse)

print("Running...")

home_dir <- "C:\\Users\\pdow490\\Documents\\HSIL_local\\Ecology_data"

input_dir <- str_c(home_dir, "downloads", sep="\\")

# get contents of downloads folder
data_dirs <- list.files(input_dir)

# loop through each subdirectory to access profile data files
index = 1
for (idir in data_dirs) {
  ipath <- str_c(input_dir, idir, sep="\\")
  fileList <- list.files(ipath)
  # get list of location files in subdirectory idir
  locationList <- fileList[str_detect(fileList,"MarineAmbientLocationDetails")]
  
  # write out warning if more than one location file
  if (length(locationList)>1) {
    print("WARNING - multiple location files.")
  }

  jpath <- str_c(input_dir, idir, locationList, sep="\\")
  dir_data <- read.csv(jpath, stringsAsFactors=FALSE)

  print(sprintf("Directory: %s;  File: %s", idir, locationList))
    
  # for first iteration, set to final data frame, otherwise append
  if (index == 1) {
    ecy_data_combined <- dir_data
  } else {
    ecy_data_combined <- bind_rows(ecy_data_combined, dir_data)
  }
  index <- index + 1
}

# select data columns of interest, rename lat/lon columns.
ecy_location_info <- ecy_data_combined %>%
    select(Location_Name, Location_Description,
           Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           Calculated_Longitude_Decimal_Degrees_NAD83HARN) %>%
    rename(lat = Calculated_Latitude_Decimal_Degrees_NAD83HARN,
           lon = Calculated_Longitude_Decimal_Degrees_NAD83HARN)

# write to file
print(sprintf("Writing...\n"))
outpath <- str_c(home_dir, "ecy_location_info.csv", sep="\\")
write.csv(ecy_data_combined, outpath)

#  clean up
rm(data_dirs, dir_data,fileList,home_dir,idir,index,input_dir,ipath,jpath)
rm(locationList,outpath,ecy_data_combined)


