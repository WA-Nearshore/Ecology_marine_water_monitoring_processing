################################################################################
#
#  Compile Ecology marine monitoring profiles results into one data file.
#
#  This script compiles all profile results data files in a given location 
#  as downloaded from Dept of Ecology's EIM data system in May 2004.
#
#  These are data collected by Ecology's Marine Water Monitoring Program.
#
#  This script assumes the input data files are spread across multiple folders
#  from a series of separate downloads from the EIM system. A typical home 
#  directory for this script (specified by variable home_dir) would look like:
# 
#   downloads/
#      files_download1/
#          profile_data1.csv
#          profile_data2.csv
#          location_data.csv
#      files_download2/
#      files_download3/
#   Zip_downloads
#      download1.zip
#      download2.zip
#      download3.zip
#  
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
  ipath <- str_c(input_dir, idir, sep="/")
  fileList <- list.files(ipath)
  # get list of profile data files in subdirectory idir
  profileList <- fileList[str_detect(fileList,"MarineAmbientProfileResults")]
  
  # initialize a vector of data frames to hold data from up to 10 input files
  # in this idir subdirectory (5 was max num. files in May2024 downloads)
  inDataFrames <- vector("list", length=10)
  for (ifile in profileList) {
    jpath <- str_c(ipath, ifile, sep="/")
    inDataFrames[[index]] <- read.csv(jpath, stringsAsFactors=FALSE)
    
    print(sprintf("Directory: %s;  File: %s", idir, ifile))
  }
  # combine all data files for this directory (idir)
  dir_data_combined <- bind_rows(inDataFrames)
 
  # for first iteration, set to final data frame, otherwise append
  if (index == 1) {
    ecy_data_combined <- dir_data_combined
  } else {
    ecy_data_combined <- bind_rows(ecy_data_combined, dir_data_combined)
  }
  index <- index + 1
}

# select data columns of interest, rename depth to include units (meters)
# and convert date from string to Date format and rename
ecy_profile_data <- ecy_data_combined %>%
       select(Location_ID, Location_Description, Instrument_ID, 
              Field_Collection_Date, Depth_Value, Result_Parameter_Name,
              Result_Value, Result_Value_Units) %>%
       rename(Depth_Value_m = Depth_Value) %>%
       mutate(date = mdy(Field_Collection_Date)) %>%
       select(-Field_Collection_Date)

# write to file
print(sprintf("Writing...\n"))
outpath <- str_c(home_dir, "ecy_profile_data.csv", sep="/")
write.csv(ecy_profile_data, outpath)

# clean up
rm(data_dirs, dir_data_combined, ecy_data_combined, fileList, idir)
rm(ifile,inDataFrames, index, ipath, jpath, outpath, profileList, home_dir)
rm(input_dir)





