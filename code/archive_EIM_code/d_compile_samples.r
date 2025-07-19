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
#  from a series of separate downloads from the EIM system. These folders can
#  have any name (e.g., assigned by EIM at Zip creation) except "Zip_downloads" 
#  which is reserved for a folder containing the downloaded Zip archives which 
#  were previously uncompressed to create the input data for this script.
#  
#  June 2024,  PD
#
################################################################################

library(tidyverse)

print("Running...")

source_dir <- "C:\\Users\\pdow490\\Documents\\HSIL_local\\Ecology_data"

# get contents 
data_dirs <- list.files(source_dir)
# remove "Zip_downloads" from the list so will be ignored
data_dirs <- data_dirs[! data_dirs %in% "Zip_downloads"]

# loop through each subdirectory to access profile data files
index = 1
for (idir in data_dirs) {
  ipath <- str_c(source_dir, idir, sep="/")
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

# select data columns of interest 
ecy_data <- ecy_data_combined %>%
       select(Location_ID, Location_Description, Instrument_ID, 
              Field_Collection_Date, Depth_Value, Result_Parameter_Name,
              Result_Value, Result_Value_Units) %>%
       rename(Depth_Value_m = Depth_Value)

# write to file
outpath <- str_c(source_dir, "ecy_data.csv", sep="/")
write.csv(ecy_data, outpath)




