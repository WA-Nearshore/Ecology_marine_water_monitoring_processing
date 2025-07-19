################################################################################
# Unzip all Zip files in a folder
#
# **** This script ultimately not used due to the size of the Zip files
# **** involved (>100MB). This made manual unzipping from a Citrix server much
# **** more efficient.
#
# **** But the large file sizes led to moving all data files to local disk, 
# **** where this script could have been used.
#
# May 2024
################################################################################

library(tidyverse)

target_folder <- 'Ecology_data'
source_folder <- 'Ecology_data/Zip_downloads'

zipList <- list.files(source_folder)

for (iZip in zipList) {
  iZipPath <- str_c(source_folder,iZip, sep="/") 
  print(iZipPath)
  unzip(iZipPath, exdir=target_folder)
}