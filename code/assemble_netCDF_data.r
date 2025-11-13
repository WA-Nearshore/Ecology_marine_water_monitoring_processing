################################################################################
#
#  Read and assemble Ecology marine water monitoring data.
#
#  This code file orchestrates the reading in and integration of marine water 
#  monitoring data from the Washington State Dept. of Ecology. The data files
#  are in netCDF format. The source netCDF files were available as of 2025 
#  at this URL:
#  https://ecology.wa.gov/Research-Data/Monitoring-assessment/Puget-Sound-and-marine-monitoring/Water-column-data
#  
#  After successful execution, the assembled data is available in the R
#  workspace, i.e. it is not written to file. The following data objects will
#  be available in the workspace:
#   ecy_meas_qa = data frame containing the consolidated data in wide form with
#                 14 variables in separate columns.
#   ecy_stn_tbl = data frame of stations compiled from annual station tables
#   var_metadata = variables with units, missing value and short description
#   varList = list of variables
#   qaList = list of qa variables (same sequence as varList)
# var_metadata and ecy_stn_tbl are also written to csv file.
#
# Dependencies:
#   code/b_compile_annual_netCDF.r
#   code/e_add_date_count_to_station_file.r
# 
#  July 2024. Initial development.
#  Nov 2025.  Final version at project close.
#
################################################################################

library(tidyverse)


# set paths -  intended to allow work with local data.
local_netCDF_home <- 'C:\\Users\\pdow490\\Documents\\HSIL_local\\Ecology_data\\downloads_netCDF'
home_local <- 'C:/Users/pdow490/Documents/HSIL_local/Ecology_data/'


# Read annual netCDF data files and consolidate. This code creates these objects
# in current session workspace:
print("Compiling data...")
source("code/b_compile_annual_netCDF.r")

# Add temperature data count to station table - both in the workspace and the
# version saved to csv.
print("Adding temperature data counts...")
source("code/e_add_date_count_to_station_file.r")


