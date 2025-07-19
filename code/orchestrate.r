################################################################################
#  Orchestration script that calls/sources other code files to:
#    - Read annual netCDF data files downloaded from Ecology with marine water
#      monitoring data and compile.
#    - Integrate separate data for NRR001 (Tacoma Narrows) that was downloaded
#      from Ecology ~2018 but is missing from both the currently available
#      netCDF data and data available in EIM.
#    - Create calendars of data availability for each measured variable.
#    - Create color profiles for each variable at each station.
#    - Create seasonal graphs of 14 variables in the dataset for the 25 stations
#      that meet the minimum criteria for number of site visits/profiles.
#
#  Note that the first run of this script had a 16 minute run time.
#
#  July 2024
#
################################################################################

library(tidyverse)
source("code/definitions.r")



# Read annual netCDF data files and consolidate. This code creates these objects
# in current session workspace:
#   ecy_meas_qa = the consolidated data
#   ecy_stn_tbl = table of stations compiled from annual station tables
#   var_metadata = variables with units, missing value and short description
#   varList = list of variables
#   qaList = list of qa variables (same sequence as varList)
#   sepsym = symbol used in path for current system
# var_metadata is also written to csv files.
print("Compiling data...")
source("code/b_compile_annual_netCDF.r")


#  Integrate data from station NRR001 (Tacoma Narrows) that was downloaded in
#  2018 but is not currently distributed either in the updated netCDF data
#  or the out-of-date data from EIM.  
#  (1) The NRR records are converted to match the structure of ecy_meas_qa and 
#      added to ecy_meas_qa.
#  (2) An NRR001 station record is added to the copy of ecy_stn_tbl that exists 
#      in the current R workspace.
print("Integrating NRR001...")
source("code/get_NRR_data.r")

# Make station-variable table as basis for point layer with points at each stn
# for each var
print("Making stn-var table...")
source("code/make_stn_var_tbl.r")

# Add temperature data count to station table - both in the workspace and the
# version saved to csv.
print("Adding temperature data counts...")
source("code/e_add_date_count_to_station_file.r")


# loop through variables to prep data and create graphs
for (var_index in seq_along(varList)) {
 
  print(sprintf("Processing variable %s",varList[var_index])) 
   
  # Create data calendar for this variable.
  print(sprintf("   Creating calendar..."))
  source("code/d_calendar_graph.r")

  # Create color profiles
  print(sprintf("   Creating color profiles..."))
  source("code/color_prof_histo_by_depth.r")
  
   
  # Source code that preps data for graphing for this variable:
  #  - joins measurement variable with associated qa variable
  #  - joins station attributes and filters for HSIL stations only (drops
  #    outer coast and Hood Canal)
  #  - create freq. histograms of data by depth and stations by no. site visits
  #  - loops through HSIL stations to insert NA records for missing months
  #  - reduces depth profiles in 3 ways:  0.5m obs, ave of 0.5 to 5m, 
  #    difference between 0.5 and 5 m obs
  #  - resulting data in current session workspace is named ecy_var_graphing
  print(sprintf("   Preparing seasonal graph data..."))
  source("code/f_seasonal_graphs_data_prep.r")
  
  # Loops through stations to make seasonal graphs for given variable 
  # (var_index) for the 3 methods of reducing the depth profile to single value
  print(sprintf("   Creating seasonal graphs..."))
  source("code/g_seasonal_graphs.r") 
   
}








 