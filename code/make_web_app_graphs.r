################################################################################
#  Create graphs for use in project web app for exploring the Ecology marine
#  water monitoring dataset. Three sets of graphs are created:
#    - Create calendars of data availability for each measured variable.
#    - Create color profiles for each variable at each station.
#    - Create seasonal graphs of 14 variables in the dataset for the 25 stations
#      that meet the minimum criteria for number of site visits/profiles.
#
#  The data frame 'ecy_meas_qa' must exist in the R workspace. The source file
#  assemble_netCDF_data.r can be run beforehand to create this data frame.
#
#  July 2024
#
################################################################################

library(tidyverse)



# loop through variables to prep data and create graphs
for (var_index in seq_along(varList)) {
 
  print(sprintf("Processing variable %s",varList[var_index])) 
   
  # Create data calendar for this variable. Data filtered by QA flag and
  # location within study area
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








 