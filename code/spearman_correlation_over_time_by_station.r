###############################################################################
#  Calculate Spearman correlation and significance for each variable in the
#  Ecology dataset against time.
#
#  Required input:  the data frame ecy_meas_qa must be present in the workspace.
#  This data frame is created in b_compile_annual_netCDF.r
#  
#  August 2025
#
###############################################################################

library(tidyverse)



# get enhanced station table exported from GIS with subgroup attribute that can 
# be used to filter stations in the HSIL study area 
stn_tbl_path <- str_c("output_tables", "ecy_stations_subgroups_tbl.csv", sep=sepsym)
stations <- read.csv(stn_tbl_path, stringsAsFactors=FALSE)




# pivot longer so variables are no longer in separate columns
long_good_data_recs <- ecy_meas_qa %>%
    pivot_longer(PO4:Temp, names_to = "parameter", values_to = "value") %>%
    select(Depth,obs_index,station_index,Station,date,parameter,value) %>%
    drop_na(value)