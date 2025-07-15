################################################################################
#
# make_stn_var_table
# 
# This code creates a table with a record for each station-variable combination.
#
# This table can be used in an app to create a station map that can be filtered 
# to show only stations that have data for a specified variable.
#
# The following data frames must be available in the current working R session.
# Both are created in b_compile_annual_netCDF.r:
#   ecy_meas_qa = Ecology dataset
#   ecy_stn_tbl = list of Ecology stations (incl. NRR001) with lat/lon
#  
#
# September 2024
#
################################################################################

library(tidyverse)


# Filter out NA and missing value and QA fail records by variable.
varCol <- 1
qaCol <- 15
for (ivar in seq(1,14)) {
   ecy_meas_qa[,varCol] <- ifelse(ecy_meas_qa[,qaCol] != 2 |
                                  ecy_meas_qa[,varCol] < -99000 |
                                  !is.numeric(ecy_meas_qa[,varCol]),
                                  NA, ecy_meas_qa[,varCol])
   varCol <- varCol + 1
   qaCol <- qaCol + 1
}

# pivot longer so variables are no longer in separate columns
long_good_data_recs <- ecy_meas_qa %>%
    pivot_longer(PO4:Temp, names_to = "parameter", values_to = "value") %>%
    select(Depth,obs_index,station_index,Station,date,parameter,value) %>%
    drop_na(value)

# summarize to unique station-variable records
stn_var_recs <- long_good_data_recs %>%
    group_by(Station, parameter) %>%
    summarize(count = n(), .groups="drop_last")

# join lat/long for use in GIS from ecy_stn_tbl which must be in workspace
stn_var_tbl <- stn_var_recs %>%
   left_join(ecy_stn_tbl, by="Station", keep=FALSE)

# Join regional subgroup from station table exported from ArcGIS
# open station file exported from ArcGIS, with regional subgroups
filepath <- str_c("output_tables","ecy_stations_subgroups_tbl.csv", sep="/")
ecy_stations_in <- read.csv(filepath, stringsAsFactors=FALSE)
# selection only key (Station) and required attribute (subgroup)
ecy_stations_in_sel <- ecy_stations_in %>% select(Station,subgroup)
# join onto our station table
stn_var_tbl_jn <- stn_var_tbl %>%
   left_join(ecy_stations_in_sel, by="Station")



write.csv(stn_var_tbl_jn, file="output_tables/ecy_stn_var_tbl.csv",row.names=FALSE)

rm(varCol,qaCol,long_good_data_recs,stn_var_recs,ivar)
rm(filepath, ecy_stations_in, ecy_stations_in_sel, stn_var_tbl)

