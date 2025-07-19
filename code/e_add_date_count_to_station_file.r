################################################################################
#  Summarize date count by site.
#
#  A variable is added to the station file created in ArcGIS with subgroups,
#  that contains a count of field dates for each site.
#
#  Data frame 'ecy_meas_qa' must be in session memory, having been previously
#  created with script 'b_compile_annual_netCDF.r'.
#
#  July 2024
################################################################################

library(tidyverse)


# open station file exported from ArcGIS
filepath <- str_c("output_tables","ecy_stations_subgroups_tbl.csv", sep="/")
ecy_stations_in <- read.csv(filepath, stringsAsFactors=FALSE)

# filter for valid records
ecy_var <- ecy_meas_qa %>%
    select(Temp, TempQC, Station, date) %>%
    filter(is.numeric(Temp))

# summarize to get count of unique date records by station
# first, create list of unique station dates
ecy_var_stn_dates <- ecy_var %>%
    group_by(Station, date) %>%
    summarize(dummy=1)
# second, summarize count of records by station
ecy_stn_date_cnt <- ecy_var_stn_dates %>%
    group_by(Station) %>%
    summarize(Temp_date_count = n())

stations_cnt <- ecy_stations_in %>%
   left_join(ecy_stn_date_cnt, by="Station")

# write out amended station table
write.csv(stations_cnt, file="output_tables/ecy_stations_subgroups_cnt_tbl.csv")

rm(filepath, ecy_stations_in, ecy_var, ecy_var_stn_dates)
rm(ecy_stn_date_cnt, stations_cnt)