################################################################################
#  Prep Ecology data for seasonal graphs 
#
#  This code extracts a single measurement variable (and associated qa variable)
#  from the compiled Ecology marine water monitoring data.
#  A table is created for creating seasonal graphs for the station specified
#  by variable var_index defined in orchestrate.r 
#  
#  The code below is intended to be called by orchestrate.r, but it can be used
#  by itself. In either case, the following objects must exist in the
#  current working session:
#     ecy_meas_qa = the aggregated Ecology marine water monitoring data created
#                   in b_combile_annual_netCDF.r
#     var_index = index of selected variable (temp, salinity, NO3, ...) within
#                 vectors varList and qaList. var_index is defined in 
#                 orchestration.r 
#     varList = vector of measurement variable names in the Ecology dataset, 
#               defined in b_compile_annual_netCDF.r, e.g. "NO3", "Temp", 
#               "Salinity"
#     qaList = vector of of qa variables (e.g. "NO3_QC", "TempQC") that take 
#              coded values for {none | pass | fill} in order that
#              matches order in varList. Defined in orchestration.r
# 
#  The output from this script is data frame 'ecy_var_graphing' which is
#  available in the active R session workspace.
#  
#  July 2024
################################################################################

library(lubridate)
source("code/prof_reduce_funcs.r")


############## set parameters

# set min. date count for a station to be included in graphing. This is the
# min. number of days visited over the Ecology data record where the designated
# variable was measured. Divide by 12 for rough number of years.
min_date_count <- 1



##############  start processing


################################################################################
#  prep the data - take Ecology data frame created previously and select
#  columns relevant for the specified variable, filter for HSIL stations.
################################################################################

# filter for valid records only of selected variable
ecy_var <- ecy_meas_qa %>%
    select(varList[var_index], qaList[var_index], Station, date, Depth)
# makes var names generic to generalize code across variables
names(ecy_var) <- c("Var","VarQC","Station","date","Depth")
# filter for numeric values with passing QC (2)
ecy_var <- ecy_var %>%  filter(is.numeric(Var), VarQC==2)
  
# get enhanced station table exported from GIS with subgroup attribute and use
# to filter for HSIL study area sites only
stn_tbl_path <- str_c("output_tables", "ecy_stations_subgroups_tbl.csv", sep=sepsym)
stations <- read.csv(stn_tbl_path, stringsAsFactors=FALSE)

ecy_var_jn <- ecy_var %>%
    left_join(stations, by="Station") %>%
    filter(subgroup == "HSIL_study_area") %>%
    select(-station_number, -Longitude, -Latitude, Temp_date_count)


################################################################################
# create histograms to better understand the data: overall data frequency by
# depth and frequency of stations by amount of data (as measured by number of
# dates sampled).
################################################################################
# create histogram of data frequency by depth
# phist <- ggplot(data = ecy_var_jn,
#                 mapping = aes(x=Depth)) +
#          geom_histogram() +
#          theme_bw() +
#          scale_x_continuous(name="Depth (m)")

# create histogram of station frequency by date count
hsil_stns <- stations %>% filter(subgroup == "HSIL_study_area")
# phist2 <- ggplot(data = hsil_stns,
#                  mapping = aes(x=date_count)) + 
#           geom_histogram() +
#           theme_bw()



#############################################################################
# Create data for seasonal graphs with records for all sequential months
# between data record start and stop for each station. No-data months are NA
# so that ggplot function geom_path will leave gaps in seasonal curves.
#############################################################################

# Check if each sample date is unique within a given month (i.e. no cases of
# two dates - at month start and month end, with an adjacent month with no dates).
# First, reduce multiple data records on same date (e.g. across depth) to a 
# single date record (stn_dates).
stn_dates <- ecy_var_jn %>%
   group_by(Station, date) %>%
   summarize(, .groups="drop_last")
# Second, count how many station-date records there are by month (stn_yr_mon)
stn_yr_mon <- stn_dates %>%
   mutate(year = year(date), month = month(date)) %>%
   group_by(Station, year, month) %>%
   summarize(count=n(), .groups="drop_last") %>%
   arrange(desc(count))
   
# Data frame stn_yr_mon confirms there are cases with multiple station visits
# within a single month. This affects how we fit these records into a
# sequential-month set of records. Of 6357 records of station-year-month in the
# dataset, 35 of these had multiple data records in the same month.
# Examine data records for case with greatest number of visits in a month (5) 
# which was at EAP001 in July 2009.
#   Summary: ecy_var_jn   nrows=1042627
#            stn_dates    nrows=6405
#            stn_yr_mon   nrows=6357
#            stn_yr_mon_5 nrows=5
#            ecy_recs_5   nrows=1117
#    stn_yr_mon filtered for count>1  nrows=35
stn_yr_mon_5 <- stn_dates %>%
    mutate(year=year(date), month=month(date)) %>%
    filter(Station=="EAP001", year==2009, month==7)
ecy_recs_5 <- ecy_var_jn %>%
    mutate(year=year(date), month=month(date)) %>%
    filter(Station=="EAP001", year==2009, month==7)



# Based on the occurrence of multiple station visits within a month, the approach
# to creating graphing data will create consecutive-month NA records and filter
# for those records with no matching stn-yr-mon combo in the data, then append
# these records to the data.


# Loop through stations to create consecutive-month NA records that match the
# start and stop months of the station's data record.
# First filter stations - retain stations in station list and in the data that
#  have date count above minimum.
hsil_stns_filt <- hsil_stns %>% filter(Temp_date_count >= min_date_count)
ecy_var_jn_cntFilt <- ecy_var_jn %>% filter(Temp_date_count >= min_date_count)
# Create data frame to append NA records to
recsNA <- data.frame(Station=character(), year=integer(), month=integer(),
                     date=Date(), Var0.5=numeric(), Var05to5=numeric(),
                     VarDiff=numeric())

for (istn in hsil_stns_filt$Station) {
 
  # get first and last dates
  stn_data <- ecy_var_jn_cntFilt %>% filter(Station == istn)
  first <- min(stn_data$date, na.rm=TRUE)
  last <- max(stn_data$date, na.rm=TRUE)
  
  first_yr <- year(first)
  last_yr <- year(last)

  # only proceed if this station has data for this variable
  if (!is.na(first_yr) & !is.na(last_yr)) {
    # loop through years and create consecutive-month NA data frame 
    months <- seq(1,12,by=1)
    for (iyr in seq(first_yr, last_yr, by=1)) {
      stn_yr_recs <- data.frame(Station=istn, year=iyr, month=months,
                                date=ymd(paste(iyr,months,15,sep=" ")),
                                Var0.5=as.numeric(NA), Var05to5=as.numeric(NA), 
                                VarDiff=as.numeric(NA))
      recsNA <- rbind(recsNA, stn_yr_recs)
    } 
  }
}   # close for loop through stations to create consecutive-month NA records



# Reduce the data to match structure of the consecutive-month NA data frame. 
# Summarize multi-depth profiles to single values using three alternative:
#   1) value at 0.5 depth; 
#   2) mean value over 0.5-5m depth band;
#   3) difference in values between 0.5 and 5m depths
ecy_var_depth_collapse <- ecy_var_jn_cntFilt %>%
    mutate(year=year(date), month=month(date), day = day(date)) %>%
    group_by(Station, year, month, day) %>%
    summarize(Var0.5 = fdepth05(Var, Depth),
              Var05to5 = fdepth05to5(Var, Depth),
              VarDiff = fdepthDiff(Var, Depth),
              .groups="drop_last")

# add date from year, month and day columns, then delete day
ecy_var_depth_collapse <- ecy_var_depth_collapse %>%
   mutate(date = ymd(paste(year,month,day,sep=" "))) %>%
   select(-day)

# Now the depth-summarized data (ecy_var_depth_collapse) and the
# consecutive-month NA df (recsNA) have same structure

# filter recsNA for NA records with no matching stn-yr-mon record in data
recsNAfilt <- recsNA %>% 
   anti_join(ecy_var_depth_collapse, by=c("Station", "year", "month"))

# append the filtered NA records to the data, drop unneeded columns
ecy_var_graphing <- rbind(ecy_var_depth_collapse, recsNAfilt)
# ecy_var_graphing <- ecy_var_graphing %>%
#     select(-subgroup, -date_count)



# clean up
rm(ecy_var_jn_cntFilt, first, first_yr, istn,iyr,last, last_yr, min_date_count)
rm(months, recsNA, recsNAfilt, stn_tbl_path)
rm(ecy_var_depth_collapse)
rm(ecy_var, ecy_var_jn, hsil_stns, stn_dates, stn_yr_mon)
rm(hsil_stns_filt, stn_data)
