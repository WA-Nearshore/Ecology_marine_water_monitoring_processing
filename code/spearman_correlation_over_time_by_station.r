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
library(psych)


########  functions #########
get_spearman <- function (data_frame) {
  spearman_out <- corr_test(data.frame$time, data.frame$value,
                            method="spearman")
  spearman_stats <- data.frame(r = spearman_out$r,
                               p = spearman_out$p)
  return(spearman_stats)
}
date_to_numeric <- function(date) {
  reference_date <- ymd("1995-01-01")
  daysdiff <- date - reference_date
  numeric_days <- as.numeric(daysdiff, units="days")
  return(numeric_days)
}


# get enhanced station table exported from GIS with subgroup attribute that can 
# be used to filter stations in the HSIL study area 
stn_tbl_path <- str_c("output_tables", "ecy_stations_subgroups_tbl.csv", sep=sepsym)
stations <- read.csv(stn_tbl_path, stringsAsFactors=FALSE)
ecy_meas_qa_filt <- ecy_meas_qa %>%
  left_join(stations, by="Station") %>%
  filter(subgroup == "HSIL_study_area")

# loop through variables to filter values that don't pass QA
for (ivar in seq(1:14)) {
  ecy_meas_qa_filt[,ivar] <- ifelse(is.na(ecy_meas_qa_filt[,ivar+14]), NA,
                               ifelse(ecy_meas_qa_filt[,ivar+14] == 2,
                               ecy_meas_qa_filt[,ivar], NA))
}                           

# pivot longer so variables are no longer in separate columns
ecy_filt_long <- ecy_meas_qa %>%
    pivot_longer(PO4:Temp, names_to = "parameter", values_to = "value") %>%
    select(Depth,obs_index,station_index,Station,date,parameter,value) %>%
    drop_na(value)

# add time as day since 1970-01-01, Spearman requires numberic variable
reference_date <- ymd("1995-01-01")
ecy_filt_long_days <- ecy_filt_long %>%
#   mutate(ndays_time = map_int(date, date_to_numeric))
   mutate(ndays_time = as.numeric(date - reference_date, units="days"))

# group by station and parameter and get Spearman stats
spearman.out <- ecy_filt_long_days %>%
  group_by(Station, parameter) %>%
  mutate(spearman_r = (corr.test(ndays_time, value, method="spearman"))$r)



