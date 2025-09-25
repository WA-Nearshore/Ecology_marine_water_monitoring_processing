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

# set depth that defines bottom of layer (top=surface) for parameter averaging
mx_depth <- 10

# set min number of records for Spearman to be run on Station-parameters.
# Value of 5 results in 31 station-prm combos being removed out of 1011. 
min_n_Spearman <- 5



########  functions #########
get_spearman <- function (data_frame) {
  spearman_out <- corr_test(data.frame$time, data.frame$value, method="spearman")
  spearman_stats <- data.frame(r = spearman_out$r, p = spearman_out$p)
  return(spearman_stats)
}


# get enhanced station table exported from GIS with subgroup attribute that can 
# be used to filter stations in the HSIL study area 
stn_tbl_path <- str_c("output_tables", "ecy_stations_subgroups_tbl.csv", sep=sepsym)
stations <- read.csv(stn_tbl_path, stringsAsFactors=FALSE)
ecy_meas_qa_filt <- ecy_meas_qa %>% left_join(stations, by="Station") %>%
                                    filter(subgroup == "HSIL_study_area")

# loop through variables to swap out values that don't pass QA (set to NA)
for (ivar in seq(1:14)) {
  ecy_meas_qa_filt[,ivar] <- ifelse(is.na(ecy_meas_qa_filt[,ivar+14]), NA,
                               ifelse(ecy_meas_qa_filt[,ivar+14] == 2,
                               ecy_meas_qa_filt[,ivar], NA))
}                           

# pivot longer so variables are no longer in separate columns
ecy_filt_long <- ecy_meas_qa_filt %>%
    pivot_longer(PO4:Temp, names_to = "parameter", values_to = "value") %>%
    select(Depth,obs_index,Station,date,parameter,value) %>%
    drop_na(value)

# get mean values for all parameters within depth 
ecy_filt_long_mean_values <- ecy_filt_long %>%
  group_by(Station, parameter, date) %>%
  summarize(prm_mean_val = mean(value))
  
# get data counts by Station-parameter pairs so low n cases can be removed
# Use of corr.test below for Spearman gives error for n=1 cases.
station_prm_record_count <- ecy_filt_long_mean_values %>%
  group_by(Station,parameter) %>%
  summarize(rec_count = n()) %>%
  arrange(rec_count)

# join data counts back onto table of mean depth values and then filter 
ecy_filt_long_mean_jn <- ecy_filt_long_mean_values %>%
  left_join(station_prm_record_count, by=join_by(Station, parameter)) %>%
  filter(rec_count >= min_n_Spearman)

# add time as day since 1989-01-01, Spearman requires numeric variable
reference_date <- ymd("1989-01-01")
ecy_filt_long_mean_jn_days <- ecy_filt_long_mean_jn %>%
   mutate(ndays_time = as.numeric(date - reference_date, units="days"))


# group by station and parameter and get Spearman stats
spearman.out <- ecy_filt_long_mean_jn_days %>%
  group_by(Station, parameter) %>%
  summarize(spearman_r = (corr.test(ndays_time, prm_mean_val, method="spearman"))$r,
            spearman_pval = (corr.test(ndays_time, prm_mean_val, method="spearman"))$p)



