###############################################################################
#  Calculate Spearman correlation and significance for each variable in the
#  Ecology dataset against time.
#
#  Required input:  the data frame ecy_meas_qa must be present in the workspace.
#  This data frame is created in b_compile_annual_netCDF.r
#  
#  The threshold of min. number of records for a station and parameter to be
#  retained in the data for analysis is flawed.  Specifically, the number of 
#  records could be consecutive records in one year. This could identify
#  seasonal trend as a station trend.
#
#  Rather than fix this problem (perhaps by adding a min. number of years),
#  focus shifted to the trend analysis based on particular months.
#
#  August 2025
#
###############################################################################

library(tidyverse)
library(psych)

# set depth that defines bottom of layer (top=surface) for parameter averaging
mx_depth <- 10

# Set min number of monthly records for Spearman to be run on Station-parameters.
# Also set min number of years spanned by these monthly records. 
min_n_Spearman <- 8
min_yr_span <- 8



########  functions #########
get_spearman <- function (data_frame) {
  spearman_out <- corr_test(data.frame$time, data.frame$value, method="spearman")
  spearman_stats <- data.frame(r = spearman_out$r, p = spearman_out$p)
  return(spearman_stats)
}


###############################################################################
# prepare data frame for Spearman analyses
###############################################################################

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

# get mean values for all parameters within specified depth band 
ecy_filt_long_mean_values <- ecy_filt_long %>%
  filter(Depth <= mx_depth) %>%
  group_by(Station, parameter, date) %>%
  summarize(prm_mean_val = mean(value))
  
# Get data counts and year span by Station-parameter pairs so low n cases 
# can be removed
station_prm_record_count <- ecy_filt_long_mean_values %>%
  group_by(Station, parameter, month) %>%
  summarize(rec_count = n()) %>%
  arrange(rec_count)

# join data counts back onto table of mean depth values 
ecy_filt_long_mean_jn <- ecy_filt_long_mean_values %>%
  left_join(station_prm_record_count, by=join_by(Station, parameter)) 

# filter records for station-parameter min. number of records
ecy_long_mean_passMinN <- ecy_filt_long_mean_jn %>%
  filter(rec_count >= min_n_Spearman)
# get list of stations that were filtered out for not meeting min N
pass_stns <- unique(ecy_long_mean_passMinN$Station)
all_stns <- unique(ecy_filt_long_mean_jn$Station)
fail_stns <- setdiff(all_stns, pass_stns)


# add time as day since 1989-01-01, Spearman requires numeric variable
reference_date <- ymd("1989-01-01")
ecy_long_mean_passMinN_days <- ecy_long_mean_passMinN %>%
   mutate(ndays_time = as.numeric(date - reference_date, units="days"))



###############################################################################
#  get Spearman stats across all months and years in the prepared data 
###############################################################################

# group by station and parameter and get Spearman stats
spearman.out <- ecy_long_mean_passMinN_days %>%
  group_by(Station, parameter) %>%
  summarize(spearman_r = (corr.test(ndays_time, prm_mean_val, method="spearman"))$r,
            spearman_pval = (corr.test(ndays_time, prm_mean_val, method="spearman"))$p)

# add an integer value for significance category (0=n.s., 1=0.05, 2=0.01)
spearman.out <- spearman.out %>%
  mutate(sig_category = ifelse(spearman_pval <= 0.01, 2, 
                               ifelse(spearman_pval <= 0.05, 1, 0)))


###############################################################################
#  prepare alternate data for month-based correlations & get Spearman stats
###############################################################################

# add month and year variables
ecy_long_mean_passMinN_days_months <- ecy_long_mean_passMinN_days %>%
  mutate(month = month(date), year=year(date))


# get number of monthly records and number of years spanned, for filtering
ecy_sample_size_span <- ecy_long_mean_passMinN_days_months %>%
  group_by(Station, parameter, month) %>%
  summarize(n_months = n(), yr_span = max(year) - min(year))


# get Spearman stats
spearman.months.out <- ecy_long_mean_passMinN_days_months %>%
  group_by(Station, parameter, month) %>%
  summarize(spearman_r = (corr.test(ndays_time, prm_mean_val, method="spearman"))$r,
            spearman_pval = (corr.test(ndays_time, prm_mean_val, method="spearman"))$p)

# add an integer value for significance category (0=n.s., 1=0.05, 2=0.01)
spearman.months.out <- spearman.months.out %>%
  mutate(sig_category = ifelse(spearman_pval <= 0.01, 2, 
                               ifelse(spearman_pval <= 0.05, 1, 0)))



