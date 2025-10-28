###############################################################################
#  Calculate Spearman correlation and significance for each variable in the
#  Ecology dataset. Specifically, the mean parameter value within the specified
#  surface depth layer (e.g. 10 m) for a given calendar month is correlated
#  against time.
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

# Set min number of monthly records for Spearman to be run on Station-parameters.
# Also set min number of years spanned by these monthly records. 
min_n_Spearman <- 8
min_yr_span <- 8



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
# can be removed - this is a crude filter
station_prm_record_count <- ecy_filt_long_mean_values %>%
  group_by(Station, parameter) %>%
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

# add month and year variables
ecy_long_mean_passMinN_days_months <- ecy_long_mean_passMinN_days %>%
  mutate(month = month(date), year=year(date))

# get number of monthly records and number of years spanned, for filtering
ecy_sample_size_span <- ecy_long_mean_passMinN_days_months %>%
  group_by(Station, parameter, month) %>%
  summarize(n_months = n(), yr_span = max(year) - min(year))

# join dataset summary stats back onto data for filtering
ecy_long_mean_jn <- ecy_long_mean_passMinN_days_months %>%
  left_join(ecy_sample_size_span, by=join_by(Station,parameter,month))

# filter data to meet sample size and year span requirements
ecy_long_mean_month_filt <- ecy_long_mean_jn %>%
  filter(n_months >= min_n_Spearman, yr_span >= min_yr_span)



###############################################################################
# Get Spearman correlation coefficients and significance 
###############################################################################

# get Spearman stats
spearman.months.out <- ecy_long_mean_month_filt %>%
  group_by(Station, parameter, month) %>%
  summarize(
     spearman_r = (corr.test(ndays_time, prm_mean_val, method="spearman"))$r,
     spearman_pval = (corr.test(ndays_time, prm_mean_val, method="spearman"))$p
  )

# Adjust p-values for multiple tests and
# add an integer value for significance category (0=n.s., 1=0.05, 2=0.01)
spearman.months.out2 <- spearman.months.out %>%
  mutate(
    spearman_pval_adj = p.adjust(spearman_pval, method="holm"),
    sig_category = case_when(
      abs(spearman_r - 0.00) < 0.000000001 ~ 0, 
      spearman_pval_adj <= 0.01 ~ 3*spearman_r/abs(spearman_r),
      spearman_pval_adj <= 0.05 ~ 2*spearman_r/abs(spearman_r),
      TRUE ~ 1*spearman_r/abs(spearman_r))
  )




