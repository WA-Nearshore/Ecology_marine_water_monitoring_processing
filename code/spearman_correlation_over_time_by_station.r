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
long_good_data_recs <- ecy_meas_qa %>%
    pivot_longer(PO4:Temp, names_to = "parameter", values_to = "value") %>%
    select(Depth,obs_index,station_index,Station,date,parameter,value) %>%
    drop_na(value)

# group by station and parameter and get Spearman stats
spearman.out <- long_good_data_recs %>%
  group_by(Station, parameter) %>%
  mutate(spearman_r = (corr.test(date,value,method="spearman"))$r)



