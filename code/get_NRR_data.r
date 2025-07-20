################################################################################
#  This code integrates Ecology data from station NRR001 (Tacoma Narrows) into
#  the most recent data downloaded from Ecology as netCDF files.
#
#  The up-to-date Ecology data is now available as netCDF files (through 
#  12/2023). But this data downloaded in June 2024 does not include any 
#  NRR001 data.
#
#  This code was used to first check if there was NRR001 data in the EIM data
#  also downloaded in June 2024 (this data is out-of-date and so was 
#  abandoned as HSIL synthesis work shifted to the netCDF data).
#
#  Use of this script confirmed that the NRR station is not present in the EIM 
#  data.
#
#  The main purpose here is to integrate the NRR001 data downloaded in ~2018
#  into the up-to-date Ecology data compiled into data frame ecy_meas_qa (in
#  b_compile_annual_netCDF.r).
#
#  Also a record is added to the station table (ecy_stn_tbl).
#  
#  August 2024
################################################################################

library(tidyverse)

# EIM data path
# inFilePath <- str_c(home_local, "/ecy_profile_data.csv")

# read EIM data and get list of unique stations
# eim_data <- read.csv(inFilePath, stringsAsFactors=FALSE)
# stns_eim <- unique(eim_data$Location_ID)

# nrr_in_eim <- "NRR001" %in% stns_eim
# print(sprintf("NRR001 is in the 6/2024 EIM data:  %s", as.character(nrr_in_eim)))
# the above print statement shows that the EIM data does not contain NRR data.


####### read in NRR data files from 2018 
nrr_discrete_path <- str_c(nrr_data_path, 'NRR001_0_discrete.csv', sep="/")
nrr_discrete <- read.csv(nrr_discrete_path, stringsAsFactors=FALSE)

nrr_prof_download_path <- str_c(nrr_data_path, 'NRR001_0_profile_download.csv', sep="/")
nrr_prof_downl <- read.csv(nrr_prof_download_path, stringsAsFactors=FALSE)


####### convert NRR profile data to ecy_meas_qa table structure
####### QA variables set to PASS to avoid data deletion in later scripts
nrr_prof_converted <- data.frame(
  PO4 =  as.numeric(NA),
  SiOH4 = as.numeric(NA), 
  NH4 = as.numeric(NA), 
  NO2 = as.numeric(NA), 
  NO3 = as.numeric(NA), 
  Xmiss_25cm = nrr_prof_downl$light.transmission...., 
  BatC = as.numeric(NA),
  FluorAdjusted = nrr_prof_downl$chlorophyll.raw..ug.l.,
  Turb = as.numeric(NA),
  DOAdjusted = nrr_prof_downl$dissolved.oxygen..mg.l..raw,
  Salinity = nrr_prof_downl$salinity..psu.,
  Density = nrr_prof_downl$density..sigma.t.,
  Cond = as.numeric(NA),
  Temp = nrr_prof_downl$temperature..centigrade.,
  PO4_QC = as.numeric(NA),
  SiOH4_QC = as.numeric(NA),
  NH4_QC = as.numeric(NA),
  NO2_QC = as.numeric(NA),
  NO3_QC = as.numeric(NA),
  Xmiss_25cmQC = as.numeric(2),
  BatCQC = as.numeric(NA),
  FluorAdjustedQC = as.numeric(2),
  TurbQC = as.numeric(NA),
  DOAdjustedQC = as.numeric(2),
  SalinityQC = as.numeric(2),
  DensityQC = as.numeric(2),
  CondQC = as.numeric(NA),
  TempQC = as.numeric(2),
  Depth = nrr_prof_downl$depth..meters.,
  obs_index = as.numeric(NA),
  station_index = as.numeric(NA),
  Station = nrr_prof_downl$station,
  date = mdy(nrr_prof_downl$date)
)

####### convert NRR discrete data to ecy_meas_qa table structure
####### QA variables set to PASS to avoid data deletion in later scripts
nrr_discrete_converted <- data.frame(
  PO4 = nrr_discrete$PO4.uM.D,
  SiOH4 = nrr_discrete$SiOH4.uM.D,
  NH4 = nrr_discrete$NH4.uM.D,
  NO2 = nrr_discrete$NO2.uM.D,
  NO3 = nrr_discrete$NO3.uM.D,
  Xmiss_25cm = as.numeric(NA), 
  BatC = nrr_discrete$Beam_c,
  FluorAdjusted = as.numeric(NA), 
  Turb = nrr_discrete$Turbidity, 
  DOAdjusted = as.numeric(NA), 
  Salinity = as.numeric(NA), 
  Density = as.numeric(NA), 
  Cond = nrr_discrete$Cond,
  Temp = as.numeric(NA), 
  PO4_QC = as.numeric(2),
  SiOH4_QC = as.numeric(2),
  NH4_QC = as.numeric(2),
  NO2_QC = as.numeric(2),
  NO3_QC = as.numeric(2),
  Xmiss_25cmQC = as.numeric(NA),
  BatCQC = as.numeric(NA),
  FluorAdjustedQC = as.numeric(NA),
  TurbQC = as.numeric(NA),
  DOAdjustedQC = as.numeric(NA),
  SalinityQC = as.numeric(NA),
  DensityQC = as.numeric(NA),
  CondQC = as.numeric(NA),
  TempQC = as.numeric(NA),
  Depth = nrr_discrete$Depth,
  obs_index = as.numeric(NA),
  station_index = as.numeric(NA),
  Station = nrr_discrete$Station,
  date = mdy(nrr_discrete$Date)
)


# Combine NRR discrete and profile converted data
nrr_recs <- rbind(nrr_prof_converted, nrr_discrete_converted)

# Combine the NRR records with the compiled Ecology dataset
ecy_meas_qa <- rbind(ecy_meas_qa, nrr_recs)


# Also add a record for NRR001 to the station table (ecy_stn_tbl)
nrr_stn_rec <- data.frame(station_number = 77,
                      Station = "NRR001",
                      Longitude = -122.549833,
                      Latitude = 47.3165)
ecy_stn_tbl <- rbind(ecy_stn_tbl, nrr_stn_rec)


print(sprintf("NRR001 added to data and stn tables.")) 

write.csv(ecy_stn_tbl, file="output_tables/ecy_stn_tbl.csv")

rm(inFilePath)
rm(nrr_discrete,nrr_prof_downl,nrr_prof_converted)
rm(nrr_discrete_path, nrr_prof_download_path, nrr_data_path)
rm(nrr_discrete_converted, nrr_stn_rec) 
rm(nrr_recs)


