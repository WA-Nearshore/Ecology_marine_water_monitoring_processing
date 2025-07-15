################################################################################
#  Code to open and explore netCDF data files containing Ecology Marine Water
#  monitoring data.
#
#  Files downloaded on 6/7/2024 from
#  https://ecology.wa.gov/research-data/monitoring-assessment/puget-sound-and-marine-monitoring/water-column-data
#
#  June 2024
#
################################################################################

library(ncdf4)
library(tidyverse)

home <- 'C:\\Users\\pdow490\\Documents\\HSIL_local\\Ecology_data\\downloads_netCDF'

# get list of netCDF files
inList <- list.files(home)

# open first file for exploration
filepath <- str_c(home, inList[[1]], sep="\\")
nc1 <- nc_open(filepath)

# gives a summary of the 75 data variables in the data object
print(nc1)

# gives a more concise listing of just the 75 variable names
attributes(nc1$var)

# gives a concise listing of the 4 dimension variables
attributes(nc1$dim)


# extract dimension variables from netCDF data object
# these all fail: "this dimension HAS NO DIMVAR!
stations <- ncvar_get(nc1, "stations")
profiles <- ncvar_get(nc1, "profiles")
obs <- ncvar_get(nc1, "obs")
maxSL <- ncvar_get(nc1, "max_string_length")

# extract key variables from netCDF data object
temp <- ncvar_get(nc1, "Temp")
depth <- ncvar_get(nc1, "Depth")
datetime <- ncvar_get(nc1, "UTCDatetime")
fielddate <- ncvar_get(nc1, "FieldDate")
lon <- ncvar_get(nc1, "Longitude")
lat <- ncvar_get(nc1, "Latitude")
station <- ncvar_get(nc1, "Station")
station_num <- ncvar_get(nc1, "station_number")
station_index <- ncvar_get(nc1, "station_index")
obs_index <- ncvar_get(nc1, "obs_index")
row_size <- ncvar_get(nc1, "row_size")

# get attributes for the temp variable
fillvalue <- ncatt_get(nc1, "Temp", "_FillValue")
lat_attr <- ncatt_get(nc1, "Temp", "Latitude")

# look more closely at dates
dim(datetime)
tunits <- ncatt_get(nc1, "UTCDatetime", "units")
dim(fielddate)
tfunits <- ncatt_get(nc1, "FieldDate", "units")


# extract remaining variables
PO4 <- ncvar_get(nc1, "PO4")
SiOH4 <- ncvar_get(nc1, "SiOH4")
NH4 <- ncvar_get(nc1, "NH4")
NO2 <- ncvar_get(nc1, "NO2")
NO3 <- ncvar_get(nc1, "NO3")
trans <- ncvar_get(nc1, "Xmiss_25cm")
beam_att <- ncvar_get(nc1, "BatC")
fluor <- ncvar_get(nc1, "FluorAdjusted")
turb <- ncvar_get(nc1, "Turb")
DO <- ncvar_get(nc1, "DOAdjusted")
sal <- ncvar_get(nc1, "Salinity")
dens <- ncvar_get(nc1, "Density")
cond <- ncvar_get(nc1, "Cond")


# do annual netCDf files start over with obs index (i.e. profile)?
# open second file for exploration
filepath2 <- str_c(home, inList[[2]], sep="\\")
nc1 <- nc_open(filepath2)
obs_index2 <- ncvar_get(nc1, "obs_index")

# it appears that the profile IDs (obs_index) numbering continues from the
# first year; i.e. 1999: [1:435]
# secondyear: 2000 [436:885]






