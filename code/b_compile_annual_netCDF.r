################################################################################
#
#  Compile Ecology marine water monitoring data from annual netCDF files
#  into one file.
#
#  Measurement variables are extracted as separate vectors from the netCDF.
#  QC variables are extracted that are associated with each measurement var.
#  Derived var 'depth' and dimension vars (time, station, profile) are also
#  extracted as vectors.
#
#  Four output tables are produced:  measurements, profiles, stations and
#  variable metadata.
#  Only the stations table is written to csv for use in GIS.
#
#  The measurements table (ecy_meas_qa) is available within the R session for 
#  use by visualization scripts - it is not written to file due to its size.
#  Visualization scripts should be run in sequence after this script is run.
#  This script can be run by itself, but the intent is that it is sourced 
#  from the code orchestrate.r which also handles visualization.
# 
#  July 2024
#
################################################################################

library(ncdf4)
library(tidyverse)



################################################################################
#  Definitions
################################################################################

# set vector of measured variables to extract (each with assoc QA variable).
# In 1999 netCDF file, each of these vectors has length 34,419. 
varList <- c("PO4","SiOH4","NH4","NO2","NO3","Xmiss_25cm","BatC","FluorAdjusted",
             "Turb","DOAdjusted","Salinity","Density","Cond","Temp")
# set vector of QA variables to extract in same order as measured vars.
# In 1999 netCDF file, each of these vectors has length 34,419.
qaList <- c("PO4_QC","SiOH4_QC","NH4_QC","NO2_QC","NO3_QC","Xmiss_25cmQC",
            "BatCQC","FluorAdjustedQC","TurbQC","DOAdjustedQC","SalinityQC",
            "DensityQC","CondQC","TempQC")

# Depth & dimension vars that match the length of the measurement vectors. 
# In 1999 netCDF file, each of these vectors has length 34,419.
dimList_meas <- c("Depth","UTCDatetime","obs_index")
# Dimension vars that combine as columns in a "profiles" table. 
# In 1999 netCDF file, each of these vectors has length 435.
dimList_prof <- c("FieldDate","profile_index","row_size","station_index")
# Dimension vars that combine as columns in a "stations" table.
# In 1999 netCDF file, each of these vectors has length 45.
dimList_stn <- c("station_number", "Station", "Longitude", "Latitude")


# declare vectors to hold units, description ("long_name") and fill (missing)
# value for each measurement variable
var_units <- rep(NA, length(varList))
var_desc <- rep(NA, length(varList))
var_fillvalue <- rep(NA, length(varList))



################################################################################
#  Loop through netCDF files and process
################################################################################

# get list of netCDF files; all files assumed netCDF
inList <- list.files(local_netCDF_home)

file_index <- 1

for (ifile in inList) {
  
  print(sprintf("Processing %s",ifile))
  filepath <- str_c(local_netCDF_home, ifile, sep=sepsym)
  nc <- nc_open(filepath) 

  #################################################################### 
  #  loop through measured variables, extract from this file & compile
  #################################################################### 
  var_index <- 1
  for (ivar in varList) {
   
    # get measured values
    var_tmp <- data.frame(ncvar_get(nc, ivar))
    names(var_tmp) <- ivar
 
    # if first file, compile attributes for all measurement variables.
    # All other annual files assumed to have the same attribute values.
    if (file_index == 1) {
      iunits <- ncatt_get(nc, ivar, "units")
      idesc <- ncatt_get(nc, ivar, "long_name")
      ifill <- ncatt_get(nc, ivar, "_FillValue")

      var_units[var_index] <- iunits$value
      var_desc[var_index] <- idesc$value
      var_fillvalue[var_index] <- ifill$value
    } 
    # if first measurement variable in ifile, save vector length and start new
    # data table for this file, starting with first column
    if (var_index == 1) {
      data_len <- dim(var_tmp)[1]
      file_meas_tbl <- var_tmp 
    } else {
      # check var vector length is consistent within this data file
      if (dim(var_tmp)[1] != data_len) {
        print(sprintf("Data vector length mismatch. %s.  %s.",ifile,ivar))
      }
      # add this variable to the compiled data table
      file_meas_tbl <- cbind(file_meas_tbl, var_tmp) 
      
    } # close else block for variables 2+
    
    var_index <- var_index + 1 
  } ############## end loop through variables ####################### 
 
  

  #################################################################### 
  #  loop through QA variables, extract from this file & compile
  #################################################################### 
  qa_index <- 1
  for (iqa in qaList) {

    # get values for QA variable iqa
    qa_tmp <- data.frame(ncvar_get(nc, iqa))
    names(qa_tmp) <- iqa
   
    # if first qa variable in ifile, save vector length and start new data table
    # for this file, starting with first column
    if (qa_index == 1) {
      data_len <- dim(qa_tmp)[1]
      file_qa_tbl <- qa_tmp
    } else {
      # check var vector length is consistent within this data file
      if (dim(qa_tmp)[1] != data_len) {
        print(sprintf("QA vector length mismatch. %s.  %s.",ifile,iqa))
      }
      # add this variable to the compiled data table
      file_qa_tbl <- cbind(file_qa_tbl, qa_tmp) 
    }  # close else block for qa variables 2+ 
    
    qa_index <- qa_index + 1 
  }  # close loop through QA variables ##################
  
   
  
  #################################################################### 
  #  loop through derived depth and dimension variables 
  #################################################################### 
  
  ########## measurement variable attributes
  dim_index <- 1
  for (idim in dimList_meas) {

    dim_tmp <- data.frame(ncvar_get(nc, idim))
    names(dim_tmp) <- idim
    
    if (dim_index == 1) {
      dim_length <- dim(dim_tmp)[1]
      compiled_meas_attr <- dim_tmp
    } else {
      # check for vector length consistency
      if (dim(dim_tmp)[1] != dim_length) {
        print(sprintf("Dimension (meas) length mismatch. %s. %s",ifile,idim))
      }
      compiled_meas_attr <- cbind(compiled_meas_attr, dim_tmp) 
    }
    dim_index <- dim_index + 1
  }  ###### close for loop through measurement "dimensions", i.e. attributes
 
   
  ########## profile variables/attributes 
  dim_index <- 1
  for (idim in dimList_prof) {

    # get values for QA variable
    dim_tmp <- data.frame(ncvar_get(nc, idim))
    names(dim_tmp) <- idim
    
    if (dim_index == 1) {
      dim_length <- dim(dim_tmp)[1]
      compiled_prof_attr <- dim_tmp
    } else {
      # check for vector length consistency
      if (dim(dim_tmp)[1] != dim_length) {
        print(sprintf("Dimension (prof) length mismatch. %s. %s",ifile,idim))
      }
      compiled_prof_attr <- cbind(compiled_prof_attr, dim_tmp) 
    }
    dim_index <- dim_index + 1
  }  ########## close for loop through profile "dimensions", i.e. attributes
 
   
  ########## station variables/attributes 
  dim_index <- 1
  for (idim in dimList_stn) {

    # get station dimension variables and rename
    dim_tmp <- data.frame(ncvar_get(nc, idim))
    names(dim_tmp) <- idim
    
    if (dim_index == 1) {
      dim_length <- dim(dim_tmp)[1]
      compiled_stn_attr <- dim_tmp
    } else {
      # check for vector length consistency
      if (dim(dim_tmp)[1] != dim_length) {
        print(sprintf("Dimension (meas) length mismatch. %s. %s",ifile,idim))
      }
      compiled_stn_attr <- cbind(compiled_stn_attr, dim_tmp) 
    }
    dim_index <- dim_index + 1
  }  ########## close for loop through station "dimensions", i.e. attributes


  # compile data from the annual netCDF data files 
  if (file_index == 1) {
    ecy_meas_tbl <- file_meas_tbl
    ecy_qa_tbl <- file_qa_tbl 
    ecy_meas_attr_tbl <- compiled_meas_attr 
    ecy_prof_tbl <- compiled_prof_attr 
    ecy_stn_tbl_dups <- compiled_stn_attr 
  } else {
    ecy_meas_tbl <- rbind(ecy_meas_tbl, file_meas_tbl) 
    ecy_qa_tbl <- rbind(ecy_qa_tbl, file_qa_tbl)
    ecy_meas_attr_tbl <- rbind(ecy_meas_attr_tbl, compiled_meas_attr) 
    ecy_prof_tbl <- rbind(ecy_prof_tbl, compiled_prof_attr) 
    ecy_stn_tbl_dups <- rbind(ecy_stn_tbl_dups, compiled_stn_attr) 
  }
   

  file_index <- file_index + 1  
  
}  ##################### end loop through netCDF files ##################




###############################################################################
# post-processing
###############################################################################

# reduce station records compiled across yearly data files to a unique stn list
stn_no_uniq <- data.frame(station_number = unique(ecy_stn_tbl_dups$station_number))
ecy_stn_tbl <- stn_no_uniq %>% 
     left_join(ecy_stn_tbl_dups,by="station_number", keep=FALSE, multiple="any")

# combine measurement var columns with qa var columns and meas attributes
ecy_meas_qa <- cbind(ecy_meas_tbl, ecy_qa_tbl, ecy_meas_attr_tbl)


# We now have three tables: measurements/qa, profiles, stations 
# Goal is to add needed info to the measurements table and it becomes the
# input data for analysis/visualization. The key var to add is station.
#
# Join profiles table onto measurements table by matching profile_index value
# from profiles tables with obs_index from measurements table. 
ecy_meas_qa <- ecy_meas_qa %>%
      left_join(ecy_prof_tbl, by=c("obs_index" = "profile_index")) %>%
      select(-UTCDatetime, -row_size)

# Now join the stations table, using station index as the matching key
# and convert the FieldDate from integer to date data type
ecy_meas_qa <- ecy_meas_qa %>%
       left_join(ecy_stn_tbl, by=c("station_index" = "station_number")) %>%
       mutate(date = as_date(FieldDate, origin="1970-01-01")) %>%
       select(-Longitude, -Latitude, -FieldDate)
     
# create a data frame that relates measurement variables with units and 
# description. A 'QC' record is added to the end which defines the QC flags
var_metadata <- data.frame(
  var = varList,
  units = var_units,
  missing_val = var_fillvalue,
  description = var_desc
)
QArec <- data.frame(
  var = "QC",
  units = "none",
  missing_val = "none",
  description = "0=none, 1=Fail, 2=Pass"
)
var_metadata <- rbind(var_metadata, QArec)



# save metadata table
write.csv(ecy_stn_tbl, file="output_tables/ecy_stn_tbl.csv")
write.csv(var_metadata, file="output_tables/ecy_var_metadata.csv")


# clean up
rm(compiled_meas_attr, compiled_prof_attr, ecy_meas_tbl)
rm(compiled_stn_attr, data_len, dim_index, dim_length, dim_tmp, dimList_meas)
rm(dimList_prof, dimList_stn, file_index, file_meas_tbl, file_qa_tbl, filepath)
rm(idesc, idim, ifile, ifill, inList, iqa, iunits, ivar, nc, qa_index)
rm(qa_tmp, sysname, var_index)
rm(var_tmp)
rm(ecy_stn_tbl_dups, stn_no_uniq)
rm(ecy_meas_attr_tbl, ecy_prof_tbl, ecy_qa_tbl, var_desc)
rm(var_units, QArec, var_fillvalue)


