################################################################################
# Create tables for Experience Builder app to provide interactive access to
# visualizations of the Ecology marine water monitoring data.
#
# This is a part of the HSIL synthesis of existing data for environmental data.
# 
# Requirements:
#   var_table.csv  Variable table must exist in folder output_tables from
#                  running create_var_table.r
#
# The output tables created include:
#   calendarURLs
#   colorProfileURLs
#   seasonalURLs
#
# August 2024
################################################################################

library(tidyverse)
source("code/definitions.r")


################################################################################
# create table of calendar graph URLs
# 14 records; 1 calendar for each of the 14 variables
################################################################################

# read in variable table (list of variable codes & more readable labels)
fpath <- "output_tables/var_table.csv"
var_table <- read.csv(fpath, stringsAsFactors=FALSE)


urlStem <- "https://fortress.wa.gov/dnr/adminsa/gisdata/nearshorephotos/"
prjPath <- "hsil_kelp/synthesis_env/"

calendar_tbl <- var_table %>%
   mutate(calendarURL = str_c(urlStem,prjPath,"calendar_",var_codes,".png", sep=""))
          
fpath <- "output_tables/calendarURLs.csv"
write.csv(calendar_tbl, file=fpath)



################################################################################
# create table of color profile graph URLs
# 350 records; 1 visualization for each of the 14 variables for each of 25 sites.
################################################################################

# set path to where graphs are to be saved 
fstem <- str_c(home_local,"output_graphs", sep="")

# initialize the color profile table and append to it in loop below
colorProfTbl <- data.frame(
  station = character(),
  variable = character(),
  profileURL = character()
)

# loop through variables and stations and construct table records with urls
for (ivar in var_table$var_codes) {
   for (istn in stnList) {
      # construct file name and url
      fname1 <- str_c("colorProf",istn,ivar, sep="_")
      fname2 <- str_c(fname1,".png", sep="")
      url <- str_c(urlStem,prjPath,fname2, sep="")

      rec <- data.frame(
        station = istn,
        variable = ivar,
        profileURL = url
      )
      colorProfTbl <- rbind(colorProfTbl, rec)
      
   }  # close for loop through stations 
}  # close for loop through variables

fpath <- "output_tables/colorProfTbl.csv"
write.csv(colorProfTbl, file=fpath, row.names=FALSE)






