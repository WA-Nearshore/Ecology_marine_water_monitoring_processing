################################################################################
# Create tables for Experience Builder app to provide interactive access to
# visualizations of the Ecology marine water monitoring data.
#
# If this script is run after orchestrate.r, then two of the three prerequisites 
# listed below will be met.
#
# This is a part of the HSIL synthesis of existing data for environmental data.
# 
# Requirements:
#   var_table.csv  Variable table must exist in folder output_tables in the
#                  working directory. This csv file was created by 
#                  running create_var_table.r.
#   home_local     Path to local data. Defined in definitions.r, called by
#                  orchestrate.r.
#   stnList        Vector of station codes. Created in color_prof_histo_by_depth.r,
#                  which is called by orchestrate.r. These are Ecology stations
#                  filtered to just the HSIL study area.
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

print("Calendar graphs URLs")

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
################################################################################

print("Color profile graph URLs")

# set path to where graphs are to be saved 
fstem <- str_c(home_local,"output_graphs", sep="")

# initialize the color profile table and append to it in loop below
colorProfTbl <- data.frame(
  station = character(),
  variable = character(),
  profileURL = character()
)

# loop through variables and stations and construct table records with urls
# for color depth profiles
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


################################################################################
# create table of seasonal graph URLs 
################################################################################

print("Seasonal graph URLs")

# initialize the seasonal graph table and append to it in loop below
seasGraphTbl <- data.frame(
  station = character(),
  variable = character(),
  prof_summ_method = character(),
  seasonalURL = character()
)

# loop through variables, stations and profile summary methods and construct 
# table records with urls for seasonal graphs 
methList <- c("Surface (0.5 m)", "Average (0.5 to 5 m)", 
              "Difference (0.5 & 5 m)")
methFileNameList <- c("05", "ave05to5", "diff05and5")
for (ivar in var_table$var_codes) {
   for (istn in stnList) {
      for (imeth in seq_along(methList)) {
         # construct file name and url
         fname2a <- str_c("seas", methFileNameList[imeth],ivar,istn, sep="_")
         fname2b <- str_c(fname2a, ".png", sep="")
         seasURL <- str_c(urlStem,prjPath,fname2b, sep="")
      
         recSeasonal <- data.frame(
           station = istn,
           variable = ivar,
           prof_summ_method = methList[imeth],
           seasonalURL = seasURL
         )
         seasGraphTbl <- rbind(seasGraphTbl, recSeasonal)
      }
   }
}

fpath <- "output_tables/seasGraphTbl.csv"
write.csv(seasGraphTbl, file=fpath, row.names=FALSE)

# also save profile summary methods table
profSummMethods <- data.frame(
  label = methList,
  short_name = methFileNameList
)
fpath2 <- "output_tables/profSummMethods.csv"
write.csv(profSummMethods, file=fpath2, row.names=FALSE)



