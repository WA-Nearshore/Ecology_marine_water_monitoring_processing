################################################################################
#  Create data calendar visualization of Ecology marine water monitoring data.
#  This script operates on a single variable at a time with the specific 
#  variable selected with the value of var_index set prior to sourcing this 
#  script.
#
#  This script effectively filters data based on QA flag by setting measurement
#  data to NA if the associated QA variable does not have a value of 2 (pass).
#
#  This script also filters for Ecology stations within the project study area.
#
#  The following data objects must already exist in the active 
#  R session workspace:
#    var_index    Index number within varList for specific variable to process.
#                 Is set in orchestrate.r, or can be set manually.
#    ecy_meas_qa  Input data. Created in b_compile_annual_netCDF.r.
#    varList      List of variables containted in data. Created in
#                 b_compile_annual_netCDF.r
#    qaList       List of QA flags ordered to match list of variables. Created
#                 in b_compile_annual_netCDF.r
#
#  The intent is that this code is sourced by script orchestration.r which
#  handles prerequisites.
#
#  Alternatively, this script can be run on its own if the script
# 'b_compile_annual_netCDF.r' is run previously and, separately,  variable 
#  var_index is assigned an integer value between 1 and 14.
#
#  July 2024
#
################################################################################

library(tidyverse)
source("code/definitions.r")


################################################
# prep data for graphing this variable 
################################################

# Select the variable and associated QA variable from the Ecology dataset
ecy_var <- ecy_meas_qa %>%
    select(varList[var_index], qaList[var_index], Station, date)
# Make column names generic to generalize the coding
names(ecy_var) <- c("Var","VarQA","Station","date")
# Consolidate different permutations of no-data as NA. Permutations are 
# 1) already is NA, 2) QA variable is 'failed', and 3) Value is the designated
# value for missing data, -99999.8984375.
ecy_var_ed <- ecy_var %>%
   mutate(Var = ifelse(!is.numeric(Var) | VarQA != 2 | Var < -99000, NA, Var))

# Ensure a complete station list for this variable - so each variable graphs
# with consistent set of stations. First, get complete site list for all vars. 
stns_all <- data.frame(Station = unique(ecy_meas_qa$Station))

# get missing stations for this variable (includes stations with no data or just
# NA records. 
ecy_var_good <- ecy_var_ed %>% filter(!is.na(Var))
stns_miss <- stns_all %>% 
                 anti_join(ecy_var_good, by="Station")
# if missing stations for this variable, create artificial records and flag
if (dim(stns_miss)[1] > 0) {
   add_stn_recs <- data.frame(Var=NA, VarQA=NA, Station=stns_miss$Station,
                              date=ymd("19990101"), data_avail="not_available")

   # join the records created for missing stations to the data table
   ecy_var_good_ed <- ecy_var_good %>% mutate(data_avail="available") 
   ecy_var_stns <- rbind(ecy_var_good_ed, add_stn_recs)
} else {
   ecy_var_stns <- ecy_var_good %>% mutate(data_avail="available")
}


# get enhanced station table exported from GIS with subgroup attribute and use
# to filter for HSIL study area sites only
stn_tbl_path <- str_c("output_tables", "ecy_stations_subgroups_tbl.csv", sep="/")
stations <- read.csv(stn_tbl_path, stringsAsFactors=FALSE)

ecy_var_graph <- ecy_var_stns %>%
    left_join(stations, by="Station") %>%
    filter(subgroup == "HSIL_study_area")

# convert data_avail to a factor with specified order for custom symbolizaiton
ecy_var_graph$data_avail <- factor(ecy_var_graph$data_avail,
                                   levels=c("available", "not_available"))


################################################
# Create calendar graph for this variable 
################################################

# read table with list of variable codes and more readable labels
var_table <- read.csv("output_tables/var_table.csv", stringsAsFactors=FALSE)

# set x-axis date breaks
xbreaks_orig <- as.Date(c("2000-01-01","2005-01-01","2010-01-01","2015-01-01",
                   "2020-01-01","2025-01-01"))
xbreaks <- as.Date(c("1990-01-01", "1995-01-01", "2000-01-01","2005-01-01",
                     "2010-01-01","2015-01-01",
                   "2020-01-01","2025-01-01"))

minor_x <- as.Date(c("1999-01-01",
                     "2001-01-01","2002-01-01","2003-01-01","2004-01-01",
                     "2006-01-01","2007-01-01","2008-01-01","2009-01-01",
                     "2011-01-01","2012-01-01","2013-01-01","2014-01-01",
                     "2016-01-01","2017-01-01","2018-01-01","2019-01-01",
                     "2021-01-01","2022-01-01","2023-01-01","2024-01-01"))

# make the calendar graph for the selected variable
pgtitle <- str_c(var_table[var_index,"var_labels"],"Data Availability",
                 sep=" ")
pcal <- ggplot(data=ecy_var_graph,
       mapping=aes(x=date, y=Station, alpha=data_avail)) +
       geom_point(size=0.2) +
       theme_bw() +
       theme(
         axis.title.x = element_text(margin = margin(t=10)),
         axis.title.y = element_text(margin = margin(r=20)),
         axis.text.x = element_text(size=5),
         axis.text.y = element_text(size=5),
         title = element_text(size=6),
         panel.border = element_rect(color="black"),
         panel.grid.major.x = element_line(linewidth=0.1, color="gray80"),
         panel.grid.major.y = element_line(linewidth=0.1, color="gray80"),
         panel.grid.minor.y = element_line(linewidth=0.1),
         panel.grid.minor.x = element_blank(),
         legend.position = "none"
       ) +
       scale_x_date(breaks = xbreaks, date_labels="%Y", minor_breaks=minor_x) +
       scale_y_discrete(limits=rev) +
       scale_alpha_discrete(range=c(1,0)) +
       ggtitle(pgtitle)

# save to png file
# fname <- str_c("output_graphs/calendar_", varList[var_index],".png")
fstem <- str_c(home_local, "output_graphs", sep="/")
fname <- str_c("calendar_",varList[var_index],".png",
               sep="")
fpath <- str_c(fstem, fname, sep="/")
ggsave(fpath, width=4, height=6, unit="in", dpi=150)

# cleanup
rm(ecy_var, ecy_var_good, ecy_var_stns)
rm(xbreaks, minor_x, pcal, fname)
if (dim(stns_miss)[1] > 0) {
  rm(add_stn_recs, ecy_var_good_ed)
}
rm(pgtitle)
rm(stn_tbl_path, stns_all, stns_miss)
rm(ecy_var_ed,ecy_var_graph,fpath,fstem,sysname)

  
  