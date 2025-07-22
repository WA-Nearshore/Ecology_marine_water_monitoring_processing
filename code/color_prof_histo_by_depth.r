################################################################################
# Create color ramp profile graph for selected variable and selected site.
# Faceted by year.
#
# Create freq. histograms of data by depth faceted by variable (all stations
# aggregated). 

# Also produce freq. histogram of data by depth for all sites for the selected
# measurement variable.
#
# Objects required to be in the current session workspace:  
#   ecy_meas_qa  compiled Ecology data; created in b_compile_annual_netCDF.r
#   var_index  index for character vector varList indicating a specific variable,
#              created in orchestrate.r if used to source this file.
#   stnList  list of all stations
#   varList  vector of variable codes, created in b_compile_annual_netCDF.r
#   stn_var_tbl_jn  data frame of station-variable combinations with lat/lon
#                   and regional subgroup (to isolate HSIL study area stations).
#                   If make_stn_var_tbl.r is run prior to this script, then
#                   this data frame will be in the current R workspace.
#
# July 2024
################################################################################

library(ggExtra)
library(viridis)
source("code/definitions.r")


# flag for plotting accompanying histograms
demo_plots <- FALSE


# Define function to reduce multiple profiles in a given month to one profile 
# by random selection.
get_obs <- function(obs_index) {
  if (length(obs_index) == 1) {
    return(obs_index)
  } else {
    return(sample(obs_index, size=1))
  }
}

# get name of variable being processed 
var_name <- colnames(ecy_meas_qa)[var_index]



################################################################################
# Prepare data for graphing
################################################################################

# Filter from ecy_meas_qa for specified variable, and make column names generic.
ecy_var <- ecy_meas_qa %>%
   select(varList[var_index], Depth, Station, date, obs_index)
names(ecy_var) <- c("Var", "Depth", "Station", "date", "obs_index") 

###### Find and resolve cases of multiple profiles within a single month.
# First, reduce multiple profile obs to one record for that profile.
# stn_date_obs_cnt gives count of obs per profile
obs_dates <- ecy_var %>%
   group_by(Station, date, obs_index) %>%
   summarize(stn_date_obs_cnt=n(), .groups="drop_last") %>%
   mutate(year=year(date), month=month(date))
# Second, group profiles by stn-yr-mon and reduce to single profile where
# multiple profiles occur in a given month. In this case the single is randomly 
# selected in function get_obs.
obs_yr_mon <- obs_dates %>%
   group_by(Station, year, month) %>%
   summarize(stn_mon_day_cnt=n(), sel_obs_index = get_obs(obs_index),
             .groups="drop_last")
# now use final list of profiles (each with unique value of obs_index) to 
# filter by profile, and add year/month
ecy_var_filt <- ecy_var %>%
   filter(obs_index %in% obs_yr_mon$sel_obs_index) %>%
   mutate(year=year(date), month=month(date))

# Filter out NA records
ecy_var_filt2 <- ecy_var_filt %>% filter(!is.na(Var))


# assemble graph y-axis title (same for all stations)
ytitle <- paste("Monthly ",var_name," (",
                var_metadata[var_metadata$var == var_name, "units"],")")



################################################################################
# Loop through stations, create graph and write to png 
################################################################################

# open station file exported from ArcGIS, with regional subgroups
filepath <- str_c("output_tables","ecy_stations_subgroups_tbl.csv", sep="/")
ecy_stations_in <- read.csv(filepath, stringsAsFactors=FALSE)
ecy_stations_subgroup <- ecy_stations_in %>%
     select(Station, subgroup)

# get station list for this variable, then join ecy_stations_in
stnList_df <- stn_var_tbl_jn %>% 
              filter(parameter==var_name & 
                     subgroup == "HSIL_study_area") %>%
              select(Station)
stnList <- stnList_df$Station

for (istn in stnList) {

   ecy_var_filt_stn <- ecy_var_filt2 %>% filter(Station==istn)

   # Create color profile graphs for this variable and this station 
   pstn <-ggplot(data = ecy_var_filt_stn,
              mapping = aes(month, Depth, fill=Var)) +
     geom_tile() + 
     scale_fill_viridis(name=ytitle,option ="C") +
     facet_wrap(~year) +
     scale_y_continuous(trans = "reverse") +
     scale_x_continuous(breaks =c(1,6,12)) +
     theme_bw(base_size = 8) +
     labs(title= paste("Monthly ",var_name," Profiles - Station ",istn), 
          x="Month", y="Depth (meters)") +
     theme(legend.position = "bottom")+
     theme(plot.title=element_text(size = 14))+
     theme(strip.background = element_rect(fill="white"))+
     theme(axis.text.y=element_text(size=6)) +
     theme(strip.background = element_rect(colour="white"))+
     theme(plot.title=element_text(hjust=0))+
     theme(axis.ticks=element_blank())+
     theme(axis.text=element_text(size=7))+
     theme(legend.title=element_text(size=8))+
     theme(legend.text=element_text(size=6))+
     removeGrid()

   # write graph to png file
   fstem <- str_c(home_local, "output_graphs", sep="/")
   fname1 <- str_c("AAcolorProf",istn,var_name, sep="_")
   fname2 <- str_c(fname1,".png", sep="")
   fpath <- str_c(fstem,fname2, sep="/")
   
   print(fpath)
   
   ggsave(fpath, width=7, height=9, unit="in", dpi=150)

}




# if 'demo_plots' == TRUE, plot freq histogram of monthly visits by depth 
# for specified variable aggregated across stations

if (demo_plots) {
   # first reshape data to long form
   ecy_meas_long <- ecy_meas_qa %>%
     select(-PO4_QC, -SiOH4_QC, -NH4_QC, -NO2_QC, -NO3_QC, -Xmiss_25cmQC,
            -BatCQC, -FluorAdjustedQC, -TurbQC, -DOAdjustedQC, -SalinityQC,
            -DensityQC, -CondQC, -TempQC) %>%
     pivot_longer(cols=c("PO4","SiOH4","NH4","NO2","NO3","Xmiss_25cm","BatC",
                         "FluorAdjusted","Turb","DOAdjusted","Salinity","Density",
                         "Cond","Temp"),
                  names_to="variable") %>%
     drop_na(value) 

   pvars <- ggplot(data = ecy_meas_long, 
                   mapping = aes(x=Depth)) +
            geom_histogram() +
            facet_wrap(~variable) +
            theme_bw() +
            theme(strip.background = element_rect(fill="white"))

   # select nutrient variables only
   nuts <- c("NH4","NO2","NO3","PO4","SiOH4")
   ecy_meas_long_nuts <- ecy_meas_long %>% filter(variable %in% nuts)
   pnuts <- ggplot(data = ecy_meas_long_nuts, 
                   mapping = aes(x=Depth)) +
            geom_histogram() +
            facet_wrap(~variable) +
            theme_bw() +
            theme(strip.background = element_rect(fill="white")) +
            scale_x_continuous(limits=c(0,12))
}   # close demo_plots if block


# clean up
rm(demo_plots, ecy_var_ed, ecy_var_filt, ecy_var_filt_stn)
rm(ecy_var_filt2, fname, fstem, istn, obs_dates, obs_yr_mon)
rm(pstn, var_name, ytitle)
rm(get_obs, demo_plots, var_name, pstn, ytitle)
rm(ecy_var,filepath,fname1,fname2,fpath,stnList_df)
rm(ecy_stations_in)




