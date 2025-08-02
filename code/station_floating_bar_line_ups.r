###############################################################################
#  
#  Make station "line up" graphs.
#  These are floating bar graphs where bars indicate range of data for each
#  station for a given month range/season and given variable.
#
#  Input data must be already loaded in the workspace:
#    ecy_meas_qa   data frame of measurement and QA variables
#    varList   char vector of variable column names in data
#
#  July 2025
#
###############################################################################

library(tidyverse)
library(viridis)


###############################################################################
# set parameters
###############################################################################
# specify variable by index:
# 1-5:   "PO4","SiOH4","NH4","NO2","NO3",
# 6-10:  "Xmiss_25cm","BatC","FluorAdjusted","Turb","DOAdjusted",
# 11-14: "Salinity","Density","Cond","Temp")
var_index <- 14
var_name <- "Temperature (deg C)"
legend_name <- "Median\nTemperature"

# set threshold for removing stations with low count of season sample dates 
count_filter <- 8


###############################################################################
# read ocean distance file & join 
###############################################################################

# Open table with distance-from-ocean and HSIL flag, join to ecy_meas_qa which
# must be already in the worksapce (from running b_compile_annual_netCDF.r
# and filter for HSIL
filepath2 <- str_c("output_tables","ecy_stations_distance_export.csv", sep="/")
ecy_stn_dist <- read.csv(filepath2, stringsAsFactors=FALSE)
ecy_stn_dist_sel <- ecy_stn_dist %>% select(Station, subgroup, ocean_dist_km)
ecy_meas_qa_jn <- ecy_meas_qa %>% 
  left_join(ecy_stn_dist_sel, by="Station") %>%
  filter(subgroup=="HSIL_study_area") %>%
  select(all_of(varList[var_index]), Depth, obs_index, Station, date, subgroup)
# generalize variable name so code can be used for any variable
vnames <- names(ecy_meas_qa_jn)
vnames[1] <- "Value"
names(ecy_meas_qa_jn) <- vnames


###############################################################################
# add season variable for graph facetting by season 
###############################################################################

# add season variable - for use in facetting
ecy_meas_qa_season <- ecy_meas_qa_jn %>%
  mutate(season = case_when(
    month(date) < 4 ~ "Jan-Feb-Mar",
    month(date) < 7 ~ "Apr-May-Jun",
    month(date) < 10 ~ "Jul-Aug-Sep",
    month(date) < 13 ~ "Oct-Nov-Dec" 
  ))
seas_levels <- c("Jan-Feb-Mar", "Apr-May-Jun", "Jul-Aug-Sep", "Oct-Nov-Dec")
ecy_meas_qa_season <- ecy_meas_qa_season %>%
  mutate(season_fct = factor(season, levels=seas_levels)) %>%
  select(-season)


########## filter for measurements in top 10m of water column and
########## order stations by distance to ocean

# Select 0-10 m depth
ecy_meas_qa_season_10m <- ecy_meas_qa_season %>%
   filter(Depth <= 10.0)

# get station order by distance from ocean and use as factor levels
ecy_stn_dist_sel_sort <- ecy_stn_dist_sel %>%
  arrange(ocean_dist_km)
stn_levels <- ecy_stn_dist_sel_sort$Station



###############################################################################
# get stats by station & season; record num. of sample dates for each stat 
###############################################################################
# group by station and season and summarize stats
ecy_season_stats <- ecy_meas_qa_season_10m %>%
  group_by(Station, season_fct) %>%
  summarize(mean = mean(Value, na.rm=TRUE),
            min = min(Value, na.rm=TRUE),
            max = max(Value, na.rm=TRUE),
            median = median(Value, na.rm=TRUE),
            stdev = sd(Value, na.rm=TRUE),
            data_count = length(unique(date)) 
  )
ecy_season_stats_fct <- ecy_season_stats %>%
  mutate(Station_fct = factor(Station, levels=stn_levels))

# Make histogram of station-season frequencies by data count to 
# help select a threshold for number of data pts.
phist <- ggplot(data=ecy_season_stats_fct,
                mapping = aes(x=data_count)) +
         geom_histogram(binwidth=1, fill="gray40", color="white", center=0.5) +
         theme_bw() +
         theme(
           axis.title.x = element_text(margin = margin(t=10)),
           axis.title.y = element_text(margin = margin(r=10))
         ) +
         scale_x_continuous(name="Count of Season Sample Dates") +
         scale_y_continuous(name="Number of Occurrences")

# to look at distribution of residuals, make needed join and calc residuals
ecy_season_10m_jn1 <- ecy_meas_qa_season_10m %>%
  left_join(ecy_season_stats_fct, by = join_by("Station", "season_fct")) %>%
  mutate(z_residual = (Value - mean)/stdev)
# make freq. histogram of z residuals
p_res_hist <- ggplot(data=ecy_season_10m_jn1,
                     mapping=aes(x=z_residual)) +
              geom_histogram(aes(y=..density..), binwidth=0.1, fill="gray65", 
                             color="white") +
              theme_bw() +
              theme(
                axis.title.x = element_text(margin = margin(t=10)),
                axis.title.y = element_text(margin = margin(r=10))
              ) +
              scale_x_continuous(name="Residual Z Value") +
              scale_y_continuous(name="Frequency")
# add normal curve
p_res_hist2 <- p_res_hist +
    stat_function(fun=dnorm, color="gray35", linewidth=1.5)




###############################################################################
# filter out stations with low season data counts in any season; first create
# histogram to help specify data count cutoff
###############################################################################

ecy_season_stn_min_counts <- ecy_season_stats_fct %>%
  group_by(Station) %>%
  summarize(min_count = min(data_count))
# get list of stations that pass filter (count_filter set above)
ecy_season_stns_pass_list <- ecy_season_stn_min_counts %>%
  filter(min_count >= count_filter)
# filter out sites with any season count less than count_filter set above
ecy_season_stats_fct_nfilt <- ecy_season_stats_fct %>%
  filter(Station %in% ecy_season_stns_pass_list$Station)





###############################################################################
# make floating bar graph
###############################################################################

# make floating bar graph with mean or median facetted by season
p <- ggplot(data=ecy_season_stats_fct_nfilt,
            mapping=aes(x=Station_fct, ymin=min, ymax=max, y=median,
                        fill=median)) +
     geom_crossbar() +
     facet_wrap(vars(season_fct), nrow=4) +
     theme_bw() +
     theme(
       axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
       axis.title.x = element_text(margin=margin(t=10)),
       axis.title.y = element_text(margin=margin(r=10))
     ) +
     scale_fill_viridis(option="H", discrete=FALSE, name=legend_name) +
     scale_x_discrete(name="Station") +
     scale_y_continuous(name=var_name)









