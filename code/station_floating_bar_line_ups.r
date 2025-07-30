###############################################################################
#  
#  Make station "line up" graphs.
#  These are floating bar graphs where bars indicate range of data for each
#  station for a given month range/season and given variable.
#
#  Input data must be already loaded in the workspace:
#    ecy_meas_qa   data frame of measurement and QA variables
#
#  July 2025
#
###############################################################################

library(tidyverse)
library(viridis)


# specify variable by index:
# 1-5:   "PO4","SiOH4","NH4","NO2","NO3",
# 6-10:  "Xmiss_25cm","BatC","FluorAdjusted","Turb","DOAdjusted",
# 11-14: "Salinity","Density","Cond","Temp")
var_index <- 14
var_name <- "Temperature (deg C)"
legend_name <- "Median\nTemperature"

# Open table with distance-from-ocean and HSIL flag, join and filter for HSIL
filepath2 <- str_c("output_tables","ecy_stations_distance_export.csv", sep="/")
ecy_stn_dist <- read.csv(filepath2, stringsAsFactors=FALSE)
ecy_stn_dist_sel <- ecy_stn_dist %>% select(Station, subgroup, ocean_dist_km)
ecy_meas_qa_jn <- ecy_meas_qa %>% 
  left_join(ecy_stn_dist_sel, by="Station") %>%
  filter(subgroup=="HSIL_study_area") %>%
  select(all_of(varList[var_index]), Depth, obs_index, Station, date, subgroup)

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

# Select 0-10 m depth
ecy_meas_qa_season_10m <- ecy_meas_qa_season %>%
   filter(Depth <= 10.0)

# get station order by distance from ocean and use as factor levels
ecy_stn_dist_sel_sort <- ecy_stn_dist_sel %>%
  arrange(ocean_dist_km)
stn_levels <- ecy_stn_dist_sel_sort$Station

# group by station and season and summarize stats
ecy_season_stats <- ecy_meas_qa_season_10m %>%
  group_by(Station, season_fct) %>%
  summarize(mean = mean(Temp, na.rm=TRUE),
            min = min(Temp, na.rm=TRUE),
            max = max(Temp, na.rm=TRUE),
            median = median(Temp, na.rm=TRUE)
  )
ecy_season_stats_fct <- ecy_season_stats %>%
  mutate(Station_fct = factor(Station, levels=stn_levels))


# make floating bar graph with mean or median facetted by season
p <- ggplot(data=ecy_season_stats_fct,
            mapping=aes(x=Station_fct, ymin=min, ymax=max, y=median,
                        fill=median)) +
     geom_crossbar() +
     facet_wrap(vars(season_fct), nrow=4) +
     theme_bw() +
     theme(
       axis.text.x = element_text(angle=90),
       axis.title.x = element_text(margin=margin(t=10)),
       axis.title.y = element_text(margin=margin(r=10))
     ) +
     scale_fill_viridis(option="H", discrete=FALSE, name=legend_name) +
     scale_x_discrete(name="Station") +
     scale_y_continuous(name=var_name)









