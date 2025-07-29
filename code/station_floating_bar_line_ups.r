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



# specify variable by index:
# 1-5:   "PO4","SiOH4","NH4","NO2","NO3",
# 6-10:  "Xmiss_25cm","BatC","FluorAdjusted","Turb","DOAdjusted",
# 11-14: "Salinity","Density","Cond","Temp")
var_index <- 14


# add season variable - for use in facetting
ecy_meas_qa_season <- ecy_meas_qa %>%
  mutate(season = case_when(
    month(date) < 4 ~ "Jan-Feb-Mar",
    month(date) < 7 ~ "Apr-May-Jun",
    month(date) < 10 ~ "Jul-Aug-Sep",
    month(date) < 13 ~ "Oct-Nov-Dec" 
  ))

# group by station and season and summarize stats
ecy_season_stats <- ecy_meas_qa_season %>%
  group_by(Station, season) %>%
  summarize(mean = mean(Temp, na.rm=TRUE),
            min = min(Temp, na.rm=TRUE),
            max = max(Temp, na.rm=TRUE),
            median = median(Temp, na.rm=TRUE)
  )


# make floating bar graph with mean or median facetted by season
p <- ggplot(data=ecy_season_stats,
            mapping=aes(x=Station, ymin=min, ymax=max, y=median)) +
     geom_crossbar() +
     facet_wrap(vars(season), nrow=4) +
     theme_bw() +
     theme(
       axis.text.x = element_text(angle=90)
     )









