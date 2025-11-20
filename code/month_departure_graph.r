###############################################################################
#
#  Make departure graph from Ecology marine monitoring data.
#  Departures from long-term means are shown for all "Spearman" stations for
#  one month-of-year.  Each site is depicted as a point in temp-zscore space.
#
#  The data frame ecy_long_mean_jn must be available in the workspace. It is
#  created by code in spearman_correlation_over_time_by_station.r
#  November 2025.
#
###############################################################################

# if prerequisite 'ecy_long_mean_jn' must be created, run these two lines once
# i.e. comment lines out if this script is run repeatedly
# source("code/assemble_netCDF_data.r")
# source("code/spearman_correlation_over_time_by_station.r")

# set parameters for the graphs
sel_month <- 2
sel_year <- 2024
month_labels <- "February"
sel_prm <- "Temp"

# Our requirements for Spearman to be run on a station-month.
# Set min number of monthly records & min number of years spanned 
min_n_Spearman <- 8
min_yr_span <- 8


######################################################################
# Filters for month, parameter and Spearman requirements 
######################################################################

# filter for months and parameter
ecy_long_mean_jn_filt <- ecy_long_mean_jn %>%
  filter(month == sel_month, parameter == sel_prm)

# filter to just stations used for Spearman correlations
ecy_long_mean_jn_filt2 <- ecy_long_mean_jn_filt %>%
  filter(n_months >= min_n_Spearman, yr_span >= min_yr_span)


######################################################################
# get station-month stats
######################################################################

# get station-month mean and sd
ecy_stn_stats <- ecy_long_mean_jn_filt2 %>%
  group_by(Station, month) %>%
  summarize(lt_mean = mean(prm_mean_val), lt_sd = sd(prm_mean_val))
# join back onto data and calc z-scores and temp. departures
ecy_long_z <- ecy_long_mean_jn_filt2 %>%
  left_join(ecy_stn_stats, by=join_by(Station, month)) %>%
  mutate(zscore = (prm_mean_val - lt_mean)/lt_sd,
         departure = prm_mean_val - lt_mean)


######################################################################
# convert stations to factor in order of dist to ocean 
######################################################################

# Open table with distance-from-ocean and HSIL flag
filepath2 <- str_c("output_tables","ecy_stations_distance_export.csv", sep="/")
ecy_stn_dist <- read.csv(filepath2, stringsAsFactors=FALSE)
ecy_stn_dist_sel <- ecy_stn_dist %>% select(Station, subgroup, ocean_dist_km)

# get station order by distance from ocean to use as factor levels
ecy_stn_dist_sel_sort <- ecy_stn_dist_sel %>% arrange(ocean_dist_km)
stn_levels <- ecy_stn_dist_sel_sort$Station

ecy_long_z_fct <- ecy_long_z %>%
  mutate(Station = factor(Station, levels=stn_levels))


# filter for 2024 data
ecy_long_z_fct_Y <- ecy_long_z_fct %>% filter(year == sel_year)

# make departure graph
p4 <- ggplot(data = ecy_long_z_fct_Y,
             mapping = aes(x=Station, y=departure)) +
      geom_segment(aes(x=Station, xend=Station, y=0, yend=departure),
                   color="gray20", linewidth=0.8) +
      geom_point(color="sienna", size=2.5) +
      geom_hline(yintercept=0, color="gray60", linewidth=0.8) +
      theme_bw() +
      theme(
       axis.title.y = element_text(margin = margin(r=10)),
       axis.title.x = element_blank(),
       axis.text.x = element_text(angle=90)
      ) +
      scale_y_continuous(name = "Departure (deg C)", limits=c(-1.2, 1.2))
      


