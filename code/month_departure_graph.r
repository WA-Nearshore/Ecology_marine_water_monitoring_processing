###############################################################################
#
#  Make departure graph from Ecology marine monitoring data.
#  Departures from long-term means are shown for all "Spearman" stations for
#  one month-of-year.  Each site is depicted as a point in temp-zscore space.
#
#  The data frame cy_long_mean_jn must b4e available in the workspace. It is
#  created by code in spearman_correlation_over_time_by_station.r
#  November 2025
#
###############################################################################

# set parameters for the graphs
sel_month <- 2
sel_year <- 2024
month_labels <- "February"
sel_prm <- "Temp"

# Requirements for Spearman
# Set min number of monthly records for Spearman to be run.
# Also set min number of years spanned by these monthly records. 
min_n_Spearman <- 8
min_yr_span <- 8


# filter for months and parameter
ecy_long_mean_jn_filt <- ecy_long_mean_jn %>%
  filter(month == sel_month, parameter == sel_prm)

# filter to just stations used for Spearman correlations
ecy_long_mean_jn_filt2 <- ecy_long_mean_jn_filt %>%
  filter(n_months >= min_n_Spearman, yr_span >= min_yr_span)

# get station-month mean and sd
ecy_stn_stats <- ecy_long_mean_jn_filt2 %>%
  group_by(Station, month) %>%
  summarize(lt_mean = mean(prm_mean_val), lt_sd = sd(prm_mean_val))
# join back onto data and calc z-scores and temp. departures
ecy_long_z <- ecy_long_mean_jn_filt2 %>%
  left_join(ecy_stn_stats, by=join_by(Station, month)) %>%
  mutate(zscore = (prm_mean_val - lt_mean)/lt_sd,
         departure = prm_mean_val - lt_mean)

ecy_long_z_fct <- ecy_long_z %>%
  mutate(month = factor(month))
levels(ecy_long_z_fct$month) <- month_labels

# Open table with distance-from-ocean and HSIL flag
filepath2 <- str_c("output_tables","ecy_stations_distance_export.csv", sep="/")
ecy_stn_dist <- read.csv(filepath2, stringsAsFactors=FALSE)
ecy_stn_dist_sel <- ecy_stn_dist %>% select(Station, subgroup, ocean_dist_km)
ecy_meas_qa_jn <- ecy_meas_qa %>% 
  left_join(ecy_stn_dist_sel, by="Station") %>%
  filter(subgroup=="HSIL_study_area") %>%
  select(all_of(varList[var_index]), Depth, obs_index, Station, date, subgroup)



# filter for 2024 data
ecy_long_z_fct_Y <- ecy_long_z_fct %>% filter(year == sel_year)

p4 <- ggplot(data = ecy_long_z_fct_Y,
             mapping = aes(x=zscore, y=departure)) +
      geom_point(color="gray60", size=2.0) +
      theme_bw() +
      theme(
       panel.grid.major.x = element_line(color="gray"),
       axis.title.y = element_text(margin = margin(r=10)),
      )

