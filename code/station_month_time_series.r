###############################################################################
# plot selected variable for selected month across years for 
# a) all Spearman stations
# b) all stations
#
# The input data 'ecy_long_mean_jn' must be already in the workspace.
# It is created in the code in file spearman_correlation_over_time_by_station.r
#
# October 2025
#
###############################################################################

library(tidyverse)


###############################################################################
# Time series for a single specified month 
###############################################################################

# set parameters for the graph
sel_month <- 10
sel_prm <- "Temp"


# filter for month and parameter
ecy_long_mean_jn_filt <- ecy_long_mean_jn %>%
  filter(month == sel_month, parameter == sel_prm)

##### first graph all stations regardless of data record lentth and span
p1 <- ggplot(data = ecy_long_mean_jn_filt,
             mapping = aes(x=date, y=prm_mean_val, group=Station)) +
      geom_line(color="gray60") +
      geom_point(color="gray60", size=1.0) +
      theme_bw() +
      scale_x_date(breaks=c(as.Date("2000-01-01"), as.Date("2005-01-01"),
                            as.Date("2010-01-01"), as.Date("2015-01-01"),
                            as.Date("2020-01-01"), as.Date("2025-01-01")),
                   date_labels="%Y", date_minor_breaks ="1 year",
                   name = "Date") +
      scale_y_continuous(name = "Mean Temperature 0-10m (deg C)")

##### second, filter to just stations used for Spearman correlations
ecy_long_mean_jn_filt2 <- ecy_long_mean_jn_filt %>%
  filter(n_months >= min_n_Spearman, yr_span >= min_yr_span)
p2 <- ggplot(data = ecy_long_mean_jn_filt2,
             mapping = aes(x=date, y=prm_mean_val, group=Station)) +
      geom_line(color="gray60") +
      geom_point(color="gray60", size=1.0) +
      theme_bw() +
      theme(
        axis.title.y = element_text(margin = margin(r=10)) 
      ) +
      scale_x_date(breaks=c(as.Date("2000-01-01"), as.Date("2005-01-01"),
                            as.Date("2010-01-01"), as.Date("2015-01-01"),
                            as.Date("2020-01-01"), as.Date("2025-01-01")),
                   date_labels="%Y", date_minor_breaks ="1 year",
                   name = "Date") +
      scale_y_continuous(name = "Mean Temperature 0-10m (deg C)")

########### third, use Spearman-filtered data, but now plot z-scores
# get station mean and sd
ecy_stn_stats <- ecy_long_mean_jn_filt2 %>%
  group_by(Station) %>%
  summarize(lt_mean = mean(prm_mean_val), lt_sd = sd(prm_mean_val))
# join back onto data and calc z-scores
ecy_long_z <- ecy_long_mean_jn_filt2 %>%
  left_join(ecy_stn_stats, by="Station") %>%
  mutate(zscore = (prm_mean_val - lt_mean)/lt_sd)

p3 <- ggplot(data = ecy_long_z,
             mapping = aes(x=date, y=zscore, group=Station)) +
      geom_hline(yintercept = 0, color="gray20") +
      geom_line(color="gray60") +
      geom_point(color="gray60", size=1.0) +
      theme_bw() +
      theme(
       panel.grid.major.x = element_line(color="gray"),
       axis.title.y = element_text(margin = margin(r=10))
      ) +
      scale_x_date(breaks=c(as.Date("2000-01-01"), as.Date("2005-01-01"),
                            as.Date("2010-01-01"), as.Date("2015-01-01"),
                            as.Date("2020-01-01"), as.Date("2025-01-01")),
                   date_labels="%Y", date_minor_breaks ="1 year",
                   name = "Date") +
      scale_y_continuous(name = "z-score")


###############################################################################
# Time series for specified 3 months as facets 
###############################################################################

# set parameters for the graphs
sel_months <- c(2,3,4)
month_labels <- c("February", "March", "April")
sel_prm <- "Temp"

# filter for months and parameter
ecy_long_mean_jn_filt <- ecy_long_mean_jn %>%
  filter(month %in% sel_months, parameter == sel_prm)

# filter to just stations used for Spearman correlations
ecy_long_mean_jn_filt2 <- ecy_long_mean_jn_filt %>%
  filter(n_months >= min_n_Spearman, yr_span >= min_yr_span)

# get station-month mean and sd
ecy_stn_stats <- ecy_long_mean_jn_filt2 %>%
  group_by(Station, month) %>%
  summarize(lt_mean = mean(prm_mean_val), lt_sd = sd(prm_mean_val))
# join back onto data and calc z-scores
ecy_long_z <- ecy_long_mean_jn_filt2 %>%
  left_join(ecy_stn_stats, by=join_by(Station, month)) %>%
  mutate(zscore = (prm_mean_val - lt_mean)/lt_sd)

ecy_long_z_fct <- ecy_long_z %>%
  mutate(month = factor(month))
levels(ecy_long_z_fct$month) <- month_labels

p4 <- ggplot(data = ecy_long_z_fct,
             mapping = aes(x=date, y=zscore, group=Station)) +
      geom_hline(yintercept = 0, color="gray20") +
      geom_line(color="gray60") +
      geom_point(color="gray60", size=1.0) +
      theme_bw() +
      theme(
       panel.grid.major.x = element_line(color="gray"),
       axis.title.y = element_text(margin = margin(r=10)),
       strip.text = element_text(size=12)
      ) +
      scale_x_date(breaks=c(as.Date("2000-01-01"), as.Date("2005-01-01"),
                            as.Date("2010-01-01"), as.Date("2015-01-01"),
                            as.Date("2020-01-01"), as.Date("2025-01-01")),
                   date_labels="%Y", date_minor_breaks ="1 year",
                   name = "Date") +
      scale_y_continuous(name = "z-score") +
      facet_wrap(vars(month), nrow=3)





