###############################################################################
#  Create heat maps showing Spearman correlation significance with
#  station along y-axis, month along x-axis and a separate panel for each
#  parameter.
#
#  Input data must already be loaded into the R workspace.
#
#  October 2025
###############################################################################

library(tidyverse)


# select 9 variables of interest
selected_vars <- c("Temp", "Salinity", "NO3", "Turb", "FluorAdjusted",
                   "Xmiss_25cm")

# filter data for these selected variables 
# convert significance category to a factor for use with fill
# convert Station variable to factor in alpha order
stn_levels <- sort(unique(spearman.months.out2$Station))
spearman.out.9vars <- spearman.months.out2 %>%
  filter(parameter %in% selected_vars) %>%
  mutate(sig_category = factor(sig_category, levels=c(-3,-2,-1,0,1,2,3)),
         Station = factor(Station, levels=stn_levels, ordered=TRUE))

color_vector <- c("-3" = "#A23196", "-2" = "#CC9DCE", "-1" = "#E3D6EB",
                  "0" = "white",
                  "1" = "#EFE3D4", "2" = "#EDBF8B", "3" = "#E97E09")



p <- ggplot(data=spearman.out.9vars,
            mapping = aes(x=month, y=Station, fill=sig_category)) +
     geom_tile() +
     facet_wrap(vars(parameter)) +
     theme_bw() +
     theme(
      panel.grid.major.x = element_line(color = "gray50"),
      panel.grid.minor.x = element_line(color = "gray85"),
      panel.grid.major.y = element_line(color = "gray90")
     ) +
     scale_x_continuous(breaks = c(1,3,6,9,12),
                        minor_breaks = c(2,4,5,7,8,10,11)) +
     scale_y_discrete(limits=rev) +
     scale_fill_manual(values = color_vector, na.value="gray30",
                       name = "Significance Category",
                       labels = c("p<0.01 (neg)",
                                 "p<0.05 (neg)",
                                 "ns (neg)",
                                 "0 correlation",
                                 "ns (pos)",
                                 "p<0.05 (pos)",
                                 "p<0.01 (pos)"))






