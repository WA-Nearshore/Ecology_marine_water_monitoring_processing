# read in HSIL station csv with distance from ocean, site groupings and
# yvalues for visualization.

library(tidyverse)

sites <- read.csv("output_tables/ecy_stations_distances_groups_ypos.csv",
                  stringsAsFactors=FALSE)


p <- ggplot(data=sites,
            mapping=aes(x=dist_km, y=yposition, fill=branching, color=branching),
            group=branching) +
     geom_path() +
     geom_point() +
     theme_bw() +
     scale_y_continuous(limits = c(8,12))