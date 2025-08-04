################################################################################
# Create seasonal graphs
#
# The following objects must exist in the current session workspace:
#   ecy_var_graphing  Data to be graphed, created in f_seasonal_graphs_data_prep.r
#                     Contains data for selected variable only.
#   varList  List of variables in Ecology data, created in 
#            b_compile_annual_netCDF.r
#
#  Note - this should be restructured so there is a function that can be called
#  even from command line to create seasonal graph for specified site and
#  specified profile reduction method. Perhaps add a depth band specifier.
#
# July 2024
################################################################################

source("code/definitions.r")


# add day-of-year for consistent x-axis values across different years
ecy_var_graph2 <- ecy_var_graphing %>%
   mutate(day_of_yr = yday(date))

# for x axis labeling, get day-of-year for first day of each month
firsts <- data.frame(year=rep(2001,times=12),
                     month=seq(1,12,by=1),
                     day=rep(1,times=12))
firsts <- firsts %>%
  mutate(date = ymd(paste(year,month,day,sep=" ")),
         day_of_yr = yday(date))


################################################################################
# Construct graphs for values at 0.5m, mean of 0.5 to 5m, diff 0.5 & 5m
################################################################################

# get enhanced station table exported from GIS with subgroup attribute and use
# to filter for HSIL study area sites only
stn_tbl_path <- str_c("output_tables", "ecy_stations_subgroups_tbl.csv", sep= "/") 
stn_tbl <- read.csv(stn_tbl_path, stringsAsFactors=FALSE)
# filter for HSIL stations
hsil_recs <- stations %>% filter(subgroup == "HSIL_study_area")
hsil_stns <- hsil_recs$Station

# loop through stations here
for (istn in hsil_stns) {

  # add factor to distinguish selected site, and site-yr combo for grouping 
  # annual lines
  ecy_var_graph2 <- ecy_var_graph2 %>%
    mutate(aes_groups = ifelse(Station==istn,"yes","no"),
           site_yr = str_c(Station,year,sep="_")) %>%
    arrange(date)
  # change site grouping (selected / not-selected) to factor to control drawing
  # order so selected site graphed last.
  ecy_var_graph2$aes_groups <- factor(ecy_var_graph2$aes_groups, 
                                      levels=c("no","yes"))
  
  # pivot longer so the 3 profile summary methods in 3 columns becomes one column
  ecy_var_graph_long <- ecy_var_graph2 %>%
      pivot_longer(Var0.5:VarDiff, names_to="summary_method", values_to="Value")
  
  
  
  # construct y-axis title element for units
  units <- str_c("(",var_metadata$units[var_index],")", sep="")

  # construct theme to be used for each of the 3 profile reduction methods
  myTheme <-  theme_bw() +
    theme(
      axis.title.x = element_text(margin = margin(t=5), size=6),
      axis.title.y = element_text(margin = margin(r=5), size=6),
      axis.text.x = element_text(size=5),
      axis.text.y = element_text(size=5),
      title = element_text(size=6),
      panel.grid.major.x = element_line(linewidth=0.1, color="gray80"),
      panel.grid.major.y = element_line(linewidth=0.1, color="gray80"),
      panel.grid.minor.y = element_line(linewidth=0.1),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
    )
  # shared x-axis labels
  myLabels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
           "Aug","Sep","Oct","Nov","Dec") 

  # set up vectors to drive loop to make the 3 graphs
  title_snip <- c("at 0.5m", "mean over 0.5 to 5m", "Diff 0.5 & 5m")
  fname_elem <- c("05", "ave05to5", "diff05and5")
  var_names <- c("Var0.5", "Var05to5", "VarDiff")
  
  for (ireduce in seq_along(title_snip)) { 
     ytitle <- str_c(varLabel[var_index],title_snip[ireduce],units, sep=" ")
     graph_title <- str_c(istn,varLabel[var_index],title_snip[ireduce],
                          sep="    ")
     
     # filter data for specific profile summary method indicated by ireduce
     final_graph_data <- ecy_var_graph_long %>%
         filter(summary_method == var_names[ireduce])
  
     p <- ggplot(data = final_graph_data,
                 mapping = aes(x=day_of_yr, y=Value, group=site_yr)) +
       geom_path(data = subset(final_graph_data, aes_groups=="no"), color="gray70",
                 linewidth=0.1) +
       geom_point(data = subset(final_graph_data, aes_groups=="no"),
                  color="gray70", size=0.1) +
       geom_path(data = subset(final_graph_data, aes_groups=="yes"), 
                 color="black", linewidth=0.25) +
       geom_point(data = subset(final_graph_data, aes_groups=="yes"),
                  color="black", size=0.2) +
       myTheme + 
       scale_x_continuous(name="month", breaks=firsts$day_of_yr, labels=myLabels) +
       scale_y_continuous(name=ytitle) +
       ggtitle(graph_title)
  
     fname1 <- str_c("seas", fname_elem[ireduce], varList[var_index], istn, sep="_")
     fname2 <- str_c(fname1, ".png", sep="")
     path <- str_c("output_graphs", sep="/")
     outfilename <- str_c(path, fname2, sep="/")
     ggsave(p, filename=outfilename, width=5,  height=2.5, unit="in", dpi=200)
  }  # close for loop through profile reduction methods
  

}  # close for loop through stations

# clean up
rm(ecy_var_graph2, firsts, units, myTheme)
rm(myLabels, title_snip, fname_elem, ytitle, p)
rm(fname1, fname2, outfilename)

