# Synthesis of Ecology Marine Water Monitoring Data #
#### Nearshore Habitat Program ####
#### Washington State Department of Natural Resources ####

This code repository contains code to read, assemble, and work with netCDF data 
distributed by the Washington State Department of Ecology (Ecology) from 
their Marine Water Monitoring Program.

The source netCDF files are available from Ecology at: 
https://ecology.wa.gov/Research-Data/Monitoring-assessment/Puget-Sound-and-marine-monitoring/Water-column-data

The output from the code in this repository were used to create a web app for 
browsing the Ecology dataset:  
https://experience.arcgis.com/experience/94aa60295f53496fbd6327c829d18d8a

Some source code files listed below will require all code to be in a subfolder
'code' within the R working directory. Some will require subfolders to exist
for writing output:  output_graphs, output_tables.

## Source file descriptions

### Essential code files
* 'assemble_netCDF_data.r': Read & assemble data from netCDF files. The assembled data, a station table and variable metadata are created in the workspace.
* 'make_web_app_graphs.r': Orchestrate creation of various graphs for use in a web app to explore the data.
* 'station_floating_bar_line_ups.r':  Create vertical floating bar graphs to summarize data.

### Code files related to Spearman correlation analysis for trends
* 'spearman_correlation_over_time_by_station.r': Get Spearman correlation and significance by month and station.
* 'spearman_by_month_tile_graphs.r': Create heat map diagram for 6 variables to show significance by month.
* 'spearman_test.r': Conduct a Spearman test.
* 'station_month_time_series.r': Graph time series (data & z-values) for data subject here to Spearman.


### Secondary code files
* 'b_compile_annual_netCDF.r': Read & assemble netCDF files. Can be used directly, but intended to be called by assemble_netCDF_data.r.
* 'e_add_date_count_to_station_file.r': Adds data count by station for temperature to the station file. Called by assemble_netCDF_data.r
* 'd_calendar_graph.r': Makes data availability 'calendar' diagrams. Called by make_web_app_graphs.r.
* 'color_prof_histo_by_depth.r': Makes color profile diagrams. Called by make_web_app_graphs.r
* 'f_seasonal_graphs_data_prep.r': Prepare data for seasonal graphing. Called by make_web_app_graphs.r
* 'g_seasonal_graphs.r': Create seasonal graphs for all variables and all stations. Called by make_web_app_graphs.r

