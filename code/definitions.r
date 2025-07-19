# definitions to support scripts to process Ecology data and produce
# visualizations



# variable labels to use for y axis label - matches order of varList (defined
# in b_compile_annual_netCDF.r)
varLabel <- c("PO4", "SiOH4", "NH4", "NO2", "NO3", "Transmission 25cm",
              "Beam Attenuation", "Fluorescence", "Turbidity", 
              "Dissolved Oxygen", "Salinity", "Density", "Conductivity",
              "Temperature")

# set location of local netCDF files based on which system is running the code
sysname <- Sys.info()["sysname"]
nodename <- Sys.info()["nodename"]
if (sysname == "Windows") {    # DNR Windows laptop
  local_netCDF_home <- 'C:\\Users\\pdow490\\Documents\\HSIL_local\\Ecology_data\\downloads_netCDF'
  home_local <- 'C:/Users/pdow490/Documents/HSIL_local/Ecology_data/'
  nrr_data_path <- 'C:/Users/pdow490/OneDrive/Work-current/HSI/synthesis_existing_env_data/Ecy_MarineMonitoring/data/Ecology_NRR_data_2018'
  sepsym <- "\\"
} else if (sysname == "Darwin" & nodename == "Family-Mac-mini.local") { 
  local_netCDF_home <- '/Users/pete/work_local/Ecy_MarineMonitoring/downloads_netCDF'
  home_local <- '/Users/pete/work_local'
  nrr_data_path <- '/Users/pete/OneDrive/Work-current/HSI/synthesis_existing_env_data/Ecy_MarineMonitoring_MacMini/data/Ecology_NRR_data_2018'
  sepsym <- "/"
} else if (sysname == "Darwin" & nodename == "Petes-MacBook-Pro.local") {
  local_netCDF_home <- '/Users/peterdowty/work_local/Ecy_MarineMonitoring/downloads_netCDF'
  home_local <- '/Users/peterdowty/work_local'
  nrr_data_path <- '/Users/peterdowty/OneDrive/Work-current/HSI/synthesis_existing_env_data/Ecy_MarineMonitoring/data/Ecology_NRR_data_2018'
  sepsym <- "/"
} else {
  print("Error identifying system.")
}
