###############################################################################
# Create table with list of Ecology data variables.
# Export as csv for use for filtering in Experience Builder app.
#
# July 2024
###############################################################################


varLabel <- c("PO4", "SiOH4", "NH4", "NO2", "NO3", "Transmission 25cm",
              "Beam Attenuation", "Fluorescence", "Turbidity", 
              "Dissolved Oxygen", "Salinity", "Density", "Conductivity",
              "Temperature")
varList <- c("PO4","SiOH4","NH4","NO2","NO3","Xmiss_25cm","BatC","FluorAdjusted",
             "Turb","DOAdjusted","Salinity","Density","Cond","Temp")

var_table <- data.frame(var_codes=varList, var_labels=varLabel)

write.csv(var_table, file="output_tables/var_table.csv", row.names=FALSE)
