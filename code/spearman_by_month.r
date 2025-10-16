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

# filter data for these selected variables; and convert significance category
# to a factor for use with fill
spearman.out.9vars <- spearman.months.out %>%
  filter(parameter %in% selected_vars) %>%
  mutate(sig_category = factor(sig_category, levels=c(0,1,2)))



p <- ggplot(data=spearman.out.9vars,
            mapping = aes(x=month, y=Station, fill=sig_category)) +
     geom_tile() +
     facet_wrap(vars(parameter)) +
     scale_fill_manual(values=c("white","gray80","gray30","white"))



