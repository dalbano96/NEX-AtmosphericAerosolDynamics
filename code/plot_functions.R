#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# plot_functions.R
# Desc - Holds general plotting functions
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# WORK IN PROGRESS
#--------------------------------------------------------------#
plot.linechart.AOD <- function(data, start.time, end.time) {
  data %>%
    na.omit %>%
    subset(data, DateTime.GMT >= start.time & DateTime.GMT <= end.time) %>%
    ggplot(aes(x = DateTime.GMT, y = X440.870_Angstrom_Exponent)) +
    geom_point(aes(color = DateTime.GMT))
}

tempdf <- subset(hourly.pm25.FRM.14_17, County.Name == "Washoe" & Site.Num == 16)

tempdf %>%
  subset(DateTime.Local >= "2016-01-01" & DateTime.Local <= "2016-12-31") %>%
  ggplot() +
  geom_point(aes(x = DateTime.Local, y = Sample.Measurement))


