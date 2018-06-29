#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# plot_functions.R
# Desc - Holds general plotting functions
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# @desc: Plots linechart for hourly trends of single day
#   for pm2.5
# @param:
#--------------------------------------------------------------#
plot.linechart.pm25 <- function(start.time, end.time, data.method.code, site.num_list, county.name_list) {
  hourly.pm25.FRM.14_17 %>%
    subset(subset = Site.Num %in% site.num_list & County.Name %in% county.name_list & DateTime.Local >= start.time & DateTime.Local <= end.time  & Method.Code == data.method.code) %>%
    ggplot(aes(x = DateTime.Local, y = Sample.Measurement, group = Site.Num, color = as.character(Site.Num))) +
    geom_line() + 
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Time", y = "Micrograms/cubic meter", color = "Sites") +
    ggtitle(paste0(hourly.pm25.FRM.14_17$State.Name, " [", start.time, " - ", end.time, "] ", "Method Code: ", data.method.code))
}

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


