#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# functions-main.R
#--------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(geojsonR)
library(leaflet)
library(sp)
library(lubridate)
library(reshape2)

#--------------------------------------------------------------#
# @desc: Plots linechart for hourly trends of single day
#   for pm2.5
# @param:
#--------------------------------------------------------------#
plot.linechart.pm25 <- function(data, start.time, end.time, data.method.code) {
  data %>%
    subset(DateTime.Local >= start.time & DateTime.Local <= end.time  & Method.Code == data.method.code) %>%
    ggplot(aes(x = DateTime.Local, y = Sample.Measurement, group = Site.Num, color = as.character(Site.Num))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Date/Time", y = "Micrograms/cubic meter", color = "Sites")
    # ggtitle(paste0(data.date, ", ", data$State.Name, ", Method Code: ", data.method.code))
}

plot.linechart.AOD <- function(data, start.time, end.time) {
  data %>%
    na.omit %>%
    subset(data, DateTime.GMT >= start.time & DateTime.GMT <= end.time) %>%
    ggplot(aes(x = DateTime.GMT, y = X440.870_Angstrom_Exponent)) +
    geom_point(aes(color = DateTime.GMT))
}
