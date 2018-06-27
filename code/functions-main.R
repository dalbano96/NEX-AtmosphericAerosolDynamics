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
# Functions
#--------------------------------------------------------------#
plot.linechart <- function(data, data.date, data.method.code) {
  data %>%
    subset(Date.Local == data.date & Method.Code == data.method.code) %>%
    ggplot(aes(x = Time.Local, y = Sample.Measurement, group = Site.Num, color = as.character(Site.Num))) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Local time", y = "Micrograms/cubic meter", color = "Sites") +
    ggtitle(paste0(data.date, ", ", data$State.Name, ", Method Code: ", data.method.code))
}
