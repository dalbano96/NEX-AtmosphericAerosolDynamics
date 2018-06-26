#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# maps-main.R
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
# Map all site locations
# PM2.5 FRM/FEM data
# Method.Code - 170
# Used to determine site locations to analyze
#--------------------------------------------------------------#
leaflet(unique(select(subset(hourly.pm25.FRM.14_17, Method.Code == 170), c(Longitude, Latitude, Site.Num, County.Name, Method.Code)))) %>%
  addCircles(~Longitude, ~Latitude,
             label = ~paste("Site Num: ", Site.Num, ", ",
                            "County Name: ", County.Name, ", ",
                            "Method Code: ", Method.Code)) %>%
  addCircles(data = unique(select(hourly.AOD.AMES.14_17, 
                                  c(Site_Latitude.Degrees.,
                                    Site_Longitude.Degrees., 
                                    AERONET_Site_Name))),
             lng = ~Site_Longitude.Degrees.,
             lat = ~Site_Latitude.Degrees.,
             color = "red",
             label = ~paste("Site Name: ", AERONET_Site_Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar()

