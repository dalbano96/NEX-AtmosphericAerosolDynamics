#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# maps-main.R
# Desc - Loads map data with site information for EPA and AERONET
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

aeronet.sites <- read.delim("data/aeronet_locations_v3_2015_lev20.txt", header = TRUE, sep = ",")

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
  addCircles(data = unique(select(aeronet.sites, 
                                  c(Longitude.decimal_degrees.,
                                    Latitude.decimal_degrees.,
                                    Site_Name))),
             lng = ~Longitude.decimal_degrees.,
             lat = ~Latitude.decimal_degrees.,
             color = "red",
             label = ~paste("Site Name: ", Site_Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar()

