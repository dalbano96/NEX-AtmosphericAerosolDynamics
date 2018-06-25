#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
#--------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(geojsonR)
library(leaflet)
library(sp)

#--------------------------------------------------------------#
# Read csv to data frame
#--------------------------------------------------------------#
hourly.pm25.FRM.2014 <- read.csv("data/hourly_88101_2014.csv", stringsAsFactors = FALSE)
hourly.pm25.FRM.2015 <- read.csv("data/hourly_88101_2015.csv", stringsAsFactors = FALSE)
hourly.pm25.FRM.2016 <- read.csv("data/hourly_88101_2016.csv", stringsAsFactors = FALSE)
hourly.pm25.FRM.2017 <- read.csv("data/hourly_88101_2017.csv", stringsAsFactors = FALSE)

hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.2014, hourly.pm25.FRM.2015)
hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.14_17, hourly.pm25.FRM.2016)
hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.14_17, hourly.pm25.FRM.2017)

hourly.AOD.AMES.14_17 <- read.csv("data/20140101_20171231_NASA_Ames/20140101_20171231_NASA_Ames.csv", stringsAsFactors = FALSE)

#--------------------------------------------------------------#
# Functions
#--------------------------------------------------------------#
plot.linechart <- function(data, data.date, data.method.code) {
  data.map <- data %>%
    subset(Date.Local == data.date & Method.Code == data.method.code) %>%
    ggplot(aes(x = Time.Local, y = Sample.Measurement, group = Site.Num, color = as.character(Site.Num))) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Local time", y = "Micrograms/cubic meter", color = "Sites") +
    ggtitle(paste0(data.date, ", ", data$State.Name, ", Method Code: ", data.method.code))
  return data.map
}

#--------------------------------------------------------------#
# Map all site locations
# PM2.5 FRM/FEM data
# Method.Code - 170
# AERONET Site - NASA_Ames
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
  addProviderTiles(providers$CartoDB.Positron)

#--------------------------------------------------------------#
# Map site locations for AERONET AOD sites
#--------------------------------------------------------------#
leaflet(unique(select(hourly.AOD.AMES.14_17, c(Site_Latitude.Degrees., Site_Longitude.Degrees., AERONET_Site_Name)))) %>%
  addCircles(~Site_Longitude.Degrees., ~Site_Latitude.Degrees., 
             label = ~paste("Site Name: ", AERONET_Site_Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron)

#--------------------------------------------------------------#
# Date to observe (2014-01-01 to 2017-12-31)
#--------------------------------------------------------------#
observed.date <- "2017-09-18"

#--------------------------------------------------------------#
# Hourly Data of San Francisco-Oakland region
#--------------------------------------------------------------#
# Extract data from local sites
sf_oak.sites <- subset(hourly.pm25.FRM.14_17, 
                  (Site.Num == 5 |
                     Site.Num == 1001 |
                     Site.Num == 6) & 
                    (County.Name == "San Mateo" |
                       County.Name == "Santa Clara"))

# Plot site data
sf_oak.method.code = 170
sf_oak.plot.linechart <- plot.linechart(sf_oak.sites, observed.date, sf_oak.method.code)
sf_oak.plot.linechart

#--------------------------------------------------------------#
# # Hourly Data of New York City region
# #--------------------------------------------------------------#
# # Extract data from local sites
# nyc.sites <- subset(hourly.pm25.FRM.14_17, 
#                        (Site.Num == 110 |
#                           Site.Num == 124) & 
#                          (County.Name == "Bronx" |
#                             County.Name == "Queens"))
# 
# # Plot site data
# nyc.plot <- plot.linechart(nyc.sites, observed.date, 4)
# nyc.plot
# 
# #--------------------------------------------------------------#
# # Hourly Data of Hawaii region
# #--------------------------------------------------------------#
# # Extract data from local sites
# hi.sites <- subset(hourly.pm25.FRM.14_17,
#                    (Site.Num == 1006 |
#                       Site.Num == 7 |
#                       Site.Num == 2023 |
#                       Site.Num == 2016 |
#                       Site.Num == 2020 |
#                       Site.Num == 1012) &
#                      (County.Name == "Hawaii"))
# 
# # Plot the site data
# hi.plot <- plot.linechart(hi.sites, observed.date, 1)
# hi.plot

#--------------------------------------------------------------#
# Notes
#--------------------------------------------------------------#
(count(select(subset(hourly.pm25.FRM.14_17, Sample.Measurement < 0.00), Sample.Measurement)) / count(hourly.pm25.FRM.14_17)) * 100
unique(select(subset(hourly.pm25.FRM.14_17, State.Name == "New York"), POC))

#--------------------------------------------------------------#
# Map ALL active site locations
#--------------------------------------------------------------#
# aqsSites.all <- read.csv("data/aqs_sites.csv", stringsAsFactors = FALSE)
# aqsSites.open <- subset(aqsSites.all, Site.Closed.Date == "")
# leaflet(unique(select(subset(aqsSites.open, !is.null(Site.Closed.Date)),c(Longitude, 
#                                                                           Latitude, 
#                                                                           Site.Number, 
#                                                                           County.Name)))) %>%
#   addCircles(~Longitude, ~Latitude, 
#              label = ~paste("Site Num: ", Site.Number, ";",
#                             "County Name: ", County.Name)) %>%
#   addTiles() %>%
#   addProviderTiles(providers$CartoDB.Positron)



#--------------------------------------------------------------#
# Comment here
#--------------------------------------------------------------#
