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
library(lubridate)
library(reshape2)

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

# Convert Date columns to date objects
hourly.pm25.FRM.14_17$Date.Local <- as.Date(hourly.pm25.FRM.14_17$Date.Local, format = "%F")
hourly.pm25.FRM.14_17$Date.GMT <- as.Date(hourly.pm25.FRM.14_17$Date.GMT, format = "%F")

# importing and formatting AOD datasets
hourly.AOD.AMES.14_17 <- NULL
hourly.AOD.AMES.14_17 <- read.csv("data/20140101_20171231_NASA_Ames/20140101_20171231_NASA_Ames.csv", stringsAsFactors = FALSE)

# Change date format of Date.Local
hourly.AOD.AMES.14_17$Date.dd.mm.yyyy. <- as.Date(hourly.AOD.AMES.14_17$Date.dd.mm.yyyy., format = "%m/%d/%y")
# Rename date and time to Date.Local and Time.Local
hourly.AOD.AMES.14_17 <- rename(hourly.AOD.AMES.14_17, Date.Local = Date.dd.mm.yyyy.)
hourly.AOD.AMES.14_17 <- rename(hourly.AOD.AMES.14_17, Time.Local = Time.hh.mm.ss.)

# Removes minutes and seconds from Time.Local
hourly.AOD.AMES.14_17$Time.Local <- strptime(hourly.AOD.AMES.14_17$Time.Local, format = "%H")
# Removes date portion and formats as character
hourly.AOD.AMES.14_17$Time.Local <- format(hourly.AOD.AMES.14_17$Time.Local, format = "%H:%M:%S")

# Joining date and time into single column
hourly.pm25.FRM.14_17$DateTime.Local <- as.POSIXct(paste(hourly.pm25.FRM.14_17$Date.Local, hourly.pm25.FRM.14_17$Time.Local), format = "%Y-%m-%d %H:%M")
hourly.AOD.AMES.14_17$DateTime.Local <- as.POSIXct(paste(hourly.AOD.AMES.14_17$Date.Local, hourly.AOD.AMES.14_17$Time.Local), format = "%Y-%m-%d %H")

# Setting all -999 to NA
hourly.AOD.AMES.14_17 <- hourly.AOD.AMES.14_17 %>%
  mutate(AOD_1640nm = replace(AOD_1640nm, AOD_1640nm == -999, NA)) %>%
  mutate(AOD_1020nm = replace(AOD_1020nm, AOD_1020nm == -999, NA)) %>%
  mutate(AOD_870nm = replace(AOD_870nm, AOD_870nm == -999, NA)) %>%
  mutate(AOD_675nm = replace(AOD_675nm, AOD_675nm == -999, NA)) %>%
  mutate(AOD_500nm = replace(AOD_500nm, AOD_500nm == -999, NA)) %>%
  mutate(AOD_440nm = replace(AOD_440nm, AOD_440nm == -999, NA)) %>%
  mutate(AOD_380nm = replace(AOD_380nm, AOD_380nm == -999, NA)) %>%
  mutate(AOD_340nm = replace(AOD_340nm, AOD_340nm == -999, NA))

# Collecting needed columns
testdf <- hourly.AOD.AMES.14_17[, c(1,2,5:7,10,19,22,25,26,114)]

ggplot(subset(testdf, Date.Local > "2015-08-15" & Date.Local < "2015-09-13")) +
  geom_point(aes(x = DateTime.Local, y = AOD_1640nm, color = "AOD_1640nm")) +
  geom_point(aes(x = DateTime.Local, y = AOD_1020nm, color = "AOD_1020nm")) +
  geom_point(aes(x = DateTime.Local, y = AOD_870nm, color = "AOD_870nm")) +
  geom_point(aes(x = DateTime.Local, y = AOD_675nm, color = "AOD_675nm")) +
  geom_point(aes(x = DateTime.Local, y = AOD_500nm, color = "AOD_500nm")) +
  geom_point(aes(x = DateTime.Local, y = AOD_440nm, color = "AOD_440nm")) +
  geom_point(aes(x = DateTime.Local, y = AOD_380nm, color = "AOD_380nm")) +
  geom_point(aes(x = DateTime.Local, y = AOD_340nm, color = "AOD_340nm"))

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
observed.date <- "2016-09-22"

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

ggplot(data = subset(hourly.AOD.AMES.14_17, as.character(Date.Local) > "2015-09-01" & as.character(Date.Local) < "2015-09-08")) +
  geom_point(data = subset(sf_oak.sites, Date.Local > "2015-09-01" & Date.Local < "2015-09-08" & Method.Code == 170), 
             mapping = aes(x = Time.Local, y = Sample.Measurement, color = as.character(Date.Local))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

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
