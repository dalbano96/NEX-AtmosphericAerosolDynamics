#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# AERONET-AOD-main.R
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
# Read AOD csv to data frame
#--------------------------------------------------------------#
# importing and formatting AOD datasets
hourly.AOD.AMES.14_17 <- NULL
hourly.AOD.AMES.14_17 <- read.csv("data/20140101_20171231_NASA_Ames/20140101_20171231_NASA_Ames.csv", stringsAsFactors = FALSE)

# Change date format of Date.GMT
hourly.AOD.AMES.14_17$Date.dd.mm.yyyy. <- as.Date(hourly.AOD.AMES.14_17$Date.dd.mm.yyyy., format = "%m/%d/%y")

# Rename date and time to Date.GMT and Time.GMT
hourly.AOD.AMES.14_17 <- rename(hourly.AOD.AMES.14_17, Date.GMT = Date.dd.mm.yyyy.)
hourly.AOD.AMES.14_17 <- rename(hourly.AOD.AMES.14_17, Time.GMT = Time.hh.mm.ss.)

# Removes minutes and seconds from Time.GMT
hourly.AOD.AMES.14_17$Time.GMT <- strptime(hourly.AOD.AMES.14_17$Time.GMT, format = "%H")
# Removes date portion and formats Time.GMT as character type
hourly.AOD.AMES.14_17$Time.GMT <- format(hourly.AOD.AMES.14_17$Time.GMT, format = "%H:%M:%S")

# Joining date and time into single column
hourly.pm25.FRM.14_17$DateTime.GMT <- as.POSIXct(paste(hourly.pm25.FRM.14_17$Date.GMT, hourly.pm25.FRM.14_17$Time.GMT), format = "%Y-%m-%d %H:%M")
hourly.AOD.AMES.14_17$DateTime.GMT <- as.POSIXct(paste(hourly.AOD.AMES.14_17$Date.GMT, hourly.AOD.AMES.14_17$Time.GMT), format = "%Y-%m-%d %H")

# Setting all -999 to NA
hourly.AOD.AMES.14_17 <- hourly.AOD.AMES.14_17 %>%
  mutate(AOD_1640nm = replace(AOD_1640nm, AOD_1640nm == -999, NA)) %>%
  mutate(AOD_1020nm = replace(AOD_1020nm, AOD_1020nm == -999, NA)) %>%
  mutate(AOD_870nm = replace(AOD_870nm, AOD_870nm == -999, NA)) %>%
  mutate(AOD_675nm = replace(AOD_675nm, AOD_675nm == -999, NA)) %>%
  mutate(AOD_500nm = replace(AOD_500nm, AOD_500nm == -999, NA)) %>%
  mutate(AOD_440nm = replace(AOD_440nm, AOD_440nm == -999, NA)) %>%
  mutate(AOD_380nm = replace(AOD_380nm, AOD_380nm == -999, NA)) %>%
  mutate(AOD_340nm = replace(AOD_340nm, AOD_340nm == -999, NA)) %>%
  mutate(X440.870_Angstrom_Exponent = replace(X440.870_Angstrom_Exponent, X440.870_Angstrom_Exponent == -999, NA))

ggplot(subset(hourly.AOD.AMES.14_17, DateTime.GMT >= "2015-08-01" & DateTime.GMT <= "2015-12-31"),
       aes(x = DateTime.GMT, y = X440.870_Angstrom_Exponent)) +
  geom_point()

# Graph of AOD
ggplot(subset(hourly.AOD.AMES.14_17, Date.GMT > "2015-08-15" & Date.GMT < "2015-09-13")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_1640nm, color = "AOD_1640nm")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_1020nm, color = "AOD_1020nm")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_870nm, color = "AOD_870nm")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_675nm, color = "AOD_675nm")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_500nm, color = "AOD_500nm")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_440nm, color = "AOD_440nm")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_380nm, color = "AOD_380nm")) +
  geom_point(aes(x = DateTime.GMT, y = AOD_340nm, color = "AOD_340nm"))

# Collecting needed columns
testdf <- NULL
testdf <- hourly.AOD.AMES.14_17[, c(114,5:7,10,19,22,25,26)]
testdf <- gather(testdf, AOD, value, -DateTime.GMT)

# 2015
ggplot(subset(testdf, DateTime.GMT > "2015-01-01" & DateTime.GMT < "2015-12-31"),
       aes(x = DateTime.GMT, y = value, group = AOD, color = AOD)) +
  geom_point() +
  ggtitle("2015")

# 2016
ggplot(subset(testdf, DateTime.GMT > "2016-01-01" & DateTime.GMT < "2016-12-31"),
       aes(x = DateTime.GMT, y = value, group = AOD, color = AOD)) +
  geom_point() +
  ggtitle("2016")

#--------------------------------------------------------------#
# Map site locations for AERONET AOD sites
#--------------------------------------------------------------#
leaflet(unique(select(hourly.AOD.AMES.14_17, c(Site_Latitude.Degrees., Site_Longitude.Degrees., AERONET_Site_Name)))) %>%
  addCircles(~Site_Longitude.Degrees., ~Site_Latitude.Degrees., 
             label = ~paste("Site Name: ", AERONET_Site_Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron)



