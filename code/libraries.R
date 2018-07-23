#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# libraries.R
# Desc - Contains libraries used in workspace
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
library(gridExtra)
library(RSQLite)
library(sqldf)
library(psych)
library(mgcv)
library(zoo)

years.all <- c("2014", "2015", "2016", "2017")

months.all <- c("January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December")

# Merging land use column
system.time(epa_site_info <-read.csv("data/aqs_sites.csv", stringsAsFactors = FALSE))
testdf <- NULL

# system.time(testdf <- semi_join(hourly.pm25.FRM.14_17, epa_site_info, by = c("Latitude", "Longitude")))

system.time(testdf <- hourly.pm25.FRM.14_17 %>%
              right_join(epa_site_info, by = c("Latitude", "Longitude")))




