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

# Filters months by season automatically?
yq <- as.yearqtr(as.yearmon(pm_sites.hawaii$DateTime.Local, "%m/%d/%Y") + 1/12)
pm_sites.hawaii$Season <- factor(format(yq, "%q"), levels = 1:4, 
                        labels = c("Winter", "Spring", "Summer", "Fall"))


