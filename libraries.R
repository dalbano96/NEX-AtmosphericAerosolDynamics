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

months <- c("January",
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

hours <- c("00:00",
           "01:00",
           "02:00",
           "03:00",
           "04:00",
           "05:00",
           "06:00",
           "07:00",
           "08:00",
           "09:00",
           "10:00",
           "11:00",
           "12:00",
           "13:00",
           "14:00",
           "15:00",
           "16:00",
           "17:00",
           "18:00",
           "19:00",
           "20:00",
           "21:00",
           "22:00",
           "23:00")
