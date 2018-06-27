#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# meteorological-main.R
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

hourly.rh_dp.2015 <- read.csv("data/hourly_RH_DP_2015.csv", stringsAsFactors = FALSE)


