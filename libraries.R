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



# TESTING SHIT
df <- NULL
df <- mv.sites
df$date.avg <- paste(mv.sites$Date.Local)
tapply(df$Sample.Measurement, df$date.local, FUN = mean)
