#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-main.R
# Desc - Loads and tidys EPA PM2.5 data
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
# Read EPA csv to data frame
#--------------------------------------------------------------#
read_EPA_csv <- function(filename) {
  df <- NULL
  df <- read.csv(filename, stringsAsFactors = FALSE)
  df <- subset(df, Sample.Measurement >= 0.00)
  return(df)
}

hourly.pm25.FRM.2014 <- read_EPA_csv("data/hourly_88101_2014.csv")
hourly.pm25.FRM.2015 <- read_EPA_csv("data/hourly_88101_2015.csv")
hourly.pm25.FRM.2016 <- read_EPA_csv("data/hourly_88101_2016.csv")
hourly.pm25.FRM.2017 <- read_EPA_csv("data/hourly_88101_2017.csv")

hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.2014, hourly.pm25.FRM.2015)
hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.14_17, hourly.pm25.FRM.2016)
hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.14_17, hourly.pm25.FRM.2017)

# Convert Date columns to date objects
hourly.pm25.FRM.14_17$Date.Local <- as.Date(hourly.pm25.FRM.14_17$Date.Local, format = "%F")
hourly.pm25.FRM.14_17$Date.GMT <- as.Date(hourly.pm25.FRM.14_17$Date.GMT, format = "%F")

# Joining date and time into single column
hourly.pm25.FRM.14_17$DateTime.GMT <- as.POSIXct(paste(hourly.pm25.FRM.14_17$Date.GMT, 
                                                       hourly.pm25.FRM.14_17$Time.GMT), 
                                                 format = "%Y-%m-%d %H:%M")

hourly.pm25.FRM.14_17$DateTime.Local <- as.POSIXct(paste(hourly.pm25.FRM.14_17$Date.Local, 
                                                       hourly.pm25.FRM.14_17$Time.Local), 
                                                 format = "%Y-%m-%d %H:%M")

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
sf_oak.plot.linechart.pm25 <- plot.linechart.pm25(sf_oak.sites, 
                                        "2017-01-01", "2017-12-31", 
                                        sf_oak.method.code)
sf_oak.plot.linechart.pm25

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno, NV
#--------------------------------------------------------------#
# Site 22 missing from year > 2015
reno.site.num_list <- c(16, 22, 1005, 1007)
reno.site.county_name_list <- c("Washoe")
reno.sites <- subset(hourly.pm25.FRM.14_17, subset = Site.Num %in% reno.site.num_list &
                       County.Name %in% reno.site.county_name_list)

reno.method.code <- 170
reno.start_date <- "2016-01-01"
reno.end_date <- "2016-12-31"
rn.plot.linechart.pm25 <- plot.linechart.pm25(reno.sites,reno.start_date, reno.end_date, reno.method.code)
rn.plot.linechart.pm25

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
