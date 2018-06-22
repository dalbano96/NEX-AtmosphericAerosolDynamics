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

#--------------------------------------------------------------#
# Functions
#--------------------------------------------------------------#
plot.linechart <- function(data, data.date, data.poc) {
  data %>%
    subset(Date.Local == data.date & POC == data.poc) %>%
    ggplot(aes(x = Time.Local, y = Sample.Measurement, group = Site.Num, color = as.character(Site.Num))) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Local time", y = "Micrograms/cubic meter", color = "Sites") +
    ggtitle(paste0(data.date, " ", unique(select(data, State.Name))))
}

#--------------------------------------------------------------#
# Map site locations containing PM2.5 FRM/FEM data
#--------------------------------------------------------------#
leaflet(unique(select(hourly.pm25.FRM.14_17, c(Longitude, Latitude, Site.Num, County.Name, POC)))) %>%
  addCircles(~Longitude, ~Latitude, 
             label = ~paste("Site Num: ", Site.Num, ",",
                           "County Name: ", County.Name, ", ",
                           "POC: ", POC)) %>%
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
                     Site.Num == 11 |
                     Site.Num == 12 |
                     Site.Num == 9) & 
                    (County.Name == "San Francisco" |
                       County.Name == "Alameda"))

# Plot site data
sf_oak.plot <- plot.linechart(sf_oak.sites, observed.date, 3)
sf_oak.plot

#--------------------------------------------------------------#
# Hourly Data of New York City region
#--------------------------------------------------------------#
# Extract data from local sites
nyc.sites <- subset(hourly.pm25.FRM.14_17, 
                       (Site.Num == 110 |
                          Site.Num == 124) & 
                         (County.Name == "Bronx" |
                            County.Name == "Queens"))

# Plot site data
nyc.plot <- plot.linechart(nyc.sites, observed.date, 4)
nyc.plot

#--------------------------------------------------------------#
# Hourly Data of Hawaii region
#--------------------------------------------------------------#
# Extract data from local sites
hi.sites <- subset(hourly.pm25.FRM.14_17,
                   (Site.Num == 1006 |
                      Site.Num == 7 |
                      Site.Num == 2023 |
                      Site.Num == 2016 |
                      Site.Num == 2020 |
                      Site.Num == 1012) &
                     (County.Name == "Hawaii"))

# Plot the site data
hi.plot <- plot.linechart(hi.sites, observed.date, 1)
hi.plot

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
