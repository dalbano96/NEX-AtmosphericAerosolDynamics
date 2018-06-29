#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# map_functions.R
# Desc - Loads map data with site information for EPA and AERONET
#--------------------------------------------------------------#

aeronet.sites <- read.delim("data/aeronet_locations_v3_2015_lev20.txt", header = TRUE, sep = ",")

#--------------------------------------------------------------#
# Map all site locations
# PM2.5 FRM/FEM data
# Method.Code - 170
# Used to determine site locations to analyze
#--------------------------------------------------------------#
leaflet(unique(select(subset(hourly.pm25.FRM.14_17), c(Longitude, Latitude, Site.Num, County.Name)))) %>%
  addCircles(~Longitude, ~Latitude,
             label = ~paste("Site Num: ", Site.Num, ", ",
                            "County Name: ", County.Name)) %>%
  addCircles(data = unique(select(aeronet.sites, 
                                  c(Longitude.decimal_degrees.,
                                    Latitude.decimal_degrees.,
                                    Site_Name))),
             lng = ~Longitude.decimal_degrees.,
             lat = ~Latitude.decimal_degrees.,
             color = "red",
             label = ~paste("Site Name: ", Site_Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar()

get.summary <- function(site_num, county_name) {
  summary(subset(hourly.pm25.FRM.14_17, Site.Num = site_num, County.Name = county_name))
}

get.summary(1001, "San Mateo")
