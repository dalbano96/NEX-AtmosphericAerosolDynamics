#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# map_functions.R
# Desc - Loads map data with site information for EPA and AERONET
#--------------------------------------------------------------#

aeronet.site.txt <- c("data/aeronet_locations_v3_2014_lev20.txt", 
                      "data/aeronet_locations_v3_2015_lev20.txt", 
                      "data/aeronet_locations_v3_2016_lev20.txt",
                      "data/aeronet_locations_v3_2017_lev20.txt")

aod.2014 <- read.delim("data/aeronet_locations_v3_2014_lev20.txt", header = TRUE, sep = ",")
aod.2015 <- read.delim("data/aeronet_locations_v3_2015_lev20.txt", header = TRUE, sep = ",")
aod.2016 <- read.delim("data/aeronet_locations_v3_2016_lev20.txt", header = TRUE, sep = ",")
aod.2017 <- read.delim("data/aeronet_locations_v3_2017_lev20.txt", header = TRUE, sep = ",")

#--------------------------------------------------------------#
# Map all site locations
# Used to determine site locations to analyze
#--------------------------------------------------------------#
leaflet(unique(select(subset(hourly.pm25.FRM.14_17), c(Longitude, Latitude, Site.Num, County.Name)))) %>%
  addCircles(~Longitude, ~Latitude,
             label = ~paste("[EPA] Site Num: ", Site.Num, ", ",
                            "County Name: ", County.Name)) %>%
  addCircles(data = unique(select(aod.2017, 
                                  c(Longitude.decimal_degrees.,
                                    Latitude.decimal_degrees.,
                                    Site_Name))),
             lng = ~Longitude.decimal_degrees.,
             lat = ~Latitude.decimal_degrees.,
             color = "red",
             label = ~paste("[AERONET] Site Name: ", Site_Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar()

# Get list of methods from site
get.site_methods <- function(site_num, county_name) {
  unique(select(subset(hourly.pm25.FRM.14_17, Site.Num == site_num &
                County.Name == county_name),
         c(Site.Num, County.Name, Method.Code, Method.Type, Method.Name, POC)))
}

get.site_methods(306, "San Bernardino")
get.site_methods(5, "Santa Clara")
get.site_methods(6, "Santa Clara")

get.site_methods(16, "Washoe")
get.site_methods(22, "Washoe")
get.site_methods(1005, "Washoe")
get.site_methods(1007, "Washoe")

