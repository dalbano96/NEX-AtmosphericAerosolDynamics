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
             label = ~paste("[EPA] Site Num: ", Site.Num, ", ",
                            "County Name: ", County.Name)) %>%
  addCircles(data = unique(select(aeronet.sites, 
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

tempdf <- bind_rows(subset(hourly.pm25.FRM.14_17, Site.Num == 16 & County.Name == "Washoe"),
                    subset(hourly.pm25.FRM.14_17, Site.Num == 22 & County.Name == "Washoe"))
tempdf %>%
  ggplot
