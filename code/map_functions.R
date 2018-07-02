#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# map_functions.R
# Desc - Loads map data with site information for EPA and AERONET
#--------------------------------------------------------------#

add.year <- function(df, year) {
  rep(year, nrow(df))
}

#--------------------------------------------------------------#
# Load AERONET sites based on available AOD data by year
#--------------------------------------------------------------#
# aeronet.site.txt <- c("data/aeronet_locations_v3_2014_lev20.txt", 
#                       "data/aeronet_locations_v3_2015_lev20.txt", 
#                       "data/aeronet_locations_v3_2016_lev20.txt",
#                       "data/aeronet_locations_v3_2017_lev20.txt")

aod.2014 <- read.delim("data/aeronet_locations_v3_2014_lev20.txt", header = TRUE, sep = ",")
aod.2015 <- read.delim("data/aeronet_locations_v3_2015_lev20.txt", header = TRUE, sep = ",")
aod.2016 <- read.delim("data/aeronet_locations_v3_2016_lev20.txt", header = TRUE, sep = ",")
aod.2017 <- read.delim("data/aeronet_locations_v3_2017_lev20.txt", header = TRUE, sep = ",")

# head(aod.2014)
# head(aod.2015)
# head(aod.2016)
# head(aod.2017)

# Adds year column to df. Allows row binding
aod.2014$year <- add.year(aod.2014, 2014)
aod.2015$year <- add.year(aod.2015, 2015)
aod.2016$year <- add.year(aod.2016, 2016)
aod.2017$year <- add.year(aod.2017, 2017)

# head(aod.2014)
# head(aod.2015)
# head(aod.2016)
# head(aod.2017)

# aod.by_year <- c(aod.2014, aod.2015, aod.2016, aod.2017)
# aod.2014.new <- aod.2014
# aod.2014.new$year <- rep(2014, nrow(aod.2014.new))

# Combine all site locations to single df
aod.sites <- NULL
aod.sites <- bind_rows(aod.2014, aod.2015)
aod.sites <- bind_rows(aod.sites, aod.2016)
aod.sites <- bind_rows(aod.sites, aod.2017)

#--------------------------------------------------------------#
# Map all site locations from 2014-2017
# Used to determine site locations to analyze
#--------------------------------------------------------------#
leaflet(unique(select(subset(hourly.pm25.FRM.14_17), c(Longitude, Latitude, Site.Num, County.Name)))) %>%
  addCircles(~Longitude, ~Latitude,
             label = ~paste("[EPA] Site Num: ", Site.Num, ", ",
                            "County Name: ", County.Name)) %>%
  addCircles(data = unique(select(aod.sites, 
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

#--------------------------------------------------------------#
# WORK IN PROGRESS
# Map site locations based on specified date range
# Used to determine site locations to analyze
# THE PROBLEM IS FILTERING THE AOD SITES (by year, not date)
#--------------------------------------------------------------#
map_by_year <- function(df_pm, df_aod, start_date, end_date) {
  leaflet(unique(select(subset(df_pm, 
                               DateTime.Local >= start_date &
                                 DateTime.Local <= end_date), 
                        c(Longitude, Latitude, Site.Num, County.Name)))) %>%
    addCircles(~Longitude, ~Latitude,
               label = ~paste("[EPA] Site Num: ", Site.Num, ", ",
                              "County.Name: ", County.Name)) %>%
    addCircles(data = unique(select(subset(aod.sites, ))))
}

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

