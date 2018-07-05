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
aod.2014$Year <- add.year(aod.2014, 2014)
aod.2015$Year <- add.year(aod.2015, 2015)
aod.2016$Year <- add.year(aod.2016, 2016)
aod.2017$Year <- add.year(aod.2017, 2017)

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
leaflet(unique(select(hourly.pm25.FRM.14_17, c(Longitude, Latitude, Site.Num, County.Name)))) %>%
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
             radius = 5000,
             label = ~paste("[AERONET] Site Name: ", Site_Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar()

#--------------------------------------------------------------#
# Map site locations based on specified date range
# Used to determine site locations to analyze
# THE PROBLEM IS FILTERING THE AOD SITES (by year, not date)
# TODO: FIGURE OUT A WAY TO MANAGE OVERLAPPING POINTS
#--------------------------------------------------------------#
df <- unique(select(subset(hourly.pm25.FRM.14_17,
                           DateTime.Local >= "2015-01-01" &
                           DateTime.Local <= "2015-12-31"),
                    c(Longitude, Latitude, Site.Num, County.Name, POC)))

# Test
head(unique(select(subset(hourly.pm25.FRM.14_17, Site.Num == 1002 & County.Name == "San Joaquin"), POC)), n = 9)

map_by_year <- function(start_date, end_date, year) {
  leaflet(unique(select(subset(hourly.pm25.FRM.14_17,
                               DateTime.Local >= start_date & DateTime.Local <= end_date), 
                        c(Longitude, Latitude, Site.Num, County.Name)))) %>%
    addCircles(~Longitude, ~Latitude,
               label = ~paste("[EPA] Site Num: ", Site.Num, ", ",
                              "County Name: ", County.Name)) %>%
    addCircles(data = unique(select(subset(aod.sites, Year == year), 
                                    c(Longitude.decimal_degrees.,
                                      Latitude.decimal_degrees.,
                                      Site_Name))),
               lng = ~Longitude.decimal_degrees.,
               lat = ~Latitude.decimal_degrees.,
               color = "red",
               radius = 5000,
               label = ~paste("[AERONET] Site Name: ", Site_Name)) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addScaleBar()
}

# View list of available sites for particular year
map_by_year("2014-01-01", "2014-12-31", "2014")
map_by_year("2015-01-01", "2015-12-31", "2015")
map_by_year("2016-01-01", "2016-12-31", "2016")
map_by_year("2017-01-01", "2017-12-31", "2017")

# Get list of POC from site
get.POC <- function(site_num, county_name) {
  unique(select(subset(hourly.pm25.FRM.14_17, Site.Num == site_num &
                County.Name == county_name),
         c(POC)))
}

get.POC(1001, "San Mateo")
get.POC(5, "Santa Clara")
get.POC(6, "Santa Clara")

get.POC(16, "Washoe")
get.POC(22, "Washoe")
get.POC(1005, "Washoe")
get.POC(1007, "Washoe")

