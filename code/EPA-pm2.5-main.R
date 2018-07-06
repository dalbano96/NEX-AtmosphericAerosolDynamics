#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-main.R
# Desc - Loads and tidys EPA PM2.5 data
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# Function to read CSV file
#--------------------------------------------------------------#
read_EPA_csv <- function(filename) {
  df <- NULL
  df <- read.csv(filename, stringsAsFactors = FALSE)
  df <- subset(df, Sample.Measurement > 0.00)
  return(df)
}

#--------------------------------------------------------------#
# Read ALL hourly PM data to data frame from 2014-2017
#--------------------------------------------------------------#
# 1) Input CSV file
hourly.pm25.FRM.2014 <- read_EPA_csv("data/hourly_88101_2014.csv")
hourly.pm25.FRM.2015 <- read_EPA_csv("data/hourly_88101_2015.csv")
hourly.pm25.FRM.2016 <- read_EPA_csv("data/hourly_88101_2016.csv")
hourly.pm25.FRM.2017 <- read_EPA_csv("data/hourly_88101_2017.csv")

# 2) Combine data frames (2014-2017) into single dataframe
hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.2014, hourly.pm25.FRM.2015)
hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.14_17, hourly.pm25.FRM.2016)
hourly.pm25.FRM.14_17 <- bind_rows(hourly.pm25.FRM.14_17, hourly.pm25.FRM.2017)

# 3) Convert Date columns to date objects
hourly.pm25.FRM.14_17$Date.Local <- as.Date(hourly.pm25.FRM.14_17$Date.Local, format = "%F")
hourly.pm25.FRM.14_17$Date.GMT <- as.Date(hourly.pm25.FRM.14_17$Date.GMT, format = "%F")

# 4) Joining date and time into single column for GMT and local
# GMT time zone set for DateTime.GMT
hourly.pm25.FRM.14_17$DateTime.GMT <- as.POSIXct(paste(hourly.pm25.FRM.14_17$Date.GMT, 
                                                       hourly.pm25.FRM.14_17$Time.GMT), 
                                                 format = "%Y-%m-%d %H:%M")

hourly.pm25.FRM.14_17$DateTime.Local <- as.POSIXct(paste(hourly.pm25.FRM.14_17$Date.Local, 
                                                       hourly.pm25.FRM.14_17$Time.Local), 
                                                 format = "%Y-%m-%d %H:%M")

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno, NV
#--------------------------------------------------------------#
pm_sites.reno <- filter.pm_sites.reno("2014-01-01 00:00",
                                      "2017-12-31 23:00",
                                      1)
plot.pm(pm_sites.reno)

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

# # Graph aggregated hourly mean data by month in a year
# ag %>%
#   subset(Year.Local == "2014"
#          & Month.Local %in% c("June", "July", "January")) %>%
#   ggplot(aes(x = Time.Local, y = Sample.Measurement, ymax = 15, color = as.character(Month.Local))) +
#   geom_point() +
#   geom_smooth(aes(group = as.character(Month.Local)), se = FALSE) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# # Filtering by hour (Could also filter by date)
# tempdf <- NULL
# tempdf <- subset(mv.sites,
#                  Time.Local == "00:00" &
#                    Date.Local >= "2015-01-01" &
#                    Date.Local <= "2015-01-31")
#
# # Arithmetic Mean
# tempdf %>%
#   summarise_at(vars(Sample.Measurement), funs(mean)) %>% round(3)
#
# # Geometric Mean
# tempdf %>%
#   summarise_at(vars(Sample.Measurement), funs(geometric_mean)) %>% round(3)
#
# # Plot histogram
# ggplot(tempdf, aes(x = Sample.Measurement)) +
#   geom_histogram() +
#   ggtitle(paste0(head(mv.sites$Date.Local, n = 1), " to ",
#                  tail(mv.sites$Date.Local, n = 1)))
