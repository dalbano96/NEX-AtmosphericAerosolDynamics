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
# Read EPA csv to data frame
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
# Hourly Data of Mountain View, CA region
#--------------------------------------------------------------#
mv.site.num_list <- c(5, 1001, 6)
mv.site.county_name_list <- c("San Mateo", "Santa Clara")
mv.site.state_name <- "California"
mv.poc <- 3
mv.start_date <- "2014-01-01 00:00"
mv.end_date <- "2017-01-31 23:00"

mv.sites <- subset(hourly.pm25.FRM.14_17, subset = Site.Num %in% mv.site.num_list & 
                     County.Name %in% mv.site.county_name_list &
                     State.Name == mv.site.state_name &
                     DateTime.Local >= mv.start_date &
                     DateTime.Local <= mv.end_date &
                     POC == mv.poc)

# Set to local time zone
mv.sites$DateTime.Local <- ymd_hms(mv.sites$DateTime.Local, tz = "America/Los_Angeles")

# Parse Date.Local into month and year columns
mv.sites <- mv.sites %>%
  mutate(Month.Local = month(Date.Local, label = TRUE, abbr = FALSE),
         Year.Local = year(Date.Local))

# Graph
mv.plot.linechart.pm25 <- scatter.plot.pm25(mv.sites)
mv.plot.linechart.pm25

# Testing geometric means calculation for each hour in a give date range
geometric_mean <- function(values) {
  gmean <- prod(values) ^ (1 / length(values))
  return(gmean)
}

# TODO: Find a way to simplify this process instead of manually doing it
mv.sites %>%
  filter(Year.Local == "2016"
         & Month.Local == "July"
         & Time.Local == "12:00") %>%
  summarise_at(vars(Sample.Measurement), funs(geometric_mean)) %>% round(3)

# # Standard deviation
# sd(mv.sites$Sample.Measurement)
# 
# # Arithmetic Mean
# mv.sites %>%
#   summarise_at(vars(Sample.Measurement), funs(mean)) %>% round(3)
# 
# # Geometric Mean
# mv.sites %>%
#   summarise_at(vars(Sample.Measurement), funs(geometric_mean)) %>% round(3)
# 
# # Plot histogram
# ggplot(mv.sites, aes(x = Sample.Measurement)) +
#   geom_histogram(binwidth = 2) +
#   ggtitle(paste0(head(mv.sites$DateTime.Local, n = 1), " to ",
#                  tail(mv.sites$DateTime.Local, n = 1)))

# Filtering by hour (Could also filter by date)
tempdf <- NULL
tempdf <- subset(mv.sites,
                 Time.Local == "00:00" &
                   Date.Local >= "2015-01-01" &
                   Date.Local <= "2015-01-31")

# Arithmetic Mean
tempdf %>%
  summarise_at(vars(Sample.Measurement), funs(mean)) %>% round(3)

# Geometric Mean
tempdf %>%
  summarise_at(vars(Sample.Measurement), funs(geometric_mean)) %>% round(3)

# Plot histogram
ggplot(tempdf, aes(x = Sample.Measurement)) +
  geom_histogram() +
  ggtitle(paste0(head(mv.sites$Date.Local, n = 1), " to ",
                 tail(mv.sites$Date.Local, n = 1)))


#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno, NV
#--------------------------------------------------------------#
# Site 22 missing from year > 2015
reno.site.num_list <- c(16, 22, 1005, 1007)
reno.site.county_name_list <- c("Washoe")
reno.site.state_name <- "Nevada"
reno.poc <- min(unique(reno.sites$POC))

reno.sites$DateTime.Local <- ymd_hms(reno.sites$DateTime.Local, tz = "America/Los_Angeles")
reno.start_date <- "2014-01-01"
reno.end_date <- "2017-12-31"

reno.sites <- subset(hourly.pm25.FRM.14_17, subset = Site.Num %in% reno.site.num_list & 
                       County.Name %in% reno.site.county_name_list &
                       State.Name == reno.site.state_name &
                       POC == reno.poc &
                       DateTime.Local >= reno.start_date &
                       DateTime.Local <= reno.end_date)

reno.plot.linechart.pm25 <- scatter.plot.pm25(reno.sites,reno.start_date, reno.end_date)
reno.plot.linechart.pm25

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
