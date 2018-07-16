#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-functions.R
# Desc - Functions that loads and tidys EPA PM2.5 data
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
load_all_csv.pm_data <- function() {
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
  pm.2014 <- read_EPA_csv("data/hourly_88101_2014.csv")
  pm.2015 <- read_EPA_csv("data/hourly_88101_2015.csv")
  pm.2016 <- read_EPA_csv("data/hourly_88101_2016.csv")
  pm.2017 <- read_EPA_csv("data/hourly_88101_2017.csv")
  
  # 2) Combine data frames (2014-2017) into single dataframe
  pm.df <- bind_rows(pm.2014, pm.2015)
  pm.df <- bind_rows(pm.df, pm.2016)
  pm.df <- bind_rows(pm.df, pm.2017)
  
  # 3) Convert Date columns to date objects
  pm.df$Date.Local <- as.Date(pm.df$Date.Local, format = "%F")
  pm.df$Date.GMT <- as.Date(pm.df$Date.GMT, format = "%F")
  
  
  # 4) Joining date and time into single column for GMT and local
  # GMT time zone set for DateTime.GMT
  pm.df$DateTime.GMT <- as.POSIXct(paste(pm.df$Date.GMT, 
                                         pm.df$Time.GMT), 
                                   format = "%Y-%m-%d %H:%M")
  
  pm.df$DateTime.Local <- as.POSIXct(paste(pm.df$Date.Local, 
                                           pm.df$Time.Local), 
                                     format = "%Y-%m-%d %H:%M")
  
  # 5) Convert Time columns to time objects (WIP)
  pm.df$Time.Local <- hour(hms::parse_hm(pm.df$Time.Local))
  pm.df$Time.GMT <- hour(hms::parse_hm(pm.df$Time.GMT))
  
  return(pm.df)
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_data <- function(site_nums, county_names, state_name, poc, start_date, end_date, timezone = "America/Los_Angeles") {
  filtered_data <- subset(hourly.pm25.FRM.14_17, subset = Site.Num %in% site_nums & 
                       County.Name %in% county_names &
                       State.Name == state_name &
                       DateTime.Local >= start_date &
                       DateTime.Local <= end_date &
                       POC == poc)
  
  # Format to correct local time zone
  filtered_data$DateTime.Local <- ymd_hms(filtered_data$DateTime.Local, tz = timezone)
  
  # Parse Date.Local into month and year columns
  filtered_data <- filtered_data %>%
    mutate(Month.Local = month(Date.Local, label = TRUE, abbr = FALSE),
           Year.Local = year(Date.Local))
  return(filtered_data)
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.reno <- function(start = "2014-01-01 00:00", end = "2017-12-31 23:00") {
  # Specify observed site numbers
  reno.site_nums <- c(16)

  # Specify observed county names
  reno.county_names <- c("Washoe")
  
  # Specify observed State name
  reno.state_name <- "Nevada"
  
  # Specify observed POC
  reno.poc <- 3

  # Specify observed start date/time
  reno.start_date <- start
  
  # Specify observed start date/time
  reno.end_date <- end
  
  # Set observed timezone
  reno.timezone <- "America/Los_Angeles"
  
  # Filter data
  return(filter.pm_data(reno.site_nums, 
                        reno.county_names, 
                        reno.state_name, reno.poc, 
                        reno.start_date, 
                        reno.end_date,
                        reno.timezone))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.balt <- function(start = "2014-01-01 00:00", end = "2017-12-31 23:00") {
  # Specify observed site numbers
  balt.site_nums <- c(40)
  
  # Specify observed county names
  balt.county_names <- c("Baltimore (City)")
  
  # Specify observed State name
  balt.state_name <- "Maryland"
  
  # Specify observed POC
  balt.poc <- 3
  
  # Specify observed start date/time
  balt.start_date <- start
  
  # Specify observed start date/time
  balt.end_date <- end
  
  # Set observed timezone
  balt.timezone <- "America/New_York"
  
  # Filter data
  return(filter.pm_data(balt.site_nums, 
                        balt.county_names, 
                        balt.state_name, balt.poc, 
                        balt.start_date, 
                        balt.end_date,
                        balt.timezone))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.denv <- function(start = "2014-01-01 00:00", end = "2017-12-31 23:00") {
  # Specify observed site numbers
  denv.site_nums <- c(26, 28, 2)
  
  # Specify observed county names
  denv.county_names <- c("Denver")
  
  # Specify observed State name
  denv.state_name <- "Colorado"
  
  # Specify observed POC
  denv.poc <- 3
  
  # Specify observed start date/time
  denv.start_date <- start
  
  # Specify observed start date/time
  denv.end_date <- end
  
  # Set observed timezone
  denv.timezone <- "America/Denver"
  
  # Filter data
  return(filter.pm_data(denv.site_nums, 
                        denv.county_names, 
                        denv.state_name, denv.poc, 
                        denv.start_date, 
                        denv.end_date,
                        denv.timezone))
}

#--------------------------------------------------------------#
# TODO: allow it to plot from multiple sites
# @desc: Plots PM data for a given time range
#   for pm2.5
# @param:
#--------------------------------------------------------------#
plot.all.pm <- function(data) {
  data %>%
    ggplot(aes(x = DateTime.Local, 
               y = Sample.Measurement, 
               group = Site.Num, color = as.character(Site.Num))) +
    geom_point(position = position_dodge(width = 0.75)) +
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Time", y = "Micrograms/cubic meter", color = "Sites") +
    ggtitle(paste0(data$State.Name, " [", 
                   head(data$DateTime.Local, n = 1), " to ", 
                   tail(data$DateTime.Local, n = 1), "] ", "POC: ", 
                   data$POC))
}

#--------------------------------------------------------------#
# TODO: allow it to plot from multiple sites
# @desc: Plots hourly PM averages for a given monthly period
#   of a given year
# @param:
#--------------------------------------------------------------#
plot.hourly_mean.pm <- function(data, years = years.all, months = months.all) {
  # TODO: Possibly aggregate by every three months eventually
  ag <- aggregate(Sample.Measurement ~ Time.Local+Month.Local+Year.Local,
                  data, geometric.mean)
  ag %>%
    subset(Year.Local %in% years
           & Month.Local %in% months) %>%
    # ggplot(aes(x = Time.Local, y = Sample.Measurement,
    #            color = Month.Local)) +
    ggplot(aes(x = Time.Local, y = Sample.Measurement)) +
    geom_point() +
    facet_grid(Year.Local ~ Month.Local) +
    # facet_wrap(~ Month.Local) +
    geom_smooth(aes(group = Month.Local), se = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "PM2.5 Concentration (Micrograms/cubic meter)", color = ("Month")) +
    scale_x_continuous(breaks = waiver()) +
    ggtitle(paste0("PM2.5 FRM - Aggregated Hourly Data, ", data$State.Name), subtitle = paste0(unique(years), collapse = ", "))
}


# 
# acf(resid(mod$lme), lag.max = 36, main = "ACF")
# pacf(resid(mod$lme), lag.max = 36, main = "pACF")
# layout(1)

# Testing db integration
# dbdir <- file.path("data/", 'NEX-AAS-db.db')
# con <- dbConnect(SQLite(), dbdir)
# 
# dbListTables(con)
# src_sqlite(embedded = "data/NEX-AAS-db.db")
# sqlite_pm <- tbl(src_sqlite, "hourly_88101_2015")
# dbListFields(con, "hourly_88101_2015")
# 
# dbDisconnect(con, shutdown = TRUE)
