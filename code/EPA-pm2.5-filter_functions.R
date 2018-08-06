#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-filter_functions.R
# Desc - 
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
load_all_csv.pm_data <- function() {
  # Set wd to files' location
  setwd("data/88101 2014-2017/")
  
  # Get csv filenames
  file_names <- list.files(pattern = "*.csv")
  
  # Load files to data frame
  pm.df <- do.call(rbind, lapply(file_names, function(x) read.csv(x, stringsAsFactors = FALSE)))
  
  # Subset nonzero and positive values
  pm.df <- pm.df %>% subset(Sample.Measurement > 0.00)
  
  # Convert Date columns from character to date objects
  pm.df$Date.Local <- as.Date(pm.df$Date.Local, format = "%F")
  pm.df$Date.GMT <- as.Date(pm.df$Date.GMT, format = "%F")
  
  # Joining date and time into single column for local and GMT
  pm.df$DateTime.Local <- as.POSIXct(paste(pm.df$Date.Local, 
                                           pm.df$Time.Local), 
                                     format = "%F %R")
  pm.df$DateTime.GMT <- force_tz(as.POSIXct(paste(pm.df$Date.GMT, 
                                                  pm.df$Time.GMT), 
                                            format = "%F %R"), tz = "GMT")
  
  # Convert Time columns to integer values b/w [0-23]
  pm.df$Time.Local <- hour(hms::parse_hm(pm.df$Time.Local))
  pm.df$Time.GMT <- hour(hms::parse_hm(pm.df$Time.GMT))
  
  # Parse into month and year columns for Date.Local and Date.GMT
  pm.df <- pm.df %>%
    mutate(Month.Local = month(Date.Local, label = TRUE, abbr = FALSE),
           Year.Local = year(Date.Local),
           Month.GMT = month(Date.GMT, label = TRUE, abbr = FALSE),
           Year.GMT = month(Date.GMT))
  
  # Cateogrizes time by season
  temp.season.local <- as.yearqtr(as.yearmon(pm.df$DateTime.Local, "%F") + 1/12)
  pm.df$Season.Local <- factor(format(temp.season.local, "%q"), levels = 1:4,
                                       labels = c("Winter", "Spring", "Summer", "Fall"))
  
  temp.season.gmt <- as.yearqtr(as.yearmon(pm.df$DateTime.GMT, "%F") + 1/12)
  pm.df$Season.GMT <- factor(format(temp.season.gmt, "%q"), levels = 1:4,
                             labels = c("Winter", "Spring", "Summer", "Fall"))
  
  # Finished. Set wd to project root
  setwd("../..")
  
  # Load AQS site information
  site_info <- read.csv("data/aqs_sites.csv", stringsAsFactors = FALSE)
  site_info <- dplyr::rename(site_info, Site.Num = Site.Number)
  site_info <- transform(site_info, State.Code = as.integer(State.Code))
  return_df <- left_join(pm.df, site_info, by.x = c("County.Name", "Site.Num"), copy = FALSE)
  
  return(return_df)
}

#--------------------------------------------------------------#
# @desc: Filters PM data frame based on specified location.
# @param: PM SITE INFO ->Site number(s), county names(s),
#   state name, start date (if specified by user), end date
#   (if specified by user), timezone.
#--------------------------------------------------------------#
filter.pm_data <- function(site_nums, county_names, state_name) {
  return(subset(all.pm, subset = Site.Num %in% site_nums & 
                       County.Name %in% county_names &
                       State.Name == state_name))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.reno <- function() {
  # Specify observed site numbers
  reno.site_nums <- c(16, 1005)
  # reno.site_nums <- c(1005)

  # Specify observed county names
  reno.county_names <- c("Washoe")
  
  # Specify observed State name
  reno.state_name <- "Nevada"
  
  # Filter data
  df <- filter.pm_data(reno.site_nums, 
                        reno.county_names, 
                        reno.state_name)
  
  # Filter by lowest POC
  return(subset(df, POC == min(df$POC)))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.balt <- function() {
  # Specify observed site numbers
  balt.site_nums <- c(40)
  
  # Specify observed county names
  balt.county_names <- c("Baltimore (City)")
  
  # Specify observed State name
  balt.state_name <- "Maryland"
  
  # Filter data
  df <- filter.pm_data(balt.site_nums, 
                        balt.county_names, 
                        balt.state_name)
  
  # Filter by lowest POC
  return(subset(df, POC == min(df$POC)))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.denv <- function() {
  # Specify observed site numbers
  # denv.site_nums <- c(26, 28, 2)
  denv.site_nums <- c(2)
  
  # Specify observed county names
  denv.county_names <- c("Denver")
  
  # Specify observed State name
  denv.state_name <- "Colorado"
  
  # Filter data
  df <- filter.pm_data(denv.site_nums, 
                        denv.county_names, 
                        denv.state_name)
  
  # Filter by lowest POC
  return(subset(df, POC == min(df$POC)))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.ny <- function() {
  # Specify observed site numbers
  # ny.site_nums <- c(26, 28, 2)
  ny.site_nums <- c(110)
  
  # Specify observed county names
  ny.county_names <- c("Bronx")
  
  # Specify observed State name
  ny.state_name <- "New York"
  
  # Filter data
  df <- filter.pm_data(ny.site_nums, 
                        ny.county_names, 
                        ny.state_name)
  
  # Filter by lowest POC
  return(subset(df, POC == min(df$POC)))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.LosAng <- function() {
  # Specify observed site numbers
  LosAng.site_nums <- c(4008)
  
  # Specify observed county names
  LosAng.county_names <- c("Los Angeles")
  
  # Specify observed State name
  LosAng.state_name <- "California"
  
  # Filter data
  df <- filter.pm_data(LosAng.site_nums, 
                        LosAng.county_names, 
                        LosAng.state_name)
  
  # Filter by lowest POC
  return(subset(df, POC == min(df$POC)))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.hawaii <- function() {
  # Specify observed site numbers
  hawaii.site_nums <- c(2021, 1006, 2023, 7, 2016, 2020, 1012)
  # hawaii.site_nums <- c(1006,1012,7)
  # hawaii.site_nums <- 1006
  
  # Specify observed county names
  hawaii.county_names <- c("Hawaii")
  
  # Specify observed State name
  hawaii.state_name <- "Hawaii"
  
  # Filter data
  df <- filter.pm_data(hawaii.site_nums, 
                       hawaii.county_names, 
                       hawaii.state_name)
  
  # Filter by lowest POC
  return(subset(df, POC == min(df$POC)))
}



