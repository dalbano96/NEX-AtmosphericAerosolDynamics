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
  return(pm.df)
}

#--------------------------------------------------------------#
# @desc: Filters PM data frame based on specified location.
# @param: PM SITE INFO ->Site number(s), county names(s),
#   state name, start date (if specified by user), end date
#   (if specified by user), timezone.
#--------------------------------------------------------------#
filter.pm_data <- function(site_nums, county_names, state_name, poc) {
  return(subset(hourly.pm25.FRM.14_17, subset = Site.Num %in% site_nums & 
                       County.Name %in% county_names &
                       State.Name == state_name &
                       POC %in% poc))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.reno <- function() {
  # Specify observed site numbers
  # reno.site_nums <- c(16, 1005)
  reno.site_nums <- c(1005)

  # Specify observed county names
  reno.county_names <- c("Washoe")
  
  # Specify observed State name
  reno.state_name <- "Nevada"
  
  # Specify observed POC
  reno.poc <- c(1,3)
  
  # Filter data
  return(filter.pm_data(reno.site_nums, 
                        reno.county_names, 
                        reno.state_name,
                        reno.poc))
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
  
  # Specify observed POC
  balt.poc <- 3
  
  # Filter data
  return(filter.pm_data(balt.site_nums, 
                        balt.county_names, 
                        balt.state_name,
                        balt.poc))
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
  
  # Specify observed POC
  denv.poc <- 3
  
  # Filter data
  return(filter.pm_data(denv.site_nums, 
                        denv.county_names, 
                        denv.state_name,
                        denv.poc))
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
  
  # Specify observed POC
  ny.poc <- 4
  
  # Filter data
  return(filter.pm_data(ny.site_nums, 
                        ny.county_names, 
                        ny.state_name,
                        ny.poc))
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
  
  # Specify observed POC
  LosAng.poc <- 3
  
  # Filter data
  return(filter.pm_data(LosAng.site_nums, 
                        LosAng.county_names, 
                        LosAng.state_name,
                        LosAng.poc))
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.pm_sites.hawaii <- function() {
  # Specify observed site numbers
  # hawaii.site_nums <- c(2021, 1006, 2023, 7, 2016, 2020, 1012)
  # hawaii.site_nums <- c(1006,1012,7)
  hawaii.site_nums <- 1006
  
  # Specify observed county names
  hawaii.county_names <- c("Hawaii")
  
  # Specify observed State name
  hawaii.state_name <- "Hawaii"
  
  # Specify observed POC
  hawaii.poc <- 1
  
  # Filter data
  return(filter.pm_data(hawaii.site_nums, 
                        hawaii.county_names, 
                        hawaii.state_name,
                        hawaii.poc))
}

#--------------------------------------------------------------#
# TODO: allow it to plot from multiple sites
# @desc: Plots PM data for a given time range
#   for pm2.5
# @param:
#--------------------------------------------------------------#
plot.all.pm <- function(data, years = years.all, months = months.all) {
  data %>%
    subset(Year.Local %in% years
           & Month.Local %in% months) %>%
    ggplot(aes(x = strptime(DateTime.Local, format = "%F %H:%M"), 
               y = Sample.Measurement, 
               group = Site.Num, color = as.character(Site.Num))) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Time", y = "PM2.5 Concentration (Micrograms/cubic meter)", color = "Sites") +
    scale_x_datetime(date_breaks = "3 months") +
    ggtitle(paste0(data$State.Name, " [", 
                   head(data$DateTime.Local, n = 1), " to ", 
                   tail(data$DateTime.Local, n = 1), "] ", "POC: ", 
                   data$POC))
}

#--------------------------------------------------------------#
# NOTE: Function is a mess :)
# TODO: allow it to plot from multiple sites
# @desc: Plots hourly PM averages for a given monthly period
#   of a given year
# @param:
#--------------------------------------------------------------#
plot.hourly_mean.pm <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons)
  
  ag <- aggregate(Sample.Measurement ~ Time.Local+Season.Local+Year.Local,
                  df, geometric.mean)
  ag %>%
    ggplot(aes(x = Time.Local, y = Sample.Measurement)) +
    geom_point() +
    facet_grid(Season.Local ~ Year.Local) +
    stat_smooth(method = "gam",
                aes(x = Time.Local, y = Sample.Measurement),
                formula = y ~ s(x, bs = "cc", k = 24)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "PM2.5 Concentration (Micrograms/cubic meter)", color = ("Site")) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 23),
                       label = c("Midnight", "06:00", "Noon", "18:00", "23:00")) +
    ggtitle(paste0("PM2.5 FRM - Aggregated Hourly data - ", df$County.Name, ", ", df$State.Name),
            subtitle = paste0(unique(years), collapse = ", ")) +
    theme_bw()
}

#--------------------------------------------------------------#
# (In Progress) - Box/Whisker plot
# @desc:
# @param:
#--------------------------------------------------------------#
plot.daily_mean_peak.pm <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
           Season.Local %in% seasons)
  
  ag <- aggregate(Sample.Measurement ~ Date.Local+Season.Local,
                  df, geometric.mean)
  
  df %>%
    ggplot(aes(x = Date.Local, y = Sample.Measurement)) +
    geom_boxplot(aes(group = Date.Local), outlier.shape = 1) +
    facet_wrap(~ Season.Local) +
    geom_point(data = ag, aes(x = Date.Local, y = Sample.Measurement), shape = 17) +
    labs(x = "Date", y = "PM2.5 Concentration (Micrograms/cubic meter)") +
    ggtitle(paste0("PM2.5 FRM - Daily Average - ", df$County.Name, ", ", df$State.Name),
            subtitle = paste0(unique(years), collapse = ", ")) +
    theme_bw()
}

#--------------------------------------------------------------#
# @desc: Plots correlation b/w daily average and daily peak
# @param: 
#--------------------------------------------------------------#
plot.r2.daily_avg_peak.pm <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons)
  
  ag <- do.call(data.frame, aggregate(Sample.Measurement ~ Date.Local+Season.Local+Year.Local, df, 
                                      FUN = function(df) c(Mean = mean(df), 
                                                           Peak = max(df))))
  
  cors <- ddply(ag, c("Season.Local"), 
                summarise, cor = round(cor(Sample.Measurement.Mean, 
                                Sample.Measurement.Peak), 2))
  
  ag %>%
    ggplot(aes(x = Sample.Measurement.Mean, y = Sample.Measurement.Peak)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # facet_grid(Season.Local ~ Year.Local) +
    facet_wrap(~ Season.Local) +
    # labs(x = "Daily Average (mg/m^3)", y = "Daily Peak (mg/m^3)") +
    ggtitle(paste0("PM2.5 FRM - Correlation Coefficient (Daily Average vs. Daily Peak) - ", df$County.Name, ", ", df$State.Name),
            subtitle = paste0(unique(years), collapse = ", ")) +
    geom_text(data = cors, aes(label = paste("R^2 = ", cor)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    theme_bw()
}

#--------------------------------------------------------------#
# (WIP)
# @desc: 
# @param: 
#--------------------------------------------------------------#
plot.r2.aod_pm <- function(pm.df, aod.df, years = years.all, seasons = seasons.all) {
  pm.df <- pm.df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons)
  
  aod.df <- aod.df  %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons)
  
  # Calculates average and peak values for sample measurement of PM
  pm.ag <- do.call(data.frame, aggregate(Sample.Measurement ~ Date.Local+Season.Local+Year.Local, pm.df, 
                                      FUN = function(pm.df) c(Mean = mean(pm.df), 
                                                           Peak = max(pm.df))))
  
  # Calculates average and peak values for sample measurement of AOD
  aod.ag <- do.call(data.frame, aggregate(Sample.Measurement ~ Date.Local+Season.Local+Year.Local, aod.df, 
                                         FUN = function(aod.df) c(Mean = mean(aod.df), 
                                                                 Peak = max(aod.df))))
  # TODO: Merge aod and pm? - This would be problematic...
  cors <- cor(x = pm.ag$Sample.Measurement, y = aod.ag$Sample.Measurement)
  
  ag %>%
    ggplot(aes(x = Sample.Measurement.Mean, y = Sample.Measurement.Peak)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # facet_grid(Season.Local ~ Year.Local) +
    facet_wrap(~ Season.Local) +
    labs(x = "Daily Average", y = "Daily Peak") +
    ggtitle(paste0("PM2.5 FRM - Correlation Coefficient (Daily Average vs. Daily Peak) - ", df$County.Name, ", ", df$State.Name),
            subtitle = paste0(unique(years), collapse = ", ")) +
    geom_text(data = cors, aes(label = paste("R^2 = ", cor)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    theme_bw()
}
