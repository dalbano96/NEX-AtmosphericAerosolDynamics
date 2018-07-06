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
filter.pm.data <- function(site_nums, county_names, state_name, poc, start_date, end_date) {
  filtered_data <- subset(hourly.pm25.FRM.14_17, subset = Site.Num %in% site_nums & 
                       County.Name %in% county_names &
                       State.Name == state_name &
                       DateTime.Local >= start_date &
                       DateTime.Local <= end_date &
                       POC == poc)
  
  # Set to local time zone
  filtered_data$DateTime.Local <- ymd_hms(filtered_data$DateTime.Local, tz = "America/Los_Angeles")
  
  # Parse Date.Local into month and year columns
  filtered_data <- filtered_data %>%
    mutate(Month.Local = month(Date.Local, label = TRUE, abbr = FALSE),
           Year.Local = year(Date.Local))
  return(filtered_data)
}

#--------------------------------------------------------------#
# @desc: Plots linechart for hourly trends of single day
#   for pm2.5
# @param:
#--------------------------------------------------------------#
scatter.plot.pm25 <- function(data) {
  data %>%
    ggplot(aes(x = DateTime.Local, 
               y = Sample.Measurement, 
               group = Site.Num, color = as.character(Site.Num))) +
    geom_point(position = position_dodge(width = 0.75)) +
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Time", y = "Micrograms/cubic meter", color = "Sites") +
    # scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %y")) +
    ggtitle(paste0(data$State.Name, " [", 
                   head(data$DateTime.Local, n = 1), " to ", 
                   tail(data$DateTime.Local, n = 1), "] ", "POC: ", 
                   data$POC))
}

#--------------------------------------------------------------#
# @desc: Plots hourly PM averages for a monthly period
#   within a year
# @param:
#--------------------------------------------------------------#
plot.ag <- function(data, year, months) {
  data %>%
    subset(Year.Local == year
           & Month.Local %in% months) %>%
    ggplot(aes(x = Time.Local, y = Sample.Measurement, ymax = 15, color = as.character(Month.Local))) +
    geom_point() +
    geom_smooth(aes(group = as.character(Month.Local)), se = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
