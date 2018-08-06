#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-plot_functions.R
# Desc - 
#--------------------------------------------------------------#

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
                  df, mean)
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
# (WIP) - Box/Whisker plot
# @desc:
# @param:
#--------------------------------------------------------------#
plot.box.mean_peak.pm <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons)
  
  ag <- aggregate(Sample.Measurement ~ Date.Local+Season.Local,
                  df, mean)
  
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
plot.corr.avg_peak.pm <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons)
  
  # Aggregate Seasonal/Yearly data, as well as calculate mean and find peak value
  ag <- do.call(data.frame, aggregate(Sample.Measurement ~ Date.Local+Season.Local+Year.Local, df, 
                                      FUN = function(df) c(Mean = mean(df), 
                                                           Peak = max(df))))
  
  # Calculate correlation coefficient by Season
  cors <- ddply(ag, c("Season.Local", "Year.Local"), 
                summarise, cor = round(cor(Sample.Measurement.Mean, 
                                           Sample.Measurement.Peak), 2))
  
  # Count number of data entries by Season
  nums <- ddply(ag, c("Season.Local", "Year.Local"),
                summarise, num = n())
  
  ag %>%
    ggplot(aes(x = Sample.Measurement.Mean, y = Sample.Measurement.Peak)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(Season.Local ~ Year.Local) +
    labs(x = "Daily Average (Micrograms/cubic meter)", y = "Daily Peak (Micrograms/cubic meter)") +
    ggtitle(paste0("PM2.5 FRM - Correlation b/w Daily Average and Daily Peak - ", df$County.Name, ", ", df$State.Name),
            subtitle = paste0(unique(years), collapse = ", ")) +
    geom_text(data = cors, aes(label = paste("r = ", cor)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    geom_text(data = nums, aes(label = paste("n = ", num)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 4.2) +
    theme_bw()
}

#--------------------------------------------------------------#
# (WIP)
# @desc: 
# @param: 
#--------------------------------------------------------------#
plot.by_environment.pm <- function(df = all.pm) {
  df <- pm_sites.hawaii
  df <- df %>%
    subset(!is.na(Location.Setting) &
             Location.Setting != "")
  
  ag <- aggregate(Sample.Measurement ~ Month.Local+Year.Local+Location.Setting,
                  df, mean)
  
  ag %>%
    ggplot(aes(x = Month.Local, y = Sample.Measurement)) +
    geom_point() +
    facet_grid(Location.Setting ~ Year.Local) +
    stat_smooth(method = "gam",
                aes(x = Month.Local, y = Sample.Measurement),
                formula = y ~ s(x, bs = "cc", k = 12)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Month", y = "PM2.5 Concentration (Micrograms/cubic meter)", color = ("Site")) +
    # ggtitle(paste0("PM2.5 FRM - Aggregated Monthly Data by Location Environment - ", df$County.Name, ", ", df$State.Name),
    #         subtitle = paste0(unique(years), collapse = ", ")) +
    theme_bw()
}

#--------------------------------------------------------------#
# (WIP)
# @desc: 
# @param: 
#--------------------------------------------------------------#
plot.corr.aod_pm <- function(pm.df, aod.df, years = years.all, seasons = seasons.all) {
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
    geom_text(data = cors, aes(label = paste("r = ", cor)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    theme_bw()
}