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
  
  # Aggregate Seasonal/Yearly data, as well as calculate mean and find peak value
  ag <- gather(do.call(data.frame, aggregate(Sample.Measurement ~ Date.Local+Season.Local+Year.Local, df,
                                      FUN = function(df) c(Mean = mean(df),
                                                           Peak = max(df)))),
               key = "Type", value = "Sample.Measurement", Sample.Measurement.Mean, Sample.Measurement.Peak)
  
  # # Finds the peak value for each hour of each day?
  # # It is possible to group by day and find the max value from there
  # ag <- gather(do.call(data.frame, aggregate(cbind(Sample.Measurement, Time.Local) ~ Date.Local+Season.Local+Year.Local, df,
  #                                     FUN = function(df) c(Mean = mean(df),
  #                                                          Peak = max(df),
  #                                                          Time = select(df, c("Time.Local"))))),
  #              key = "Type", value = "Sample.Measurement", Sample.Measurement.Mean, Sample.Measurement.Peak)
  # 
  # ah <- select(df, c("Sample.Measurement", "Date.Local", "Time.Local", "Season.Local", "Year.Local"))
  
  ag %>%
    ggplot() +
    geom_point(aes(x = Date.Local, y = Sample.Measurement, color = Type, shape = Type), size = 2.5) +
    scale_color_manual(values = c("Sample.Measurement.Peak" = "Red", 
                                  "Sample.Measurement.Mean" = "Blue")) +
    stat_summary(aes(x = Date.Local, y = Sample.Measurement), geom = "linerange", size = 0.2, alpha = 0.8) +
    labs(x = "Date", 
         y = "PM2.5 Concentration (Micrograms/cubic meter)") +
    ggtitle(paste0("PM2.5 FRM - Daily Average and Peak - ", df$County.Name, ", ", df$State.Name),
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
                                           Sample.Measurement.Peak,
                                           method = "spearman"), 2))
  
  # Count number of data entries by Season
  ag.counts <- ddply(ag, c("Season.Local", "Year.Local"),
                summarise, count = n())
  
  ag %>%
    ggplot(aes(x = Sample.Measurement.Mean, y = Sample.Measurement.Peak)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(Season.Local ~ Year.Local) +
    labs(x = "Daily Average (Micrograms/cubic meter)", y = "Daily Peak (Micrograms/cubic meter)") +
    coord_fixed(ratio = 1, xlim = c(0,75), ylim = c(0,75)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggtitle(paste0("PM2.5 FRM - Correlation b/w Daily Average and Daily Peak - ", df$County.Name, ", ", df$State.Name)) +
    geom_text(data = cors, aes(label = paste("r = ", cor)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    geom_text(data = ag.counts, aes(label = paste("n = ", count)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 4.2) +
    theme_bw()
}

#--------------------------------------------------------------#
# @desc: Plots PM measurements by location environment
# @param: 
#--------------------------------------------------------------#
plot.environment.hourly_mean.pm <- function(df = all.pm, seasons = seasons.all, years = years.all, state.name) {
  df <- df %>%
    subset(!is.na(Location.Setting) &
             Location.Setting != "" &
             Season.Local %in% seasons &
             Year.Local %in% years &
             State.Name == state.name)
  
  ag <- aggregate(Sample.Measurement ~ Location.Setting+Time.Local+Season.Local+Year.Local,
                  df, mean)
  
  ag %>%
    ggplot(aes(x = Time.Local, y = Sample.Measurement, color = Location.Setting, group = Location.Setting)) +
    geom_point() +
    facet_grid(Season.Local ~ Year.Local) +
    stat_smooth(method = "gam",
                aes(x = Time.Local, y = Sample.Measurement),
                formula = y ~ s(x, bs = "cc", k = 12)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Month", y = "PM2.5 Concentration (Micrograms/cubic meter)", color = ("Site")) +
    # ggtitle(paste0("PM2.5 FRM - Aggregated Monthly Data by Location Environment - ", df$County.Name, ", ", df$State.Name),
    #         subtitle = paste0(unique(years), collapse = ", ")) +
    theme_bw()
}

#--------------------------------------------------------------#
# @desc:
# @param: 
#--------------------------------------------------------------#
plot.environment.site_count.pm <- function(df = all.pm, seasons = seasons.all, years = years.all, state.name) {
  df <- df %>%
    subset(!is.na(Location.Setting) &
             Location.Setting != "" &
             Season.Local %in% seasons &
             Year.Local %in% years &
             State.Name == state.name)
  
  df %>%
    ggplot(aes(x = Location.Setting, fill = factor(Location.Setting))) +
    geom_bar() +
    theme_bw()
}

#--------------------------------------------------------------#
# (WIP)
# @desc: Plot correlations b/w PM and AOD
# @param: 
#--------------------------------------------------------------#
plot.corr.aod_pm <- function(pm.df, aod.df, years = years.all, seasons = seasons.all) {
  pm.df <- pm_sites.reno
  aod.df <- aod_sites.reno
  
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
  aod.ag <- do.call(data.frame, aggregate(AOD_500nm ~ Date.Local+Season.Local+Year.Local, aod.df, 
                                          FUN = function(aod.df) c(Mean = mean(aod.df), 
                                                                   Peak = max(aod.df))))
  
  pm.cors <- ddply(pm.ag, c("Season.Local", "Year.Local"), 
                summarise, cor = round(cor(Sample.Measurement.Mean, 
                                           Sample.Measurement.Peak,
                                           method = "spearman"), 2))
  
  aod.cors <- ddply(aod.ag, c("Season.Local", "Year.Local"),
                    summarise, cor = round(cor(aod.ag$AOD_500nm.Mean,
                                               aod.ag$AOD_500nm.Peak,
                                               method = "spearman"), 2))
  
  pm.ag %>%
    ggplot(aes(x = Sample.Measurement.Mean, y = Sample.Measurement.Peak)) +
    geom_point() +
    geom_point(aod.ag, aes(x = AOD_500nm.Mean, y = AOD_500nm.Peak)) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(Season.Local ~ Year.Local) +
    labs(x = "Daily Average", y = "Daily Peak") +
    # ggtitle(paste0("PM2.5 FRM - Correlation Coefficient (Daily Average vs. Daily Peak) - ", df$County.Name, ", ", df$State.Name),
    #         subtitle = paste0(unique(years), collapse = ", ")) +
    # geom_text(data = pm.cors, aes(label = paste("r = ", cor)),
    #           x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    theme_bw()
}