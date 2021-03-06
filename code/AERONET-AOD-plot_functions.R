#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# AERONET-AOD-plot_functions.R
# Desc - Loads and tidys AERONET data
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# Note: Not suitable due to lack of consistency
# @desc: 
# @param:
#--------------------------------------------------------------#
plot.hourly_mean.aod <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons &
             AOD_500nm > 0.0)
  
  ag <- aggregate(AOD_500nm ~ Time.Local+Season.Local+Year.Local,
                  df, geometric.mean)
  ag %>%
    ggplot(aes(x = Time.Local, y = AOD_500nm)) +
    geom_point() +
    facet_grid(Season.Local ~ Year.Local) +
    stat_smooth(method = "gam",
                aes(x = Time.Local, y = AOD_500nm),
                formula = y ~ s(x, bs = "cc", k = 24)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm") +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 23),
                       label = c("Midnight", "06:00", "Noon", "18:00", "23:00")) +
    ggtitle(paste0("AOD AERONET - Aggregated Hourly data - ", df$AERONET_Site)) +
    theme_bw()
}

#--------------------------------------------------------------#
# @desc: Plots correlation b/w daily average and daily peak
# @param: 
#--------------------------------------------------------------#
plot.corr.avg_peak.aod <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons &
             AOD_500nm > 0.0)
  
  ag <- do.call(data.frame, aggregate(AOD_500nm ~ Date.Local+Season.Local+Year.Local, df, 
                                      FUN = function(df) c(Mean = mean(df), 
                                                           Peak = max(df))))
  
  cors <- ddply(ag, c("Season.Local"), 
                summarise, cor = round(cor(AOD_500nm.Mean, 
                                           AOD_500nm.Peak,
                                           method = "spearman"), 2))
  
  # Count number of data entries by Season
  ag.counts <- ddply(ag, c("Season.Local", "Year.Local"),
                     summarise, count = n())
  
  ag %>%
    ggplot(aes(x = AOD_500nm.Mean, y = AOD_500nm.Peak)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(Season.Local ~ Year.Local) +
    # facet_wrap(~ Season.Local) +
    # labs(x = "Daily Average (mg/m^3)", y = "Daily Peak (mg/m^3)") +
    # ggtitle(paste0("PM2.5 FRM - Correlation Coefficient (Daily Average vs. Daily Peak) - ", df$County.Name, ", ", df$State.Name),
    #         subtitle = paste0(unique(years), collapse = ", ")) +
    # geom_text(data = cors, aes(label = paste("r = ", cor)),
    #           x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    # geom_text(data = ag.counts, aes(label = paste("n = ", count)),
    #           x = -Inf, y = Inf, hjust = -0.2, vjust = 4.2) +
    theme_bw()
}