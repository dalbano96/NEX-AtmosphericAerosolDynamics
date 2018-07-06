#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# plot_functions.R
# Desc - Holds general plotting functions
#--------------------------------------------------------------#

# #--------------------------------------------------------------#
# # @desc: Plots linechart for hourly trends of single day
# #   for pm2.5
# # @param:
# #--------------------------------------------------------------#
# scatter.plot.pm25 <- function(data) {
#   data %>%
#     ggplot(aes(x = DateTime.Local, 
#                y = Sample.Measurement, 
#                group = Site.Num, color = as.character(Site.Num))) +
#     geom_point(position = position_dodge(width = 0.75)) +
#     geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     labs(x = "Time", y = "Micrograms/cubic meter", color = "Sites") +
#     # scale_x_date(breaks = date_breaks("6 months"), labels = date_format("%b %y")) +
#     ggtitle(paste0(data$State.Name, " [", 
#                    head(data$DateTime.Local, n = 1), " to ", 
#                    tail(data$DateTime.Local, n = 1), "] ", "POC: ", 
#                    data$POC))
# }
# 
# #--------------------------------------------------------------#
# # @desc: Plots hourly averages for a monthly period
# #   within a year
# # @param:
# #--------------------------------------------------------------#
# plot.ag <- function(data, year, months) {
#   data %>%
#     subset(Year.Local == year
#            & Month.Local %in% months) %>%
#     ggplot(aes(x = Time.Local, y = Sample.Measurement, ymax = 15, color = as.character(Month.Local))) +
#     geom_point() +
#     geom_smooth(aes(group = as.character(Month.Local)), se = FALSE) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
# }


#--------------------------------------------------------------#
# WORK IN PROGRESS
#--------------------------------------------------------------#
plot.linechart.AOD <- function(data, start.time, end.time) {
  data %>%
    na.omit %>%
    subset(data, DateTime.GMT >= start.time & DateTime.GMT <= end.time) %>%
    ggplot(aes(x = DateTime.GMT, y = X440.870_Angstrom_Exponent)) +
    geom_point(aes(color = DateTime.GMT))
}

tempdf <- subset(hourly.pm25.FRM.14_17, County.Name == "Washoe" & Site.Num == 16)

tempdf %>%
  subset(DateTime.Local >= "2016-01-01" & DateTime.Local <= "2016-12-31") %>%
  ggplot() +
  geom_point(aes(x = DateTime.Local, y = Sample.Measurement))


