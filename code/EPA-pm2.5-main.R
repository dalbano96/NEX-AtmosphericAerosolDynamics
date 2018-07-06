#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-main.R
# Desc - Loads and tidys EPA PM2.5 data
#--------------------------------------------------------------#

# Load all data from csv files
hourly.pm25.FRM.14_17 <- load_all_csv.pm_data()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno, NV
#--------------------------------------------------------------#
pm_sites.reno <- filter.pm_sites.reno("2014-01-01 00:00",
                                      "2017-12-31 23:00",
                                      1)
plot.pm(pm_sites.reno)

#--------------------------------------------------------------#
# TODO: Integrate aggregate step into script(s)
#--------------------------------------------------------------#
ag <- aggregate(Sample.Measurement ~ Time.Local+Month.Local+Year.Local, mv.sites, geometric_mean)

ag %>%
  subset(Year.Local == "2014"
         & Month.Local == "January") %>%
  ggplot(aes(x = Time.Local, y = Sample.Measurement)) +
  geom_point()
