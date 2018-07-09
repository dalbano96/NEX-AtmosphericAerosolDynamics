#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-main.R
# Desc - 
#--------------------------------------------------------------#

# Load all data from csv files
hourly.pm25.FRM.14_17 <- load_all_csv.pm_data()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno, NV
#--------------------------------------------------------------#
# Here is where the pm data is filtered for Reno sites.
# Reno filter function contains specific filter parameters for Reno (Site number, County, etc.)
# Date can be specified. If not, default date range includes all data from [2014-01-01 to 2017-12-31]
pm_sites.reno <- filter.pm_sites.reno()

# Plots all hourly PM data for Reno
plot.all.pm(pm_sites.reno)

# Plots aggregated hourly data for Reno
# Able to specify which year(s) and month(s) to observe
plot.hourly_mean.pm(pm_sites.reno, years.all, months.all)

plot.hourly_mean.pm(pm_sites.reno, years.all, c("January", "July"))

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Baltimore, MD
#--------------------------------------------------------------#
pm_sites.balt <- filter.pm_sites.balt()
plot.all.pm(pm_sites.balt)
