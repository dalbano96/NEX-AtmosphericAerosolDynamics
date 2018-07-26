#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-main.R
# Desc - 
#--------------------------------------------------------------#

# Load all data from csv files
system.time(hourly.pm25.FRM.14_17 <- load_all_csv.pm_data())

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno, NV
#--------------------------------------------------------------#
# Here is where the pm data is filtered for Reno sites.
# Reno filter function contains specific filter parameters for Reno (Site number, County, etc.)
# Date can be specified. If not, default date range includes all data from [2014-01-01 to 2017-12-31]
pm_sites.reno <- filter.pm_sites.reno()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Baltimore, MD
#--------------------------------------------------------------#
pm_sites.balt <- filter.pm_sites.balt()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Denver, CO
#--------------------------------------------------------------#
pm_sites.denv <- filter.pm_sites.denv()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of NYC, NY
#--------------------------------------------------------------#
pm_sites.ny <- filter.pm_sites.ny()


#--------------------------------------------------------------#
# Hourly PM2.5 Data of Los Angeles, CA
#--------------------------------------------------------------#
pm_sites.LosAng <- filter.pm_sites.LosAng()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Hawaii
#--------------------------------------------------------------#
pm_sites.hawaii <- filter.pm_sites.hawaii()

Sys.sleep()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of all sites
#--------------------------------------------------------------#
complete_pmsites <- NULL
complete_pmsites <- bind_rows(pm_sites.reno, pm_sites.balt)
complete_pmsites <- bind_rows(complete_pmsites, pm_sites.denv)
complete_pmsites <- bind_rows(complete_pmsites, pm_sites.ny)
complete_pmsites <- bind_rows(complete_pmsites, pm_sites.LosAng)
complete_pmsites <- bind_rows(complete_pmsites, pm_sites.hawaii)

plot.hourly_mean.pm(pm_sites.hawaii)

plot.cc.monthly(complete_pmsites, "Reno, Baltimore, Denver, New York, Los Angeles, Hawaii")
plot.cc.seasonal(complete_pmsites, "Reno, Baltimore, Denver, New York, Los Angeles, Hawaii")
plot.cc.hourly(complete_pmsites, "Reno, Baltimore, Denver, New York, Los Angeles, Hawaii")

plot.daily_mean.peak.pm(pm_sites.hawaii, "2017", "May")
plot.daily_mean.peak.pm(pm_sites.ny, "2017", "June")
plot.daily_mean.peak.pm(pm_sites.reno, "2017", "May")
