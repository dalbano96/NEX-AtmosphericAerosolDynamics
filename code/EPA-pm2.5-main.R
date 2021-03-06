#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# EPA-pm2.5-main.R
# Desc - 
#--------------------------------------------------------------#

# Load all data from csv files
system.time(all.pm <- load_all_csv.pm_data())

# Set default values for years, months, and seasons based on available PM data
years.all <- unique(all.pm$Year.Local)
months.all <- unique(all.pm$Month.Local)
seasons.all <- unique(all.pm$Season.Local)

# Load temperature data
system.time(all.temperature <- load_all_csv.temperature())

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno, NV
#--------------------------------------------------------------#
# Here is where the pm data is filtered for Reno sites.
# Reno filter function contains specific filte r parameters for Reno (Site number, County, etc.)
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

#--------------------------------------------------------------#
# PM2.5 Data for SF-Oak Region
#--------------------------------------------------------------#
pm_sites.RA <- filter.pm_sites.RA()

#--------------------------------------------------------------#
# PM2.5 Data for Philadelphia Region
#--------------------------------------------------------------#
pm_sites.RB <- filter.pm_sites.RB()

#--------------------------------------------------------------#
# PM2.5 Data for Minneapolis, MN Region
#--------------------------------------------------------------#
pm_sites.RC <- filter.pm_sites.RC()

#--------------------------------------------------------------#
# Hourly PM2.5 Data of all sites
#--------------------------------------------------------------#
# complete_pmsites <- NULL
# complete_pmsites <- bind_rows(pm_sites.reno, pm_sites.balt)
# complete_pmsites <- bind_rows(complete_pmsites, pm_sites.denv)
# complete_pmsites <- bind_rows(complete_pmsites, pm_sites.ny)
# complete_pmsites <- bind_rows(complete_pmsites, pm_sites.LosAng)
# complete_pmsites <- bind_rows(complete_pmsites, pm_sites.hawaii)
