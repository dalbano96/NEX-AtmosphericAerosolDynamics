#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# AERONET-AOD-main.R
# Desc - Loads and tidys AERONET data
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# Read AOD from db to dataframe
#--------------------------------------------------------------#
system.time(all.aod <- load_all.aod_data())

aod_sites.reno <- filter.aod_sites.reno()
aod_sites.denv <- filter.aod_sites.denv()
aod_sites.balt <- filter.aod_sites.balt()
aod_sites.ny <- filter.aod_sites.ny()