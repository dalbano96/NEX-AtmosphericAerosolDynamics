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
