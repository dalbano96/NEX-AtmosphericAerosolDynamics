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

# Plots all hourly PM data for Reno
plot.all.pm(pm_sites.reno)
ggsave("figures/Reno NV/Reno-allpts.png", dpi = 300)

# Plots aggregated hourly data for Reno
# Able to specify which year(s) and month(s) to observe
plot.hourly_mean.pm(pm_sites.reno)
plot.hourly_mean.pm(pm_sites.reno, years.all[1])
plot.hourly_mean.pm(pm_sites.reno, "2016")
ggsave("figures/Reno NV/Reno-12mo1yr.png", dpi = 300)
plot.hourly_mean.pm(pm_sites.reno, years.all, months.all[c(1,8)])
plot.hourly_mean.pm(pm_sites.reno, years.all, months.all[c(3,6,9,12)])
ggsave("figures/Reno-3mo4yr-01.png", dpi = 300)
plot.hourly_mean.pm(pm_sites.reno, "2016", months.all[1:12])
plot.hourly_mean.pm(pm_sites.reno)

# Plot cyclic cubic splines for Reno, NV
mod <- NULL
mod <- gamm(Sample.Measurement ~ s(as.numeric(Month.Local), bs = "cc", k = 12) + s(Time.Local),
            data = pm_sites.reno)
layout(matrix(1:2, ncol = 2))
plot(mod$gam, scale = 0)
layout(1)

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Baltimore, MD
#--------------------------------------------------------------#
pm_sites.balt <- filter.pm_sites.balt()
plot.all.pm(pm_sites.balt)
plot.hourly_mean.pm(pm_sites.balt)
plot.hourly_mean.pm(pm_sites.balt, years.all[1:2])
plot.hourly_mean.pm(pm_sites.balt, "2014")

# Plot cyclic cubic splines for Baltimore, MD
mod <- NULL
mod <- gamm(Sample.Measurement ~ s(as.numeric(Month.Local), bs = "cc", k = 12) + s(Time.Local),
            data = pm_sites.balt)
layout(matrix(1:2, ncol = 2))
plot(mod$gam, scale = 0)
layout(1)

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Denver, CO
#--------------------------------------------------------------#
pm_sites.denv <- filter.pm_sites.denv()
plot.all.pm(pm_sites.denv)
plot.hourly_mean.pm(pm_sites.denv)
plot.hourly_mean.pm(pm_sites.denv, years.all, months.all[c(1, 6,12)])
plot.hourly_mean.pm(pm_sites.denv, "2014")

# Plot cyclic cubic splines for Denver, CO
mod <- NULL
mod <- gamm(Sample.Measurement ~ s(as.numeric(Month.Local), bs = "cc", k = 12) + s(Time.Local),
            data = pm_sites.denv)
layout(matrix(1:2, ncol = 2))
plot(mod$gam, scale = 0)
layout(1)

#--------------------------------------------------------------#
# Hourly PM2.5 Data of NYC, NY
#--------------------------------------------------------------#
pm_sites.ny <- filter.pm_sites.ny()
plot.all.pm(pm_sites.ny)
plot.all.pm(pm_sites.ny, "2015", months.all[1:4])
plot.hourly_mean.pm(pm_sites.ny)
plot.hourly_mean.pm(pm_sites.ny, "2016")
plot.hourly_mean.pm(pm_sites.ny, years.all, months.all[4:8])

mod <- NULL
mod <- gamm(Sample.Measurement ~ s(as.numeric(Month.Local), bs = "cc", k = 12) + s(Time.Local),
            data = pm_sites.ny)
layout(matrix(1:2, ncol = 2))
plot(mod$gam, scale = 0)
layout(1)


# Binding all sites (WIP)
filtered_pmsites <- NULL
filtered_pmsites <- bind_rows(pm_sites.reno, pm_sites.balt)
filtered_pmsites <- bind_rows(filtered_pmsites, pm_sites.denv)
filtered_pmsites <- bind_rows(filtered_pmsites, pm_sites.ny)

# Testing correlation coefficient
pm_aeronet <- function() {
  ag_pm <- aggregate(Sample.Measurement ~ Time.Local+Month.Local+Year.Local, 
                  pm_sites.reno, geometric.mean)
  ag_aeronet <- aggregate(X440.870_Angstrom_Exponent)
  
}

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Los Angeles, CA
#--------------------------------------------------------------#
pm_sites.LosAng <- filter.pm_sites.LosAng()
plot.all.pm(pm_sites.LosAng)
pm_sites.LosAng %>%
  plot.hourly_mean.pm("2016")

mod <- NULL
mod <- gamm(Sample.Measurement ~ s(as.numeric(Month.Local), bs = "cc", k = 12) + s(Time.Local),
            data = pm_sites.LosAng)
layout(matrix(1:2, ncol = 2))
plot(mod$gam, scale = 0)
layout(1)

mod <- gamm(Sample.Measurement ~ s(Time.Local, bs = "cc", k = 24),
            data = pm_sites.LosAng)
plot(mod$gam, scale = 0)

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Hawaii
#--------------------------------------------------------------#
pm_sites.hawaii <- filter.pm_sites.hawaii()

plot.hourly_mean.pm(pm_sites.hawaii)

mod <- NULL
mod <- gamm(Sample.Measurement ~ s(as.numeric(Month.Local), bs = "cc", k = 12) + s(Time.Local),
            data = pm_sites.hawaii)
layout(matrix(1:2, ncol = 2))
plot(mod$gam, scale = 0)
layout(1)
