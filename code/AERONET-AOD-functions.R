#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# AERONET-AOD-functions.R
# Desc - 
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# Read AOD to data frame (In progress)
#--------------------------------------------------------------#
load_all.aod_data <- function (){  
  # import ALL AERONET file into MonetDBlite on disk (even if you don't have much RAM)
  # the trick is that it has some metadata lines at the top
  
  ### file paths you will need to change
  # absolute path to downloaded Aeronet data (e.g. version 3 level 2.0)
  # https://aeronet.gsfc.nasa.gov/cgi-bin/combined_data_access_v3
  # after downloading file should have the following name (your path will differ)
  aeronet.file <- file.path("/Users/darylalbano/Downloads/All_Sites_Times_All_Points_AOD20.dat")
  # aeronet.file <- file.path("/Users/darylalbano/NEX-AtmosphericAerosolDynamics/data/20140101_20171231_Univ_of_Nevada-Reno/20140101_20171231_Univ_of_Nevada-Reno.lev20")
  # aeronet.file <- file.path("/Users/darylalbano/NEX-AtmosphericAerosolDynamics/data/20140101_20171231_NASA_Ames/20140101_20171231_NASA_Ames.lev20")
  
  # folder where monetdb can put the database (absolute path)
  dbdir <- file.path( "/Users/darylalbano/Downloads" , 'rudis' )
  # (if we don't want this to be persistent we can use a temporary directory)
  
  
  ### check the top of the file
  # this AERONET is a large text file! 
  utils:::format.object_size(file.size(aeronet.file), "auto")
  # can't read it all in at once without a lot of RAM
  
  # we can see that it starts with some metadata
  top <- readLines(aeronet.file, n = 8)
  # we see it is comma delimited
  # and we see the headers are really on line 7
  top
  
  mdb <- dbConnect(MonetDBLite(), dbdir)
  
  # now we guess the column types by reading in a small fraction of the rows
  # important to skip the first 6 rows as we decided above
  guess <- read.csv(aeronet.file, stringsAsFactors=FALSE, nrows=1000, skip = 6)
  create <- sprintf("CREATE TABLE allaeronet ( %s )", 
                    paste0(sprintf('"%s" %s', colnames(guess), 
                                   sapply(guess, dbDataType, dbObj=mdb)), collapse=","))
  
  # just from guess we can see which are interesting fields
  names(guess)
  # lots of fields that might have AOD data
  aodnames <- grep("AOD_[0-9]{3,4}nm", names(guess), value = TRUE)
  length(aodnames)
  # and how many non-missing do we have per row (in this first sample of data)
  table(apply(guess[, aodnames], 1, function(x) sum(x != -999)))
  
  ## pulling all of the data into MonetDBlite ####
  ### prep to make the table
  # make a connection and get rid of the old table if it exists since
  # we are just playing around. in real life you prbly want to keep
  # the giant table there vs recreate it every time
  try(invisible(dbSendQuery(mdb, "DROP TABLE allaeronet")), silent=TRUE)
  
  # we build the table creation dynamically from what we've learned from guessing
  invisible(dbSendQuery(mdb, create))
  
  # and then we load the data into the database, skipping the metadata lines & header and specifying a comma
  # took 11 minutes on a macbook air
  system.time(invisible(dbSendQuery(mdb, paste0("COPY OFFSET 8 
                                                INTO allaeronet 
                                                FROM '", 
                                                aeronet.file, 
                                                "' USING  DELIMITERS ',' NULL as ''"))))
  
  # now wire it up to dplyr
  # mdb_src <- src_monetdb(embedded="/Users/darylalbano/Downloads/rudis/")
  mdb_src <- src_monetdb(embedded="/Users/darylalbano/Downloads/rudis/")
  mdb_aeronet <- dplyr::tbl(src = mdb_src, "allaeronet")
  
  # and now we can use dplyr verbs to work with the underlying object
  # https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
  # mdb_aeronet %>% summarise(n = n())
  glimpse(mdb_aeronet)
  # next steps would be to filter down to the subset of data we want and work with it
  
  # (WIP) - Removing -999 values, simpler method
  # aod.m <- as.matrix(mdb_aeronet)
  # aod.m[aod.m < 0] <- 0
  # aod.df <- as.data.frame(aod.m)
  
  aod.df <- as.data.frame(mdb_aeronet)
  
  # Replace (:) with (/) for Date column
  aod.df$Date.dd.mm.yyyy. <- gsub(':', '/', aod.df$Date.dd.mm.yyyy.)
  
  # Changing date format of Date.GMT. Corrects to "yyyy-mm-dd"
  aod.df$Date.dd.mm.yyyy. <- as.Date(aod.df$Date.dd.mm.yyyy., format = "%d/%m/%Y")
  
  # Rename date/time columns to Date.GMT and Time.GMT since those units were measured in GMT time zone
  aod.df <- dplyr::rename(aod.df, Date.GMT = Date.dd.mm.yyyy.)
  aod.df <- dplyr::rename(aod.df, Time.GMT = Time.hh.mm.ss.)
  
  # Joining date and time into a new column "DateTime.GMT"
  aod.df$DateTime.GMT <- ymd_hms(as.POSIXct(paste(aod.df$Date.GMT, 
                                                                 aod.df$Time.GMT), format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")
  
  # Round time to nearest hour
  aod.df$DateTime.GMT <- round_date(aod.df$DateTime.GMT, unit = "hour")
  
  # shut it down when done
  dbDisconnect(mdb, shutdown = TRUE)
  
  # if the dbdir was a non-temporary location then the data remain there
  utils:::format.object_size(sum(file.info(list.files(dbdir, all.files = TRUE, full.names = TRUE, recursive = TRUE))$size), "auto")

    # return data frame
  return(aod.df)
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.aod_data <- function(site_names) {
  site_names <- c("Univ_of_Nevada-Reno")
  df <- subset(all.aod, AERONET_Site %in% site_names)
  local_tz <- tz_lookup_coords(lat = unique(df$Site_Latitude.Degrees.),
                                        lon = unique(df$Site_Longitude.Degrees.),
                                        method = "accurate")
  df$DateTime.Local <- with_tz(df$DateTime.GMT, tzone = local_tz)
  df$Time.Local <- hms::as.hms(df$DateTime.Local)
  df$Date.Local <- lubridate::date(df$DateTime.Local)
  df$Year.Local <- lubridate::year(df$DateTime.Local)
  df$Month.Local <- lubridate::month(df$DateTime.Local, label = TRUE, abbr = FALSE)
  
  temp_seasonlist <- as.yearqtr(as.yearmon(df$DateTime.Local, "%F") + 1/12)
  df$Season.Local <- factor(format(temp_seasonlist, "%q"), levels = 1:4,
                            labels = c("Winter", "Spring", "Summer", "Fall"))
  return(df)
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.aod_sites.reno <- function() {
  reno.site_names <- c("Univ_of_Nevada-Reno")
  # After filtering data by sitename, remove -999 values
  aod.df <- filter.aod_data(reno.site_names)
  return(aod.df)
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.aod_sites.denv <- function() {
  
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.aod_sites.balt <- function() {
  
}

#--------------------------------------------------------------#
# @desc:
# @param:
#--------------------------------------------------------------#
filter.aod_sites.ny <- function() {
  
}

#--------------------------------------------------------------#
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
    # labs(x = "Hour", y = "PM2.5 Concentration (Micrograms/cubic meter)", color = ("Site")) +
    scale_x_continuous(breaks = c(0, 6, 12, 18, 23),
                       label = c("Midnight", "06:00", "Noon", "18:00", "23:00")) +
    ggtitle(paste0("PM2.5 FRM - Aggregated Hourly data - ", df$County.Name, ", ", df$State.Name),
            subtitle = paste0(unique(years), collapse = ", ")) +
    theme_bw()
}

#--------------------------------------------------------------#
# @desc: Plots correlation b/w daily average and daily peak
# @param: 
#--------------------------------------------------------------#
plot.corr.daily_avg_peak.aod <- function(df, years = years.all, seasons = seasons.all) {
  df <- df %>%
    subset(subset = Year.Local %in% years &
             Season.Local %in% seasons &
             AOD_500nm > 0.0)
  
  ag <- do.call(data.frame, aggregate(AOD_500nm ~ Date.Local+Season.Local+Year.Local, df, 
                                      FUN = function(df) c(Mean = mean(df), 
                                                           Peak = max(df))))
  
  cors <- ddply(ag, c("Season.Local"), 
                summarise, cor = round(cor(AOD_500nm.Mean, 
                                           AOD_500nm.Peak), 2))

  # num_counts <- ddply(cors, c("Season.Local"),
  #                 summarise, num_count = tally(cors))  
  
  ag %>%
    ggplot(aes(x = AOD_500nm.Mean, y = AOD_500nm.Peak)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(Season.Local ~ Year.Local) +
    # facet_wrap(~ Season.Local) +
    labs(x = "Daily Average (mg/m^3)", y = "Daily Peak (mg/m^3)") +
    ggtitle(paste0("PM2.5 FRM - Correlation Coefficient (Daily Average vs. Daily Peak) - ", df$County.Name, ", ", df$State.Name),
            subtitle = paste0(unique(years), collapse = ", ")) +
    geom_text(data = cors, aes(label = paste("r = ", cor)),
              x = -Inf, y = Inf, hjust = -0.2, vjust = 2.2) +
    theme_bw()
}
