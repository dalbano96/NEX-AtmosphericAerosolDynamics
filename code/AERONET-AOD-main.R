#--------------------------------------------------------------#
# Daryl Albano
# NEX - NASA Ames
# Summer 2018
# Atmospheric Aerosol Dynamics
# AERONET-AOD-main.R
# Desc - Loads and tidys AERONET data
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# Read AOD csv to data frame
#--------------------------------------------------------------#
# 1) Importing AOD datasets
hourly.AOD.AMES.14_17 <- NULL
hourly.AOD.AMES.14_17 <- read.csv("data/20140101_20171231_NASA_Ames/20140101_20171231_NASA_Ames.csv", stringsAsFactors = FALSE)

# 2) Changing date format of Date.GMT. Corrects to "yyyy-mm-dd"
hourly.AOD.AMES.14_17$Date.dd.mm.yyyy. <- as.Date(hourly.AOD.AMES.14_17$Date.dd.mm.yyyy., format = "%m/%d/%y")

# 3) Rename date/time columns to Date.GMT and Time.GMT since those units were measured in GMT time zone
hourly.AOD.AMES.14_17 <- rename(hourly.AOD.AMES.14_17, Date.GMT = Date.dd.mm.yyyy.)
hourly.AOD.AMES.14_17 <- rename(hourly.AOD.AMES.14_17, Time.GMT = Time.hh.mm.ss.)

# 4) Joining date and time into a new column "DateTime.GMT"
# Setting to GMT time zone
hourly.AOD.AMES.14_17$DateTime.GMT <- ymd_hms(as.POSIXct(paste(hourly.AOD.AMES.14_17$Date.GMT,
                                                               hourly.AOD.AMES.14_17$Time.GMT),
                                                         format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")

# Converting to local (PDT) time zone
hourly.AOD.AMES.14_17$DateTime.Local <- with_tz(hourly.AOD.AMES.14_17$DateTime.GMT, tz = "America/Los_Angeles")

# 5) Setting all -999 to NA TODO: There has to be a better way to format this.
hourly.AOD.AMES.14_17 <- hourly.AOD.AMES.14_17 %>%
  mutate(AOD_1640nm = replace(AOD_1640nm, AOD_1640nm == -999, NA)) %>%
  mutate(AOD_1020nm = replace(AOD_1020nm, AOD_1020nm == -999, NA)) %>%
  mutate(AOD_870nm = replace(AOD_870nm, AOD_870nm == -999, NA)) %>%
  mutate(AOD_675nm = replace(AOD_675nm, AOD_675nm == -999, NA)) %>%
  mutate(AOD_500nm = replace(AOD_500nm, AOD_500nm == -999, NA)) %>%
  mutate(AOD_440nm = replace(AOD_440nm, AOD_440nm == -999, NA)) %>%
  mutate(AOD_380nm = replace(AOD_380nm, AOD_380nm == -999, NA)) %>%
  mutate(AOD_340nm = replace(AOD_340nm, AOD_340nm == -999, NA)) %>%
  mutate(X440.870_Angstrom_Exponent = replace(X440.870_Angstrom_Exponent, X440.870_Angstrom_Exponent == -999, NA))

# Graph!
hourly.AOD.AMES.14_17 %>%
  na.omit() %>%
  subset(DateTime.GMT >= "2015-08-01 00:00" & DateTime.GMT <= "2015-12-31 23:59") %>%
  ggplot(aes(x = Time.GMT, y = X440.870_Angstrom_Exponent)) +
  geom_point(aes(group = as.character(Date.GMT)))

# Collecting needed columns
testdf <- NULL
testdf <- hourly.AOD.AMES.14_17[, c(114,5:7,10,19,22,25,26)]
testdf <- gather(testdf, AOD, value, -DateTime.GMT)

# 2015
ggplot(subset(testdf, DateTime.GMT > "2015-01-01" & DateTime.GMT < "2015-12-31"),
       aes(x = DateTime.GMT, y = value, group = AOD, color = AOD)) +
  geom_point() +
  ggtitle("2015")

# 2016
ggplot(subset(testdf, DateTime.GMT > "2016-01-01" & DateTime.GMT < "2016-12-31"),
       aes(x = DateTime.GMT, y = value, group = AOD, color = AOD)) +
  geom_point() +
  ggtitle("2016")


#--------------------------------------------------------------#
# Read AOD csv to data frame (Reno, NV)
#--------------------------------------------------------------#
# 1) Importing AOD datasets
hourly.AOD.Reno.14_17 <- NULL
hourly.AOD.Reno.14_17 <- read.csv("data/20140101_20171231_Univ_of_Nevada-Reno/20140101_20171231_Univ_of_Nevada-Reno.csv", stringsAsFactors = FALSE)

# 2) Changing date format of Date.GMT. Corrects to "yyyy-mm-dd"
hourly.AOD.Reno.14_17$Date.dd.mm.yyyy. <- as.Date(hourly.AOD.Reno.14_17$Date.dd.mm.yyyy., format = "%m/%d/%y")

# 3) Rename date/time columns to Date.GMT and Time.GMT since those units were measured in GMT time zone
hourly.AOD.Reno.14_17 <- rename(hourly.AOD.Reno.14_17, Date.GMT = Date.dd.mm.yyyy.)
hourly.AOD.Reno.14_17 <- rename(hourly.AOD.Reno.14_17, Time.GMT = Time.hh.mm.ss.)

# 4) Joining date and time into a new column "DateTime.GMT"
hourly.AOD.Reno.14_17$DateTime.GMT <- as.POSIXct(paste(hourly.AOD.Reno.14_17$Date.GMT,
                                                       hourly.AOD.Reno.14_17$Time.GMT),
                                                 format = "%Y-%m-%d %H:%M:%S")

# 5) Setting all -999 to NA TODO: There has to be a better way to format this.
hourly.AOD.Reno.14_17 <- hourly.AOD.Reno.14_17 %>%
  mutate(AOD_1640nm = replace(AOD_1640nm, AOD_1640nm == -999, NA)) %>%
  mutate(AOD_1020nm = replace(AOD_1020nm, AOD_1020nm == -999, NA)) %>%
  mutate(AOD_870nm = replace(AOD_870nm, AOD_870nm == -999, NA)) %>%
  mutate(AOD_675nm = replace(AOD_675nm, AOD_675nm == -999, NA)) %>%
  mutate(AOD_500nm = replace(AOD_500nm, AOD_500nm == -999, NA)) %>%
  mutate(AOD_440nm = replace(AOD_440nm, AOD_440nm == -999, NA)) %>%
  mutate(AOD_380nm = replace(AOD_380nm, AOD_380nm == -999, NA)) %>%
  mutate(AOD_340nm = replace(AOD_340nm, AOD_340nm == -999, NA)) %>%
  mutate(X440.870_Angstrom_Exponent = replace(X440.870_Angstrom_Exponent, X440.870_Angstrom_Exponent == -999, NA))

# Graph!
start.date <- "2017-01-01 00:00"
end.date <- "2017-12-31 23:59"

hourly.AOD.Reno.14_17 %>%
  na.omit() %>%
  subset(DateTime.GMT >= start.date & DateTime.GMT <= end.date & X440.870_Angstrom_Exponent > 0.00) %>%
  ggplot(aes(x = DateTime.GMT, y = X440.870_Angstrom_Exponent)) +
  geom_point() +
  xlab("Time") +
  ggtitle(paste0("University of Nevada at Reno", " [", start.date, " to ", end.date, "]"))


# # Graph of AOD
# ggplot(subset(hourly.AOD.AMES.14_17, Date.GMT > "2015-08-15" & Date.GMT < "2015-09-13")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_1640nm, color = "AOD_1640nm")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_1020nm, color = "AOD_1020nm")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_870nm, color = "AOD_870nm")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_675nm, color = "AOD_675nm")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_500nm, color = "AOD_500nm")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_440nm, color = "AOD_440nm")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_380nm, color = "AOD_380nm")) +
#   geom_point(aes(x = DateTime.GMT, y = AOD_340nm, color = "AOD_340nm"))
