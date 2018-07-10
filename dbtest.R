#--------------------------------------------------------------#
# Read all Level 2.0 AOD Data Points
#--------------------------------------------------------------#

library(MonetDBLite)
library(DBI)
library(dplyr)

aeronet.file <- file.path("/Users/darylalbano/NEX-AtmosphericAerosolDynamics/data/All_Sites_Times_All_Points_AOD20.dat")
dbdir <- file.path("/Users/darylalbano/NEX-AtmosphericAerosolDynamics/data/", 'rudis')
utils:::format.object_size(file.size(aeronet.file), "auto")

top <- readLines(aeronet.file, n = 8)
top

mdb <- dbConnect(MonetDBLite(), dbdir)
guess <- read.csv(aeronet.file, stringsAsFactors = FALSE, nrows = 1000, skip = 6)
create <- sprintf("CREATE TABLE allaeronet ( %s )",
                  paste0(sprintf('"%s" %s', colnames(guess),
                                 sapply(guess, dbDataType, dbObj=mdb)), collapse=","))
names(guess)
aodnames <- grep("AOD_[0-9]{3,4}nm", names(guess), value = TRUE)
length(aodnames)
table(apply(guess[, aodnames], 1, function(x) sum(x != -999)))

try(invisible(dbSendQuery(mdb, "DROP TABLE allaeronet")), silent=TRUE)

invisible(dbSendQuery(mdb, create))

# 11 minutes?!
system.time(invisible(dbSendQuery(mdb, paste0("COPY OFFSET 8 
                                 INTO allaeronet 
                                              FROM '", 
                                              aeronet.file, 
                                              "' USING  DELIMITERS ','"))))

mdb_src <- src_monetdb(embedded="/Users/darylalbano/NEX-AtmosphericAerosolDynamics/data/rudis")
mdb_aeronet <- tbl(mdb_src, "allaeronet")

mdb_aeronet %>% summarise(n = n())
glimpse(mdb_aeronet)

# shut it down when done
dbDisconnect(mdb, shutdown=TRUE)

