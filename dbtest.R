# import ALL AERONET file into MonetDBlite on disk (even if you don't have much RAM)
# the trick is that it has some metadata lines at the top
library(MonetDBLite)
library(DBI)
library(dplyr)

### file paths you will need to change
# absolute path to downloaded Aeronet data (e.g. version 3 level 2.0)
# https://aeronet.gsfc.nasa.gov/cgi-bin/combined_data_access_v3
# after downloading file should have the following name (your path will differ)
aeronet.file <- file.path("/Users/darylalbano/Downloads/All_Sites_Times_All_Points_AOD20.dat")

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
mdb <- dbConnect(MonetDBLite(), dbdir)
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
mdb_src <- src_monetdb(embedded="/Users/darylalbano/Downloads/rudis/")
mdb_aeronet <- tbl(mdb_src, "allaeronet")

# and now we can use dplyr verbs to work with the underlying object
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
mdb_aeronet %>% summarise(n = n())
glimpse(mdb_aeronet)
# next steps would be to filter down to the subset of data we want and work with it

# shut it down when done
dbDisconnect(mdb, shutdown=TRUE)

# if the dbdir was a non-temporary location then the data remain there
utils:::format.object_size(sum(file.info(list.files(dbdir, all.files = TRUE, full.names = TRUE, recursive = TRUE))$size), "auto")