#==============================================================================================================
# Data and code from "Mutual mate guarding and limited sexual conflict in a sex-role reversed shorebird"
# Contributor: Johannes Krietsch
# ❗This script is provided as reference only. It contains links to the internal database of the Max Planck 
# Institute for Ornithology, from which it pulls the data and exports all the collected data to ./DATA
#==============================================================================================================

### Summary
# NANO_TAGS_TEST
# CAPTURES
# NESTS
# RESIGHTINGS
# PATERNITY

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'auksRuak', 'ggplot2'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#--------------------------------------------------------------------------------------------------------------
# NANO_TAGS_TEST
#--------------------------------------------------------------------------------------------------------------

# All test data from Nanotags on the roof

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID == 999] # ID = 999 are the test data, tags where on the BASC building 
d[, datetime_ := as.POSIXct(datetime_, tz = 'UTC')]
g = dbq(con, "SELECT gps_id, gps_point, datetime_ gps_time, 
               lat, lon FROM FIELD_2018_REPHatBARROW.GPS_POINTS")
DBI::dbDisconnect(con)

# table with tagID and GPS waypoints
dl = data.table(gps_id = rep(2, 10),
                gps_point = rep(85:89, each = 2),
                tagID = 91:100)

dl = merge(dl, g, by = c('gps_point', 'gps_id') )

# merge actual location with all points
d = merge(d, dl[, .(tagID, lat_wp = lat, lon_wp = lon)], by = 'tagID', all.x = TRUE)

# subset data relevant for this study
d = d[, .(year_, tagID, datetime_, lat, lon, lat_wp, lon_wp)]

# check data
summary(d)
sapply(d, function(x) sum(is.na(x)))

# save data
write.table(d, './DATA/NANO_TAGS_TEST.txt', quote = TRUE, sep = '\t', row.names = FALSE)


# setnames(d, c('lat', 'lon'), c('lat1', 'lon1'))
# 
# st_transform_DT(d, lat = 'lat1', lon = 'lon1')
# st_transform_DT(d, lat = 'lat_wp', lon = 'lon_wp')

#--------------------------------------------------------------------------------------------------------------
# NANO_TAGS
#--------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID != 999] # exclude test data
d[is.na(lon)] # check that no NA
d[, datetime_ := as.POSIXct(datetime_, tz = 'UTC')]
DBI::dbDisconnect(con)






