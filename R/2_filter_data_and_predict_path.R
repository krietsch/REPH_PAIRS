#========================================================================================================================
# Filter data and predict path
#========================================================================================================================

# Summary
# 1. Calculate basic track characteristics
# 2. 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'trip'), 
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID != 999] # exclude test data
d[is.na(lon)] # check that no NA
d[, datetime_ := anytime(datetime_)]
# d[, datetime_y := anytime(paste0('2020-', substr(datetime_, 6, 19)) )]

dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
dn = dn[year_ > 2016]
DBI::dbDisconnect(con)

# Change projection
st_transform_DT(d)
# st_transform_DT(dn)

#------------------------------------------------------------------------------------------------------------------------
# 1. Apply speed filter
#------------------------------------------------------------------------------------------------------------------------

# unique ID and order
d[, ID_year := paste0(ID, '_', year_)]
setorder(d, year_, ID_year, datetime_)

# distance between consecutive points
d[, lon2         := data.table::shift(lon, type = 'lead'), by = ID_year]
d[, lat2         := data.table::shift(lat, type = 'lead'), by = ID_year]
d[, distance_btw := sqrt(sum((c(lon, lat) - c(lon2, lat2))^2)) , by = 1:nrow(d)]

# time between consecutive points
d[, datetime_2   := data.table::shift(datetime_, type = 'lead'), by = ID_year]
d[, time_btw     := as.numeric(difftime(datetime_2, datetime_, units = 'sec'))]

# speed
d[, speed := distance_btw / time_btw]
hist(d$speed*3.6)
hist(d[speed > 5 & speed < 50]$speed*3.6, breaks = 40)

# values above threshold
speed_filter(d, ID = 'ID_year', speed = 'speed', max_speed = 30)

# d[, speed_over_threshold := speed > 30]
# d[, bout := windR::bCounter(speed_over_threshold), by = ID_year]
# d[, seq := seq_len(.N), by = .(ID_year, bout)]
# d[, error := seq == 2 & speed_over_threshold == TRUE]

# d[error == TRUE]

# d = d[is.na(error)]

DT = copy(d)
ID = 'ID_year' 
speed = 'speed'
max_speed = 30




speed_filter <- function(DT, ID, speed, max_speed){
  
  if(nrow(DT) > 0) {
    setnames(DT, c(ID, speed), c('IDu', 'speed'))
    
    # speed over may
    DT[, speed_over_threshold := speed > max_speed]
    
    # select couts
    bCounter <- function(x){
      n = length(x)
      y = x[-1] != x[-n]
      i = c(which(y | is.na(y)), n)
      lengths = diff(c(0L, i))
      bout_length = rep(lengths, lengths)
      ids = 1:length(lengths)
      bout_id = rep(ids, lengths)
      bout_id 
      }
    
    DT[, bout := bCounter(speed_over_threshold), by = IDu]
    
    # select only outlier
    DT[, seq := seq_len(.N), by = .(IDu, bout)]
    DT[, error := seq == 2 & speed_over_threshold == TRUE]
    
    # delete unwanted columns
    DT[ ,c('bout','seq') := NULL]
    
    setnames(DT, c('IDu', 'speed'), c(ID, speed))
    
  }
}



d[, .N, speed_over_threshold]


# check with example
ds = d[ID == 270170245]

setorder(ds, datetime_)

ds[speed_over_threshold == TRUE]

ds[bout < 3]


ggplot(ds) +
  geom_path(aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(aes(lon, lat, color = speed_over_threshold), size = 1.5) + 
  theme_bw()


ggplot(ds) +
  geom_path(aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(aes(lon, lat, color = error), size = 1.5) + 
  geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'red', size = 1.5)
  theme_bw()

ds[speed > 50]

50*3.6






# unique ID
d[, ID_year := paste0(year_, '_', ID)]

# distance between points
d[, lon2     := data.table::shift(lon, type = 'lead'), by = ID_year]
d[, lat2     := data.table::shift(lat, type = 'lead'), by = ID_year]
d[, dist     := sqrt(sum((c(lon, lat) - c(lon2, lat2))^2)) , by = 1:nrow(d)]

# flight time
d[, datetime_2  := data.table::shift(datetime_, type = 'lead'), by = ID_year]
d[, TbtwPoints  := as.numeric(difftime(datetime_2, datetime_, units = 'sec'))]
d[, flight_time := as.numeric(difftime(datetime_[c(.N)], datetime_[c(1)], units = 'hours')), by = ID_year]
hist(d$flight_time)

# ground speed
d[, g_speed := dist/TbtwPoints]
hist(d$g_speed*3.6)
hist(d[g_speed < 5]$g_speed*3.6, breaks = 40)

#------------------------------------------------------------------------------------------------------------------------
# 2. 
#------------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------------------------
# Outliers altitude
#------------------------------------------------------------------------------------------------------------------------

require(tsoutliers)

setorder(d, ID, datetime_)
d[, tagID_ID := paste0(tagID, '_', ID)]
d[, pointID  := seq_len(.N), by = tagID_ID]

d[pointID == 1 & altitude > 100]

d[, altitude_2  := data.table::shift(altitude, type = 'lead'), by = ID]
d[, delta_alt   := abs(altitude - altitude_2), by = ID]
d[, altitude_3  := data.table::shift(altitude, type = 'lag'), by = ID]
d[, delta_alt_2 := abs(altitude - altitude_3), by = ID]

d[delta_alt > 1300 & delta_alt_2 > 1300, outlier := TRUE]


ds = d[ID == 270170245]

ds = d[ID == 270170050]

ds = d[ID == 270170764]

ds = d[ID == 270170715]

ds = d[ID == 270170765]

ds = d[ID == 270170720]
ds = d[ID == 270170754]


ggplot(ds) +
  geom_path(aes(lon, lat, color = altitude), size = 0.5) + 
  geom_point(aes(lon, lat, color = altitude), size = 1.5) + 
  scale_color_viridis() +
  theme_bw()

ggplot() +
  geom_point(data = ds, aes(datetime_, altitude)) +
  geom_point(data = ds[delta_alt > 1000], aes(datetime_, delta_alt), color = 'red')

ggplot() +
  geom_point(data = ds, aes(datetime_, altitude)) +
  geom_point(data = ds[outlier == TRUE], aes(datetime_, altitude), color = 'red')





ggplot() +
  geom_point(data = ds, aes(datetime_, speed)) 



ggplot() +
  geom_point(data = d, aes(datetime_y, altitude)) +
  geom_point(data = d[delta_alt > 700], aes(datetime_y, altitude), color = 'red')


ggplot() +
  geom_point(data = d, aes(datetime_y, altitude)) +
  geom_point(data = d[outlier == TRUE], aes(datetime_y, altitude), color = 'red')



#-------------------------------------------------------------------------------------------------------------------------------------
# Deal with outliers in GPS positions
#-------------------------------------------------------------------------------------------------------------------------------------

require(trip)
require(sp)

ds = d[ID == 270170245]

ds = d[ID == 270170764]

ds = d[ID == 270170254]

setorder(ds, datetime_)


ggplot(ds) +
  geom_path(aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(aes(lon, lat, color = datetime_), size = 1.5) + 
  scale_color_viridis( trans = scales::time_trans()) +
  theme_bw()

rs = SpatialPointsDataFrame(ds[ , .(lon, lat)], ds[, .(datetime_ ,ID)], proj4string = (CRS(PROJ)), match.ID = FALSE)

plot(rs)





rs.trip <- trip(rs, TORnames = c('datetime_', 'ID'))


rs.filtered <- rs[speedfilter(rs.trip, max.speed = 1),]

plot(rs, col = 2)
plot(rs.filtered, add=T)




#-------------------------------------------------------------------------------------------------------------------------------------
# Ground Speed
#-------------------------------------------------------------------------------------------------------------------------------------

# calculate time between points
d[, datetime_2  := data.table::shift(datetime_, type = 'lead'), by = ID]
d[, TbtwPoints  := as.numeric(difftime(datetime_2, datetime_, units = 'sec'))]

d[, lon2 := data.table::shift(lon, type = 'lead'), by = ID]
d[, lat2 := data.table::shift(lat, type = 'lead'), by = ID]

# calculate difference between points
d[, dist := sqrt(sum((c(lat, lon) - c(lat2, lon2))^2)) , by = 1:nrow(d)]

# ground speed
d[, g_speed := dist/TbtwPoints]


plot(g_speed ~ speed, d)
plot(g_speed ~ speed, d[g_speed < 25 & speed < 25])




boxplot(d[g_speed < 25 & g_speed > 1]$g_speed)
boxplot(d[speed < 25 & speed > 1]$speed)






