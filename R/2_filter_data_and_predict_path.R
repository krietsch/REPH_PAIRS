#========================================================================================================================
# Filter data and predict path
#========================================================================================================================

# Summary
# 1. Calculate basic track characteristics
# 2. 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'trip', 'foreach', 'sf'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

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

# Change projection to equal area
st_transform_DT(d)
# st_transform_DT(dn)

d_save = copy(d)

#------------------------------------------------------------------------------------------------------------------------
# 1. Apply speed filter
#------------------------------------------------------------------------------------------------------------------------

# step to not always load data again 
d = copy(d_save)


# unique ID and order
d[, ID_year := paste0(ID, '_', year_)]
setorder(d, year_, ID_year, datetime_)

# calculate track characteristics
track_characteristics(d, ID = 'ID_year')

# plot speed
hist(d$speed*3.6)
hist(d[speed > 5 & speed < 50]$speed*3.6, breaks = 40)
quantile(d$speed, probs = c(0.99), na.rm = TRUE)

# assign values above threshold
speed_filter(d, ID = 'ID_year', speed = 'speed', max_speed = 30)
d[, .N, error]

# # visual check of the errors
# de = d[error == TRUE]
# 
# foreach(i = 1:nrow(de)) %do% {
# 
#   # subset
#   ID_ = de[i, ]$ID
#   dt_ = de[i, ]$datetime_
#   ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]
# 
#   # plot
#   bm = create_bm(ds)
#   bm +
#     geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
#     geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
#     geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5) 
# 
#   ggsave(paste0('./OUTPUTS/FIGURES/error_speed_filter_30/', ID_, '.png'), plot = last_plot(),
#          width = 177, height = 177, units = c('mm'), dpi = 'print')
# 
# }
# 
# ggplot(data = d[!is.na(speed)]) +
#   geom_point(aes(speed, gps_speed, color = error))

# all clear errors

# remove errors
d = d[error == FALSE]

# calculate track characteristics again without errors
track_characteristics(d, ID = 'ID_year')

# assign values above threshold
speed_filter(d, ID = 'ID_year', speed = 'speed', max_speed = 20)
d[, .N, error]

# # visual check of the errors
# de = d[error == TRUE]
# 
# foreach(i = 1:nrow(de)) %do% {
# 
#   # subset
#   ID_ = de[i, ]$ID
#   dt_ = de[i, ]$datetime_
#   ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]
# 
#   # plot
#   bm = create_bm(ds)
#   bm +
#     geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
#     geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
#     geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)
# 
#   ggsave(paste0('./OUTPUTS/FIGURES/error_speed_filter_20/', ID_, '.png'), plot = last_plot(),
#          width = 177, height = 177, units = c('mm'), dpi = 'print')
# 
# }
# 
# ggplot(data = d[!is.na(speed)]) +
#   geom_point(aes(speed, gps_speed, color = error))

# all clear errors 

# remove errors
d = d[error == FALSE]

# calculate track characteristics again without errors
track_characteristics(d, ID = 'ID_year')

# assign values above threshold
speed_filter(d, ID = 'ID_year', speed = 'speed', max_speed = 20)
d[, .N, error]

# visual check of the errors
de = d[error == TRUE]

d[, speed20 := speed > 20]
de = d[speed20 == TRUE]

d[, speed_over_threshold := speed > 20]

# # visual check of the errors
# de = d[error == TRUE]
# 
# foreach(i = 1:nrow(de)) %do% {
#   
#   # subset
#   ID_ = de[i, ]$ID
#   dt_ = de[i, ]$datetime_
#   ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]
#   
#   # plot
#   bm = create_bm(ds)
#   bm +
#     geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
#     geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
#     geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)
#   
#   ggsave(paste0('./OUTPUTS/FIGURES/error/', ID_, '.png'), plot = last_plot(),
#          width = 177, height = 177, units = c('mm'), dpi = 'print')
#   
# }
# 
# ggplot(data = d[!is.na(speed)]) +
#   geom_point(aes(speed, gps_speed, color = error))

# some are real tracks


# How to separate errors and and true positions
distance_filter(d, ID = 'ID_year', distance_btw = 'distance_btw', max_distance = 1500, max_distance_before_after = 100)

d[, speed20 := speed > 10]



# visual check of the errors
de = d[error == TRUE]

foreach(i = 1:nrow(de)) %do% {

  # subset
  ID_ = de[i, ]$ID
  dt_ = de[i, ]$datetime_
  ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]

  # plot
  bm = create_bm(ds)
  bm +
    geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
    geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
    geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)

  ggsave(paste0('./OUTPUTS/FIGURES/error_distance/', ID_, '.png'), plot = last_plot(),
         width = 177, height = 177, units = c('mm'), dpi = 'print')

}

# remove errors
d = d[error == FALSE]

# calculate track characteristics again without errors
track_characteristics(d, ID = 'ID_year')

ggplot(data = d[!is.na(speed)]) +
  geom_point(aes(speed, gps_speed, color = error))

hist(d[speed > 5 & speed < 50]$speed*3.6, breaks = 40)


d[speed > 20]





# step to not always load data again 
d = copy(d_save)


# unique ID and order
d[, ID_year := paste0(ID, '_', year_)]
setorder(d, year_, ID_year, datetime_)

# calculate track characteristics
track_characteristics(d, ID = 'ID_year')

# How to separate errors and and true positions
distance_filter(d, ID = 'ID_year', distance_btw = 'distance_btw', max_distance = 1500, max_distance_before_after = 100)

d[error == TRUE]

# visual check of the errors
de = d[error == TRUE]

foreach(i = 1:nrow(de)) %do% {
  
  # subset
  ID_ = de[i, ]$ID
  dt_ = de[i, ]$datetime_
  ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]
  
  # plot
  bm = create_bm(ds)
  bm +
    geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
    geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
    geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)
  
  ggsave(paste0('./OUTPUTS/FIGURES/error_distance_only/', ID_, '.png'), plot = last_plot(),
         width = 177, height = 177, units = c('mm'), dpi = 'print')
  
}



# remove errors
d = d[error == FALSE]

# calculate track characteristics
track_characteristics(d, ID = 'ID_year')

ggplot(data = d[!is.na(speed)]) +
  geom_point(aes(speed, gps_speed, color = error))

hist(d[speed > 5 & speed < 50]$speed*3.6, breaks = 40)

d[speed > 20]

# went this fast
ID_ = 270170765
dt_ = anytime('2018-07-14 03:01:25')

ID_ = 273145068
dt_ = anytime('2019-06-19 23:07:22')

ID_ = 273145087
dt_ = anytime('2019-06-20 21:55:42')


ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]

bm = create_bm(ds)

bm + 
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = speed), size = 1.5) +
  scale_color_viridis(direction = -1)





# create test data
d1 = data.table(ID = 'bird1', lat = 1:10, lon = 1:10, 
                datetime_ = seq('2021-01-21 00:01:00' %>% as.POSIXct, '2021-01-21 02:30:00' %>% as.POSIXct, by = '15 mins'))

d2 = data.table(ID = 'bird2', lat = 2:11, lon = 1:10, 
                datetime_ = seq('2021-01-21 00:01:00' %>% as.POSIXct, '2021-01-21 02:30:00' %>% as.POSIXct, by = '15 mins'))

d3 = data.table(ID = 'bird3', lat = 2:11, lon = 1:10, 
                datetime_ = seq('2021-01-21 00:01:00' %>% as.POSIXct, '2021-01-21 02:30:00' %>% as.POSIXct, by = '15 mins'))

# add outlier
d2[5, lat := lat + 20]

# add movement
d3[6, lat := lat + 9]
d3[7:10, lat := lat + 20]
# d3[8, lat := lat + 10]

d = rbindlist(list(d1, d2, d3))

ggplot(data = d) +
  geom_point(aes(lon, lat, group = ID, color = ID))

track_characteristics(d, ID = 'ID')


# speed filter
# speed_filter(d, ID = 'ID', speed = 'speed', max_speed = 0.02)

# distance filter
distance_filter(d, ID = 'ID', distance_btw = 'distance_btw', max_distance = 5, max_distance_before_after = 3)

# d[, lon_before := data.table::shift(lon, type = 'lag'), by = ID]
# d[, lat_before := data.table::shift(lat, type = 'lag'), by = ID]
# d[, lon_after  := data.table::shift(lon, type = 'lead'), by = ID]
# d[, lat_after  := data.table::shift(lat, type = 'lead'), by = ID]
# d[, distance_btw_ab := sqrt(sum((c(lon_after, lat_after) - c(lon_before, lat_before))^2)), by = 1:nrow(d)]
# 
# d[distance_over_threshold_ab := distance_btw_ab > max_distance_before_after]


ggplot() +
  geom_path(data = d, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
  geom_point(data = d[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
  geom_point(data = d[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)


d





# distance between point before and after
d[, lon_1         := data.table::shift(lon, type = 'lag'), by = ID]
d[, lat_1         := data.table::shift(lat, type = 'lag'), by = ID]
d[, distance_btw_1 := sqrt(sum((c(lon2, lat2) - c(lon_1, lat_1))^2)) , by = 1:nrow(d)]

d[, distance3_bigger_20 := distance_btw3 > 20]


de = d[speed_over_threshold == TRUE]


# went this fast
ID_ = 270170765
dt_ = anytime('2018-07-14 03:01:25')

ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]



ID_ = 270170073
dt_ = anytime('2018-06-18 11:56:05')

ds = d[ID == ID_ & datetime_ > c(dt_ - 3600) & datetime_ < c(dt_ + 3600)]


ggplot(data = ds) +
  geom_point(aes(datetime_, speed))

ggplot(data = ds) +
  geom_point(aes(datetime_, distance_btw))

ds = d[ID == ID_]


ggplot(data = ds) +
  geom_point(aes(datetime_, distance_btw))


ggplot(data = d[!is.na(speed)]) +
  geom_point(aes(speed, gps_speed, color = error))



d[, speed20 := speed > 20]

d[speed20 == TRUE]


d[, .N, error]

# went this fast
ID_ = 270170765
dt_ = anytime('2018-07-14 03:01:25')

ID_ = 270170073
dt_ = anytime('2018-06-18 11:56:05')


ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]

bm = create_bm(ds)

bm + 
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = speed), size = 1.5) +
  scale_color_viridis(direction = -1)


ggplot(ds) +
  geom_path(aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(aes(lon, lat, color = speed), size = 1.5) + 
  theme_bw()


bm = create_bm(d, buffer = 15000)

bm + 
  geom_path(data = d, aes(lon, lat, group = ID), size = 0.2, color = 'black', alpha = 0.5) 


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






