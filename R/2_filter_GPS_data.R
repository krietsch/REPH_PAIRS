#==============================================================================================================
# Data and code from "Mutual mate guarding and limited sexual conflict in a sex-role reversed shorebird"
# Contributor: Johannes Krietsch
# ❗This script is provided as reference only. It contains links to the internal database of the Max Planck 
# Institute for Ornithology, from which it pulls the data and exports all the collected data to ./DATA
#==============================================================================================================

### Summary
# NANO_TAGS_TEST data
# NANO_TAGS data

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'auksRuak', 'viridis', 'ggplot2'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#--------------------------------------------------------------------------------------------------------------
# NANO_TAGS_TEST data
#--------------------------------------------------------------------------------------------------------------

# All test data from Nano tags on the roof

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
# NANO_TAGS data
#--------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID != 999] # exclude test data
d[is.na(lon)] # check that no NA
d[, datetime_ := as.POSIXct(datetime_, tz = 'UTC')]
DBI::dbDisconnect(con)

# Change projection to equal area
st_transform_DT(d)

# 821 were filtered NA or duplicates
821 / (d %>% nrow + 821) * 100

#--------------------------------------------------------------------------------------------------------------
#' # Apply speed filter 
#--------------------------------------------------------------------------------------------------------------

# unique ID and order
d[, ID_year := paste0(ID, '_', year_)]
setorder(d, year_, ID_year, datetime_)

# calculate track characteristics
track_characteristics(d, ID = 'ID_year')
d[, speed := speed * 3.6] # m/s to km/h

# plot raw data (most positions are below 10 km/h)
d[speed < 10] %>% nrow # exclude from plot
d[speed > 200] %>% nrow # exclude from plot

ggplot(data = d[speed > 10 & speed < 200]) +
  geom_histogram(aes(x = speed), fill = 'grey85', color = 'grey50', binwidth = 5) +
  xlab('Speed (km/h)') +
  theme_classic()

# found fastest speed within a real track = 104 km/h
ID_ = 273145068
dt_ = as.POSIXct('2019-06-19 23:07:22', tz = 'UTC')
ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]

bm = create_bm(ds)

bm + 
  ggtitle(paste0(ds[speed > 100]$speed %>% round(., 2), ' km/h')) +
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = speed), size = 1.5) +
  scale_color_viridis(direction = -1)


# assign values above threshold
speed_filter(d, ID = 'ID_year', speed = 'speed', max_speed = 105)
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
#     ggtitle(paste0(de[i, ]$speed %>% round(., 2), ' km/h')) +
#     geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
#     geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
#     geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)
# 
#   ggsave(paste0('./OUTPUTS/INSPECTION/error_speed/', ID_, '.png'), plot = last_plot(),
#          width = 177, height = 177, units = c('mm'), dpi = 'print')
# 
# }

# visual inspection shows all clear errors
paste0(d[error == TRUE] %>% nrow, '/', d %>% nrow)

# remove errors
d = d[error == FALSE]

# calculate track characteristics again without errors
track_characteristics(d, ID = 'ID_year')
d[, speed := speed * 3.6] # m/s to km/h

ggplot(data = d[speed > 10 & speed < 200]) +
  geom_histogram(aes(x = speed), fill = 'grey85', color = 'grey50', binwidth = 5) +
  xlab('Speed (km/h)') +
  theme_classic()

#--------------------------------------------------------------------------------------------------------------
#' # Apply distance filter 
#--------------------------------------------------------------------------------------------------------------

# assign values above thresholds
distance_filter(d, ID = 'ID_year', distance_btw = 'distance_btw', max_distance = 2500, 
                max_distance_before_after = 100)
d[, .N, error]

# # visual check of the errors (showed 2.5 km and 100 m work to remove clear errors)
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
#     ggtitle(paste0(de[i, ]$speed %>% round(., 2), ' km/h ', de[i, ]$distance_btw %>% round(., 2), ' m')) +
#     geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
#     geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
#     geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)
# 
#   ggsave(paste0('./OUTPUTS/INSPECTION/error_distance/', ID_, '.png'), plot = last_plot(),
#          width = 177, height = 177, units = c('mm'), dpi = 'print')
# 
# }

# visual inspection shows all clear errors
paste0(d[error == TRUE] %>% nrow, '/', d %>% nrow)

# remove errors
d = d[error == FALSE]

# calculate track characteristics again without errors
track_characteristics(d, ID = 'ID_year')
d[, speed := speed * 3.6] # m/s to km/h

ggplot(data = d[speed > 10]) +
  geom_histogram(aes(x = speed), fill = 'grey85', color = 'grey50', binwidth = 5) +
  xlab('Speed (km/h)') +
  theme_classic()

ggplot(data = d[!is.na(speed)]) +
  geom_point(aes(speed, gps_speed)) +
  xlab('Speed (km/h)') +
  theme_classic()

#--------------------------------------------------------------------------------------------------------------
#' # Check altitude
#--------------------------------------------------------------------------------------------------------------

# plot raw data
ggplot(data = d[altitude > 200]) +
  geom_histogram(aes(x = altitude), fill = 'grey85', color = 'grey50', binwidth = 10) +
  xlab('Altitude (m)') +
  theme_classic()

# visual check of high altitudes
de = d[altitude > 2000]

# foreach(i = 1:nrow(de)) %do% {
# 
#   # subset
#   ID_ = de[i, ]$ID
#   dt_ = de[i, ]$datetime_
#   ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]
# 
#   # plot along time
#   ggplot() +
#     ggtitle(paste0(ds$altitude %>% max %>% round(., 2), ' m')) +
#     geom_path(data = ds, aes(datetime_, altitude, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
#     geom_point(data = ds, aes(datetime_, altitude, color = altitude), size = 1.5) +
#     scale_color_viridis(direction = -1) +
#     theme_classic()
# 
#   ggsave(paste0('./OUTPUTS/INSPECTION/high_altitude/', ID_, '.png'), plot = last_plot(),
#          width = 177, height = 177, units = c('mm'), dpi = 'print')
# 
#   # plot on map
#   bm = create_bm(ds)
# 
#   bm +
#     ggtitle(paste0(ds$altitude %>% max %>% round(., 2), ' m')) +
#     geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
#     geom_point(data = ds, aes(lon, lat, color = altitude), size = 1.5) +
#     scale_color_viridis(direction = -1)
# 
#   ggsave(paste0('./OUTPUTS/INSPECTION/high_altitude/', ID_, '_map.png'), plot = last_plot(),
#          width = 177, height = 177, units = c('mm'), dpi = 'print')
# 
# }

# could all be real! 70532 left in strong headwind and turned. Might have reached 7600 m!


# subset highest flight
ID_ = 270170532
dt_ = as.POSIXct('2019-07-11 14:33:56', tz = 'UTC')
ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]

# plot on map
bm = create_bm(ds)

bm + 
  ggtitle(paste0(ds$altitude %>% max %>% round(., 2), ' m')) +
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = altitude), size = 1.5) +
  scale_color_viridis(direction = -1)

# plot along time
ggplot() +
  ggtitle(paste0(ds$altitude %>% max %>% round(., 2), ' m')) +
  geom_path(data = ds, aes(datetime_, altitude, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(datetime_, altitude, color = altitude), size = 1.5) +
  scale_color_viridis(direction = -1) +
  theme_classic()

# subset what happend afterwards
ID_ = 270170532
ds = d[ID == ID_ & datetime_ > as.POSIXct('2019-07-11 11:33:56', tz = 'UTC')]

# last position
d[ID == 270170532]$datetime_ %>% max

# plot on map
bm = create_bm(ds)

bm + 
  ggtitle(paste0(ds$altitude %>% max %>% round(., 2), ' m')) +
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = speed), size = 1.5) +
  scale_color_viridis(direction = -1)

# plot along time
ggplot() +
  geom_path(data = ds, aes(datetime_, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(datetime_, lat, color = speed), size = 1.5) +
  scale_color_viridis(direction = -1) +
  theme_classic()

# checked db, male had chicks hatched at 30 June 
# how long did he drift?
ds = d[ID == ID_ & datetime_ > as.POSIXct('2019-07-17 11:33:56', tz = 'UTC') & lat > -2075000]
ds$datetime_ %>% max - ds$datetime_ %>% min

#--------------------------------------------------------------------------------------------------------------

# subset relevant data
d = d[, .(year_, tagID, ID, datetime_, lat, lon, gps_speed, altitude, batvolt)]

# save data
fwrite(d, './DATA/NANO_TAGS.txt', quote = TRUE, sep = '\t', row.names = FALSE)


# version information
sessionInfo()
