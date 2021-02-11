#==============================================================================================================
# Filter GPS data using a speed and distance filter
#==============================================================================================================

# Summary
# 1. Apply speed filter 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf'), 
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
DBI::dbDisconnect(con)

# Change projection to equal area
st_transform_DT(d)

#--------------------------------------------------------------------------------------------------------------
# 1. Apply speed filter 
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
  geom_histogram(aes(x = speed), fill = 'grey50', color = 'grey20', binwidth = 5) +
  xlab('speed (km/h)') +
  theme_classic()

# found fastest speed within a real track = 104 km/h
ID_ = 273145068
dt_ = anytime('2019-06-19 23:07:22')
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
  geom_histogram(aes(x = speed), fill = 'grey50', color = 'grey20', binwidth = 5) +
  xlab('speed (km/h)') +
  theme_classic()

#--------------------------------------------------------------------------------------------------------------
# 2. Apply distance filter 
#--------------------------------------------------------------------------------------------------------------

# assign values above thresholds
distance_filter(d, ID = 'ID_year', distance_btw = 'distance_btw', max_distance = 2500, max_distance_before_after = 100)
d[, .N, error]

# visual check of the errors (showed 2.5 km and 100 m work to remove clear errors)
de = d[error == TRUE]

foreach(i = 1:nrow(de)) %do% {

  # subset
  ID_ = de[i, ]$ID
  dt_ = de[i, ]$datetime_
  ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]

  # plot
  bm = create_bm(ds)
  bm +
    ggtitle(paste0(de[i, ]$speed %>% round(., 2), ' km/h ', de[i, ]$distance_btw %>% round(., 2), ' m')) +
    geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
    geom_point(data = ds[error == FALSE], aes(lon, lat), color = 'dodgerblue4', size = 1.5) +
    geom_point(data = ds[error == TRUE], aes(lon, lat), color = 'firebrick2', size = 1.5)

  ggsave(paste0('./OUTPUTS/INSPECTION/error_distance/', ID_, '.png'), plot = last_plot(),
         width = 177, height = 177, units = c('mm'), dpi = 'print')

}


# visual inspection shows all clear errors
paste0(d[error == TRUE] %>% nrow, '/', d %>% nrow)

# remove errors
d = d[error == FALSE]

# calculate track characteristics again without errors
track_characteristics(d, ID = 'ID_year')
d[, speed := speed * 3.6] # m/s to km/h

ggplot(data = d[speed > 10]) +
  geom_histogram(aes(x = speed), fill = 'grey50', color = 'grey20', binwidth = 5) +
  xlab('speed (km/h)') +
  theme_classic()

ggplot(data = d[!is.na(speed)]) +
  geom_point(aes(speed, gps_speed, color = error)) +
  theme_classic()

#--------------------------------------------------------------------------------------------------------------
# 3. Check altitude
#--------------------------------------------------------------------------------------------------------------

d_save = copy(d)


ggplot(data = d[altitude > 200]) +
  geom_histogram(aes(x = altitude), fill = 'grey50', color = 'grey20', binwidth = 10) +
  xlab('altitude (m)') +
  theme_classic()



d[altitude > 6000]


ID_ = 270170532
dt_ = anytime('2019-07-11 14:33:56')
ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]


ggplot() +
  ggtitle(paste0(ds[altitude > 6000]$altitude %>% round(., 2), ' km/h')) +
  geom_path(data = ds, aes(datetime_, altitude, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(datetime_, altitude, color = altitude), size = 1.5) +
  scale_color_viridis(direction = -1) +
  theme_classic()

bm = create_bm(ds)

bm + 
  ggtitle(paste0(ds[altitude > 6000]$altitude %>% round(., 2), ' m')) +
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = altitude), size = 1.5) +
  scale_color_viridis(direction = -1)





bm = create_bm(ds)

bm + 
  ggtitle(paste0(ds[altitude > 6000]$altitude %>% round(., 2), ' km/h')) +
  geom_path(data = ds, aes(datetime_, altitude, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(datetime_, altitude, color = altitude), size = 1.5) +
  scale_color_viridis()


ggplot() +
  ggtitle(paste0(ds[altitude > 6000]$altitude %>% round(., 2), ' km/h')) +
  geom_path(data = ds, aes(datetime_, altitude, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(datetime_, altitude, color = altitude), size = 1.5)




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

















