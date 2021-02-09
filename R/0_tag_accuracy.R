#=========================================================================================================================
# Check the accuracy of the tags
#=========================================================================================================================

# Summary
# 1. Get GPS locations of the test tags (average way point)
# 2. Merge actual location with all points
# 3. Quantify the error

# Packages
sapply( c('data.table', 'sdb','foreach', 'wadeR', 'sdbvis', 'auksRuak', 'ggplot2', 'windR', 'sf'),
        require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID == 999] # ID = 999 are the test data, tags where on the BASC building 

#-------------------------------------------------------------------------------------------------------------------------
# 1. Get GPS locations of the test tags (average way point)
#-------------------------------------------------------------------------------------------------------------------------

g  = dbq(con, "SELECT gps_id, gps_point, datetime_ gps_time, 
               lat, lon FROM FIELD_2018_REPHatBARROW.GPS_POINTS")
DBI::dbDisconnect(con)

dl = data.table(gps_id = rep(2, 10),
                gps_point = rep(85:89, each = 2),
                tagID = 91:100)

dl = merge(dl, g, by = c('gps_point', 'gps_id') )

#-------------------------------------------------------------------------------------------------------------------------
# 2. Merge actual location with all points
#-------------------------------------------------------------------------------------------------------------------------

# transform in equal area projection
st_transform_DT(dl)
st_transform_DT(d)

d = merge(d, dl[, .(tagID, lat_wp = lat, lon_wp = lon)], by = 'tagID', all.x = TRUE)

# calculate difference between WP and Nanotag
d[, dist := sqrt(sum((c(lat, lon) - c(lat_wp, lon_wp))^2)) , by = 1:nrow(d)]

# exclude some data from tag 94 that had many totally wrong positions
d = d[dist < 1000]

# mean tag position
d[, lat_m  := median(lat),  by = tagID]
d[, lon_m := median(lon), by = tagID]

# calculate difference between WP and mean Nanotag position
d[, dist_m := sqrt(sum((c(lat_m, lon_m) - c(lat_wp, lon_wp))^2)) , by = 1:nrow(d)]

# calculate difference mean Nanotag position and each Nanotag position
d[, dist_each_m := sqrt(sum((c(lat, lon) - c(lat_m, lon_m))^2)) , by = 1:nrow(d)]

d = d[dist < 1000]

d1 = d[, .SD[1], by = tagID]

#-------------------------------------------------------------------------------------------------------------------------
# 3. Quantify the error with test locations
#-------------------------------------------------------------------------------------------------------------------------

median_ = median(d$dist_each_m)
q95 = quantile(d$dist_each_m, probs = c(0.95))

ggplot(data = d) +
  ggtitle('Distance test location') +
  geom_histogram(aes(dist_each_m), bins = 60, fill = 'grey85', color = 'grey50') +
  geom_vline(xintercept = median_, color = 'firebrick3') +
  geom_text(aes(median_, Inf, label = round(median_, 1)), vjust = 1, hjust = -1, size = 8, color = 'firebrick3') +
  geom_vline(xintercept = q95, color = 'dodgerblue4') +
  geom_text(aes(q95, Inf, label = round(q95, 1)), vjust = 1, hjust = -1, size = 8, color = 'dodgerblue4') +
  xlab('Distance (m)') +
  theme_bw(base_size = 18)

p = 
ggplot(data = d[dist_each_m < 40]) +
  ggtitle('Distance test location') +
  geom_histogram(aes(dist_each_m), bins = 60, fill = 'grey85', color = 'grey50') +
  geom_vline(xintercept = median_, color = 'firebrick3') +
  geom_text(aes(median_, Inf, label = round(median_, 1)), vjust = 1, hjust = -1, size = 8, color = 'firebrick3') +
  geom_vline(xintercept = q95, color = 'dodgerblue4') +
  geom_text(aes(q95, Inf, label = round(q95, 1)), vjust = 1, hjust = -1, size = 8, color = 'dodgerblue4') +
  xlab('Distance (m)') +
  theme_bw(base_size = 18)
p 

ggsave('./OUTPUTS/FIGURES/Error_positions_on_roof.png', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')

quantile(d$dist_each_m, probs = seq(0, 1, 0.05))
quantile(d$dist, probs = seq(0, 1, 0.05))


plot(lat_wp ~ lon_wp, d1, col = 'blue')
points(lat_m ~ lon_m, d1, col = 'red')


plot(lat ~ lon, d)
points(lat_wp ~ lon_wp, d1, col = 'blue')
points(lat_m ~ lon_m, d1, col = 'red')

#-------------------------------------------------------------------------------------------------------------------------
# 4. Accuracy with real incubation data
#-------------------------------------------------------------------------------------------------------------------------

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
dn = dbq(con, 'select * FROM NESTS')
DBI::dbDisconnect(con)

d[, datetime_ := as.POSIXct(datetime_)]
d = d[tagID == 92]
d = d[datetime_ > as.POSIXct('2018-06-20 12:22:00')]

dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# change projection
st_transform_DT(d)
dn = dn[!is.na(lon)]
st_transform_DT(dn)

# Incubation data
b = read.table('//ds/raw_data_kemp/FIELD/Barrow/2018/DATA/RAW_DATA/MSR/R203_2018_07_16_MSR323219_180625_155302.csv', skip = 27, header = FALSE, sep = ';')

b = data.table(datetime_  = as.POSIXct(b$V1),
               t_surface = as.numeric(b$V2),
               t_nest    = as.numeric(b$V4))

# subset time on nest
b = b[datetime_ > as.POSIXct('2018-06-25 15:56:00') & datetime_ < as.POSIXct('2018-07-14 12:30:00')]

datetimes_inc = b$datetime_
d[, clostest_inc := closestDatetime(datetime_, datetimes_inc), by = 1:nrow(d)]

closestDatetime(datetime_ = d$datetime_[1000], datetimes = b$datetime_)

datetime_ = d$datetime_[1000]
datetimes = b$datetime_


cN = which(abs(datetimes - datetime_) == min(abs(datetimes - 
                                                   datetime_)))
cD = as.POSIXct(datetimes[cN])
cD[1]

b[t_nest > 30, inc_t := 1]
b[t_nest < 30, inc_t := 0]

d = merge(d, b[, .(datetime_, t_nest, inc_t)], by.x = 'clostest_inc', by.y = 'datetime_', all.x = TRUE)

# assign unknown (before MSR was installed)
d[datetime_ < as.POSIXct('2018-06-25 15:56:00') | datetime_ > as.POSIXct('2018-07-14 12:30:00'), inc_t := NA]

# subset nest
n = dn[nestID == 'R203_18']

# plot data 
bm = create_bm(d[!is.na(inc_t)], buffer = 10, sc_dist = 10)

p = 
  bm +
  geom_point(data = d[!is.na(inc_t)], aes(lon, lat, color = factor(inc_t)), size = 0.2) +
  geom_point(data = n, aes(lon, lat), color = 'black', size = 2, alpha = 0.5) +
  scale_colour_manual(values = c('dodgerblue4', 'firebrick3'), labels = c('off nest', 'on nest'), name = c('T>30°C'))
p

# ggsave('./REPORTS/Error_positions_on_nest_map.png', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')

# calculate distance to nest
n[, .(lon, lat)]
d[, dist := sqrt(sum((c(lon, lat) - c(-403.5346, -2076970))^2)) , by = 1:nrow(d)]

# plot distance
median_ = median(d[inc_t == 1]$dist)
q95 = quantile(d[inc_t == 1]$dist, probs = c(0.95))

ggplot(data = d[inc_t == 1]) +
  ggtitle('Distance from nest R203_18 while T>30°C') +
  geom_histogram(aes(dist), bins = 60, fill = 'grey85', color = 'grey50') +
  geom_vline(xintercept = median_, color = 'firebrick3') +
  geom_text(aes(median_, Inf, label = round(median_, 1)), vjust = 1, hjust = -1, size = 8, color = 'firebrick3') +
  geom_vline(xintercept = q95, color = 'dodgerblue4') +
  geom_text(aes(q95, Inf, label = round(q95, 1)), vjust = 1, hjust = -1, size = 8, color = 'dodgerblue4') +
  xlab('Distance (m)') +
  theme_bw(base_size = 18)

p = 
ggplot(data = d[inc_t == 1 & dist < 40]) +
  ggtitle('Distance from nest R203_18 while T>30°C') +
  geom_histogram(aes(dist), bins = 60, fill = 'grey85', color = 'grey50') +
  geom_vline(xintercept = median_, color = 'firebrick3') +
  geom_text(aes(median_, Inf, label = round(median_, 1)), vjust = 1, hjust = -1, size = 8, color = 'firebrick3') +
  geom_vline(xintercept = q95, color = 'dodgerblue4') +
  geom_text(aes(q95, Inf, label = round(q95, 1)), vjust = 1, hjust = -1, size = 8, color = 'dodgerblue4') +
  xlab('Distance (m)') +
  theme_bw(base_size = 18)
p

# ggsave('./OUTPUTS/FIGURES/Error_positions_on_nest.png', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')

d[inc_t == 1 & dist <10] %>% nrow / d[inc_t == 1] %>% nrow
d[inc_t == 1 & dist <15] %>% nrow / d[inc_t == 1] %>% nrow



d[inc_t == 0 & dist <10] %>% nrow / d[inc_t == 0] %>% nrow
d[inc_t == 0 & dist <15] %>% nrow / d[inc_t == 0] %>% nrow





