#==============================================================================================================
# Method comparision
#==============================================================================================================

# Summary
# 1. 
# 2.  
# 3. 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table

dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_10min)]
dp[, year_ := year(date_)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Example pair with good data coverage
#--------------------------------------------------------------------------------------------------------------

# subset pair
# d = d[ID %in% c(270170763, 270170764)] # R909_18
d = d[ID %in% c(270170746, 270170747)] # R304_18

# round times to 10 min intervalls
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_10min := round(datetime_, '10 mins')]
d[, datetime_ := datetime_ %>% as.character %>% as.POSIXct]
d[, datetime_10min := datetime_10min %>% as.character %>% as.POSIXct]

# check for duplicates by ID
d[, duplicated := duplicated(d, by = c('ID', 'datetime_10min'))]
d[duplicated == TRUE] %>% nrow

# mean of these instances
d = d[, .(year_ = mean(year_), datetime_ = mean(datetime_), lat = mean(lat), lon = mean(lon), 
          gps_speed = mean(gps_speed), altitude = mean(altitude), batvolt = mean(batvolt)), 
      by = .(ID, datetime_10min)]

anyDuplicated(d, by = c('ID', 'datetime_10min'))

# merge positions with sex
d[, ID := as.character(ID)]
d = merge(d, dg[, .(ID, sex)], by = 'ID', all.x = TRUE)
d[, ID := as.numeric(ID)]

# plot tracks
bm = create_bm(d, buffer = 1200, squared = TRUE)
bm = create_bm(d, buffer = 1200)

bm +
  geom_path(data = d, aes(lon, lat, group = ID, color = datetime_), size = 0.7, alpha = 0.5) + 
  geom_point(data = d, aes(lon, lat, color = datetime_, fill = sex), size = 1, shape = 21) +
  scale_color_viridis( trans = scales::time_trans(), name = 'Date')
  

#--------------------------------------------------------------------------------------------------------------
#' # Proximity
#--------------------------------------------------------------------------------------------------------------

# dp = dp[ID1 == 270170763 & ID2 == 270170764] # R909_18
dp = dp[ID1 == 270170746 & ID2 == 270170747] # R304_18

# interaction based on distance threshold
dp[, interaction := distance < 30]

# split points and merging points
dp[, interaction_before := shift(interaction, type = 'lag'), by = ID1]
dp[, interaction_after := shift(interaction, type = 'lead'), by = ID1]
dp[, split := interaction == FALSE & interaction_before == TRUE & interaction_after == FALSE]
dp[, split_simple := interaction == FALSE & interaction_before == TRUE]

dp[, merge := interaction_before == FALSE & interaction == TRUE]

# look at data
ggplot(data = dp) +
  geom_point(aes(datetime_10min, distance, color = split)) +
  theme_classic()


#--------------------------------------------------------------------------------------------------------------
#' # Dynamic interaction
#--------------------------------------------------------------------------------------------------------------

library(adehabitatLT)
library(wildlifeDI)

dt = as.ltraj(xy = d[ ,c('lon', 'lat')], date = d$datetime_10min, id = d$ID, typeII = TRUE)

# seperate by ID
ID1 = dt[1]
ID2 = dt[2]

# shift time of track by 10 min
d[, datetime_10min_shift := datetime_10min + 600]

dt = as.ltraj(xy = d[ ,c('lon', 'lat')], date = d$datetime_10min_shift, id = d$ID, typeII = TRUE)

# seperate by ID
ID1s = dt[1]
ID2s = dt[2]

# Correlation coefficient
Cr(ID1, ID2, tc = 5*60)

Cr(ID1s, ID2, tc = 5*60)
Cr(ID1, ID2s, tc = 5*60)


# DI - Dynamic interaction index
DI(ID1, ID2, tc = 5*60)

# obtain the local di analysis data-frame
di.df = DI(ID1, ID2, tc = 5*60, local = TRUE)

# Examine the temporal dynamics of local di
ggplot(data = di.df) +
  geom_line(aes(date, di)) +
  theme_classic()

# Smoothed version of local di
di.df$smooth <- 0

# 4 fixes/hour x 6 hours on either side of 12 hour centered window
w <- 6*3 
n <- dim(di.df)[1]   #no. of fixes

for (i in (w+1):(n-1-w)){
  di.temp <- di.df$di[(i-w):(i+w)]
  di.df$smooth[i] <- mean(di.temp,na.rm=T)
}

# plot smoothed data
ggplot(data = di.df) +
  geom_line(aes(date, smooth)) +
  theme_classic()

# merge with dp
di.df = data.table(di.df)
di.df[, datetime_10min := date]
dp = merge(dp, di.df[, .(datetime_10min, di, dis = smooth)], by = 'datetime_10min', all.x = TRUE)


# IAB index
df = IAB(ID1, ID2, dc = 50, tc = 5*60, local = TRUE)

# plot
ggplot(data = df) +
  geom_line(aes(date, Iab)) +
  theme_classic()


# merge with dp
df = data.table(df)
df[, datetime_10min := date]
dp = merge(dp, df[, .(datetime_10min, Iab)], by = 'datetime_10min', all.x = TRUE)


#--------------------------------------------------------------------------------------------------------------
#' # Compare methods
#--------------------------------------------------------------------------------------------------------------


# look at data
ggplot(data = dp) +
  geom_point(aes(datetime_10min, distance, color = di)) +
  scale_color_viridis() +
  theme_classic()

# look at data
ggplot(data = dp) +
  geom_point(aes(datetime_10min, distance, color = Iab)) +
  scale_color_viridis() +
  theme_classic()







# interaction based on distance threshold and error buffer
dp[, interaction := distance < 30]
dp[interaction == FALSE & interaction_after == TRUE & interaction_before == TRUE, interaction := TRUE]


dp[, split := interaction == FALSE & interaction_before == TRUE & interaction_after == FALSE]
dp[, split_simple := interaction == FALSE & interaction_before == TRUE]

dp[, merge := interaction == TRUE & interaction_before == FALSE & interaction_after == TRUE]


ds = dp[datetime_10min > as.POSIXct('2018-06-22 05:30:00') & datetime_10min < as.POSIXct('2018-06-22 07:20:00')]

bm = create_bm(ds, lon = 'lon1', lat = 'lat1', buffer = 100)

ds[, point_id := seq_along(ID1)]

ds[interaction == TRUE, type := 'interaction']
ds[interaction == FALSE, type := 'no interaction']
ds[split == TRUE, type := 'split']
ds[merge == TRUE, type := 'merge']

dp[, diff_time := difftime(datetime_1, datetime_2, units = 'mins') %>% as.numeric]


hist(dp$diff_time)
dp[diff_time > 8]

# look at data
ggplot(data = ds) +
  geom_point(aes(datetime_10min, distance, color = type)) +
  theme_classic()

ggplot(data = ds) +
  geom_point(aes(datetime_10min, distance, color = merge)) +
  theme_classic()

ggplot(data = ds) +
  geom_point(aes(datetime_10min, distance, color = interaction)) +
  theme_classic()

bm +
  geom_path(data = ds, aes(lon1, lat1), color = 'dodgerblue3', size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon1, lat1), color = 'dodgerblue3', size = 1) +
  geom_path(data = ds, aes(lon2, lat2), color = 'firebrick3', size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon2, lat2), color = 'firebrick3', size = 1) +
  ggrepel::geom_label_repel(data = ds, aes(lon1, lat1, label = point_id), segment.color = 'grey50') +
  ggrepel::geom_label_repel(data = ds, aes(lon2, lat2, label = point_id), segment.color = 'grey50')


bm +
  geom_path(data = ds, aes(lon1, lat1, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon1, lat1, color = interaction), size = 1) +
  geom_path(data = ds, aes(lon2, lat2, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon2, lat2, color = interaction), size = 1) 

bm +
  geom_path(data = ds, aes(lon1, lat1, color = di), size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon1, lat1, color = di), size = 1) +
  geom_path(data = ds, aes(lon2, lat2, color = di), size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon2, lat2, color = di), size = 1) +
  scale_color_viridis(direction = -1)


bm +
  geom_path(data = ds, aes(lon1, lat1, group = ID1, color = Iab), size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon1, lat1, color = Iab), size = 1) +
  scale_color_viridis(direction = -1)

bm +
  geom_path(data = ds, aes(lon1, lat1, group = ID1, color = di), size = 0.7, alpha = 0.5) + 
  geom_point(data = ds, aes(lon1, lat1, color = di), size = 1) +
  scale_color_viridis(direction = -1)


# spatio temproal clusters


setorder(d, ID, datetime_)
d[, pointID := seq_len(.N), by = ID]

ID = unique(d$ID)

# register cores

d = foreach(i = ID, .combine = rbind, .packages = c('data.table','tdbscan') ) %do% {
  
  # subset individual and create track
  ds = d[ID == i]
  track = dt2Track(ds, y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)
  
  z = tdbscan(track, eps = 30, minPts = 3, maxLag = 6, borderPoints = TRUE )
  
  ds[, clustID := z$clustID]
  ds
  
}


d[!is.na(clustID), ID_clustID := paste0(ID, '_', clustID)]

# create dt with convex hull polygons
dc = dt2Convexhull(d[!is.na(clustID), .(ID_clustID, lat, lon, datetime_)],
                   pid = 'ID_clustID', y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)

s = stoscan(dc)

# merge with nests 
# d = rbind(d, n[, .(ID_clustID = nest, datetime_ = initiation, latit, longit, NARL)], fill = TRUE)

d = merge(d, s, by.x = 'ID_clustID', by.y = 'pid', all.x = TRUE)

setorder(d, ID, datetime_)
d



dss = d[datetime_ > as.POSIXct('2018-06-22 05:30:00') & datetime_ < as.POSIXct('2018-06-22 07:20:00')]


bm +
  geom_path(data = dss, aes(lon, lat, color = NULL), col = 'grey', size = .5) +
  geom_point(data = dss, aes(lon, lat, color = as.character(clustID)), alpha = .5, size = 2) # , show.legend = FALSE













