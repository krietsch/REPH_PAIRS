#==============================================================================================================
# Method comparision
#==============================================================================================================

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

dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_1)]
dp[, year_ := year(date_)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

#--------------------------------------------------------------------------------------------------------------
#' # Example pair with good data coverage to compare methods
#--------------------------------------------------------------------------------------------------------------

# subset pair
d = d[ID %in% c(270170746, 270170747)] # R304_18
dp = dp[ID1 == 270170746 & ID2 == 270170747]
dn = dn[male_id == 270170746 & female_id == 270170747]

# merge with sex
dg[, ID := as.numeric(ID)]
d = merge(d, dg[, .(ID, sex)], by = 'ID', all.x = TRUE)

# merge with nest data 
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')
dp = merge(dp, dnID, by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'))

# relative nest initiation date
dp[, initiation_rel := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# look at data
bm = create_bm(d, buffer = 100)

bm +
  geom_path(data = d, aes(lon, lat, group = ID, color = sex), size = 0.7, alpha = 0.5) + 
  geom_point(data = d, aes(lon, lat, color = sex, fill = sex), size = 1, shape = 21) +
  geom_point(data = dn, aes(lon, lat), color = 'black', stroke = 2, size = 5, shape = 21) 

#--------------------------------------------------------------------------------------------------------------
#' # Simple distance threshold
#--------------------------------------------------------------------------------------------------------------

# distance threshold
threshold = sequence(15, 10, 10)

o = foreach(i = 1:length(threshold), .combine = 'rbind') %do% {
  
  distance_threshold = threshold[i]
  
  # interactions
  dp[, interaction := distance_pair < distance_threshold]

  ds = copy(dp)
  ds[, distance_threshold := distance_threshold]
  ds
 
}


setorder(o, initiation_rel)

ggplot(data = o) +
  geom_tile(aes(initiation_rel, factor(distance_threshold), fill = interaction), width = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Distance threshold') +
  theme_classic()

# ggsave('./OUTPUTS/ONE_PAIR/Barplot_dist_threshold_initiation_rel.png', plot = last_plot(),  width = 177, height = 80, units = c('mm'), dpi = 'print')



# distance threshold
threshold = sequence(10, 10, 10)


o = foreach(i = 1:length(threshold), .combine = 'rbind') %do% {
  
  distance_threshold = threshold[i]
  
  # interactions
  dp[, interaction := distance_pair < distance_threshold]
  
  # round to days
  dp[, initiation_rel0 := round(initiation_rel, 0)]
  
  # daily points of both individuals
  dp[, N_daily := .N, by = .(nestID, initiation_rel0)]
  
  # daily interactions
  dp[interaction == TRUE, N_together := .N, by = .(nestID, initiation_rel0)]
  dp[interaction == FALSE, N_together := NA]
  dp[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, initiation_rel0)]
  dp[is.na(N_together), N_together := 0]
  
  # unique data
  ds = unique(dp, by = c('nestID', 'initiation_rel0'))
  ds[, per_together := N_together / N_daily * 100]
  
  ds = ds[, .(per_together = median(per_together)), by = initiation_rel0]
  ds[, distance_threshold := distance_threshold]
  ds
  
}


setorder(o, initiation_rel0)

ggplot(data = o) +
  geom_point(aes(initiation_rel0, per_together, color = factor(distance_threshold), group = distance_threshold), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel0, per_together, color = factor(distance_threshold), group = distance_threshold), size = 1, alpha = 0.5) +
    scale_color_viridis(direction = -1, name = 'distance threshold', discrete = TRUE) +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.7))

# ggsave('./OUTPUTS/ONE_PAIR/Percentage_together_initiation_rel.png', plot = last_plot(),  width = 177, height = 80, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Simple distance threshold & minimal bout length
#--------------------------------------------------------------------------------------------------------------

# minimal bout length
bout_seq_max_value = sequence(6, 1, 1)

o = foreach(i = 1:length(bout_seq_max_value), .combine = 'rbind') %do% {
  
  bsm = bout_seq_max_value[i]
  
  # interactions
  dp[, interaction := distance_pair < 30]
  
  # count bouts of split and merge
  dp[, bout := bCounter(interaction), by = nestID]
  dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
  dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
  dp[interaction == FALSE & bout_seq_max <= i, interaction := TRUE]
  
  ds = copy(dp)
  ds[, bout_seq_max_value := bsm]
  ds
  
}


setorder(o, initiation_rel)

ggplot(data = o) +
  geom_tile(aes(initiation_rel, factor(bout_seq_max_value), fill = interaction), width = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('min bout length') +
  theme_classic()

# ggsave('./OUTPUTS/ONE_PAIR/Barplot_min_bout_initiation_rel.png', plot = last_plot(),  width = 177, height = 85, units = c('mm'), dpi = 'print')


# minimal bout length
bout_seq_max_value = sequence(6, 1, 1)


o = foreach(i = 1:length(bout_seq_max_value), .combine = 'rbind') %do% {
  
  bsm = bout_seq_max_value[i]
  
  # interactions
  dp[, interaction := distance_pair < 30]
  
  # count bouts of split and merge
  dp[, bout := bCounter(interaction), by = nestID]
  dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
  dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
  dp[interaction == FALSE & bout_seq_max <= i, interaction := TRUE]
  
  # round to days
  dp[, initiation_rel0 := round(initiation_rel, 0)]
  
  # daily points of both individuals
  dp[, N_daily := .N, by = .(nestID, initiation_rel0)]
  
  # daily interactions
  dp[interaction == TRUE, N_together := .N, by = .(nestID, initiation_rel0)]
  dp[interaction == FALSE, N_together := NA]
  dp[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, initiation_rel0)]
  dp[is.na(N_together), N_together := 0]
  
  # unique data
  ds = unique(dp, by = c('nestID', 'initiation_rel0'))
  ds[, per_together := N_together / N_daily * 100]
  
  ds = ds[, .(per_together = median(per_together)), by = initiation_rel0]
  ds[, bout_seq_max_value := bsm]
  ds
  
}


setorder(o, initiation_rel0)

ggplot(data = o) +
  geom_point(aes(initiation_rel0, per_together, color = factor(bout_seq_max_value), group = bout_seq_max_value), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel0, per_together, color = factor(bout_seq_max_value), group = bout_seq_max_value), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, name = 'minimal bout', discrete = TRUE) +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.8))

# ggsave('./OUTPUTS/ONE_PAIR/Percentage_together_min_bout_initiation_rel.png', plot = last_plot(),  width = 177, height = 80, units = c('mm'), dpi = 'print')



#--------------------------------------------------------------------------------------------------------------
#' # Include movement before to create a buffer
#--------------------------------------------------------------------------------------------------------------

### positions before and after

# shift positions
dp[, lat1_before := shift(lat1, type = 'lag'), by = nestID]
dp[, lon1_before := shift(lon1, type = 'lag'), by = nestID]
dp[, lat2_before := shift(lat2, type = 'lag'), by = nestID]
dp[, lon2_before := shift(lon2, type = 'lag'), by = nestID]

dp[, lat1_next := shift(lat1, type = 'lead'), by = nestID]
dp[, lon1_next := shift(lon1, type = 'lead'), by = nestID]
dp[, lat2_next := shift(lat2, type = 'lead'), by = nestID]
dp[, lon2_next := shift(lon2, type = 'lead'), by = nestID]

# distance to position before and after
dp[, distance1_before := sqrt(sum((c(lon1, lat1) - c(lon1_before, lat1_before))^2)) , by = 1:nrow(dp)]
dp[, distance1_next := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)) , by = 1:nrow(dp)]
dp[, distance2_before := sqrt(sum((c(lon2, lat2) - c(lon2_before, lat2_before))^2)) , by = 1:nrow(dp)]
dp[, distance2_next := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]


# distance threshold
threshold = sequence(15, 10, 10)

o = foreach(i = 1:length(threshold), .combine = 'rbind') %do% {
  
  distance_threshold = threshold[i]
  
  # interactions
  dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]
  
  ds = copy(dp)
  ds[, distance_threshold := distance_threshold]
  ds
  
}


setorder(o, initiation_rel)

ggplot(data = o) +
  geom_tile(aes(initiation_rel, factor(distance_threshold), fill = interaction), width = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Distance threshold') +
  theme_classic()

# ggsave('./OUTPUTS/ONE_PAIR/Barplot_movement_buffer_initiation_rel.png', plot = last_plot(),  width = 177, height = 85, units = c('mm'), dpi = 'print')


# distance threshold
threshold = sequence(10, 10, 10)


o = foreach(i = 1:length(threshold), .combine = 'rbind') %do% {
  
  distance_threshold = threshold[i]
  
  # interactions
  dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]
  
  # round to days
  dp[, initiation_rel0 := round(initiation_rel, 0)]
  
  # daily points of both individuals
  dp[, N_daily := .N, by = .(nestID, initiation_rel0)]
  
  # daily interactions
  dp[interaction == TRUE, N_together := .N, by = .(nestID, initiation_rel0)]
  dp[interaction == FALSE, N_together := NA]
  dp[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, initiation_rel0)]
  dp[is.na(N_together), N_together := 0]
  
  # unique data
  ds = unique(dp, by = c('nestID', 'initiation_rel0'))
  ds[, per_together := N_together / N_daily * 100]
  
  ds = ds[, .(per_together = median(per_together)), by = initiation_rel0]
  ds[, distance_threshold := distance_threshold]
  ds
  
}


setorder(o, initiation_rel0)

ggplot(data = o) +
  geom_point(aes(initiation_rel0, per_together, color = factor(distance_threshold), group = distance_threshold), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel0, per_together, color = factor(distance_threshold), group = distance_threshold), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, name = 'distance threshold', discrete = TRUE) +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.7))

# ggsave('./OUTPUTS/ONE_PAIR/Percentage_together_movement_buffer_initiation_rel.png', plot = last_plot(),  width = 177, height = 80, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' # Include movement before to create a buffer, each interaction bout should at least have one truly below 
#--------------------------------------------------------------------------------------------------------------

### positions before and after

# shift positions
dp[, lat1_before := shift(lat1, type = 'lag'), by = nestID]
dp[, lon1_before := shift(lon1, type = 'lag'), by = nestID]
dp[, lat2_before := shift(lat2, type = 'lag'), by = nestID]
dp[, lon2_before := shift(lon2, type = 'lag'), by = nestID]

dp[, lat1_next := shift(lat1, type = 'lead'), by = nestID]
dp[, lon1_next := shift(lon1, type = 'lead'), by = nestID]
dp[, lat2_next := shift(lat2, type = 'lead'), by = nestID]
dp[, lon2_next := shift(lon2, type = 'lead'), by = nestID]

# distance to position before and after
dp[, distance1_before := sqrt(sum((c(lon1, lat1) - c(lon1_before, lat1_before))^2)) , by = 1:nrow(dp)]
dp[, distance1_next := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)) , by = 1:nrow(dp)]
dp[, distance2_before := sqrt(sum((c(lon2, lat2) - c(lon2_before, lat2_before))^2)) , by = 1:nrow(dp)]
dp[, distance2_next := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]


# distance threshold
threshold = sequence(15, 10, 10)

o = foreach(i = 1:length(threshold), .combine = 'rbind') %do% {
  
  distance_threshold = threshold[i]
  
  # interactions
  dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]
  
  # simple interactions
  dp[, interaction_threshold := distance_pair < distance_threshold]
  
  # count bouts of split and merge
  dp[, bout := bCounter(interaction), by = nestID]
  dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
  dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
  
  dp[, any_interaction_threshold := any(interaction_threshold == TRUE), by = .(nestID, bout)]
  dp[any_interaction_threshold == FALSE, interaction := FALSE]
  
  ds = copy(dp)
  ds[, distance_threshold := distance_threshold]
  ds
  
}


setorder(o, initiation_rel)

ggplot(data = o) +
  geom_tile(aes(initiation_rel, factor(distance_threshold), fill = interaction), width = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Distance threshold') +
  theme_classic()

# ggsave('./OUTPUTS/ONE_PAIR/Barplot_movement_buffer_and_threshold_initiation_rel.png', plot = last_plot(),  width = 177, height = 85, units = c('mm'), dpi = 'print')


# distance threshold
threshold = sequence(10, 10, 10)


o = foreach(i = 1:length(threshold), .combine = 'rbind') %do% {
  
  distance_threshold = threshold[i]
  
  # interactions
  dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]
  
  # simple interactions
  dp[, interaction_threshold := distance_pair < distance_threshold]
  
  # count bouts of split and merge
  dp[, bout := bCounter(interaction), by = nestID]
  dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
  dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
  
  dp[, any_interaction_threshold := any(interaction_threshold == TRUE), by = .(nestID, bout)]
  dp[any_interaction_threshold == FALSE, interaction := FALSE]
  
  # round to days
  dp[, initiation_rel0 := round(initiation_rel, 0)]
  
  # daily points of both individuals
  dp[, N_daily := .N, by = .(nestID, initiation_rel0)]
  
  # daily interactions
  dp[interaction == TRUE, N_together := .N, by = .(nestID, initiation_rel0)]
  dp[interaction == FALSE, N_together := NA]
  dp[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, initiation_rel0)]
  dp[is.na(N_together), N_together := 0]
  
  # unique data
  ds = unique(dp, by = c('nestID', 'initiation_rel0'))
  ds[, per_together := N_together / N_daily * 100]
  
  ds = ds[, .(per_together = median(per_together)), by = initiation_rel0]
  ds[, distance_threshold := distance_threshold]
  ds
  
}


setorder(o, initiation_rel0)

ggplot(data = o) +
  geom_point(aes(initiation_rel0, per_together, color = factor(distance_threshold), group = distance_threshold), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel0, per_together, color = factor(distance_threshold), group = distance_threshold), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, name = 'distance threshold', discrete = TRUE) +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.7))

# ggsave('./OUTPUTS/ONE_PAIR/Percentage_together_movement_buffer_and_threshold_initiation_rel.png', plot = last_plot(),  width = 177, height = 80, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Calculate spatio-temporal clusters
#--------------------------------------------------------------------------------------------------------------


setorder(d, ID, datetime_)
d[, pointID := seq_len(.N), by = ID]

ID = unique(c(dnID[, male_id], dnID[, female_id]))


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



# merge with spatio-temporal clusters
d[, ID := as.integer(ID)]
dp = merge(dp, d[, .(ID, datetime_, c_start1 = start, c_end1 = end, clustID1 = clustID, s_clustID1 = s_clustID,
                     st_clustID1 = st_clustID)], 
           by.x = c('ID1', 'datetime_1'), by.y = c('ID', 'datetime_'), all.x = TRUE)

dp = merge(dp, d[, .(ID, datetime_, c_start2 = start, c_end2 = end, clustID2 = clustID, s_clustID2 = s_clustID,
                     st_clustID2 = st_clustID)], 
           by.x = c('ID2', 'datetime_2'), by.y = c('ID', 'datetime_'), all.x = TRUE)





setorder(d, ID, datetime_)
d

bm = create_bm(d, buffer = 100)

bm +
  geom_path(data = d, aes(lon, lat, color = NULL), col = 'grey', size = .5) +
  geom_point(data = d, aes(lon, lat, color = as.character(s_clustID)), alpha = .5, size = 2, show.legend = FALSE) 


ggplot(data = d[!is.na(s_clustID)]) +
  geom_point(aes(datetime_, factor(s_clustID), group = ID, color = sex)) +
  geom_line(aes(datetime_, factor(s_clustID), group = ID, color = sex)) +
  geom_vline(xintercept = dn$initiation, color = 'black', size = 1) +
  geom_vline(xintercept = dn$initiation + 3*86400, color = 'black', size = 1) +
  theme_classic()

# ggsave('./OUTPUTS/ONE_PAIR/Spatial_cluster_datetime.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')



ggplot(data = d[!is.na(s_clustID) & datetime_ > as.POSIXct('2018-06-23 05:30:00') & datetime_ < as.POSIXct('2018-06-27 07:20:00')]) +
  geom_point(aes(datetime_, factor(s_clustID), group = ID, color = sex)) +
  geom_line(aes(datetime_, factor(s_clustID), group = ID, color = sex)) +
  geom_vline(xintercept = dn$initiation, color = 'black', size = 1) +
  theme_classic()

# ggsave('./OUTPUTS/ONE_PAIR/Spatial_cluster_datetime_crop.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Model change in within-pair distance 
#--------------------------------------------------------------------------------------------------------------

# Packages
sapply(c('lme4', 'effects', 'multcomp', 'gtools', 'emmeans', 'broom', 'MuMIn', 'nlme'),
       function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ) )

# shift positions
dp[, lat1_next := shift(lat1, type = 'lead'), by = nestID]
dp[, lon1_next := shift(lon1, type = 'lead'), by = nestID]
dp[, lat2_next := shift(lat2, type = 'lead'), by = nestID]
dp[, lon2_next := shift(lon2, type = 'lead'), by = nestID]

# distances pair next
dp[, distance_pair_next := shift(distance_pair, type = 'lead'), by = nestID]
dp[, distance_pair_before := shift(distance_pair, type = 'lag'), by = nestID]

# distance male and female next
dp[, distance_btw_1 := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)) , by = 1:nrow(dp)]
dp[, distance_btw_2 := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]

# delta difference in pair distance
dp[, distance_btw_pair := distance_pair - distance_pair_next, by = 1:nrow(dp)]

# time between consecutive points
dp[, datetime_1_next := data.table::shift(datetime_1, type = 'lead'), by = ID1]
dp[, datetime_2_next := data.table::shift(datetime_2, type = 'lead'), by = ID2]
dp[, time_btw_1 := as.numeric(difftime(datetime_1_next, datetime_1, units = 'sec'))]
dp[, time_btw_2 := as.numeric(difftime(datetime_2_next, datetime_2, units = 'sec'))]
dp[, time_btw_pair := abs(as.numeric(difftime(datetime_1, datetime_2, units = 'sec')))]

# ground speed 
dp[, speed_1 := distance_btw_1/ time_btw_1]
dp[, speed_2 := distance_btw_2/ time_btw_2]

# how far could bird have moved in between points
dp[, distance_travelled_1 := time_btw_pair * speed_1]
dp[, distance_travelled_2 := time_btw_pair * speed_2]
dp[, distance_travelled_pair := time_btw_pair * max(speed_1, speed_2), by = 1:nrow(dp)]

# interaction based on distance threshold
dp[, interaction := distance_pair < 30]
dp[, interaction_time_btw := distance_pair < 30 + distance_travelled_pair]

# first and last interaction
dp[interaction == TRUE, first_int := min(datetime_1), by = nestID]
dp[, first_int := min(first_int, na.rm = TRUE), by = nestID]
dp[interaction == TRUE, last_int  := max(datetime_1), by = nestID]
dp[, last_int := max(last_int, na.rm = TRUE), by = nestID]


# subset period with interactions
ds = dp[!is.na(distance_btw_pair) & datetime_1 > first_int & datetime_1 < last_int]


# selected model
fm1 = lm(distance_btw_pair ~ distance_btw_1 + distance_btw_2, data = ds)

plot(allEffects(fm1))
glht(fm1) %>% summary
summary(fm1)

ggplot(data = ds) +
  geom_point(aes(distance_btw_1, distance_btw_pair), color = 'dodgerblue2', alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(distance_btw_2, distance_btw_pair), color = 'darkorange', alpha = 0.5, show.legend = FALSE) +
  theme_classic()


#--------------------------------------------------------------------------------------------------------------
#' # Look at split and merge events
#--------------------------------------------------------------------------------------------------------------

distance_threshold = 30

# interactions
dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]

# simple interactions
dp[, interaction_threshold := distance_pair < distance_threshold]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]

dp[, any_interaction_threshold := any(interaction_threshold == TRUE), by = .(nestID, bout)]
dp[any_interaction_threshold == FALSE, interaction := FALSE]

# split points and merging points
dp[, interaction_next := shift(interaction, type = 'lead'), by = nestID]
dp[, interaction_before := shift(interaction, type = 'lag'), by = nestID]

# correct for true splits
dp[interaction == TRUE & interaction_next == FALSE & distance_pair > distance_threshold, interaction := FALSE]

# split points and merging points
dp[, interaction_next := shift(interaction, type = 'lead'), by = nestID]
dp[, interaction_before := shift(interaction, type = 'lag'), by = nestID]
dp[, split := interaction_before == TRUE & interaction == FALSE]
dp[, merge := interaction_before == FALSE & interaction == TRUE]

# which ID split?
dp[split == TRUE, split_ID := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]

# which ID approached?
dp[merge == TRUE, merge_ID := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]


dp[split == TRUE, .(distance_pair_before, distance_pair, distance_pair_next, distance1_next, distance2_next, distance1_before, distance2_before, split_ID)]

dp[merge == TRUE, .(distance_pair_before, distance_pair, distance_pair_next, distance1_next, distance2_next, distance1_before, distance2_before, split_ID)]


ggplot(data = dp[split == TRUE]) +
  geom_bar(aes(initiation_rel0, fill = split_ID)) +
  theme_classic()

ggplot(data = dp[merge == TRUE]) +
  geom_bar(aes(initiation_rel0, fill = merge_ID)) +
  theme_classic()















