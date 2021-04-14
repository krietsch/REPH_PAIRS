#==============================================================================================================
# How to best define interactions
#==============================================================================================================

# Summary
# 1. Data available
# 2. Data until 
# 3. Data linked to nests

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork', 'windR'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Comparison 10 min intervals vs. closest time
#--------------------------------------------------------------------------------------------------------------

# Data
dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
dp[, time_btw := abs(as.numeric(difftime(datetime_1, datetime_2, units = 'min')))]

dpn = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table

# comparisions
dp %>% nrow
dpn %>% nrow

# difference between positions in time
ggplot(data = dp) +
  geom_histogram(aes(time_btw))

ggplot(data = dpn) +
  geom_histogram(aes(time_btw))

# number of interactions
dp[, interaction := distance < 30]
dpn[, interaction := distance_pair < 30]

dp[interaction == TRUE] %>% nrow
dpn[interaction == TRUE] %>% nrow

# subset pair
dps = dp[ID1 == 270170746 & ID2 == 270170747] # R304_18
dpns = dpn[ID1 == 270170746 & ID2 == 270170747] # R304_18

dps[interaction == TRUE] %>% nrow / dps %>% nrow * 100
dpns[interaction == TRUE] %>% nrow / dpns %>% nrow * 100

#--------------------------------------------------------------------------------------------------------------
#' # Define breeding pairs with both sexes tagged
#--------------------------------------------------------------------------------------------------------------

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = 'ID')

# ID as character
d[, ID := as.character(ID)]
dp[, ID1 := as.character(ID1)]
dp[, ID2 := as.character(ID2)]
dID[, ID := as.character(ID)]
dn[, male_id := as.character(male_id)]
dn[, female_id := as.character(female_id)]

# check if data overlap
dn = merge(dn, dID[, .(male_id = ID, start_m = start, end_m = end)], by = 'male_id', all.x = TRUE)
dn = merge(dn, dID[, .(female_id = ID, start_f = start, end_f = end)], by = 'female_id', all.x = TRUE)

# subset nests with both IDs tagged
dn = dn[!is.na(start_m) & !is.na(start_f)]

# subset nests with both IDs tagged and overlapping time intervals
dn[, overlap := DescTools::Overlap(c(start_m, end_m), c(start_f, end_f)), by = nestID]
dn = dn[overlap > 0]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

#--------------------------------------------------------------------------------------------------------------
#' # Comparison 10 min intervals vs. closest time
#--------------------------------------------------------------------------------------------------------------

# Data
dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table

# Merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'))

# interactions
dp[, interaction := distance_pair < 30]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
dp[interaction == FALSE & bout_seq_max == 1, interaction := TRUE] 

dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
dp[, bout_start := min(datetime_1), by = .(nestID, bout)]
dp[, bout_end := max(datetime_1), by = .(nestID, bout)]
dp[, bout_length := difftime(bout_end, bout_start, units = 'hours') %>% as.numeric]

# split points and merging points
dp[, interaction_before := shift(interaction, type = 'lag'), by = nestID]
dp[, split := interaction_before == TRUE & interaction == FALSE]
dp[, merge := interaction_before == FALSE & interaction == TRUE]

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

# which ID approached?
dp[merge == TRUE, merge_ID := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]

# which ID split?
dp[split == TRUE, split_ID := ifelse(distance1_next > distance2_next, 'ID1', 'ID2')]



dp[, .(interaction, split, split_ID, merge, merge_ID, distance_pair, distance1_before, distance2_before, distance1_next, distance2_next)]



dp[, .N, split_ID]
dp[, .N, merge_ID]

dp[initiation_rel < 2 & bout_length > 0.5, .N, split_ID]
dp[, .N, merge_ID]


dps = dp[ID1 == 270170746 & ID2 == 270170747] # R304_18


ggplot(data = dps) +
  geom_point(aes(datetime_1, distance_pair, group = ID1, color = as.character(split_ID))) +
  geom_line(aes(datetime_1, distance_pair, group = ID1, color = as.character(split_ID))) +
  theme_classic()


ggplot(data = dp[split == TRUE & bout_length < 10]) +
  geom_boxplot(aes(as.factor(initiation_rel), bout_length, color = split_ID))


# date without year
dp[, datetime_y := as.POSIXct(format(datetime_1, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dp[, date_y := as.Date(datetime_y)]
dp[, initiation_dy := as.Date(initiation_y)]

# relative nest initiation date
dp[, datetime_rel := difftime(datetime_y, initiation_y, units = 'days') %>% as.numeric()]
dp[, initiation_rel := difftime(date_y, initiation_dy, units = 'days') %>% as.numeric()]

# daily points of both individuals
dp[, N_daily := .N, by = .(nestID, date_y)]

# daily interactions
dp[interaction == TRUE, N_together := .N, by = .(nestID, date_y)]
dp[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, date_y)]
dp[is.na(N_together), N_together := 0]

# any within three days around initiation
dp[, any_before_initiation := any(initiation_rel < 8), by = nestID]
dp[, any_after_initiation  := any(initiation_rel > 0), by = nestID]
dp[, any_around_initiation := any_before_initiation == TRUE & any_after_initiation == TRUE, by = nestID]

# mean and median
dp[, mean_dist := mean(distance_pair, na.rm = TRUE), by = .(nestID, date_y)]
dp[, median_dist := median(distance_pair, na.rm = TRUE), by = .(nestID, date_y)]

#--------------------------------------------------------------------------------------------------------------
# Check split points
#--------------------------------------------------------------------------------------------------------------

dps = dp[ID1 == 270170746 & ID2 == 270170747] # R304_18


ggplot(data = dps[interaction == FALSE & bout_length > 0.5]) +
  geom_point(aes(datetime_1, distance_pair, color = as.character(bout))) +
  geom_line(aes(datetime_1, distance_pair, color = as.character(bout))) +
  theme_classic()


dps = dps[datetime_1 > as.POSIXct('2018-06-24 12:30:00', tz = 'UTC') & datetime_1 < as.POSIXct('2018-06-25 14:00:00', tz = 'UTC')]
dps[, point_id := seq_along(ID1)]

ggplot(data = dps) +
  geom_point(aes(datetime_1, distance_pair, color = as.character(bout))) +
  geom_line(aes(datetime_1, distance_pair, color = as.character(bout))) +
  theme_classic()

bm = create_bm(dps, lon = 'lon1', lat = 'lat1', buffer = 100)

bm + 
  geom_path(data = dps, aes(lon1, lat1, group = ID1, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon1, lat1, color = interaction), size = 1, shape = 21) +
  geom_path(data = dps, aes(lon2, lat2, group = ID2, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon2, lat2, color = interaction), size = 1, shape = 21) 






























#--------------------------------------------------------------------------------------------------------------
# Daily summaries
#--------------------------------------------------------------------------------------------------------------

# N daily interactions
ds = unique(dp[any_around_initiation == TRUE], by = c('nestID', 'date_y'))
ds[, per_together := N_together / N_daily * 100]
ds[, per_sampled := N_daily / 140 * 100]

# nests to exclude
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]


dss = ds[, .N, by = initiation_rel]


ggplot(data = ds) +
  geom_point(aes(initiation_rel, per_together, group = nestID, color = per_sampled), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel, per_together, group = nestID, color = per_sampled), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, limits = c(0, 100), name = '% day sampled') +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.8))



ggplot(data = ds) +
  geom_boxplot(aes(as.factor(initiation_rel), per_together), varwidth = TRUE) +
  geom_vline(aes(xintercept = '0'), color = 'firebrick2', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(as.factor(initiation_rel), Inf, label = N), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8)
















# mean by interaction bout
ds = dp[, .(datetime_1 = mean(datetime_1), datetime_2 = mean(datetime_2), lat1 = mean(lat1), lon1 = mean(lon1), 
            lat2 = mean(lat2), lon2 = mean(lon2), distance_pair = mean(distance_pair)), 
        by = .(ID1, ID2, nestID, interaction, bout)]


dps = ds[ID1 == 270170746 & ID2 == 270170747]
dps = dps[datetime_1 > as.POSIXct('2018-06-24 12:30:00', tz = 'UTC') & datetime_1 < as.POSIXct('2018-06-25 14:00:00', tz = 'UTC')]

ggplot(data = dps) +
  geom_point(aes(datetime_1, distance_pair)) +
  geom_line(aes(datetime_1, distance_pair)) +
  theme_classic()




bm = create_bm(dps, lon = 'lon1', lat = 'lat1', buffer = 100)

bm + 
  geom_path(data = dps, aes(lon1, lat1, group = ID1, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon1, lat1, color = interaction), size = 1, shape = 21) +
  geom_path(data = dps, aes(lon2, lat2, group = ID2, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon2, lat2, color = interaction), size = 1, shape = 21) 


dss = rbind(dps[, .(ID = ID1, lat = lat1, lon = lon1, datetime_ = datetime_1, distance_pair, bout)], 
            dps[, .(ID = ID2, lat = lat2, lon = lon2, datetime_ = datetime_2, distance_pair, bout)])


bm + 
  geom_path(data = dss, aes(lon, lat, group = bout), color = 'firebrick2', size = 0.7, alpha = 0.5) + 
  geom_point(data = dss, aes(lon, lat), size = 1, shape = 21) +
  scale_color_viridis(direction = 1) +
  geom_path(data = dss, aes(lon, lat, group = ID), color = 'grey', size = 0.7, alpha = 0.5) 







dp[split == TRUE] %>% nrow


ds = unique(dp, by = c('nestID', 'bout'))

ds[interaction == TRUE] %>% nrow
ds[interaction == TRUE & bout_length == 0] %>% nrow
ds[interaction == TRUE & bout_seq_max == 1] %>% nrow

ds[interaction == FALSE] %>% nrow
ds[interaction == FALSE & bout_length == 0] %>% nrow
ds[interaction == FALSE & bout_seq_max == 1] %>% nrow

ggplot(data = ds[bout_length < 12 & interaction == TRUE]) +
  geom_histogram(aes(bout_length))

ggplot(data = ds[bout_length < 12 & interaction == FALSE]) +
  geom_histogram(aes(bout_length))

dps = dp[ID1 == 270170746 & ID2 == 270170747] # R304_18


ggplot(data = dps[interaction == FALSE & bout_length > 0.5]) +
  geom_point(aes(datetime_1, distance_pair, color = as.character(bout))) +
  geom_line(aes(datetime_1, distance_pair, color = as.character(bout))) +
  theme_classic()


dps[interaction == FALSE & bout_length < 1]


dps = dps[datetime_1 > as.POSIXct('2018-06-24 12:30:00', tz = 'UTC') & datetime_1 < as.POSIXct('2018-06-25 14:00:00', tz = 'UTC')]
dps[, point_id := seq_along(ID1)]

ggplot(data = dps) +
  geom_point(aes(datetime_1, distance_pair, color = as.character(bout))) +
  geom_line(aes(datetime_1, distance_pair, color = as.character(bout))) +
  theme_classic()

bm = create_bm(dps, lon = 'lon1', lat = 'lat1', buffer = 100)

bm + 
  geom_path(data = dps, aes(lon1, lat1, group = ID1, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon1, lat1, color = interaction), size = 1, shape = 21) +
  geom_path(data = dps, aes(lon2, lat2, group = ID2, color = interaction), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon2, lat2, color = interaction), size = 1, shape = 21) 


bm + 
  geom_path(data = dps, aes(lon1, lat1, group = point_id, color = point_id), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon1, lat1, color = point_id), size = 1, shape = 21) +
  geom_path(data = dps, aes(lon2, lat2, group = point_id, color = point_id), size = 0.7, alpha = 0.5) + 
  geom_point(data = dps, aes(lon2, lat2, color = point_id), size = 1, shape = 21) +
  ggrepel::geom_label_repel(data = dps, aes(lon1, lat1, label = point_id), segment.color = 'grey50') +
  ggrepel::geom_label_repel(data = dps, aes(lon2, lat2, label = point_id), segment.color = 'grey50')


dss = rbind(dps[, .(ID = ID1, lat = lat1, lon = lon1, datetime_ = datetime_1, distance_pair, point_id)], 
            dps[, .(ID = ID2, lat = lat2, lon = lon2, datetime_ = datetime_2, distance_pair, point_id)])


bm + 
  geom_path(data = dss, aes(lon, lat, group = point_id), color = 'firebrick2', size = 0.7, alpha = 0.5) + 
  geom_point(data = dss, aes(lon, lat), size = 1, shape = 21) +
  scale_color_viridis(direction = 1) +
  geom_path(data = dss, aes(lon, lat, group = ID), color = 'grey', size = 0.7, alpha = 0.5) 






dps$datetime_1




