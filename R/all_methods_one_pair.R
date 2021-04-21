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
  
  # count bouts of split and merge
  dp[, bout := bCounter(interaction), by = nestID]
  dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
  dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
  dp[interaction == FALSE & bout_seq_max == 1, interaction := TRUE]
  
  # round to days
  dp[, initiation_rel := round(initiation_rel, 0)]
  
  # daily points of both individuals
  dp[, N_daily := .N, by = .(nestID, initiation_rel)]
  
  # daily interactions
  dp[interaction == TRUE, N_together := .N, by = .(nestID, initiation_rel)]
  dp[interaction == FALSE, N_together := NA]
  dp[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, initiation_rel)]
  dp[is.na(N_together), N_together := 0]
  
  # unique data
  ds = unique(dp, by = c('nestID', 'initiation_rel'))
  ds[, per_together := N_together / N_daily * 100]
  
  ds = ds[, .(per_together = median(per_together)), by = initiation_rel]
  ds[, distance_threshold := distance_threshold]
  ds
  
}



setorder(o, initiation_rel)

ggplot(data = o) +
  geom_point(aes(initiation_rel, per_together, color = factor(distance_threshold), group = distance_threshold), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel, per_together, color = factor(distance_threshold), group = distance_threshold), size = 1, alpha = 0.5) +
    scale_color_viridis(direction = -1, name = 'distance threshold', discrete = TRUE) +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.8))




ggplot(data = dp) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = -2), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  theme_classic()
























