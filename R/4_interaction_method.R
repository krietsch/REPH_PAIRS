#==============================================================================================================
# Mate guarding closest distance
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'windR', 'ggnewscale', 'doFuture', 'patchwork'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table

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
#' # Define breeding pairs with both sexes tagged
#--------------------------------------------------------------------------------------------------------------

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = 'ID')

# check if data overlap
dn = merge(dn, dID[, .(male_id = ID, start_m = start, end_m = end)], by = 'male_id', all.x = TRUE)
dn = merge(dn, dID[, .(female_id = ID, start_f = start, end_f = end)], by = 'female_id', all.x = TRUE)

# subset nests with both IDs tagged
dn = dn[!is.na(start_m) & !is.na(start_f)]

# subset nests with both IDs tagged and overlapping time intervals
dn[, overlap := DescTools::Overlap(c(start_m, end_m), c(start_f, end_f)), by = nestID]
dn = dn[overlap > 0]

# check overlap with initiation date
dn[, overlap_initiation_m := DescTools::Overlap(c(start_m, end_m), c(initiation - 86400, initiation + 86400)), by = nestID]
dn[, overlap_initiation_f := DescTools::Overlap(c(start_f, end_f), c(initiation - 86400, initiation + 86400)), by = nestID]
dn = dn[overlap_initiation_m > 0 & overlap_initiation_f > 0]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# create directory for each of these breeding pairs
dnID[, directory := paste0('//ds/grpkempenaers/Hannes/temp/PAIRS_ANIMATION_EACH/', nestID)]
dnID[, dir.create(file.path(directory), showWarnings = FALSE), by = 1:nrow(dnID)]

# unique IDs
IDu = unique(c(dnID[,  male_id], dnID[,  female_id]))

# subset d
d = d[ID %in% IDu]

# merge with nest and initiation date
dnIDu = rbind(dnID[, .(year_, ID = female_id, nestID, initiation, sex = 'F')], 
              dnID[, .(year_, ID = male_id, nestID, initiation, sex = 'M')])

d = merge(d, dnIDu[, .(year_, ID, nestID, initiation, sex)], by = c('ID', 'year_'), all.x = TRUE, allow.cartesian = TRUE)
d = d[!is.na(nestID)]

#--------------------------------------------------------------------------------------------------------------
#' # Define interactions
#--------------------------------------------------------------------------------------------------------------

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'))

# interactions
dp[, interaction := distance_pair < 30]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
dp[interaction == FALSE & bout_seq_max == 1, interaction := TRUE] 

# interaction ID
dp[, int_id := seq_len(.N), by = nestID]

# first and last interaction
dp[interaction == TRUE, first_int := min(datetime_1), by = nestID]
dp[, first_int := min(first_int, na.rm = TRUE), by = nestID]
dp[interaction == TRUE, last_int  := max(datetime_1), by = nestID]
dp[, last_int := min(last_int, na.rm = TRUE), by = nestID]

# relative nest initiation date
dp[, initiation_rel := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]


#--------------------------------------------------------------------------------------------------------------
#' # Plot for each nest
#--------------------------------------------------------------------------------------------------------------

# sort by most time together before initiation
ds = dp[initiation_rel < 0 & interaction == TRUE, .N, by = nestID]
setorder(ds, -N)

# order nest ID
dp[, nestID := factor(nestID %>% as.factor, levels = ds[, nestID])]


ggplot(data = dp) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = -2), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  theme_classic()


#--------------------------------------------------------------------------------------------------------------
#' # Interactions using different distance thresholds
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
  
  # nests to exclude
  n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
  ds = ds[!(nestID %in% n2)]
  
  # exclude pairs before mate guarding started
  ds = ds[!(initiation_rel < 0 & per_together < 50)]

  ds = ds[, .(per_together = median(per_together)), by = initiation_rel]
  ds[, distance_threshold := distance_threshold]
  ds
  
}



setorder(o, initiation_rel)

ggplot(data = o) +
  geom_point(aes(initiation_rel, per_together, color = distance_threshold, group = distance_threshold), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel, per_together, color = distance_threshold, group = distance_threshold), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, limits = c(10, 150), name = 'distance threshold') +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.8))



#--------------------------------------------------------------------------------------------------------------
#' # Interactions using different maximal bout length 
#--------------------------------------------------------------------------------------------------------------

# distance threshold
bout_seq_max_value = sequence(6, 0, 1)


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
  
  # nests to exclude
  n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
  ds = ds[!(nestID %in% n2)]
  
  # exclude pairs before mate guarding started
  ds = ds[!(initiation_rel < 0 & per_together < 50)]
  
  ds = ds[, .(per_together = median(per_together)), by = initiation_rel]
  ds[, bout_seq_max_value := bsm]
  ds
  
}


setorder(o, initiation_rel)

ggplot(data = o) +
  geom_point(aes(initiation_rel, per_together, color = bout_seq_max_value, group = bout_seq_max_value), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel, per_together, color = bout_seq_max_value, group = bout_seq_max_value), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, limits = c(0, 5), name = 'min bout lenght') +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.8))




#--------------------------------------------------------------------------------------------------------------
#' # Include movement before
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

# interaction with "movement buffer"
dp[, interaction_buffer := distance_pair < c(max(distance1_before, distance2_before) + 30), by = 1:nrow(dp)]

# simple interactions
dp[, interaction_threshold := distance_pair < distance_threshold]
dp[is.na(interaction), interaction := interaction_threshold]

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


dp

# datetime relative to nest initiation date
dp[, datetime_rel_pair := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]
dp[, datetime_rel_pair0 := round(datetime_rel_pair, 0)]

# subset data 10 days around clutch initiation
dm = dp[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10]

# Male and female together
dms = dm[interaction == TRUE, .(N_int = .N), by = .(nestID, datetime_rel_pair0)]
du = unique(dm, by = c('nestID', 'datetime_rel_pair0'))
du = merge(du, dms, by = c('nestID', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int), N_int := 0]
du[, int_prop := N_int / N_daily]


ggplot(data = du) +
  geom_point(aes(datetime_rel_pair0, int_prop, color = nestID, group = nestID), size = 2, alpha = 1) +
  geom_path(aes(datetime_rel_pair0, int_prop, color = nestID, group = nestID), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, limits = c(0, 5), name = 'min bout lenght') +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.8))


ggplot(data = dp) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction_buffer), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = -2), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  theme_classic()

ggplot(data = dp) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = -2), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  theme_classic()


