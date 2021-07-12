#==============================================================================================================
# Mate guarding
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
dp = fread('./DATA/PAIR_WISE_INTERACTIONS.txt', sep = '\t', header = TRUE) %>% data.table
dp[, year_ := year(datetime_1)]

do = fread('./DATA/PAIR_WISE_SPACE_USE.txt', sep = '\t', header = TRUE) %>% data.table

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
dpa = dbq(con, 'select * FROM PATERNITY')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

# any EPP in nest?
dpa[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dpa[, any_EPY := any(EPY == 1), by = nestID]

dpau = unique(dpa, by = 'nestID')

dn = merge(dn, dpau[, .(nestID, any_EPY)], by = 'nestID', all.x = TRUE)

# males that sired EPY 


#--------------------------------------------------------------------------------------------------------------
#' # Define breeding pairs
#--------------------------------------------------------------------------------------------------------------

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = 'ID')

# check if data overlap
dn = merge(dn, dID[, .(male_id = ID, start_m = start, end_m = end)], by = 'male_id', all.x = TRUE)
dn = merge(dn, dID[, .(female_id = ID, start_f = start, end_f = end)], by = 'female_id', all.x = TRUE)

# subset nests with both IDs tagged
dn[, both_tagged := !is.na(start_m) & !is.na(start_f), by = nestID]

# subset nests with both IDs tagged and overlapping time intervals
dn[, overlap := DescTools::Overlap(c(start_m, end_m), c(start_f, end_f)), by = nestID]
dn[, both_tagged_overlapping := overlap > 0, by = nestID]
dn[is.na(both_tagged_overlapping), both_tagged_overlapping := FALSE]

# check overlap with initiation date
dn[, overlap_initiation_m := DescTools::Overlap(c(start_m, end_m), c(initiation - 86400, initiation + 86400)), by = nestID]
dn[, overlap_initiation_f := DescTools::Overlap(c(start_f, end_f), c(initiation - 86400, initiation + 86400)), by = nestID]
dn[, both_tagged_at_initiation := overlap_initiation_m > 0 & overlap_initiation_f > 0, by = nestID]
dn[is.na(both_tagged_at_initiation), both_tagged_at_initiation := FALSE]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date, any_EPY, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# assign clutch order
setorder(dnID, male_id, initiation)
dnID[!is.na(male_id) & !is.na(female_id), clutch_together := seq_len(.N), by = .(year_, male_id, female_id)]
dnID[!is.na(male_id), male_clutch     := seq_len(.N), by = .(year_, male_id)]
dnID[!is.na(female_id), female_clutch := seq_len(.N), by = .(year_, female_id)]

#--------------------------------------------------------------------------------------------------------------
#' # Define interactions
#--------------------------------------------------------------------------------------------------------------

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2', 'year_'), by.y = c('male_id', 'female_id', 'year_'), all.x = TRUE, allow.cartesian = TRUE)

# relative nest initiation date
dp[, initiation_rel := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# mean and median daily 
dp[, date_ := as.Date(datetime_1)]
dp[, mean_dist := mean(distance_pair, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, median_dist := median(distance_pair, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# median corrected
distance_threshold = 30
dp[, distance_pair_cor := ifelse(interaction == TRUE & distance_pair > distance_threshold, distance_threshold, distance_pair)]
dp[interaction == FALSE, distance_pair_cor := distance_pair]
dp[, median_dist_cor := median(distance_pair_cor, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# Number of interactions and percentage 
dp[, N_pairwise_positions := .N, by = .(year_, pairID, nestID)]
dp[interaction == TRUE, N_pairwise_interactions := .N, by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions := mean(N_pairwise_interactions, na.rm = TRUE), by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions_per := N_pairwise_interactions / N_pairwise_positions * 100]

# Number of daily interactions and percentage 
dp[, N_pairwise_positions_daily := .N, by = .(year_, pairID, nestID, date_)]
dp[interaction == TRUE, N_pairwise_interactions_daily := .N, by = .(year_, pairID, nestID, date_)]
dp[, N_pairwise_interactions_daily := mean(N_pairwise_interactions_daily, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, N_pairwise_interactions_daily_per := N_pairwise_interactions_daily / N_pairwise_positions_daily * 100]
dp[, N_pairwise_interactions_daily_per_50 := any(N_pairwise_interactions_daily_per > 50), by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions_daily_per_90 := any(N_pairwise_interactions_daily_per > 90), by = .(year_, pairID, nestID)]

# Longest bout together
dp[, bout_start := min(c(datetime_1, datetime_2)), by = .(year_, pairID, bout)]
dp[, bout_end := max(c(datetime_1, datetime_2)), by = .(year_, pairID, bout)]
dp[, bout_length := difftime(bout_end, bout_start, units = 'mins') %>% as.numeric]
dp[interaction == TRUE, bout_max := max(bout_length, na.rm = TRUE), by = .(year_, pairID, nestID)]
dp[, bout_max := mean(bout_max, na.rm = TRUE), by = .(year_, pairID, nestID)]

# Longest bout together daily 
dp[interaction == TRUE, bout_max_daily := max(bout_length, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, bout_max_daily := mean(bout_max_daily, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# pairs with known nest and mate guarding data before clutch initiation
dp[, mg_before_initiation := any(initiation_rel < 0), by = .(year_, pairID, nestID)]
dp[nestID == 'R812_18' | nestID == 'R604_18', mg_before_initiation := FALSE] # pair with data before paired

# same sex interaction?
dp[, same_sex := sex1 == sex2]

# breeding pair
dp[, breeding_pair := !is.na(nestID)]

# summary by unique pair excluding pair wise duplicates
dsm = dp[same_sex == FALSE & sex1 == 'M'] # because nests are merged with ID1 = male
dss = dp[same_sex == TRUE & ID1 > ID2] 

dps = rbind(dsm, dss)

du = unique(dps, by = c('year_', 'pairID', 'nestID'))
dud = unique(dps, by = c('year_', 'pairID', 'nestID', 'date_'))


#--------------------------------------------------------------------------------------------------------------
#' # Define "null model" 
#--------------------------------------------------------------------------------------------------------------

# N interactions of unpaired birds for a given day relative to the initiation date 

# Identify all dates linked to relative initiation dates 
# Use N interaction of these unpaired birds 







dp = dp[!is.na(nestID)]

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

# nests to exclude
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]

# exclude pairs before mate guarding started
# ds = ds[!(initiation_rel < 0 & per_together < 50)]

setorder(ds, initiation_rel0)

ggplot(data = ds[datetime_1 < nest_state_date]) +
  geom_point(aes(initiation_rel0, per_together, color = N_daily, group = nestID), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel0, per_together, color = N_daily, group = nestID), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, name = 'N positions') +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  scale_x_continuous(limits = c(-10, 16)) +
  theme(legend.position = c(0.8, 0.7))


















