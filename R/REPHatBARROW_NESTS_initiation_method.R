#=========================================================================================================================
# REPHatBARROW NESTS initiation date method
#=========================================================================================================================

### Summary
# 1. Assign initiation date method external data
# 2. Assign initiation date method our data
# 3. Estimate egg laying datetimes based on GPS data (nest visits)

# settings, con
options(stringsAsFactors = FALSE)

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr',
          'sdbvis', 'viridis', 'patchwork', 'windR', 'tdbscan', 'doFuture'),
        require, character.only = TRUE)

#-------------------------------------------------------------------------------------------------------------------------
# 1. Assign initiation date method external data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, "select * FROM NESTS")
DBI::dbDisconnect(con)

# subset external data
d = d[external == 1]

# nests found incomplete
d[, found_incomplete := initial_clutch_size < clutch_size]

# initiation date methods
d[found_incomplete == TRUE, initiation_method := 0]
d[is.na(initiation_method) & !is.na(hatching_datetime), initiation_method := 1]
d[is.na(initiation_method) & !is.na(est_hatching_datetime), initiation_method := 2]
d[is.na(initiation_method) & !is.na(initiation), initiation_method := 2]

# UPDATE db
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
du = d[!is.na(initiation_method), .(initiation_method, pk)]

# save new values from d in a temp table
dbWriteTable(con, 'temp', du , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.initiation_method = t.initiation_method where n.pk = t.pk")
dbExecute(con,"drop table temp")

DBI::dbDisconnect(con)


#-------------------------------------------------------------------------------------------------------------------------
# 2. Assign initiation date method our data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, "select * FROM NESTS")
DBI::dbDisconnect(con)

# subset external data
d = d[external == 0]

# nests found incomplete
d[, found_incomplete := initial_clutch_size < clutch_size]

# initiation date methods
d[found_incomplete == TRUE, initiation_method := 0]
d[is.na(initiation_method) & !is.na(hatching_datetime), initiation_method := 1]
d[is.na(initiation_method) & !is.na(est_hatching_datetime), initiation_method := 2]

# by detailed observations
d[year_ == 2019 & nest == 'R612', initiation_method := 5]
d[year_ == 2019 & nest == 'R401', initiation_method := 5]

d[is.na(initiation_method) & !is.na(initiation), initiation_method := 3]

d[comments %like% 'GPS' & initiation_method != 3, initiation_method := 4]

# UPDATE db
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
du = d[!is.na(initiation_method), .(initiation_method, pk)]

# save new values from d in a temp table
dbWriteTable(con, 'temp', du , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.initiation_method = t.initiation_method where n.pk = t.pk")
dbExecute(con,"drop table temp")

DBI::dbDisconnect(con)


#-------------------------------------------------------------------------------------------------------------------------
# 3. Estimate egg laying datetimes based on GPS data (nest visits)
#-------------------------------------------------------------------------------------------------------------------------

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
d[, datetime_ := as.POSIXct(datetime_, tz = 'UTC')]

dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, datetime_1 := as.POSIXct(datetime_1, tz = 'UTC')]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, found_datetime := as.POSIXct(found_datetime, tz = 'UTC')]
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]
dg = dbq(con, 'select * FROM SEX')

dv = dbq(con, 'select * FROM NEST_VISITS')
dv[, year_ := year(date_)]
dv[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dv = dv[year_ > 2017]
dv[is.na(time_appr), time_appr := '00:00:00']
dv[, datetime_ := as.POSIXct(paste0(date_, ' ', time_appr), tz = 'UTC')]

DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

#--------------------------------------------------------------------------------------------------------------
#' # Interactions

distance_threshold = 20

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'))

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

# interactions
dp[, interaction := distance_pair < c(distance1_before + distance2_before + distance_threshold), by = 1:nrow(dp)]
# dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]

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

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]

# split points and merging points
dp[, interaction_next := shift(interaction, type = 'lead'), by = nestID]
dp[, interaction_before := shift(interaction, type = 'lag'), by = nestID]
dp[, split := interaction_before == TRUE & interaction == FALSE]
dp[, merge := interaction_before == FALSE & interaction == TRUE]

# Proportion together
dp[, date_ := as.Date(datetime_1)]
dp[, N_positions_date := .N, by = .(nestID, ID1, date_)]
dp[interaction == TRUE, N_int := .N, by = .(nestID, ID1, date_)]
dp[, prop_together := N_int / N_positions_date * 100]

#--------------------------------------------------------------------------------------------------------------
#' # GPS tagged birds with nest


# tagged breeders
dnb = rbind(dn[, .(year_, nestID, ID = male_id, sex = 'M', initiation, found_datetime, nest_state_date, 
                   initial_clutch_size, clutch_size, lat_n = lat, lon_n = lon)],
            dn[, .(year_, nestID, ID = female_id, sex = 'F', initiation, found_datetime, nest_state_date, 
                   initial_clutch_size, clutch_size, lat_n = lat, lon_n = lon)])

# unique IDs with tagging data
IDu = unique(d, by = c('ID', 'year_'))

dnb = merge(dnb, IDu[, .(year_, ID, gps_tag = TRUE)], by = c('ID', 'year_'), all.x = TRUE)
dnb = dnb[gps_tag == TRUE]

# merge with nest
d = d[ID %in% dnb$ID]
d = merge(d, dnb, by = c('ID', 'year_'), all = TRUE, allow.cartesian = TRUE)

#--------------------------------------------------------------------------------------------------------------
#' # Distance to nest

# distance to nest 
d[, distance_nest := sqrt(sum((c(lon, lat) - c(lon_n, lat_n))^2)) , by = 1:nrow(d)]
d[, dist_n10 := distance_nest < 10]
d[, dist_n20 := distance_nest < 20]
d[, dist_n30 := distance_nest < 30]

# relative nest initiation date
d[, initiation_rel := difftime(datetime_, initiation, units = 'days') %>% as.numeric()]
d[, initiation_rel0 := round(initiation_rel, 0)]

# found incomplete
d[!is.na(initial_clutch_size), found_incomplete := initial_clutch_size < clutch_size]

# any at nest
d[, any_dist_n30 := any(dist_n30 == TRUE), by = .(sex, nestID)]

# exclude nests with no data at nest
d = d[any_dist_n30 == TRUE]

#--------------------------------------------------------------------------------------------------------------
#' # Time at the nest

# nest visit bouts
setorder(d, nestID, ID, datetime_)
d[, nest_visit := bCounter(dist_n30), by = .(nestID, ID)]
d[, nest_visit_start := min(datetime_), by = .(nestID, ID, nest_visit)]
d[, nest_visit_end := max(datetime_), by = .(nestID, ID, nest_visit)]
d[, nest_visit_length := difftime(nest_visit_end, nest_visit_start, units = 'hours') %>% as.numeric, by = .(nestID, ID, nest_visit)]

d[, date_ := as.Date(datetime_)]
d[, N_positions_date := .N, by = .(nestID, ID, date_)]
d[dist_n30 == TRUE, N_nv := .N, by = .(nestID, ID)]
d[dist_n30 == TRUE, unique_nv := .N, by = .(nestID, ID, nest_visit)]
d[dist_n30 == TRUE, N_nv_date := .N, by = .(nestID, ID, date_)]
d[dist_n30 == TRUE, unique_nv_date := .N, by = .(nestID, ID, nest_visit, date_)]
d[, prop_at_nest := N_nv_date / N_positions_date * 100]
d[, prop_at_nest := mean(prop_at_nest, na.rm = TRUE), by = .(nestID, ID, nest_visit, date_)]

# time when nest is active 
d[, nest_active := datetime_ < nest_state_date]

# unique nests
nestIDu = d[, nestID] %>% unique

# register cores
registerDoFuture()
plan(multiprocess)


foreach(i = nestIDu, .packages = c('data.table', 'ggplot2', 'patchwork')) %dopar% {
  
  ds = d[dist_n30 == TRUE & nestID == i] 
  dsv = dv[nestID == i] 
  datetime_min = min(ds[, datetime_])
  datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
  initial_clutch_size = ds[1, initial_clutch_size]
  
  dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]
  
  p1 = 
    ggplot() +
    ggtitle(i) +
    geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
    geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
    geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
    geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
    geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
    geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
    geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
    scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
    ylab('Proportion at nest') + xlab('') +
    scale_color_viridis() +
    theme_classic()
  
  
  
  p2 = 
    
    if(nrow(ds[sex == 'M']) == 0) {
      
      ggplot() +
        geom_blank() +
        theme_classic()
      
    }else{
      
      ggplot(data = ds[sex == 'M']) +
        geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        geom_tile(aes(datetime_, 'M_30m', fill = distance_nest), show.legend = FALSE) +
        scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
        ylab('') + xlab('') +
        scale_fill_viridis() +
        theme_classic()
    }
  
  p3 = 
    
    if(nrow(ds[dist_n10 == TRUE & sex == 'M']) == 0) {
      
      ggplot() +
        geom_blank() +
        theme_classic()
      
    }else{
      
      ggplot(data = ds[dist_n10 == TRUE & sex == 'M']) +
        geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        geom_tile(aes(datetime_, 'M_10m', fill = distance_nest), show.legend = FALSE) +
        scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
        ylab('') + xlab('') +
        scale_fill_viridis() +
        theme_classic()
    }
  
  
  p4 = 
    if(nrow(ds[sex == 'F']) == 0) {
      
      ggplot() +
        geom_blank() +
        theme_classic()
      
    }else{
      
      ggplot(data = ds[sex == 'F']) +
        geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        geom_tile(aes(datetime_, 'F_30m', fill = distance_nest), show.legend = FALSE) +
        scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
        ylab('') + xlab('') +
        scale_fill_viridis() +
        theme_classic()
      
    }
  
  
  p5 = 
    if(nrow(ds[dist_n10 == TRUE & sex == 'F']) == 0) {
      
      ggplot() +
        geom_blank() +
        theme_classic()
      
    }else{
      
      ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
        geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        # geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
        geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
        scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
        ylab('') + xlab('') +
        scale_fill_viridis() +
        theme_classic()
    }
  
  p6 = 
    if(nrow(dps) == 0) {
      
      ggplot() +
        geom_blank() +
        theme_classic()
      
    }else{
      
      ggplot(data = dps) +
        geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
        scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
        ylab('') + xlab('Date') +
        scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
        theme_classic()
    }
  
  
  patchwork = p1 / p2 / p3 / p4 / p5 / p6
  patchwork + plot_layout(heights = c(10, 1, 1, 1, 1, 1))
  
  ggsave(paste0('./OUTPUTS/INITIATION_PAIRS/', i, '_', initial_clutch_size,'.png'), plot = last_plot(),  width = 300, height = 200, units = c('mm'), dpi = 'print')
  
}

#--------------------------------------------------------------------------------------------------------------
#' # Assign egg laying datetime for each nest were it is possible



#--------------------------------------------------------------------------------------------------------------
nestIDu[1]
i = "R1001_19"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-14 12:15:18', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-15 12:43:32', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-16 15:57:51', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-17 10:15:15', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-14 12:15:18', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-15 12:43:32', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-16 15:57:51', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-17 10:15:15', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-14 12:15:18', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-15 12:43:32', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-16 15:57:51', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-17 10:15:15', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()


ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := as.POSIXct('2019-06-14 12:15:18', tz = 'UTC')]
dn[nestID == i, initiation_egg2 := as.POSIXct('2019-06-15 12:43:32', tz = 'UTC')]
dn[nestID == i, initiation_egg3 := as.POSIXct('2019-06-16 15:57:51', tz = 'UTC')]
dn[nestID == i, initiation_egg4 := as.POSIXct('2019-06-17 10:15:15', tz = 'UTC')]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[2]
i = "R1003_19"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i]

ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-26 16:14:59', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-27 03:28:02', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-28 05:01:32', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-28 18:38:18', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-26 17:14:59', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-27 03:28:02', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-28 05:01:32', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-28 18:38:18', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := as.POSIXct('2019-06-26 17:14:59', tz = 'UTC')]
dn[nestID == i, initiation_egg2 := as.POSIXct('2019-06-27 03:28:02', tz = 'UTC')]
dn[nestID == i, initiation_egg3 := as.POSIXct('2019-06-28 05:01:32', tz = 'UTC')]
dn[nestID == i, initiation_egg4 := as.POSIXct('2019-06-28 18:38:18', tz = 'UTC')]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[3]
i = "R101_19"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i]

egg1 = as.POSIXct('2019-06-16 10:59:50', tz = 'UTC')
egg2 = as.POSIXct('2019-06-17 10:58:13', tz = 'UTC')
egg3 = as.POSIXct('2019-06-18 11:04:59', tz = 'UTC')
egg4 = as.POSIXct('2019-06-19 12:13:13', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[4]
i = "R102_19"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-17 12:00:00', tz = 'UTC')

ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[5]
i = "R1101_19"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-22 12:00:00', tz = 'UTC')

ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[6]
i = "R201_19"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-08 12:12:00', tz = 'UTC')
egg2 = as.POSIXct('2019-06-09 12:12:00', tz = 'UTC')
egg3 = as.POSIXct('2019-06-10 07:55:10', tz = 'UTC')
egg4 = as.POSIXct('2019-06-11 06:20:02', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[7]
i = "R202_19"
#--------------------------------------------------------------------------------------------------------------

# no change

#--------------------------------------------------------------------------------------------------------------
nestIDu[8]
i = "R203_18"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2018-06-23 07:22:25', tz = 'UTC')
egg2 = as.POSIXct('2018-06-24 04:36:16', tz = 'UTC')
egg3 = as.POSIXct('2018-06-24 19:51:14', tz = 'UTC')
egg4 = as.POSIXct('2018-06-25 20:58:44', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[9]
i = "R203_19"
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-11 07:39:34', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()



x = ds[sex == 'M', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

dn[nestID == i, initiation_egg1 := egg1]


#--------------------------------------------------------------------------------------------------------------
nestIDu[10]
i = 'R204_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-10 14:41:55', tz = 'UTC')
egg2 = as.POSIXct('2019-06-11 12:06:10', tz = 'UTC')
egg3 = as.POSIXct('2019-06-12 12:40:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[11]
i = 'R205_18'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2018-06-22 12:00:00', tz = 'UTC')
egg2 = as.POSIXct('2018-06-23 03:18:40', tz = 'UTC')
egg3 = as.POSIXct('2018-06-24 12:00:00', tz = 'UTC')
egg4 = as.POSIXct('2018-06-25 12:00:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]
d[ID == 270170766]

dn[nestID == i, initiation_egg2 := egg2]

#--------------------------------------------------------------------------------------------------------------
nestIDu[12]
i = 'R205_19'
#--------------------------------------------------------------------------------------------------------------

# no overlap

#--------------------------------------------------------------------------------------------------------------
nestIDu[13]
i = 'R206_19'
#--------------------------------------------------------------------------------------------------------------

# no overlap



#--------------------------------------------------------------------------------------------------------------
nestIDu[14]
i = 'R207_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-11 07:42:32', tz = 'UTC')
egg2 = as.POSIXct('2019-06-12 10:00:47', tz = 'UTC')
egg3 = as.POSIXct('2019-06-13 12:37:11', tz = 'UTC')
egg4 = as.POSIXct('2019-06-14 12:01:14', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[15]
i = 'R208_18'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-11 07:42:32', tz = 'UTC')
egg2 = as.POSIXct('2019-06-12 10:00:47', tz = 'UTC')
egg3 = as.POSIXct('2019-06-13 12:37:11', tz = 'UTC')
egg4 = as.POSIXct('2019-06-14 12:01:14', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[16]
i = 'R209_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-12 11:19:03', tz = 'UTC')
egg2 = as.POSIXct('2019-06-23 12:00:00', tz = 'UTC')
egg3 = as.POSIXct('2019-06-24 12:00:00', tz = 'UTC')
egg4 = as.POSIXct('2019-06-25 12:00:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]


#--------------------------------------------------------------------------------------------------------------
nestIDu[17]
i = 'R210_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, est_hatching_datetime, hatching_datetime)]

egg1 = as.POSIXct('2019-06-12 11:19:37', tz = 'UTC')
egg2 = as.POSIXct('2019-06-13 09:18:08', tz = 'UTC')
egg3 = as.POSIXct('2019-06-14 07:15:58', tz = 'UTC')
egg4 = as.POSIXct('2019-06-15 04:38:52', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[18]
i = 'R211_19'
#--------------------------------------------------------------------------------------------------------------

# no data from female


#--------------------------------------------------------------------------------------------------------------
nestIDu[19]
i = 'R212_19'
#--------------------------------------------------------------------------------------------------------------

# many gaps, unclear



#--------------------------------------------------------------------------------------------------------------
nestIDu[20]
i = 'R213_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 16:10:36', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 15:45:49', tz = 'UTC')
egg3 = as.POSIXct('2019-06-17 21:40:02', tz = 'UTC')
egg4 = as.POSIXct('2019-06-18 18:41:22', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[21]
i = 'R214_19'
#--------------------------------------------------------------------------------------------------------------

# no female data

#--------------------------------------------------------------------------------------------------------------
nestIDu[22]
i = 'R215_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-10 08:46:54', tz = 'UTC')
egg2 = as.POSIXct('2019-06-11 00:32:25', tz = 'UTC')
egg3 = as.POSIXct('2019-06-12 09:59:43', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[23]
i = 'R217_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 12:00:00', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 07:10:04', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[24]
i = 'R218_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-17 12:18:36', tz = 'UTC')
egg2 = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')
egg3 = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')
egg4 = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[25]
i = 'R219_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-16 15:48:41', tz = 'UTC')
egg2 = as.POSIXct('2019-06-17 12:29:51', tz = 'UTC')
egg3 = as.POSIXct('2019-06-18 14:29:58', tz = 'UTC')
egg4 = as.POSIXct('2019-06-19 10:30:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[26]
i = 'R220_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 10:21:28', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 07:15:32', tz = 'UTC')
egg3 = as.POSIXct('2019-06-17 11:37:40', tz = 'UTC')
egg4 = as.POSIXct('2019-06-18 13:36:03', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[27]
i = 'R221_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 10:00:48', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 13:40:12', tz = 'UTC')
egg3 = as.POSIXct('2019-06-17 14:26:58', tz = 'UTC')
egg4 = as.POSIXct('2019-06-18 13:50:46', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]





#--------------------------------------------------------------------------------------------------------------
nestIDu[28]
i = 'R222_19'
#--------------------------------------------------------------------------------------------------------------

# no female data




#--------------------------------------------------------------------------------------------------------------
nestIDu[29]
i = 'R223_19'
#--------------------------------------------------------------------------------------------------------------

# no female data




#--------------------------------------------------------------------------------------------------------------
nestIDu[30]
i = 'R224_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 08:28:24', tz = 'UTC')

ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[31]
i = 'R225_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 11:43:23', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 11:11:44', tz = 'UTC')
egg3 = as.POSIXct('2019-06-17 13:50:46', tz = 'UTC')
egg4 = as.POSIXct('2019-06-18 14:21:12', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[32]
i = 'R226_19'
#--------------------------------------------------------------------------------------------------------------

# no better data

#--------------------------------------------------------------------------------------------------------------
nestIDu[33]
i = 'R227_19'
#--------------------------------------------------------------------------------------------------------------


ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-16 11:07:10', tz = 'UTC')
egg2 = as.POSIXct('2019-06-17 10:40:16', tz = 'UTC')
egg3 = as.POSIXct('2019-06-18 12:57:54', tz = 'UTC')
egg4 = as.POSIXct('2019-06-19 14:11:33', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[34]
i = 'R228_19'
#--------------------------------------------------------------------------------------------------------------

# no data at initiation


#--------------------------------------------------------------------------------------------------------------
nestIDu[35]
i = 'R229_19'
#--------------------------------------------------------------------------------------------------------------

# no female data

#--------------------------------------------------------------------------------------------------------------
nestIDu[36]
i = 'R230_19'
#--------------------------------------------------------------------------------------------------------------

# no female data

#--------------------------------------------------------------------------------------------------------------
nestIDu[37]
i = 'R231_19'
#--------------------------------------------------------------------------------------------------------------

# no data

#--------------------------------------------------------------------------------------------------------------
nestIDu[38]
i = 'R232_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-22 14:01:25', tz = 'UTC')
egg2 = as.POSIXct('2019-06-23 07:21:25', tz = 'UTC')
egg3 = as.POSIXct('2019-06-24 10:59:01', tz = 'UTC')
egg4 = as.POSIXct('2019-06-25 11:20:13', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[39]
i = 'R233_19'
#--------------------------------------------------------------------------------------------------------------

# no further data


#--------------------------------------------------------------------------------------------------------------
nestIDu[40]
i = 'R234_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[41]
i = 'R302_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-10 15:47:51', tz = 'UTC')
egg2 = as.POSIXct('2019-06-11 11:21:57', tz = 'UTC')
egg3 = as.POSIXct('2019-06-12 15:06:07', tz = 'UTC')
egg4 = as.POSIXct('2019-06-14 08:10:34', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[42]
i = 'R303_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[43]
i = 'R304_18'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2018-06-24 11:29:00', tz = 'UTC')
egg2 = as.POSIXct('2018-06-25 14:27:41', tz = 'UTC')
egg3 = as.POSIXct('2018-06-26 16:34:11', tz = 'UTC')
egg4 = as.POSIXct('2018-06-27 17:10:32', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

d[ID == 270170747]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[44]
i = 'R304_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-10 13:41:56', tz = 'UTC')
egg2 = as.POSIXct('2019-06-11 11:28:07', tz = 'UTC')
egg3 = as.POSIXct('2019-06-12 10:46:53', tz = 'UTC')
egg4 = as.POSIXct('2019-06-13 20:17:54', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[45]
i = 'R305_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-13 11:45:02', tz = 'UTC')
egg2 = as.POSIXct('2019-06-14 05:20:13', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[46]
i = 'R306_19'
#--------------------------------------------------------------------------------------------------------------

# no overlap with initiation

#--------------------------------------------------------------------------------------------------------------
nestIDu[47]
i = 'R307_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-07 12:00:00', tz = 'UTC')
egg2 = as.POSIXct('2019-06-08 12:00:15', tz = 'UTC')
egg3 = as.POSIXct('2019-06-09 08:47:05', tz = 'UTC')
egg4 = as.POSIXct('2019-06-10 15:58:32', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[48]
i = 'R308_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-10 08:56:46', tz = 'UTC')
egg2 = as.POSIXct('2019-06-11 04:26:55', tz = 'UTC')
egg3 = as.POSIXct('2019-06-12 20:07:35', tz = 'UTC')
egg4 = as.POSIXct('2019-06-13 23:19:40', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[49]
i = 'R309_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-12 14:39:09', tz = 'UTC')
egg2 = as.POSIXct('2019-06-13 12:05:50', tz = 'UTC')
egg3 = as.POSIXct('2019-06-14 16:09:11', tz = 'UTC')
egg4 = as.POSIXct('2019-06-15 17:19:33', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[50]
i = 'R310_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-13 12:00:00', tz = 'UTC')
egg2 = as.POSIXct('2019-06-14 07:50:24', tz = 'UTC')
egg3 = as.POSIXct('2019-06-15 12:28:25', tz = 'UTC')
egg4 = as.POSIXct('2019-06-16 11:58:57', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]

#--------------------------------------------------------------------------------------------------------------
nestIDu[51]
i = 'R311_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[52]
i = 'R312_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-17 13:12:19', tz = 'UTC')
egg2 = as.POSIXct('2019-06-18 02:59:59', tz = 'UTC')
egg3 = as.POSIXct('2019-06-19 14:52:27', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[53]
i = 'R313_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[54]
i = 'R314_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 12:39:18', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 09:53:27', tz = 'UTC')
egg3 = as.POSIXct('2019-06-17 21:24:50', tz = 'UTC')
egg4 = as.POSIXct('2019-06-18 21:30:56', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[55]
i = 'R317_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-17 08:45:50', tz = 'UTC')
egg2 = as.POSIXct('2019-06-18 16:01:00', tz = 'UTC')
egg3 = as.POSIXct('2019-06-19 14:56:48', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[56]
i = 'R319_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[57]
i = 'R320_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[58]
i = 'R321_19'
#--------------------------------------------------------------------------------------------------------------

# no further data 

#--------------------------------------------------------------------------------------------------------------
nestIDu[59]
i = 'R322_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[60]
i = 'R325_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[61]
i = 'R401_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[62]
i = 'R402_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[63]
i = 'R404_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-14 13:31:02', tz = 'UTC')
egg2 = as.POSIXct('2019-06-15 20:36:08', tz = 'UTC')
egg3 = as.POSIXct('2019-06-16 18:06:49', tz = 'UTC')
egg4 = as.POSIXct('2019-06-17 17:10:36', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[64]
i = 'R405_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[65]
i = 'R406_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-18 11:11:51', tz = 'UTC')
egg2 = as.POSIXct('2019-06-19 12:17:09', tz = 'UTC')
egg3 = as.POSIXct('2019-06-20 08:46:07', tz = 'UTC')
egg4 = as.POSIXct('2019-06-21 15:55:27', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[66]
i = 'R407_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 10:46:01', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 08:07:01', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[67]
i = 'R408_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-14 14:40:45', tz = 'UTC')
egg2 = as.POSIXct('2019-06-15 10:42:49', tz = 'UTC')
egg3 = as.POSIXct('2019-06-16 11:01:38', tz = 'UTC')
egg4 = as.POSIXct('2019-06-17 15:05:10', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'F'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[68]
i = 'R409_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[69]
i = 'R410_19'
#--------------------------------------------------------------------------------------------------------------

# no further data 

#--------------------------------------------------------------------------------------------------------------
nestIDu[70]
i = 'R501_18'
#--------------------------------------------------------------------------------------------------------------


ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2018-06-21 04:46:36', tz = 'UTC')
egg2 = as.POSIXct('2018-06-22 02:58:36', tz = 'UTC')
egg3 = as.POSIXct('2018-06-23 05:58:39', tz = 'UTC')
egg4 = as.POSIXct('2018-06-24 13:29:08', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'F'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[71]
i = 'R501_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[72]
i = 'R502_19'
#--------------------------------------------------------------------------------------------------------------



ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-11 12:00:00', tz = 'UTC')
egg2 = as.POSIXct('2019-06-12 15:23:12', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[73]
i = 'R503_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[74]
i = 'R505_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[75]
i = 'R601_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[76]
i = 'R602_18'
#--------------------------------------------------------------------------------------------------------------


ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2018-06-23 15:18:05', tz = 'UTC')
egg2 = as.POSIXct('2018-06-24 14:42:32', tz = 'UTC')
egg3 = as.POSIXct('2018-06-25 05:37:13', tz = 'UTC')
egg4 = as.POSIXct('2019-06-26 12:00:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[77]
i = 'R602_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[78]
i = 'R603_19'
#--------------------------------------------------------------------------------------------------------------


ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-13 06:52:15', tz = 'UTC')
egg2 = as.POSIXct('2019-06-14 11:36:52', tz = 'UTC')
egg3 = as.POSIXct('2019-06-15 13:20:02', tz = 'UTC')
egg4 = as.POSIXct('2019-06-16 12:00:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[79]
i = 'R604_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-13 11:55:08', tz = 'UTC')
egg2 = as.POSIXct('2019-06-14 11:00:48', tz = 'UTC')
egg3 = as.POSIXct('2019-06-15 11:25:05', tz = 'UTC')
egg4 = as.POSIXct('2019-06-16 18:48:29', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[80]
i = 'R605_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-16 13:36:46', tz = 'UTC')
egg2 = as.POSIXct('2019-06-17 10:36:35', tz = 'UTC')
egg3 = as.POSIXct('2019-06-18 14:19:41', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[81]
i = 'R607_18'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[82]
i = 'R611_19'
#--------------------------------------------------------------------------------------------------------------

# no further data 

#--------------------------------------------------------------------------------------------------------------
nestIDu[83]
i = 'R703_19'
#--------------------------------------------------------------------------------------------------------------



ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-20 10:56:43', tz = 'UTC')
egg2 = as.POSIXct('2019-06-21 11:14:41', tz = 'UTC')
egg3 = as.POSIXct('2019-06-22 10:54:45', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[84]
i = 'R801_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-12 09:36:40', tz = 'UTC')
egg2 = as.POSIXct('2019-06-13 10:49:39', tz = 'UTC')
egg3 = as.POSIXct('2019-06-14 15:03:45', tz = 'UTC')
egg4 = as.POSIXct('2019-06-15 15:16:55', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[85]
i = 'R803_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-19 13:19:05', tz = 'UTC')
egg2 = as.POSIXct('2019-06-20 10:16:39', tz = 'UTC')
egg3 = as.POSIXct('2019-06-21 07:02:58', tz = 'UTC')
egg4 = as.POSIXct('2019-06-22 04:08:09', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'F'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()



ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[86]
i = 'R807_18'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[87]
i = 'R812_18'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[88]
i = 'R901_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[89]
i = 'R903_18'
#--------------------------------------------------------------------------------------------------------------


ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2018-06-23 09:52:54', tz = 'UTC')
egg2 = as.POSIXct('2018-06-24 08:15:22', tz = 'UTC')
egg3 = as.POSIXct('2018-06-25 15:46:58', tz = 'UTC')
egg4 = as.POSIXct('2019-06-26 12:00:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]






#--------------------------------------------------------------------------------------------------------------
nestIDu[90]
i = 'R903_19'
#--------------------------------------------------------------------------------------------------------------

# data with gaps

#--------------------------------------------------------------------------------------------------------------
nestIDu[91]
i = 'R904_19'
#--------------------------------------------------------------------------------------------------------------


ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-15 01:58:19', tz = 'UTC')
egg2 = as.POSIXct('2019-06-16 01:35:31', tz = 'UTC')
egg3 = as.POSIXct('2019-06-17 07:22:32', tz = 'UTC')
egg4 = as.POSIXct('2019-06-18 04:46:56', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'F'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()


ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[92]
i = 'R905_18'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2018-06-23 05:06:47', tz = 'UTC')
egg2 = as.POSIXct('2018-06-24 05:24:33', tz = 'UTC')
egg3 = as.POSIXct('2018-06-25 06:11:25', tz = 'UTC')
egg4 = as.POSIXct('2018-06-26 17:49:31', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()


ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[93]
i = 'R905_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[94]
i = 'R907_19'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[95]
i = 'R909_18'
#--------------------------------------------------------------------------------------------------------------

# no further data

#--------------------------------------------------------------------------------------------------------------
nestIDu[96]
i = 'R910_18'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2018-06-26 08:14:44', tz = 'UTC')
egg2 = as.POSIXct('2018-06-27 12:10:28', tz = 'UTC')
egg3 = as.POSIXct('2018-06-28 12:19:15', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()


ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[97]
i = 'R910_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-17 10:06:06', tz = 'UTC')
egg2 = as.POSIXct('2019-06-18 10:38:50', tz = 'UTC')
egg3 = as.POSIXct('2019-06-19 09:33:42', tz = 'UTC')
egg4 = as.POSIXct('2019-06-20 09:46:43', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]


#--------------------------------------------------------------------------------------------------------------
nestIDu[98]
i = 'R911_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-20 08:36:18', tz = 'UTC')
egg2 = as.POSIXct('2019-06-21 16:20:38', tz = 'UTC')
egg3 = as.POSIXct('2019-06-22 13:57:28', tz = 'UTC')
egg4 = as.POSIXct('2019-06-23 13:29:52', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

x = ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]



#--------------------------------------------------------------------------------------------------------------
nestIDu[99]
i = 'R913_19'
#--------------------------------------------------------------------------------------------------------------

ds = d[dist_n30 == TRUE & nestID == i] 
dsv = dv[nestID == i] 
datetime_min = min(ds[, datetime_])
datetime_max = min(max(ds[, datetime_]), ds[1, initiation] + 6*86400)
initial_clutch_size = ds[1, initial_clutch_size]

dps = dp[ID1 == dn[nestID == i, male_id] & ID2 == dn[nestID == i, female_id]]

dn[nestID == i, .(initiation, initiation_method, initial_clutch_size, clutch_size, found_datetime, 
                  est_hatching_datetime, hatching_datetime, comments)]

egg1 = as.POSIXct('2019-06-21 14:19:56', tz = 'UTC')
egg2 = as.POSIXct('2019-06-22 16:39:31', tz = 'UTC')
egg3 = as.POSIXct('2019-06-23 12:00:00', tz = 'UTC')
egg4 = as.POSIXct('2019-06-24 12:00:00', tz = 'UTC')


ggplot() +
  ggtitle(i) +
  geom_vline(data = ds[sex == 'M'], aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'grey50', size = 3, alpha = 0.5) +
  geom_label(data = dsv, aes(datetime_, Inf, label = clutch_size), vjust = 1) +
  geom_label(data = dsv, aes(datetime_, Inf, label = substr(time_appr, 0, 5)), vjust = 2) +
  geom_point(data = ds[sex == 'M'], aes(datetime_, prop_at_nest), color = 'dodgerblue3') +
  geom_point(data = ds[sex == 'F'], aes(datetime_, prop_at_nest), color = 'darkorange') +
  geom_point(data = dps, aes(datetime_1, prop_together), color = 'darkgreen') +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('Proportion at nest') + xlab('') +
  scale_color_viridis() +
  theme_classic()


ggplot(data = ds[dist_n10 == TRUE & sex == 'F']) +
  geom_vline(aes(xintercept = initiation), color = 'firebrick2', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  geom_tile(aes(datetime_, 'F_10m', fill = distance_nest), show.legend = FALSE) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('') +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = dps) +
  geom_tile(aes(datetime_1, 'int', fill = interaction), show.legend = FALSE) +
  geom_vline(aes(xintercept = egg1), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg2), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg3), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = egg4), color = 'black', size = 3, alpha = 0.5) +
  scale_x_datetime(limits = c(datetime_min, datetime_max), date_breaks = 'days', date_labels = '%d') +
  ylab('') + xlab('Date') +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  theme_classic()

ds[dist_n10 == TRUE & sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]

ds[sex == 'F', .(nestID, distance_nest, nest_visit_start, nest_visit_end, nest_visit_length)]


dn[nestID == i, initiation_egg1 := egg1]
dn[nestID == i, initiation_egg2 := egg2]
dn[nestID == i, initiation_egg3 := egg3]
dn[nestID == i, initiation_egg4 := egg4]
dn[nestID == i, initiation_method := 4]




#--------------------------------------------------------------------------------------------------------------
nestIDu[100]
i = 'REPH475_19'
#--------------------------------------------------------------------------------------------------------------

# no further data 
