#==============================================================================================================
# Initiation date and GPS data
#==============================================================================================================

# Summary


# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr',
          'sdbvis', 'viridis', 'patchwork', 'windR', 'tdbscan', 'doFuture'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

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
#--------------------------------------------------------------------------------------------------------------

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
#--------------------------------------------------------------------------------------------------------------

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
#--------------------------------------------------------------------------------------------------------------

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

# check data
# ggplot(data = d[sex == 'M' & any_dist_n30 == FALSE]) +
#   geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.5, show.legend = FALSE) +
#   scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
#   geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
#   geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
#   theme_classic()
# 
# ggplot(data = d[sex == 'M' & any_dist_n30 == FALSE]) +
#   geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.5, show.legend = FALSE) +
#   scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
#   geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
#   geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
#   theme_classic()

# exclude nests with no data at nest
d = d[any_dist_n30 == TRUE]

#--------------------------------------------------------------------------------------------------------------
#' # Time at the nest
#--------------------------------------------------------------------------------------------------------------

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
nestIDu = dv[nestID %in% nestIDu, nestID] %>% unique

# register cores
# registerDoFuture()
# plan(multiprocess)


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
  
  ggsave(paste0('./OUTPUTS/INITIATION_PAIRS/', initial_clutch_size, '_', i,'.png'), plot = last_plot(),  width = 300, height = 200, units = c('mm'), dpi = 'print')
  
}


#--------------------------------------------------------------------------------------------------------------
#' # Other plots
#--------------------------------------------------------------------------------------------------------------





ds = d[dist_n30 == TRUE & nestID == 'R218_19' & sex == 'F']
ds[, .(datetime_, distance_nest, nest_visit, nest_visit_length)]

dsv = dv[nestID == 'R218_19']

ggplot(data = ds) +
  geom_tile(aes(datetime_, nestID, color = distance_nest)) +
  geom_vline(aes(xintercept = initiation), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  scale_color_viridis() +
  theme_classic()



ggplot(data = ds) +
  geom_point(aes(datetime_, prop_at_nest, color = distance_nest)) +
  geom_vline(aes(xintercept = initiation), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  scale_color_viridis() +
  theme_classic()






ds = d[dist_n30 == TRUE & nestID == 'R218_19' & sex == 'F']
ds[, .(datetime_, distance_nest, nest_visit, nest_visit_length)]

dsv = dv[nestID == 'R218_19']

ggplot(data = ds) +
  geom_tile(aes(datetime_, nestID, color = distance_nest)) +
  geom_vline(aes(xintercept = initiation), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  scale_color_viridis() +
  theme_classic()




ds = d[dist_n30 == TRUE & nestID == 'R406_19' & sex == 'F']
ds[, .(datetime_, distance_nest, nest_visit, nest_visit_length)]

dsv = dv[nestID == 'R406_19']

ggplot(data = ds[dist_n10 == TRUE]) +
  geom_tile(aes(datetime_, nestID, color = distance_nest)) +
  geom_vline(aes(xintercept = initiation), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-19 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-20 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-21 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  scale_color_viridis() +
  theme_classic()



ds[dist_n30 == TRUE, .(nestID, datetime_, distance_nest)]











ggplot(data = d[sex == 'M' & nest_active == TRUE]) +
  geom_point(aes(initiation_rel, prop_at_nest, color = found_incomplete)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()

ds = unique(d[dist_n30 == TRUE], by = c('nestID', 'ID', 'date_'))
ds[, ID_nestID := paste0(ID, '_', nestID)]

ggplot(data = ds[sex == 'M' & nest_active == TRUE]) +
  geom_line(aes(initiation_rel, prop_at_nest, group = ID_nestID, color = found_incomplete)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()


d[sex == 'F']


ggplot(data = ds[sex == 'F']) +
  geom_line(aes(initiation_rel, prop_at_nest, group = ID_nestID, color = found_incomplete)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()


setorder(ds, nestID, ID, date_)
ds[, prop_at_nest_mean := frollmean(prop_at_nest, 3, na.rm = TRUE), by = .(nestID, ID)]
ds[is.na(prop_at_nest_mean), prop_at_nest_mean := prop_at_nest]

ds[, prop_at_nest_max := max(prop_at_nest, na.rm = TRUE), by = .(nestID, ID)]
ds[, prop_at_nest_peak := prop_at_nest_max == prop_at_nest]


ggplot(data = ds[sex == 'M' & datetime_ < nest_state_date]) +
  geom_smooth(aes(initiation_rel, prop_at_nest_mean), method = 'gam') +
  geom_point(aes(initiation_rel, prop_at_nest_mean, group = ID_nestID, color = found_incomplete)) +
  geom_point(data = ds[sex == 'F' & prop_at_nest_peak == TRUE], aes(initiation_rel, prop_at_nest_mean), color = 'firebrick2') +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic(base_size = 24)

ggplot(data = ds[sex == 'M' & prop_at_nest_peak == TRUE]) +
  geom_bar(aes(initiation_rel0, fill = found_incomplete)) +
  theme_classic(base_size = 24)


ggplot() +
  geom_point(data = ds, aes(datetime_, nestID, color = distance_nest)) +
  
  # geom_vline(data=tmp[c(13,26),],aes(xintercept=as.numeric(x)))
  
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(data = ds, aes(xintercept = initiation), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  scale_color_viridis() +
  theme_classic()




ds = d[dist_n30 == TRUE & nestID == 'R913_19' & sex == 'M']
ds[, .(datetime_, distance_nest, nest_visit, nest_visit_length)]
ds[dist_n10 == TRUE, .(datetime_, distance_nest, nest_visit, nest_visit_length)]

dsv = dv[nestID == 'R913_19']

ggplot(data = ds) +
  geom_point(aes(datetime_, nestID, color = distance_nest)) +
  geom_vline(data = dsv, aes(xintercept = datetime_), color = 'firebrick3', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = initiation), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:51:58', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = as.POSIXct('2019-06-19 07:11:27', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = as.POSIXct('2019-06-20 04:26:47', tz = 'UTC')), color = 'black', size = 3, alpha = 0.5) +
  scale_color_viridis() +
  theme_classic()





ggplot(data = d[sex == 'F']) +
  geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.01, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()


ggplot(data = d[sex == 'F' & nestID == 'R218_19']) +
  geom_tile(aes(datetime_, nestID, fill = dist_n20), width = 20000, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()




ggplot(data = d[sex == 'M']) +
  geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()



ggplot(data = d[sex == 'F'& found_incomplete == FALSE]) +
  geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()


ggplot(data = d[sex == 'F'& found_incomplete == TRUE]) +
  geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()

























