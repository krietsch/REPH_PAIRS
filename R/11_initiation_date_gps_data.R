#==============================================================================================================
# Initiation date and GPS data
#==============================================================================================================

# Summary


# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork', 'windR', 'tdbscan'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table

d[, datetime_ := as.POSIXct(datetime_, tz = 'UTC')]

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

# time when nest isactive 
d[, nest_active := datetime_ < nest_state_date]



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


ggplot(data = ds[sex == 'M']) +
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

























