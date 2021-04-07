#==============================================================================================================
# Spatio-temporal clusters for each pair
#==============================================================================================================

# Summary

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
dp[, year_ := year(datetime_10min)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Define breeding pairs with both sexes tagged
#--------------------------------------------------------------------------------------------------------------

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

#--------------------------------------------------------------------------------------------------------------
#' # Define clusters for each pair
#--------------------------------------------------------------------------------------------------------------

# example pair
dns = dnID[nestID == 'R304_18']
ds = d[ID %in% c(dns[, male_id], dns[, female_id])]




setorder(ds, ID, datetime_)
ds[, pointID := seq_len(.N), by = ID]

ID = unique(ds[, ID])

# register cores

ds = foreach(i = ID, .combine = rbind, .packages = c('data.table','tdbscan') ) %do% {
  
  # subset individual and create track
  dss = ds[ID == i]
  track = dt2Track(dss, y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)
  
  z = tdbscan(track, eps = 30, minPts = 3, maxLag = 6, borderPoints = TRUE )
  
  dss[, clustID := z$clustID]
  dss
  
}


ds[!is.na(clustID), ID_clustID := paste0(ID, '_', clustID)]

# create dt with convex hull polygons
dc = dt2Convexhull(ds[!is.na(clustID), .(ID_clustID, lat, lon, datetime_)],
                   pid = 'ID_clustID', y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)

s = stoscan(dc)

# merge with nests 
# d = rbind(d, n[, .(ID_clustID = nest, datetime_ = initiation, latit, longit, NARL)], fill = TRUE)

ds = merge(ds, s, by.x = 'ID_clustID', by.y = 'pid', all.x = TRUE)

setorder(ds, ID, datetime_)
ds


ds[s_clustID == 0, s_clustID := NA]



ds[, s_clustID_forward_fill := s_clustID[1], .(ID, cumsum(!is.na(s_clustID)))]
ds[is.na(s_clustID_forward_fill), s_clustID_forward_fill := 0]


ggplot(data = ds) +
  geom_point(aes(datetime_, s_clustID_forward_fill, color = ID, group = ID)) +
  geom_line(aes(datetime_, s_clustID_forward_fill, color = ID, group = ID))













