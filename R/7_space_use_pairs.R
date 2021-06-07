#==============================================================================================================
# Spatial use of pairs 
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'windR', 'ggnewscale', 'doFuture', 'patchwork','tdbscan'), 
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

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

#--------------------------------------------------------------------------------------------------------------
#' # For all pairs
#--------------------------------------------------------------------------------------------------------------

# any interactions?
dp[, any_interactions := any(interaction == TRUE), by = pairID]

# unique pair combinations 
dpu = unique(dp[ID1 < ID2], by = 'pairID')
dpu[, .N, any_interactions]

dpu = dpu[any_interactions == TRUE]

setorder(d, ID, datetime_)
d[, pointID := seq_len(.N), by = .(year_, ID)]

# register cores
require(doFuture)
registerDoFuture()
plan(multiprocess)


do = foreach(j = 1:nrow(dpu), .combine = rbind, .packages = c('data.table','tdbscan') ) %dopar% {
  
  # subset pair
  du = d[ID == dpu[j, ID1] | ID == dpu[j, ID2]]
  du[, pairID := dpu[j, pairID]]
  ID = unique(du[, ID])
  
  # tbscan for each pair
  o = foreach(i = ID, .combine = rbind) %do% {
    
    # subset individual and create track
    ds = du[ID == i]
    
    track = dt2Track(ds, y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)
    
    z = tdbscan(track, eps = 30, minPts = 3, maxLag = 6, borderPoints = TRUE )
    
    ds[, clustID := z$clustID]
    ds
    
  }
  
  
  # stoscan for each pair
  o[!is.na(clustID), ID_clustID := paste0(ID, '_', clustID)]
  
  # create dt with convex hull polygons
  dc = dt2Convexhull(o[!is.na(clustID), .(ID_clustID, lat, lon, datetime_)],
                     pid = 'ID_clustID', y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)
  
  s = stoscan(dc)
  
  # merge tdbscan and stoscan
  o = merge(o, s, by.x = 'ID_clustID', by.y = 'pid', all.x = TRUE)
  o
  
}


# save data
fwrite(do, './DATA/PAIR_WISE_SPACE_USE.txt', quote = TRUE, sep = '\t', row.names = FALSE)
















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
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# assign clutch order
setorder(dnID, male_id, initiation)
dnID[, clutch_together := seq_len(.N), by = .(year_, male_id, female_id)]

# tagged breeders
dnb = rbind(dnID[, .(year_, nestID, ID = male_id,   sex = 'M', initiation, lat_n, lon_n, clutch_together)],
            dnID[, .(year_, nestID, ID = female_id, sex = 'F', initiation, lat_n, lon_n, clutch_together)])

#--------------------------------------------------------------------------------------------------------------
#' # pair-wise spatio temporal clusters
#--------------------------------------------------------------------------------------------------------------

d = merge(d, dnb, by = c('year_', 'ID'), all.x = TRUE, allow.cartesian = TRUE)
d = d[!is.na(nestID)]

setorder(d, ID, datetime_)
d[, pointID := seq_len(.N), by = .(year_, ID)]

# register cores
require(doFuture)
registerDoFuture()
plan(multiprocess)


do = foreach(j = 1:nrow(dnID), .combine = rbind, .packages = c('data.table','tdbscan') ) %dopar% {

# subset pair
du = d[nestID == dnID[j, nestID]]
ID = unique(du[, ID])

# tbscan for each pair
  o = foreach(i = ID, .combine = rbind) %do% {
    
    # subset individual and create track
    ds = du[ID == i]
    
    track = dt2Track(ds, y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)
    
    z = tdbscan(track, eps = 30, minPts = 3, maxLag = 6, borderPoints = TRUE )
    
    ds[, clustID := z$clustID]
    ds
    
  }
  
  
  # stoscan for each pair
  o[!is.na(clustID), ID_clustID := paste0(ID, '_', clustID)]
  
  # create dt with convex hull polygons
  dc = dt2Convexhull(o[!is.na(clustID), .(ID_clustID, lat, lon, datetime_)],
                     pid = 'ID_clustID', y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)
  
  s = stoscan(dc)
  
  # merge tdbscan and stoscan
  o = merge(o, s, by.x = 'ID_clustID', by.y = 'pid', all.x = TRUE)
  o

}


# save data
fwrite(do, './DATA/PAIR_WISE_SPACE_USE.txt', quote = TRUE, sep = '\t', row.names = FALSE)

