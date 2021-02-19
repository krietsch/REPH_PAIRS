#' ---
#' title: Calculate spatio-temporal distance of points
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
# Calculate spatio-temporal distance of points
#==============================================================================================================

# Summary
# 1. Apply speed filter 
# 2. Apply distance filter 
# 3. Check altitudes

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr'), 
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

#--------------------------------------------------------------------------------------------------------------
#' # Distance between all locations
#--------------------------------------------------------------------------------------------------------------

# round times to 10 min intervalls
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_10min := round(datetime_, '10 mins')]
d[, datetime_ := datetime_ %>% as.character %>% as.POSIXct]
d[, datetime_10min := datetime_10min %>% as.character %>% as.POSIXct]

# check for duplicates by ID
d[, duplicated := duplicated(d, by = c('ID', 'datetime_10min'))]
d[duplicated == TRUE] %>% nrow

# mean of these instances
d = d[, .(year_ = mean(year_), datetime_ = mean(datetime_), lat = mean(lat), lon = mean(lon), 
          gps_speed = mean(gps_speed), altitude = mean(altitude), batvolt = mean(batvolt)), 
          by = .(ID, datetime_10min)]

anyDuplicated(d, by = c('ID', 'datetime_10min'))

# register cores
require(doFuture)
registerDoFuture()
plan(multiprocess)

# calculate distance between all points for each time point
setkey(d, datetime_)
xt = unique(d[, datetime_10min])
dp = foreach(i = xt, .combine = 'rbind', .packages = c('sf', 'data.table')) %dopar% {
  
  # subset time
  ds = d[datetime_10min == i]
  
  # create sf object
  d_sf = st_as_sf(ds, crs = PROJ, coords = c('lon', 'lat'))
  
  # calculate distance
  d_distance_matrix = st_distance(d_sf, which = 'Euclidean') %>% as.matrix
  rownames(d_distance_matrix) = ds[, ID]
  colnames(d_distance_matrix) = ds[, ID]
  
  # create pair wise table
  d_paired = as.table(d_distance_matrix) %>% data.table
  setnames(d_paired, c('ID1', 'ID2', 'distance'))
  
  d_paired[, datetime_10min := i]
  d_paired
  
}

# exclude duplicates 
dp = dp[ID1 < ID2]
dp = dp[ID1 != ID2]

# save data
# fwrite(dp, './DATA/PAIR_WISE_DIST.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------
#' # Distance of all locations to nests
#--------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
st_transform_DT(dn)
DBI::dbDisconnect(con)

# include positions of nests
dn = dn[, .(nestID, year_n = year_, lat_n = lat, lon_n = lon)]

# distance between each position and all nests
dpn = foreach(i = dn[, nestID] %>% unique, .combine = 'rbind') %dopar% {
  
  dns = dn[nestID == i]
  ds = d[year_ == dns[1, year_n]]
  ds = cbind(ds, dns)
  ds[, distance := sqrt(sum((c(lon, lat) - c(lon_n, lat_n))^2)), by = 1:nrow(ds)]
  ds = ds[, .(ID1 = ID, ID2 = nestID, distance, datetime_10min)]

}

# save data
# fwrite(dpn, './DATA/PAIR_WISE_DIST_NESTS.txt', quote = TRUE, sep = '\t', row.names = FALSE)


