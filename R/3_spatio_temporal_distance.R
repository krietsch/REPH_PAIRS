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
#' # Distance matrix
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
d = d[, .(year_ = c(1), datetime_ = mean(datetime_), lat = mean(lat), lon = mean(lon), 
          gps_speed = mean(gps_speed), altitude = mean(altitude), batvolt = mean(batvolt)), 
          by = .(ID, datetime_10min)]

anyDuplicated(d, by = c('ID', 'datetime_10min'))



# calculate distance between all points for each time point

# register cores
require(doFuture)
registerDoFuture()
plan(multiprocess)


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
  setnames(d_paired, c('ID', 'ID2', 'distance'))
  
  d_paired[, datetime_10min := i]
  d_paired
  
}








ds = d[datetime_10min == d[30, datetime_10min]]

d_sf = st_as_sf(ds, crs = PROJ, coords = c('lon', 'lat'), row.names = 'ID')

d_distance_matrix = st_distance(d_sf, which = 'Euclidean') %>% as.matrix
rownames(d_distance_matrix) = ds[, ID]
colnames(d_distance_matrix) = ds[, ID]

dw = as.table(d_distance_matrix) %>% data.table
setnames(dw, c('ID', 'ID2', 'distance'))





d[, ID_dt_10min := paste0(ID, '_', datetime_10min)]

d[]

ds = d[ID_dt_10min %in% d[duplicated == TRUE]$ID_dt_10min]

ds = ds[, .(ID, datetime_, datetime_10min, duplicated)]


ID_ = 270170063
dt_ = as.POSIXct('2019-06-14 00:17:45')
dss = d[ID == ID_ & datetime_ > c(dt_ - 3600) & datetime_ < c(dt_ + 3600)]
dss 


d$datetime_

unique(d, by = c('tagID', 'datetime_10min'))

d_sf = st_as_sf(d, crs = PROJ, coords = c('lon', 'lat'))




































