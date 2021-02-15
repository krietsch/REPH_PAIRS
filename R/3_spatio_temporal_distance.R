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


# read data
dp = fread('./DATA/PAIR_WISE_DIST.txt', sep = '\t', header = TRUE) %>% data.table

dp[, date_ := as.Date(datetime_10min)]





ds = dp[date_ == dp[4000, date_]]

ggplot(data = ds) +
  geom_histogram(aes(distance))




# round times to 10 min intervalls
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_10min := round(datetime_, '10 mins')]
d[, datetime_ := datetime_ %>% as.character %>% as.POSIXct]
d[, datetime_10min := datetime_10min %>% as.character %>% as.POSIXct]

ds = d[datetime_10min == d[4000, datetime_10min]]

bm = create_bm(ds)

bm + 
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = as.character(ID)), size = 1.5) 



# interactions
dp[, interaction := distance < 15]
dp[, .N, interaction]
ds = dp[interaction == TRUE]


dss = ds[, .N, by = .(ID1, ID2)]

ggplot(data = dss) +
  geom_histogram(aes(N))

dss[N > 500]


# found fastest speed within a real track = 104 km/h
ID_ = 270170763
dt_ = anytime('2019-06-19 23:07:22')
ds = d[ID == ID_ & datetime_ > c(dt_ - 3600*2) & datetime_ < c(dt_ + 3600*2)]
ds = d[ID == ID_]

bm = create_bm(ds)

bm + 
  geom_path(data = ds, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = ds, aes(lon, lat, color = ID), size = 1.5) +
  scale_color_viridis(direction = -1)


ds = dp[ID1 == 270170763 & ID2 == 270170764]

ggplot(data = ds) +
  geom_point(aes(datetime_10min, distance))


d[, date_ := as.Date(datetime_10min)]

dss = d[, .N, datetime_10min]
dss[, year_ := year(datetime_10min)]

ggplot(data = dss) +
  geom_point(aes(datetime_10min, N)) +
  facet_wrap(.~year_, scales = 'free')





dp[, date_ := as.Date(datetime_10min)]
du = unique(dp[interaction == TRUE], by = c('ID1', 'ID2', 'date_'))



dp[, ID1] %>% unique %>% length
du[, ID1] %>% unique %>% length







ds = copy(dp)
ds[, year_ := year(date_)]
ds = ds[year_ == 2018]

ds[, obs_id := 1:nrow(ds)]
ds = rbind(ds[, .(ID = ID1, obs_id)], ds[, .(ID = ID2, obs_id)])

require(asnipe)
require(igraph)

# create matrix with observation ID by individual
gbi = get_group_by_individual(ds[, .(ID, obs_id)], data_format = 'individuals')

# calculate a network
netw = get_network(gbi, data_format = 'GBI', association_index = 'SRI')

# plot network
pn = graph.adjacency(netw, mode = 'undirected', weighted = TRUE, diag = FALSE)
plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black')


















