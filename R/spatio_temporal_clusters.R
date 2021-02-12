#========================================================================================================================
# Calculate spatio-temporal clusters
#========================================================================================================================

# Summary
# 1. Assign locations in the study area
# 2. Find spatial cluster

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'foreach', 'doParallel', 'tdbscan', 
          'auksRuak', 'sf', 'lwgeom'), require, character.only = TRUE)

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID != 999] # exclude test data
d[is.na(lon)] # check that no NA
d[, datetime_ := anytime(datetime_)]
d[, datetime_y := anytime(paste0('2020-', substr(datetime_, 6, 19)) )]
d[, c('batvolt', 'pk', 'year_', 'speed', 'altitude') := NULL]

dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
DBI::dbDisconnect(con)

# change projection
st_transform_DT(d)

#------------------------------------------------------------------------------------------------------------------------
# 1. Assign locations in the study area
#------------------------------------------------------------------------------------------------------------------------

# points in study site
point_over_poly_DT(d, poly = study_site, buffer = 100)
setnames(d, 'poly_overlap', 'study_site')

#------------------------------------------------------------------------------------------------------------------------
# 2. Find spatial cluster
#------------------------------------------------------------------------------------------------------------------------

setorder(d, ID, datetime_)
d[, pointID := seq_len(.N), by = ID]

ID = unique(d$ID)

# register cores
cl = 50 %>% makePSOCKcluster; registerDoParallel(cl)

d = foreach(i = ID, .combine = rbind, .packages = c('data.table','tdbscan') ) %dopar% {
  
  # subset individual and create track
  ds = d[ID == i]
  track = dt2Track(ds, y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)
  
  z = tdbscan(track, eps = 30, minPts = 3, maxLag = 6, borderPoints = TRUE )
  
  ds[, clustID := z$clustID]
  ds
  
}


stopCluster(cl)
registerDoSEQ()

# saveRDS(d, './DATA/REPH_tdbscan.RDS')

#------------------------------------------------------------------------------------------------------------------------
# 3. Caculate spatial and spatio-temporal clusters
#------------------------------------------------------------------------------------------------------------------------

# load spatial clusters
d = readRDS('./DATA/REPH_tdbscan.RDS')

d[!is.na(clustID), ID_clustID := paste0(ID, '_', clustID)]

# create dt with convex hull polygons
dp = dt2Convexhull(d[!is.na(clustID), .(ID_clustID, lat, lon, datetime_)],
                   pid = 'ID_clustID', y = 'lat', x = 'lon', dt = 'datetime_', projection = PROJ)

# # add polygon for nest location
# n = dbq(con, 'select * FROM NESTS')
# n = n[year_ == 2018 & (!is.na(male_id) | !is.na(female_id))]
# st_transform_DT(n)
# 
# # assign similar id's
# n[, pid := nest]
# n[, initiation := as.POSIXct(initiation)]
# n[, arrival := initiation]
# n[is.na(arrival), arrival := as.POSIXct(found_datetime)]
# n[, departure := as.POSIXct(nest_state_date)]
# n[is.na(departure), departure := as.POSIXct(collected_datetime)]
# n[is.na(departure), departure := initiation + 86400]
# n[plot == 'study_site', study_site := TRUE]
# n[plot != 'study_site', study_site := FALSE]
# 
# nb = n[, .(pid, arrival, departure, 
#            geometry = st_as_sf(.SD, coords = c('lon', 'lat')) %>% 
#              st_buffer(dist = 3) %>% 
#              st_geometry %>%
#              st_set_crs(st_crs(PROJ)))]
# 
# dp = rbind(dp, nb)

# spatial and spatio-temporal clusters
s = stoscan(dp)

# merge with nests 
# d = rbind(d, n[, .(ID_clustID = nest, datetime_ = initiation, lat, lon, study_site)], fill = TRUE)

d = merge(d, s, by.x = 'ID_clustID', by.y = 'pid', all.x = TRUE)

setorder(d, ID, datetime_)
d

# saveRDS(d, './DATA/REPH_stoscan.RDS')

#------------------------------------------------------------------------------------------------------------------------
# 3. Calculate social network by spatio-temporal overlap
#------------------------------------------------------------------------------------------------------------------------

require(asnipe)
require(igraph)

d = readRDS('./DATA/REPH_stoscan.RDS')

# exclude non-overlapping events
d = d[!is.na(st_clustID)]
d = unique(d, by = c('ID', 'st_clustID'))

x = d[, .N, by = st_clustID]
setorder(x, N)

d[, year_ := year(datetime_)]
year_s = 2018
year_s = 2019

ds = d[year_ == year_s]
# ds = copy(d)
unique(ds$ID) %>% length()

# create matrix with observation ID by individual
gbi = get_group_by_individual(ds[, .(ID, st_clustID)], data_format = 'individuals')

# calculate a network
netw = get_network(gbi, data_format = 'GBI')

# plot network
pn = graph.adjacency(netw, mode = 'undirected',weighted = TRUE, diag = FALSE)
plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black')

# assign sex
ID_netw = unique(ds$ID)
dcn = dc[ID %in% ID_netw, .(ID, sex_observed)]
dcn = unique(dcn)
dcn[, ID := as.character(ID)]

ID_pn = data.table(ID = V(pn)$name)
ID_pn[, order := 1:nrow(ID_pn)]
ID_pn = merge(ID_pn, dcn, by = 'ID')
setorder(ID_pn, order)

V(pn)$sex = ID_pn$sex_observed

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 3, edge.color = 'black',
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])

# point size by number of observations
ds_N = ds[, .N, by = ID]
ds_N[, ID := as.character(ID)]

ID_pn = merge(ID_pn, ds_N, by = 'ID')
setorder(ID_pn, order)

V(pn)$size = as.numeric(ID_pn$N) %>% log

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size, edge.color = 'black',
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])

# point color by nest or not
load('//ds/grpkempenaers/Hannes/REPH_BREEDING_STUDY/DATA/ID_nest_information.RData') # load nest info by ID created in 1_nests_&_parentage

dnID[, any_nest := TRUE]
dnID = dnID[year_ == year_s]

ID_pn = merge(ID_pn, dnID[, .(ID, any_nest)], by = 'ID', all.x = TRUE)
ID_pn[is.na(any_nest), any_nest := FALSE]
setorder(ID_pn, order)

V(pn)$any_nest = ID_pn$any_nest

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_nest == TRUE)])


plot(pn, edge.width = 10*E(pn)$weight^2, vertex.size = V(pn)$size+2, edge.color = 'black',
     vertex.color = c('red', 'green')[1+(V(pn)$any_nest == TRUE)])


# pair-wise table
dw = as.table(netw) %>% data.table
setnames(dw, c('ID', 'ID2', 'association'))

# exclude duplicated associations
dw = dw[ID < ID2]

# merge with nest data
dn[, male_id := as.character(male_id)]
dn[, female_id := as.character(female_id)]

dw = merge(dw, dn[, .(male_id, female_id, pair1 = TRUE)], by.x = c('ID', 'ID2'), by.y = c('male_id', 'female_id'), all.x = TRUE)
dw = merge(dw, dn[, .(male_id, female_id, pair2 = TRUE)], by.x = c('ID', 'ID2'), by.y = c('female_id', 'male_id'), all.x = TRUE)
dw[, pair := ifelse(pair1 == TRUE | pair2 == TRUE, TRUE, FALSE)]
dw[, pair1 := NULL]
dw[, pair2 := NULL]

# plot association index pair vs. unknown
ggplot() +
  geom_violin(data = dw[association != 0], aes(pair, association)) +
  theme_classic(base_size = 24)


#-------------------------------------------------------------------------------------------------------------------------------------
# Some plots
#-------------------------------------------------------------------------------------------------------------------------------------

d = readRDS('./DATA/REPH_stoscan.RDS')

# points in study site
point_over_poly_DT(d, poly = study_site, buffer = 10)
# setnames(d, 'poly_overlap', 'study_site')

d[is.na(tagID) & !is.na(s_clustID)]


ds = unique(d[!is.na(st_clustID)], by = c('ID', 'st_clustID'))

x = ds[, .N, by = st_clustID]
setorder(x, N)
x

bm = create_bm(d[st_clustID == 1], buffer = 50)

bm + 
  geom_path(data = d[st_clustID == 1], aes(lon, lat, color = NULL), col = 'grey', size = .5) +
  geom_point(data = d[st_clustID == 1], aes(lon, lat, color = as.character(ID) ), alpha = .5, size = 2, show.legend = FALSE)




# plot of spatial overlapping clusters
ggplot(d, aes(lon, lat, color = as.character(s_clustID) ) ) +
  geom_path(aes(color = NULL), col = 'grey', size = .5) +
  geom_point( alpha = .5, size = 2, show.legend = FALSE)


# plot of spatial overlapping clusters
ggplot(d[study_site == TRUE], aes(lon, lat, color = as.character(st_clustID) ) ) +
  geom_path(aes(color = NULL), col = 'grey', size = .5) +
  geom_point( alpha = .5, size = 2, show.legend = FALSE) +
  coord_fixed()





# by pair

# plot of spatial overlapping clusters
ggplot(d[ID == 270170746 & study_site == TRUE | ID == 270170747 ], aes(lon, lat, color = as.character(s_clustID) ) ) +
  geom_path(aes(color = NULL), col = 'grey', size = .5) +
  geom_point( alpha = .5, size = 2, show.legend = FALSE) +
  geom_point( data = n[nest == 'R304'], aes(lon, lat), color = 'red', size = 10, shape = 1 ) +
  coord_fixed()


# plot of spatial-temporal  overlapping clusters
ggplot(d[ID == 270170746 & study_site == TRUE | ID == 270170747 ], aes(lon, lat, color = as.character(st_clustID) ) ) +
  geom_path(aes(color = NULL), col = 'grey', size = .5) +
  geom_point( alpha = .5, size = 2, show.legend = FALSE) +
  geom_point( data = n[nest == 'R304'], aes(lon, lat), color = 'red', size = 10, shape = 1 ) +
  coord_fixed()

d[ID_clustID == 'R304']

plot(dp[pid == 'R304']$geometry)
plot(nb$geometry)


ds = d[ID == 270170746 | ID == 270170747]
ds = d[ID == 270170752 | ID == 270170753]
ds = d[ID == 270170743 | ID == 270170068]
ds = d[ID == 270170763 | ID == 270170764]




do = ds[!is.na(s_clustID), .(s_arrival = min(datetime_)), by = s_clustID]
ds[, s_clustID := factor(s_clustID, levels = do$s_clustID)]

do = ds[!is.na(st_clustID), .(st_arrival = min(datetime_)), by = st_clustID]
ds[, st_clustID := factor(st_clustID, levels = do$st_clustID)]

ggplot(ds[!is.na(s_clustID)]) +
  geom_line(aes(y = s_clustID, x = datetime_, group = as.factor(ID), color = as.factor(ID)), alpha = 0.3, size = 2)

ggplot(ds[!is.na(st_clustID)]) +
  geom_line(aes(y = st_clustID, x = datetime_, group = as.factor(ID), color = as.factor(ID)), alpha = 0.3, size = 2)




ggplot(ds[!is.na(s_clustID) & datetime_ > as.POSIXct('2018-06-23 00:00:00') & datetime_ < as.POSIXct('2018-06-29 00:00:00')]) +
  geom_line(aes(y = as.character(s_clustID), x = datetime_, group = as.character(ID), color = as.character(ID)), alpha = 0.3, size = 2)



ggplot(d[ID == 270170746 & datetime_ > as.POSIXct('2018-06-23 00:00:00') & datetime_ < as.POSIXct('2018-06-29 00:00:00') |
           ID == 270170747 & datetime_ > as.POSIXct('2018-06-23 00:00:00') & datetime_ < as.POSIXct('2018-06-29 00:00:00')],
       aes(lon, lat, color = as.character(st_clustID) ) ) +
  geom_path(aes(color = NULL), col = 'grey', size = .5) +
  geom_point( alpha = .5, size = 2, show.legend = FALSE) +
  coord_fixed()


ggplot(ds[!is.na(st_clustID) & datetime_ > as.POSIXct('2018-06-23 00:00:00') & datetime_ < as.POSIXct('2018-06-29 00:00:00')]) +
  geom_line(aes(y = as.character(st_clustID), x = datetime_, group = as.character(ID), color = as.character(ID)), alpha = 0.3, size = 2)

























