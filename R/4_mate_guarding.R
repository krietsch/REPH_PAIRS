#========================================================================================================================
# Quantify mate guarding in pairs
#========================================================================================================================

# Summary
# 1. Assign locations in the study area
# 2. Distance between partners

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'foreach', 'doParallel', 'tdbscan', 
          'auksRuak', 'sf', 'lwgeom', 'windR'), require, character.only = TRUE)

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
dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

DBI::dbDisconnect(con)

# exclude missing values
d = d[!is.na(lon)]
dn = dn[!is.na(lon)]

# change projection
st_transform_DT(d)
st_transform_DT(dn)

#------------------------------------------------------------------------------------------------------------------------
# 1. Assign locations in the study area
#------------------------------------------------------------------------------------------------------------------------

# points in study site
point_over_poly_DT(d, poly = study_site, buffer = 100)
setnames(d, 'poly_overlap', 'study_site')

#------------------------------------------------------------------------------------------------------------------------
# 2. Distance between partners
#------------------------------------------------------------------------------------------------------------------------

# calculate distance between points
d[, datetime_2  := data.table::shift(datetime_, type = 'lead'), by = ID]
d[, TbtwPoints  := as.numeric(difftime(datetime_2, datetime_, units = 'sec'))]

d[, lon2 := data.table::shift(lon, type = 'lead'), by = ID]
d[, lat2 := data.table::shift(lat, type = 'lead'), by = ID]
d[, dist   := sqrt(sum((c(lon, lat) - c(lon2, lat2))^2)) , by = 1:nrow(d)]




# subset pair
nestID   = 'R909_18'
init_d   = '2018-06-27'
maleID   = 270170763 # M-W / O,W-W  ### including egglaying
femaleID = 270170764 # M-W / DB,R-W

# simple plot
ds = d[ID == maleID | ID == femaleID]

# create base map
bm = create_bm(ds, buffer = 6000, sc_dist = 10)


p = 
  bm + 
  geom_path(data = ds[ID == femaleID], aes(x = lon, y = lat), color = 'firebrick3', lineend = "round", size = 1) +
  geom_path(data = ds[ID == maleID], aes(x = lon, y = lat), color = 'dodgerblue3', lineend = "round", size = 1) +
  geom_point(data = ds[ID == femaleID], aes(x = lon, y = lat), color = 'firebrick3', size = 2) +
  geom_point(data = ds[ID == maleID], aes(x = lon, y = lat), color = 'dodgerblue3', size = 2) 
p

# png(paste0('./REPORTS/Pair_long_flight.png'), width = 700, height = 700)
# p
# dev.off()



nestID   = 'R304_18'
init_d   = as.POSIXct('2018-06-24 13:25:00') # first egg
init_d2  = as.POSIXct('2018-06-25 17:10:00') # second egg
init_d3  = as.POSIXct('2018-06-26 18:35:00') # second egg
init_d4  = as.POSIXct('2018-06-27 19:10:00') # second egg
maleID   = 270170746 # M-W / Y,O-O ### including egglaying
femaleID = 270170747 # M-W / O-O,R


# subset pair
dm = d[ID == maleID] # M-W / 
df = d[ID == femaleID]
n = dn[nestID == nestID]

datetimem = dm$datetime_

df[, closest_dtm := closestDatetime(datetime_, datetimem), by = datetime_]
df[, TbtwPoints_m_f := as.numeric(difftime(datetime_, closest_dtm, units = 'min'))]
df[, TbtwPoints_m_f := abs(TbtwPoints_m_f)]

plot(TbtwPoints_m_f ~ datetime_, df[TbtwPoints_m_f < 10])
df = df[TbtwPoints_m_f < 10]

dm[, datetime_m := datetime_]
dm[, lon_m := lon]
dm[, lat_m := lat]
dm[, dist_m := dist]

df_m = merge(df, dm[, .(datetime_m, lon_m, lat_m, dist_m)], by.x = 'closest_dtm', by.y = 'datetime_m', all.x = TRUE, allow.cartesian = TRUE)

df_m[, distf_m := sqrt(sum((c(lon, lat) - c(lon_m, lat_m))^2)) , by = 1:nrow(df_m)]

# d = read.table('example.csv', sep = ';', header = TRUE) %>% as.data.table
# d[, ID := 1]
# d[, datetime_ := t]
# df_m = d


df_m[, m1_2 := (lon_m - lon) / (lat_m - lat)]
df_m[, q1_2 := lon - m1_2 * lat]
df_m[, xM := (lat + lat_m) / 2]
df_m[, yM := (lon + lon_m) / 2]
df_m[, m_asse := -1 / m1_2]
df_m[, m_asse_lag := shift(m_asse, type = 'lag')]
df_m[, q_asse := yM - m_asse * xM]
df_m[, q_asse_lag := shift(q_asse, type = 'lag')]
df_m[, point_id := seq_len(.N), by = ID]
df_m[point_id == 1, xp := xM]
df_m[point_id == 1, yp := yM]
df_m[point_id != 1, xp := (q_asse_lag - q1_2) / abs(-m_asse_lag + m1_2)]
df_m[point_id != 1, yp := (m1_2 * q_asse_lag - m_asse_lag * q1_2) / abs(-m_asse_lag + m1_2)]
df_m[, d1_2 := ((lat - lat_m)^2 + (lon - lon_m)^2)^0.5]
df_m[point_id == 1, a := d1_2 / 2]
df_m[point_id == 1, b := d1_2 / 2]
df_m[point_id != 1, a := ((lat - xp)^2 + (lon - yp)^2)^0.5]
df_m[point_id != 1, b := ((lat_m - xp)^2 + (lon_m - yp)^2)^0.5]
df_m[, f1 := d1_2 * a / (a + b)] # d1-2*a/(a+b)
df_m[, f1_ := -d1_2 * b / (a + b)] # '-d1-2*b/(a+b)

ggplot(df_m) +
  geom_line(aes(x = datetime_, f1), col = 'red') +
  geom_line(aes(x = datetime_, f1_)) +
  geom_line(aes(datetime_, dist), color = 'firebrick3', alpha = 0.5) +
  geom_line(aes(datetime_, -dist_m), color = 'dodgerblue3', alpha = 0.5)


ggplot(df_m[datetime_ < min(datetime_, na.rm = TRUE) + 60*60*24]) +
  geom_line(aes(x = datetime_, f1), col = 'red') +
  geom_line(aes(x = datetime_, f1_)) +
  geom_line(aes(datetime_, dist), color = 'firebrick3', alpha = 0.5) +
  geom_line(aes(datetime_, -dist_m), color = 'dodgerblue3', alpha = 0.5)

# simple plot
ds = d[ID == maleID | ID == femaleID]
bm = create_bm(ds, 'lat', 'lon', buffer = 10, sc_dist = 10)

p = 
  bm + 
  geom_path(data = ds[ID == femaleID & datetime_ < min(datetime_, na.rm = TRUE) + 60*60*10], aes(x = lon, y = lat), color = 'firebrick3', lineend = "round", size = 1) +
  geom_path(data = ds[ID == maleID & datetime_ < min(datetime_, na.rm = TRUE) + 60*60*10], aes(x = lon, y = lat), color = 'dodgerblue3', lineend = "round", size = 1) +
  geom_point(data = ds[ID == femaleID & datetime_ < min(datetime_, na.rm = TRUE) + 60*60*10], aes(x = lon, y = lat), color = 'firebrick3', size = 2) +
  geom_point(data = ds[ID == maleID & datetime_ < min(datetime_, na.rm = TRUE) + 60*60*10], aes(x = lon, y = lat), color = 'dodgerblue3', size = 2) 

p



#-------------------------------------------------------------------------------------------------------------------------------------
# Overlap with nest position
#-------------------------------------------------------------------------------------------------------------------------------------

# create spatial nest point
nst = st_point(c(dn[1]$lon, dn[1]$lat)) %>% 
  st_buffer(dist = 10) %>% 
  st_geometry %>% 
  st_set_crs(st_crs(PROJ))

# create file with female points and overlap with nest
dsp = st_as_sf(df_m, coords = c('lon', 'lat')) %>% 
  st_geometry %>% 
  st_set_crs(st_crs(PROJ))

df_m[, nst_f := st_intersects(dsp, nst, sparse = FALSE)]

# create file with male points and overlap with nest
dsp = st_as_sf(df_m, coords = c('lon_m', 'lat_m')) %>% 
  st_geometry %>% 
  st_set_crs(st_crs(PROJ))

df_m[, nst_m := st_intersects(dsp, nst, sparse = FALSE)]

# translate in numbers
df_m[nst_f == TRUE, nf := - 100]
df_m[nst_m == TRUE, nm := - 150]

# ggplot(data = df_m) +
#   geom_point(aes(datetime_, nf)) +
#   geom_point(aes(datetime_, nm)) 

df_m[distf_m < 30, together := - 50]



p = bm3 + 
  geom_path(data = df_m[distf_m < 4000], aes(x = lon, y = lat), color = 'red', lineend = "round") +
  geom_path(data = df_m[distf_m < 4000], aes(x = lon_m, y = lat_m), color = 'blue', alpha = 0.5, lineend = "round") +
  geom_point(data = dn, aes(longit, latit), color = 'green', size = 2) + 
  labs(title = paste0(nestID, '_', maleID, '_', femaleID))


# Themes
theme_classic_b = function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                            base_rect_size = base_size/22) 
{
  theme_grey(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey20"), 
          panel.grid = element_line(colour = NA), 
          panel.grid.minor = element_line(size = rel(0.5)), 
          strip.background = element_rect(fill = "grey85", colour = "grey20"), 
          legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)
}




p =
  ggplot(data = df_m) +
  geom_hline(aes(yintercept = 0), color = 'grey60') +
  geom_line(aes(datetime_, dist), color = 'firebrick3', alpha = 0.5) +
  geom_line(aes(datetime_, dist_m), color = 'dodgerblue3', alpha = 0.5) +
  geom_point(aes(datetime_, distf_m)) +
  geom_point(aes(datetime_, together), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(together + 5)), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(together + 10)), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(together + 15)), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(together - 20)), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(together - 10)), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(together - 15)), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(together - 20)), color = 'darkorange', size = 1, shape = 15) +
  geom_point(aes(datetime_, nf), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf + 5)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf + 10)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf + 15)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf + 20)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf - 5)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf - 10)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf - 15)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nf - 20)), color = 'firebrick3', size = 1, shape = 15) +
  geom_point(aes(datetime_, nm), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm + 5)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm + 10)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm + 15)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm + 20)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm - 5)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm - 10)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm - 15)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_point(aes(datetime_, c(nm - 20)), color = 'dodgerblue3', size = 1, shape = 15) +
  geom_vline(aes(xintercept = init_d), color = 'green4', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = init_d2), color = 'green4', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = init_d3), color = 'green4', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = init_d4), color = 'green4', size = 3, alpha = 0.3) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d" ) +
  scale_y_continuous('Distance female - male (km)', breaks = c(0, 1000, 2000), 
                     labels = c('0', '1', '2'),
                     limits = c(-200, 2050), expand = c(0, 0)) +
  xlab('Day (June)') +
  theme_classic_b(base_size = 24)

p










