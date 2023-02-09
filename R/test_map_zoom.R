# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'windR', 'ggnewscale', 'doFuture', 'patchwork', 'magick', 'ggpubr'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, egg1 := as.POSIXct(egg1, tz = 'UTC')]
dn[, egg2 := as.POSIXct(egg2, tz = 'UTC')]
dn[, egg3 := as.POSIXct(egg3, tz = 'UTC')]
dn[, egg4 := as.POSIXct(egg4, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

#--------------------------------------------------------------------------------------------------------------
#' Connect ID data with pairwise comparison and nest data
#--------------------------------------------------------------------------------------------------------------

# all pairs with overlap
du = unique(dp[, .(pairID, year_, ID1, ID2, sex1, sex2, nestID, initiation, initiation_rel)], by = 'nestID')

# merge with nest location
dID = merge(du, dn[, .(nestID, nest, lat_n = lat, lon_n = lon, clutch_size, egg1, egg2, egg3, egg4)], by = 'nestID', all.x = TRUE)

# merge d with defined interactions

# first and last interaction
dp[interaction == TRUE, first_int := min(datetime_1), by = nestID]
dp[, first_int := min(first_int, na.rm = TRUE), by = nestID]
dp[interaction == TRUE, last_int  := max(datetime_1), by = nestID]
dp[, last_int := min(last_int, na.rm = TRUE), by = nestID]

# reshape for merge
dpID = rbind(dp[, .(ID = ID1, sex = sex1, nestID, datetime_ = datetime_1, interaction, first_int, last_int, distance_pair)], 
             dp[, .(ID = ID2, sex = sex2, nestID, datetime_ = datetime_2, interaction, first_int, last_int, distance_pair)])
dpID[, distance_pair := round(distance_pair, 0)]

# merge with d
d = merge(d, dpID[, .(ID, sex, datetime_, nestID, interaction, distance_pair, first_int, last_int)], 
          by = c('ID', 'datetime_'), all.x = TRUE)

# make ID character for plotting
d[, ID := as.character(ID)]

# distance label
d[!is.na(distance_pair), distance_pair_label := paste0(distance_pair, ' m  ')]

# plot all pairs to check
# bm = create_colored_bm(d[interaction == TRUE], lat = 'lat', lon = 'lon', buffer = 1000)
# bm + 
#   geom_point(data = d[interaction == TRUE], aes(lon, lat, group = ID, colour = ID), show.legend = FALSE)


#--------------------------------------------------------------------------------------------------------------
#' Animation for specific pair
#--------------------------------------------------------------------------------------------------------------

# create directory for each of these breeding pairs
dID[, directory := paste0('//ds/grpkempenaers/Hannes/temp/PAIRS_ANIMATION/', nestID)]
# dID[, dir.create(file.path(directory), showWarnings = FALSE), by = 1:nrow(dID)]

# subset pair
# dIDs = dID[nestID == 'R304_18'] # THE example

dIDs = dID[nestID == 'R909_18'] # pair with longest flight

# dIDs = dID[nestID == 'R317_19'] 

# subset all data from this pair
dmf = d[nestID == dIDs[, nestID]]

# subset period around interactions
dmf[, first_int := min(first_int, na.rm = TRUE)]
dmf[, last_int  := max(last_int, na.rm = TRUE)]

# 3 hours before and after
dmf = dmf[datetime_ > first_int - 3*3600 & datetime_ < last_int + 12*3600]

# create base map
bm = create_colored_bm(dmf[interaction == TRUE], lat = 'lat', lon = 'lon', buffer = 250, sc_location = 'bl', 
                       sc_cex = 0.7, sc_height = unit(0.1, "cm"))

# plot all data
bm +
  geom_point(data = dmf[interaction == TRUE], aes(lon, lat, group = ID, colour = sex), show.legend = FALSE) +
  scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4'))


# Set path to folder where it creates the pictures
tmp_path = dIDs$directory

# subset time series
ts = data.table( date = seq( dmf[, (round(min(datetime_), '10 mins'))],
                             dmf[, (round(max(datetime_), '10 mins'))], 
                             by = '10 mins') )
ts[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')]






# both sub maps
dmf1 = dmf[interaction == TRUE & datetime_ < ts[990]$date]
dmf2 = dmf[interaction == TRUE]


bm1 = create_colored_bm(dmf1, lat = 'lat', lon = 'lon', buffer = 250, sc_location = 'bl', 
                        sc_cex = 0.7, sc_height = unit(0.1, "cm"))

bm2 = create_colored_bm(dmf2, lat = 'lat', lon = 'lon', buffer = 250, sc_location = 'bl', 
                        sc_cex = 0.7, sc_height = unit(0.1, "cm"))

bm1 +
  geom_point(data = dmf1, aes(lon, lat, group = ID, colour = sex), show.legend = FALSE) +
  scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4'))

bm2 +
  geom_point(data = dmf2, aes(lon, lat, group = ID, colour = sex), show.legend = FALSE) +
  scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4'))


require(sfext)

# get bounding box 

# before movement
st_d = st_as_sf(dmf1, coords = c('lon','lat'), crs = PROJ)
rs_extent = st_d %>% st_bbox(crs = PROJ) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox_ext(asp = '16:9', crs = PROJ) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb1 = st_bbox(rs_extent) %>% data.table

dm1 = data.table(x1 = bb1$.[1], x2 = bb1$.[3], y1 = bb1$.[2], y2 = bb1$.[4])

st_d = st_as_sf(dmf2, coords = c('lon','lat'), crs = PROJ)
rs_extent = st_d %>% st_bbox(crs = PROJ) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox_ext(asp = '16:9', crs = PROJ) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb2 = st_bbox(rs_extent) %>% data.table

dm2 = data.table(x1 = bb2$.[1], x2 = bb2$.[3], y1 = bb2$.[2], y2 = bb2$.[4])


di = data.table(x1 = seq(dm1[1]$x1, dm2[1]$x1, length.out = 36),
                x2 = seq(dm1[1]$x2, dm2[1]$x2, length.out = 36),
                y1 = seq(dm1[1]$y1, dm2[1]$y1, length.out = 36),
                y2 = seq(dm1[1]$y2, dm2[1]$y2, length.out = 36))


tmp_path = '//ds/grpkempenaers/Hannes/temp/test'

di[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')]


foreach(i = 1:nrow(di), .packages = c('ggplot2', 'stringr', 'data.table', 'windR', 'ggnewscale', 'patchwork') ) %dopar% {

  
  # bm =
    ggplot() +
    geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
    geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
    geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
    geom_sf(data = osm_roads, color = 'grey70') +
    geom_sf(data = osm_buildings, color = 'grey30') +
    coord_sf(expand = FALSE, xlim = c(di[i]$x1, di[i]$x2), ylim = c(di[i]$y1, di[i]$y2)) +
    ggspatial::annotation_scale(aes(location = 'br'), text_cex = 0.7, height = unit(0.25, "cm"),
                                pad_x = unit(0.25, "cm"), pad_y = unit(0.5, "cm")) +
    theme(panel.grid.major = element_line(colour = "transparent"),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.background = element_rect(fill = '#D7E7FF'),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(fill = NA, colour = "black"),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))
  
  
  
  
  
  
  
  
  ggsave(di[i, path], plot = last_plot(), width = 1920, height = 1080, units = c('px'), dpi = 'print')
  
                         
                                        
}





tmp_path = '//ds/grpkempenaers/Hannes/temp/test'


# make animation for one
wd = getwd()
setwd(tmp_path)
system("ffmpeg -framerate 8 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p PAIR_NEST.mov")
setwd(wd)










                                        
bm =
  ggplot() +
  geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
  geom_sf(data = osm_roads, color = 'grey70') +
  geom_sf(data = osm_buildings, color = 'grey30') +
  coord_sf(expand = FALSE, xlim = c(bb$.[1], bb$.[3]), ylim = c(bb$.[2], bb$.[4])) +
  ggspatial::annotation_scale(aes(location = sc_location), text_cex = sc_cex, height = sc_height,
                              pad_x = sc_pad_x, pad_y = sc_pad_y) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.background = element_rect(fill = '#D7E7FF'),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))



180/10






# subset for test
ts = ts[990:1126, ]
# ts = ts[900:905, ]















ts = data.table( date = seq( dmf[, (round(min(datetime_), '10 mins'))],
                             dmf[, (round(max(datetime_), '10 mins'))], 
                             by = '10 mins') )
ts[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')]







st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox_ext(asp = '16:9', crs = projection) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb = st_bbox(rs_extent) %>% data.table



dm1 = data.table(x1 = bb$.[1], x2 = bb$.[2], y1 = bb$.[3], y2 = bb$.[4])


dm2 = data.table(x1 = bb2$.[1], x2 = bb2$.[2], y1 = bb2$.[3], y2 = bb2$.[4])

seq(dm1[1, x1], dm2[1, x1], by = 100)


bm =
  ggplot() +
  geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
  geom_sf(data = osm_roads, color = 'grey70') +
  geom_sf(data = osm_buildings, color = 'grey30') +
  coord_sf(expand = FALSE, xlim = c(bb$.[1], bb$.[3]), ylim = c(bb$.[2], bb$.[4])) +
  ggspatial::annotation_scale(aes(location = 'br'), text_cex = 2) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.background = element_rect(fill = '#D7E7FF'),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))



bm









buffer = 1000


DT = data.table(name = c('NARL', 'Utqiagvik'),
                lat  = c(71.320854, 71.290246),
                lon  = c(-156.648210, -156.788622))

# change projection
st_transform_DT(DT)

st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox_ext(asp = '16:9', crs = projection) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb2 = st_bbox(rs_extent) %>% data.table



bm =
  ggplot() +
  geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
  geom_sf(data = osm_roads, color = 'grey70') +
  geom_sf(data = osm_buildings, color = 'grey30') +
  coord_sf(expand = FALSE, xlim = c(bb2$.[1], bb2$.[3]), ylim = c(bb2$.[2], bb2$.[4])) +
  ggspatial::annotation_scale(aes(location = 'br'), text_cex = 2) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.background = element_rect(fill = '#D7E7FF'),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))



bm