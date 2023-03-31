#--------------------------------------------------------------------------------------------------------------
#' Animation for R220_19
#--------------------------------------------------------------------------------------------------------------

# Example of pair with longest flight together

# Load data
source('./R/8_animation_data.R')

# subset pair
dIDs = dID[nestID == 'R220_19'] 

# Set path to folder where it creates the pictures
tmp_path = paste0('//ds/grpkempenaers/Hannes/temp/PAIR_EXAMPLES/', dIDs$nestID)

# subset all data from this pair
dmf = d[nestID == dIDs[, nestID]]

# subset period around interactions
dmf[, first_int := min(first_int, na.rm = TRUE)]
dmf[, last_int  := max(last_int, na.rm = TRUE)]

# 3 hours before and 24 h after
dmf = dmf[datetime_ > first_int - 3*3600 & datetime_ < last_int + 24*3600]

# create base map
bm = create_colored_bm(dmf[interaction == TRUE], lat = 'lat', lon = 'lon', buffer = 250, sc_location = 'bl', 
                       sc_cex = 0.7, sc_height = unit(0.1, "cm"))

# plot all data
bm +
  geom_point(data = dmf[interaction == TRUE], aes(lon, lat, group = ID, colour = sex), show.legend = FALSE) +
  scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4'))


# subset time series
ts = data.table( date = seq( dmf[, (round(min(datetime_), '10 mins'))],
                             dmf[, (round(max(datetime_), '10 mins'))], 
                             by = '10 mins') )
ts[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')]


# both sub maps
dmf1 = dmf[interaction == TRUE & datetime_ < ts[184]$date]
dmf2 = dmf[interaction == TRUE]


bm1 = create_colored_bm(dmf1, lat = 'lat', lon = 'lon', buffer = 1000, sc_location = 'bl', 
                        sc_cex = 0.7, sc_height = unit(0.1, "cm"))

bm2 = create_colored_bm(dmf2, lat = 'lat', lon = 'lon', buffer = 2000, sc_location = 'bl', 
                        sc_cex = 0.7, sc_height = unit(0.1, "cm"))

bm1 +
  geom_point(data = dmf1, aes(lon, lat, group = ID, colour = sex), show.legend = FALSE) +
  scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4'))

bm2 +
  geom_point(data = dmf2, aes(lon, lat, group = ID, colour = sex), show.legend = FALSE) +
  scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4'))


require(sfext)

# get bounding box 
buffer1 = 1000
buffer2 = 2000

# before movement
st_d = st_as_sf(dmf1, coords = c('lon','lat'), crs = PROJ)
rs_extent = st_d %>% st_bbox(crs = PROJ) %>% st_as_sfc %>% st_buffer(buffer1) %>% st_bbox_ext(asp = '16:9', crs = PROJ) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb1 = st_bbox(rs_extent) %>% data.table

dm1 = data.table(x1 = bb1$.[1], x2 = bb1$.[3], y1 = bb1$.[2], y2 = bb1$.[4])

st_d = st_as_sf(dmf2, coords = c('lon','lat'), crs = PROJ)
rs_extent = st_d %>% st_bbox(crs = PROJ) %>% st_as_sfc %>% st_buffer(buffer2) %>% st_bbox_ext(asp = '16:9', crs = PROJ) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb2 = st_bbox(rs_extent) %>% data.table

dm2 = data.table(x1 = bb2$.[1], x2 = bb2$.[3], y1 = bb2$.[2], y2 = bb2$.[4])


# first zoom out 
zoom_start1  = ts[574]$date
zoom_length1 = 45 # number of 10 min intervals

# zoom back in 
zoom_start2  = ts[649]$date
zoom_length2 = 45 # number of 10 min intervals

# first zoom out
z1 = data.table(date = seq(zoom_start1, zoom_start1 + c(zoom_length1 - 1)*600 , by = '10 mins'),
                x1 = seq(dm1[1]$x1, dm2[1]$x1, length.out = zoom_length1),
                x2 = seq(dm1[1]$x2, dm2[1]$x2, length.out = zoom_length1),
                y1 = seq(dm1[1]$y1, dm2[1]$y1, length.out = zoom_length1),
                y2 = seq(dm1[1]$y2, dm2[1]$y2, length.out = zoom_length1))

# zoom back in 
z2 = data.table(date = seq(zoom_start2, zoom_start2 + c(zoom_length2 - 1)*600 , by = '10 mins'),
                x1 = seq(dm2[1]$x1, dm1[1]$x1, length.out = zoom_length1),
                x2 = seq(dm2[1]$x2, dm1[1]$x2, length.out = zoom_length1),
                y1 = seq(dm2[1]$y1, dm1[1]$y1, length.out = zoom_length1),
                y2 = seq(dm2[1]$y2, dm1[1]$y2, length.out = zoom_length1))

z = rbind(z1, z2)

# merge with ts
ts = merge(ts, z, by = 'date', all.x = TRUE)
anyDuplicated(ts$date)

# fill stationary periods

# stationary before first zoom
ts[date < z1[, min(date)], c('x1', 'x2', 'y1', 'y2') := dm1[, .(x1, x2, y1, y2)]]

# stationary after first zoom (zoomed out)
ts[date %between% c(z1[, max(date)], z2[, min(date)]), c('x1', 'x2', 'y1', 'y2') := dm2[, .(x1, x2, y1, y2)]]

# stationary after first zoom (zoomed back in)
ts[date > z2[, max(date)], c('x1', 'x2', 'y1', 'y2') := dm1[, .(x1, x2, y1, y2)]]

# check that all filled
ts[is.na(x1)]


# add egg animation
dI = data.table( datetime_ = seq(round(dIDs$initiation, '10 mins'), round(dIDs$initiation, '10 mins') + 
                                   60*60*24*dIDs$clutch_size, by = '10 mins') )
dI[, lat := dIDs$lat_n]
dI[, lon := dIDs$lon_n]

dI[datetime_ %between% c(dIDs$egg1, dIDs$egg2), egg := 1]
dI[datetime_ %between% c(dIDs$egg2, dIDs$egg3), egg := 2]
dI[datetime_ %between% c(dIDs$egg3, dIDs$egg4), egg := 3]
dI[datetime_ > dIDs$egg4, egg := 4]

dI[, s:= rev(sizeAlong( datetime_, head = 10, to = c(1.6, 20))), by = egg] # size


# add nest predation animation
dP = data.table( datetime_ = seq(round(dIDs$nest_state_date, '10 mins'), round(dIDs$nest_state_date, '10 mins') + 
                                   60*60*10, by = '10 mins') )

dP[, lat := dIDs$lat_n]
dP[, lon := dIDs$lon_n]

dP[, s:= rev(sizeAlong( datetime_, head = 10, to = c(1.6, 20)))] # size




# subset for test
# ts = ts[900:1150, ]
# ts = ts[900:905, ]

# register cores
# registerDoFuture()
# plan(multisession)

setorder(dmf, datetime_)



# loop for all plots
foreach(i = 1:nrow(ts), .packages = c('scales', 'ggplot2', 'lubridate', 'stringr', 
                                      'data.table', 'windR', 'ggnewscale', 'patchwork') ) %dopar% {
                                        
  # create base map
  bm = 
    ggplot() +
    geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
    geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
    geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
    geom_sf(data = osm_roads, color = 'grey70') +
    geom_sf(data = osm_buildings, color = 'grey30') +
    coord_sf(expand = FALSE, xlim = c(ts[i]$x1, ts[i]$x2), ylim = c(ts[i]$y1, ts[i]$y2)) +
    ggspatial::annotation_scale(aes(location = 'bl'), text_cex = 0.7, height = unit(0.1, "cm"),
                                pad_x = unit(0.25, "cm"), pad_y = unit(0.5, "cm")) +
    theme(panel.grid.major = element_line(colour = "transparent"),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.background = element_rect(fill = '#D7E7FF'),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.border = element_rect(fill = NA, colour = "black"),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))
  
  # subset data
  tmp_date = ts[i]$date   # current date
  ds = dmf[datetime_ %between% c(tmp_date - 60*60*24, tmp_date)]
  dsI = dI[datetime_ %between% c(tmp_date - 60*10, tmp_date)]
  dsP = dP[datetime_ %between% c(tmp_date - 60*10, tmp_date)]
  
  # create alpha and size
  if (nrow(ds) > 0) ds[, a:= alphaAlong(datetime_, head = 30, skew = -2) ,     by = ID] # alpha
  if (nrow(ds) > 0) ds[, s:= sizeAlong( datetime_, head = 3, to = c(0.3, 0.7)) , by = ID] # size
  
  # nest dot
  if(tmp_date < dIDs$initiation){
    p = bm + geom_point(data = dIDs, aes(lon_n, lat_n), color = '#e3c099', size = 1.6)
  } else {
    p = bm + geom_point(data = dIDs, aes(lon_n, lat_n), color = '#c38452', size = 1.6)
  }
  
  
  p = p + 
    
    # egg laying animation
    geom_point(data = dsI, aes(lon, lat), color = '#c38452', size = dsI$s, shape = 21) + 
    
    # nest predation
    geom_point(data = dsP, aes(lon, lat), color = 'indianred3', size = dsP$s, shape = 21) + 
    
    # track
    geom_path(data = ds, aes(x = lon, y = lat, group = ID, color = sex), alpha = ds$a, linewidth = ds$s, 
              lineend = "round", show.legend = FALSE) +
    scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4')) +
    
    ggnewscale::new_scale_color() +
    
    # interaction
    geom_point(data = setkey(setDT(ds), ID)[, .SD[which.max(datetime_)], ID], aes(x = lon, y = lat, color = interaction), 
               size = 2.3, stroke = 1.5, shape = 21, show.legend = FALSE) +
    scale_color_manual(values = c('TRUE' = alpha('green4', 0.2), 'FALSE' = 'transparent', 'NA' = NA)) +
    
    
    # points
    ggnewscale::new_scale_color() +
    geom_point(data = setkey(setDT(ds), ID)[, .SD[which.max(datetime_)], ID], aes(x = lon, y = lat, color = sex), 
               alpha = 1, size = 1.6, show.legend = FALSE) +
    scale_color_manual(values = c('F' = 'indianred3', 'M' = 'steelblue4')) +
    
    # datetime
    annotate('text', x = Inf, y = -Inf, hjust = 1,  vjust = -2, label = paste0(format(tmp_date, "%Y-%m-%d %H:%M  ")), size = 3) +
    
    # distance
    annotate('text', x = Inf, y = -Inf, hjust = 1,  vjust = -3.5, 
             label = paste0(setkey(setDT(ds), ID)[, .SD[which.max(datetime_)]]$distance_pair_label), size = 3) +
    
    # nest ID
    annotate('text', x = -Inf, y = Inf, hjust = 0,  vjust = 4, 
             label = paste0('  ', dIDs$nest), size = 3)
  
  
  # interaction bars
  p2 = 
    ggplot(data = dmf[datetime_ > tmp_date - 12*3600 & datetime_ < tmp_date + 12*3600]) +
    
    geom_tile(aes(datetime_, '', fill = interaction), width = 1200, show.legend = FALSE) +
    scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'white', 'NA' = 'grey50')) +
    geom_vline(aes(xintercept = tmp_date), color = 'black', linewidth = 2, alpha = 1) +
    xlab('') + ylab('') +
    scale_x_datetime(limits = c(tmp_date - 12*3600, tmp_date + 12*3600), expand = c(0, 0)) +
    theme_void() +
    theme(panel.background = element_rect(fill = 'grey70'), plot.margin = unit(rep(0, 4), "lines"))
  
  
  p3 = p + 
    inset_element(p2, left = 0, bottom = 0.97, right = 1, top = 1, on_top = TRUE) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 0, b = -3, l = -3, unit = "pt")))
  
  
  # add female male symbol
  p4 = p3 +
    inset_element(fm_symbol, left = 0, right = 0.077, bottom = 0.7, top = 1.01, on_top = TRUE) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 0, b = -3, l = -3, unit = "pt"))) 
  
  
  # add eggs
  
  # egg1 
  if (!is.na(dIDs$egg1) & tmp_date > dIDs$egg1) p4 + 
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.76, top = 0.81, on_top = TRUE) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 0, b = -3, l = -3, unit = "pt")))
  
  # egg2
  if (!is.na(dIDs$egg2) & tmp_date > dIDs$egg2) p4 + 
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.76, top = 0.81, on_top = TRUE) +
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.70, top = 0.75, on_top = TRUE) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 0, b = -3, l = -3, unit = "pt")))
  
  # egg3
  if (!is.na(dIDs$egg3) & tmp_date > dIDs$egg3) p4 + 
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.76, top = 0.81, on_top = TRUE) +
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.70, top = 0.75, on_top = TRUE) +
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.64, top = 0.69, on_top = TRUE) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 0, b = -3, l = -3, unit = "pt")))
  
  # egg3
  if (!is.na(dIDs$egg4) & tmp_date > dIDs$egg4) p4 + 
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.76, top = 0.81, on_top = TRUE) +
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.70, top = 0.75, on_top = TRUE) +
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.64, top = 0.69, on_top = TRUE) +
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.58, top = 0.63, on_top = TRUE) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 0, b = -3, l = -3, unit = "pt")))
  
  # nest failed
  if (tmp_date > dIDs$nest_state_date) p4 + 
    inset_element(reph_egg, left = 0, right = 0.07, bottom = 0.76, top = 0.81, on_top = TRUE) +
    inset_element(x_symbol, left = 0, right = 0.07, bottom = 0.76, top = 0.81, on_top = TRUE) +
    plot_annotation(theme = theme(plot.margin = margin(t = 0, r = 0, b = -3, l = -3, unit = "pt")))
  
  # save images  
  ggsave(ts[i, path], plot = last_plot(), width = 1920, height = 1080, units = c('px'), dpi = 'print')
  
}



# make animation for one
wd = getwd()
setwd(tmp_path)
system("ffmpeg -framerate 8 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p PAIR_NEST.mov")
setwd(wd)


