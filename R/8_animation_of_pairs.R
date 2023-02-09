#' ---
#' title: Animation
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
# Animation of pairs
#==============================================================================================================

# Summary

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

# dIDs = dID[nestID == 'R909_18'] # pair with longest flight

dIDs = dID[nestID == 'R317_19'] 

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


# add male and female symbol as legend
# library(showtext)
# female = intToUtf8(9792)
# male = intToUtf8(9794)
# 
# showtext_auto(enable = TRUE, record = TRUE)
# 
# ggplot() +
#   annotate("text", x = 1, y = 1, hjust = 1.5, label = female, size = 80, color = 'indianred3') +
#   annotate("text", x = 1, y = 1, hjust = 0, label = male, size = 80, color = 'steelblue4') +
#   theme_bw(base_family = "sans") +
#   theme_void()
# 
# ggsave('./DATA/FM_SYMBOL.png', plot = last_plot(), width = 500, height = 500, units = c('px'), dpi = 'print')
# 
# showtext_auto(enable = FALSE, record = TRUE)

# load as png
fm_symbol = image_read('./DATA/FM_SYMBOL.png')

fm_symbol = ggplot() +
  background_image(fm_symbol) + 
  coord_fixed() +
  theme_void()

# add egg image
reph_egg = image_read('./DATA/REPH_EGG.png')

reph_egg = ggplot() +
  background_image(reph_egg) + 
  coord_fixed() +
  theme_void()

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

# add egg animation
dI = data.table( datetime_ = seq(round(dIDs$initiation, '10 mins'), round(dIDs$initiation, '10 mins') + 
                                   60*60*24*dIDs$clutch_size, by = '10 mins') )
dI[, lat := dIDs$lat_n]
dI[, lon := dIDs$lon_n]

dI[datetime_ %between% c(dIDs$egg1, dIDs$egg2), egg := 1]
dI[datetime_ %between% c(dIDs$egg2, dIDs$egg3), egg := 2]
dI[datetime_ %between% c(dIDs$egg3, dIDs$egg4), egg := 3]
dI[datetime_ > dIDs$egg4, egg := 4]

dI[, s:= rev(sizeAlong( datetime_, head = 10, to = c(2.5, 20))), by = egg] # size




# subset for test
# ts = ts[900:1000, ]
# ts = ts[900:905, ]

# register cores
# registerDoFuture()
# plan(multisession)

setorder(dmf, datetime_)



# loop for all plots
foreach(i = 1:nrow(ts), .packages = c('scales', 'ggplot2', 'lubridate', 'stringr', 
                                      'data.table', 'windR', 'ggnewscale', 'patchwork') ) %dopar% {
                                        
  # subset data
  tmp_date = ts[i]$date   # current date
  ds = dmf[datetime_ %between% c(tmp_date - 60*60*24, tmp_date)]
  dsI = dI[datetime_ %between% c(tmp_date - 60*10, tmp_date)]
  
  # create alpha and size
  if (nrow(ds) > 0) ds[, a:= alphaAlong(datetime_, head = 30, skew = -2) ,     by = ID] # alpha
  if (nrow(ds) > 0) ds[, s:= sizeAlong( datetime_, head = 3, to = c(0.3, 0.7)) , by = ID] # size
  
  # nest dot
  if(tmp_date < dIDs$initiation){
    p = bm + geom_point(data = dIDs, aes(lon_n, lat_n), color = '#e3c099', size = 5)
  } else {
    p = bm + geom_point(data = dIDs, aes(lon_n, lat_n), color = '#c38452', size = 5)
  }

  
  p = p + 
    
    # egg laying animation
    geom_point(data = dsI, aes(lon, lat), color = '#c38452', size = dsI$s, shape = 21) + 
    
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
  
  # save images  
  ggsave(ts[i, path], plot = last_plot(), width = 1920, height = 1080, units = c('px'), dpi = 'print')
  
}




# make animation for one
wd = getwd()
setwd(tmp_path)
system("ffmpeg -framerate 8 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p PAIR_NEST.mov")
setwd(wd)

