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
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'windR', 'ggnewscale', 'doFuture', 'patchwork'), 
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
dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

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
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# create directory for each of these breeding pairs
dnID[, directory := paste0('//ds/grpkempenaers/Hannes/temp/PAIRS_ANIMATION_EACH/', nestID)]
dnID[, dir.create(file.path(directory), showWarnings = FALSE), by = 1:nrow(dnID)]

# unique IDs
IDu = unique(c(dnID[,  male_id], dnID[,  female_id]))

# subset d
d = d[ID %in% IDu]

# merge with nest and initiation date
dnIDu = rbind(dnID[, .(year_, ID = female_id, nestID, initiation, sex = 'F')], 
              dnID[, .(year_, ID = male_id, nestID, initiation, sex = 'M')])

d = merge(d, dnIDu[, .(year_, ID, nestID, initiation, sex)], by = c('ID', 'year_'), all.x = TRUE, allow.cartesian = TRUE)
d = d[!is.na(nestID)]

#--------------------------------------------------------------------------------------------------------------
#' # Define interactions
#--------------------------------------------------------------------------------------------------------------

distance_threshold = 30

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'))

# shift positions
dp[, lat1_before := shift(lat1, type = 'lag'), by = nestID]
dp[, lon1_before := shift(lon1, type = 'lag'), by = nestID]
dp[, lat2_before := shift(lat2, type = 'lag'), by = nestID]
dp[, lon2_before := shift(lon2, type = 'lag'), by = nestID]

dp[, lat1_next := shift(lat1, type = 'lead'), by = nestID]
dp[, lon1_next := shift(lon1, type = 'lead'), by = nestID]
dp[, lat2_next := shift(lat2, type = 'lead'), by = nestID]
dp[, lon2_next := shift(lon2, type = 'lead'), by = nestID]

# distance to position before and after
dp[, distance1_before := sqrt(sum((c(lon1, lat1) - c(lon1_before, lat1_before))^2)) , by = 1:nrow(dp)]
dp[, distance1_next := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)) , by = 1:nrow(dp)]
dp[, distance2_before := sqrt(sum((c(lon2, lat2) - c(lon2_before, lat2_before))^2)) , by = 1:nrow(dp)]
dp[, distance2_next := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]

# interactions
dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]

# simple interactions
dp[, interaction_threshold := distance_pair < distance_threshold]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]

dp[, any_interaction_threshold := any(interaction_threshold == TRUE), by = .(nestID, bout)]
dp[any_interaction_threshold == FALSE, interaction := FALSE]

# interaction ID
dp[, int_id := seq_len(.N), by = nestID]

# first and last interaction
dp[interaction == TRUE, first_int := min(datetime_1), by = nestID]
dp[, first_int := min(first_int, na.rm = TRUE), by = nestID]
dp[interaction == TRUE, last_int  := max(datetime_1), by = nestID]
dp[, last_int := min(last_int, na.rm = TRUE), by = nestID]

dpID = rbind(dp[, .(ID = ID1, nestID, datetime_ = datetime_1, interaction, int_id, first_int, last_int, distance_pair)], 
             dp[, .(ID = ID2, nestID, datetime_ = datetime_2, interaction, int_id, first_int, last_int, distance_pair)])

dpID[, distance_pair := round(distance_pair, 0)]

# merge with d
d = merge(d, dpID[, .(ID, datetime_, nestID, interaction, int_id, first_int, last_int, distance_pair)], 
          by = c('ID', 'datetime_', 'nestID'), all.x = TRUE)

# subset period around interactions
d[, first_int := min(first_int, na.rm = TRUE), by = nestID]
d[, last_int  := max(last_int, na.rm = TRUE), by = nestID]

# 3 hours before and after
d = d[datetime_ > first_int - 3*3600 & datetime_ < last_int - 3*3600]

#--------------------------------------------------------------------------------------------------------------
#' # Animation
#--------------------------------------------------------------------------------------------------------------

# register cores
registerDoFuture()
plan(multiprocess)

setorder(dnID, nestID)

# loop for each nest
foreach(j = 1:nrow(dnID)) %do% {
  
  # subset pair 
  dnIDs = dnID[j, ]
  
  # subset data from this pair
  dmf = d[nestID == dnIDs[, nestID]]
  
  # create base map
  bm = create_bm(dnIDs, lat = 'lat_n', lon = 'lon_n', buffer = 1000)
  # bm
  
  # plot all
  # bm +
  #   geom_path(data = dmf, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) +
  #   geom_point(data = dmf, aes(lon, lat, color = sex), size = 0.5, show.legend = FALSE) +
  #   geom_point(data = dnIDs, aes(lon_n, lat_n), color = 'black', stroke = 2, size = 5, shape = 21) + 
  #   scale_color_manual(values = c('darkorange', 'dodgerblue3'))
  
  # Set path to folder where it creates the pictures
  tmp_path = dnIDs[, directory]
  
  # subset time series ----
  ts = data.table( date = seq( dmf[, (min(datetime_))],
                               dmf[, (max(datetime_))], by = '10 mins') )
  ts[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')   ]
  
  # ts = ts[900:1200, ]
  
  # loop for all plots
  foreach(i = 1:nrow(ts), .packages = c('scales', 'ggplot2', 'lubridate', 'stringr', 
                                        'data.table', 'windR', 'ggnewscale', 'patchwork') ) %dopar% {
                                          
  # subset data
  tmp_date = ts[i]$date   # current date
  ds = dmf[datetime_ %between% c(tmp_date - 60*60*3, tmp_date)]
  
  # create alpha and size
  if (nrow(ds) > 0) ds[, a:= alphaAlong(datetime_, head = 30, skew = -2) ,     by = ID] # alpha
  if (nrow(ds) > 0) ds[, s:= sizeAlong( datetime_, head = 1, to = c(0.7, 3)) , by = ID] # size
  
  p = bm + 
    
    # track
    geom_path(data = ds, aes(x = lon, y = lat, group = ID), color = 'grey', alpha = ds$a, size = ds$s, lineend = "round") +
    
    # interaction
    geom_point(data = setkey(setDT(ds), ID)[, .SD[which.max(datetime_)], ID], aes(x = lon, y = lat, color = interaction), 
               alpha = 0.2, size = 15, show.legend = FALSE) +
    scale_color_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = NA)) +
    
    # points
    new_scale_color() +
    geom_point(data = setkey(setDT(ds), ID)[, .SD[which.max(datetime_)], ID], aes(x = lon, y = lat, color = sex), 
               alpha = 0.1, size = 10, show.legend = FALSE) +
    geom_point(data = setkey(setDT(ds), ID)[, .SD[which.max(datetime_)], ID], aes(x = lon, y = lat, color = sex), 
               alpha = 0.8, size = 5, show.legend = FALSE) +
    scale_color_manual(values = c('F' = 'darkorange', 'M' = 'dodgerblue3')) +
    
    # datetime
    annotate('text', x = Inf, y = -Inf, hjust = 1.1,  vjust = -0.5, label = paste0(format(tmp_date, "%B %d %H:00")), size = 5) +
    
    # distance
    annotate('text', x = Inf, y = -Inf, hjust = 1.3,  vjust = -2, 
             label = paste0(setkey(setDT(ds), ID)[, .SD[which.max(datetime_)]]$distance_pair, ' m'), size = 5)
  
  p
  
  # nest
  if(tmp_date < dnIDs[, initiation]){
    p1 = p + geom_point(data = dnIDs, aes(lon_n, lat_n), color = 'grey50', stroke = 2, size = 5, shape = 21)
  } else {
    p1 = p + geom_point(data = dnIDs, aes(lon_n, lat_n), color = 'black', stroke = 2, size = 5, shape = 21)
  }
  
  
  
  
  # interaction bars
  p2 = 
    ggplot(data = dmf[datetime_ > tmp_date - 12*3600 & datetime_ < tmp_date + 12*3600]) +
    
    geom_tile(aes(datetime_, '', fill = interaction), width = 600, show.legend = FALSE) +
    scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
    # geom_point(aes(datetime_, '', color = interaction), shape = '|', size = 10, show.legend = FALSE) +
    geom_vline(aes(xintercept = tmp_date), color = 'black', size = 3, alpha = 0.5) +
    xlab('') + ylab('') +
    scale_x_datetime(limits = c(tmp_date - 12*3600, tmp_date + 12*3600), expand = c(0, 0)) +
    theme_void()
  
  
  p1 + inset_element(p2, left = 0, bottom = 0.95, right = 1, top = 1, align_to = 'full')
  
  
  ggsave(ts[i, path], plot = last_plot(), width = 177, height = 177, units = c('mm'), dpi = 'print')
  
  }
}





# make animation for one
wd = getwd()
setwd(tmp_path)
system("ffmpeg -framerate 8 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p PAIR_NEST.mov")
setwd(wd)

# make animation for all
wd = getwd()

foreach(i = 1:nrow(dnID)) %dopar% {
  
  setwd(dnID[i, directory])
  system("ffmpeg -framerate 8 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p PAIR_NEST.mov")
  
}

setwd(wd)



# rename move in one folder
dnID[, old_directory := paste0(directory, '/PAIR_NEST.mov')]
dnID[, new_directory := paste0('//ds/grpkempenaers/Hannes/temp/PAIRS_ANIMATION_EACH_ALL/', nestID, '_PAIRS.mov')]


foreach(i = 1:nrow(dnID)) %do%
  
  file.copy(from = dnID[i, old_directory], to = dnID[i, new_directory], overwrite = FALSE, recursive = FALSE,
            copy.mode = TRUE, copy.date = FALSE)







