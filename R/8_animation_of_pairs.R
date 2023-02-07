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
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

dp = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dd = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt',  sep = '\t', header = TRUE, nThread = 20) %>% data.table

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
#' # Select pairs
#--------------------------------------------------------------------------------------------------------------

# first and last interaction
dp[interaction == TRUE, first_int := min(datetime_1), by = nestID]
dp[, first_int := min(first_int, na.rm = TRUE), by = nestID]
dp[interaction == TRUE, last_int  := max(datetime_1), by = nestID]
dp[, last_int := min(last_int, na.rm = TRUE), by = nestID]




pairIDs = dp[nestID == 'R903_19', .(ID1, ID2)]|> unique()|> as.vector()


dd = d[ID %in% pairIDs]


dpID = rbind(dp[, .(ID = ID1, nestID, sex = 'M', datetime_ = datetime_1, interaction, first_int, last_int)], 
             dp[, .(ID = ID2, nestID, sex = 'F', datetime_ = datetime_2, interaction, first_int, last_int)])


# merge with d
dd = merge(dd, dpID[, .(ID, datetime_, nestID, sex, interaction, first_int, last_int)], 
          by = c('ID', 'datetime_'), all.x = TRUE)


dd[interaction == TRUE]


dd[, ID := as.character(ID)]

bm = create_colored_bm(dd[interaction == TRUE], lat = 'lat', lon = 'lon', buffer = 1000)

bm + 
  geom_point(data = dd[interaction == TRUE], aes(lon, lat, group = ID, colour = ID))






# subset period around interactions
dd[, first_int_ := min(first_int, na.rm = TRUE)]
dd[, last_int_  := max(last_int, na.rm = TRUE)]

# 3 hours before and after
dd = dd[datetime_ > first_int - 3*3600 & datetime_ < last_int - 3*3600]



#--------------------------------------------------------------------------------------------------------------
#' # Animation
#--------------------------------------------------------------------------------------------------------------


# Set path to folder where it creates the pictures
tmp_path = paste0('//ds/grpkempenaers/Hannes/temp/test')


# subset time series ----
ts = data.table( date = seq( dd[, (min(datetime_))],
                             dd[, (max(datetime_))], by = '10 mins') )
ts[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')   ]




ts = ts[900:1200, ]




# register cores
# registerDoFuture()
# plan(multisession)

setorder(dd, datetime_)



  # loop for all plots
  foreach(i = 1:nrow(ts), .packages = c('scales', 'ggplot2', 'lubridate', 'stringr', 
                                        'data.table', 'windR', 'ggnewscale', 'patchwork') ) %dopar% {
                                          
    # subset data
    tmp_date = ts[i]$date   # current date
    ds = dd[datetime_ %between% c(tmp_date - 60*60*3, tmp_date)]
    
    # create alpha and size
    if (nrow(ds) > 0) ds[, a:= alphaAlong(datetime_, head = 30, skew = -2) ,     by = ID] # alpha
    if (nrow(ds) > 0) ds[, s:= sizeAlong( datetime_, head = 1, to = c(0.7, 3)) , by = ID] # size
    
    p = bm + 
      
      # track
      geom_path(data = ds, aes(x = lon, y = lat, group = ID), color = 'grey', alpha = ds$a, linewidth = ds$s, lineend = "round") +
      
      # interaction
      geom_point(data = setkey(setDT(ds), ID)[, .SD[which.max(datetime_)], ID], aes(x = lon, y = lat, color = interaction), 
                 alpha = 0.2, size = 15, show.legend = FALSE) +
      scale_color_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = NA)) +
      
      # points
      ggnewscale::new_scale_color() +
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
    
    # # nest
    # if(tmp_date < dnIDs[, initiation]){
    #   p1 = p + geom_point(data = dnIDs, aes(lon_n, lat_n), color = 'grey50', stroke = 2, size = 5, shape = 21)
    # } else {
    #   p1 = p + geom_point(data = dnIDs, aes(lon_n, lat_n), color = 'black', stroke = 2, size = 5, shape = 21)
    # }
  
    
    
    ggsave(ts[i, path], plot = last_plot(), width = 177, height = 177, units = c('mm'), dpi = 'print')
    
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







