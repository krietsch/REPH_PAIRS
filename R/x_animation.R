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
# Animation
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 'stringr'), 
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

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Animation
#--------------------------------------------------------------------------------------------------------------

# subset year
d[, year_ := year(datetime_)]
d = d[year_ == 2019]

# merge with sex
d[, ID := as.character(ID)]
d = merge(d, dg[, .(ID, sex)], by = 'ID', all.x = TRUE)


# create table with two points
DT = data.table(name = c('NARL'),
                lat  = c(71.320854),
                lon  = c(-156.648210))

# change projection
st_transform_DT(DT)

# create base map
bm = create_bm(DT, buffer = 2500)
bm


# plot all
bm + 
  geom_path(data = d, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
  geom_point(data = d, aes(lon, lat, color = sex), size = 0.5) +
  scale_color_manual(values = c('firebrick2', 'dodgerblue3'))


# Set path to folder where it creates the pictures
tmp_path = paste0('//ds/grpkempenaers/Hannes/temp/all_reph')

# register cores
# cl = 50 %>% makePSOCKcluster; registerDoParallel(cl)

# subset time series ----
ts = data.table( date = seq( d[, (min(datetime_))] %>% as.Date %>% as.POSIXct,
                             d[, (max(datetime_))] %>% as.Date %>% as.POSIXct, by = '10 mins') )
ts = ts[date > d[, (min(datetime_) - 60*60*24)] ]
ts[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')   ]


# ts = ts[900:1200, ]


foreach(i = 1:nrow(ts), .packages = c('scales', 'ggplot2', 'lubridate', 'stringr', 
                                      'data.table', 'windR', 'gridGraphics', 'png') ) %dopar% {
                                  
  # subset data
  tmp_date = ts[i]$date   # current date
  ds = d[datetime_ <= tmp_date]
  ds = d[datetime_ %between% c(tmp_date - 60*60*24, tmp_date)]

  # create alpha and size
  if (nrow(ds) > 0) ds[, a:=  alphaAlong(datetime_, head = 30, skew = -2) ,     by = ID] # alpha
  if (nrow(ds) > 0) ds[, s:=  sizeAlong( datetime_, head = 1, to = c(0.7, 3)) , by = ID] # size
  
 
  p = 
    bm + 
    
    # track
    geom_path(data = ds, aes(x = lon, y = lat, group = ID), color = 'grey', alpha = ds$a, size = ds$s, lineend = "round") +

    # points
    geom_point(data = ds[datetime_ == min(max(datetime_))], aes(lon, lat, color = sex), size = 0.5) +
    scale_color_manual(values = c('firebrick2', 'dodgerblue3')) +
    
    # datetime
    annotate('text', x = c(583430.1 - 350), y = c(7914255 - 120), label = paste0(format(tmp_date, "%B %d %H:00")), size = 12)
  
  ggsave(ts[i, path], plot = last_plot(), width = 177, height = 177, units = c('mm'), dpi = 'print')
  
}

                                        
                                        
                                        
                                        
# stopCluster(cl)
# registerDoSEQ()



wd = getwd()
setwd(tmp_path)
system("ffmpeg -framerate 20 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p PAIR_NEST.mov")
setwd(wd)

















