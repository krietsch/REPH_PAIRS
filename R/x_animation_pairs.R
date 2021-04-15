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
          'stringr', 'windR'), 
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

# YEAR = 2018
YEAR = 2019

# nest data
dnID = rbind(dn[, .(year_, ID = female_id, nestID, initiation, sex = 'F')], 
             dn[, .(year_, ID = male_id, nestID, initiation, sex = 'M')])
dnID[, ID := factor(ID)]
dnID = unique(dnID[!is.na(ID)], by = c('year_', 'ID', 'nestID'))

setorder(dnID, initiation)
dnIDu = unique(dnID, by = c('year_', 'ID'))

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

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

#--------------------------------------------------------------------------------------------------------------
#' # Define interactions
#--------------------------------------------------------------------------------------------------------------

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'))

# interactions
dp[, interaction := distance_pair < 30]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]
dp[interaction == FALSE & bout_seq_max == 1, interaction := TRUE] 

#--------------------------------------------------------------------------------------------------------------
#' # Animation
#--------------------------------------------------------------------------------------------------------------

# subset year
d[, year_ := year(datetime_)]
d = d[year_ == YEAR]

# merge with sex
d[, ID := as.character(ID)]
d = merge(d, dg[, .(ID, sex)], by = 'ID', all.x = TRUE)

# merge with nest
dnIDu[, ID := as.character(ID)]
d = merge(d, dnIDu[, .(ID, nestID, year_)], by = c('ID', 'year_'), all.x = TRUE)

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
# bm + 
#   geom_path(data = d, aes(lon, lat, group = ID), size = 0.5, color = 'grey', alpha = 0.5) + 
#   geom_point(data = d, aes(lon, lat, color = sex), size = 0.5) +
#   scale_color_manual(values = c('firebrick2', 'dodgerblue3'))


# Set path to folder where it creates the pictures
tmp_path = paste0('//ds/grpkempenaers/Hannes/temp/all_reph/', YEAR)

# register cores
# require(doFuture)
# registerDoFuture()
# plan(multiprocess)

# subset time series ----
ts = data.table( date = seq( d[, (min(datetime_))] %>% as.Date %>% as.POSIXct(., tz = 'UTC'),
                             d[, (max(datetime_))] %>% as.Date %>% as.POSIXct(., tz = 'UTC'), by = '10 mins') )
ts = ts[date > d[, (min(datetime_) - 60*60*24)] ]
ts[, path := paste0(tmp_path, '/', str_pad(1:.N, 4, 'left', pad = '0'), '.png')   ]


# ts = ts[900:1200, ]


foreach(i = 1:nrow(ts), .packages = c('scales', 'ggplot2', 'lubridate', 'stringr', 
                                      'data.table', 'windR') ) %dopar% {
                                        
  # subset data
  tmp_date = ts[i]$date   # current date
  ds = d[datetime_ <= tmp_date]
  ds = d[datetime_ %between% c(tmp_date - 60*60*3, tmp_date)]
  
  # create alpha and size
  if (nrow(ds) > 0) ds[, a:=  alphaAlong(datetime_, head = 30, skew = -2) ,     by = ID] # alpha
  if (nrow(ds) > 0) ds[, s:=  sizeAlong( datetime_, head = 1, to = c(0.7, 3)) , by = ID] # size
  
  bm + 
    
    # track
    geom_path(data = ds, aes(x = lon, y = lat, group = ID), color = 'grey', alpha = ds$a, size = ds$s, lineend = "round") +
    
    # points
    geom_point(data = ds[ds[, .I[datetime_ == max(datetime_)], by = ID]$V1], aes(lon, lat, color = nestID, shape = sex), 
               size = 1.5, show.legend = FALSE) +
    scale_shape_manual(values = c('F' = 17, 'M' = 16)) +
    # scale_color_manual(values = c('F' = 'firebrick2', 'M' = 'dodgerblue3')) +
    
    # datetime
    annotate('text', x = Inf, y = -Inf, hjust = 1.1,  vjust = -0.5, label = paste0(format(tmp_date, "%B %d %H:00")), size = 5)
  
  ggsave(ts[i, path], plot = last_plot(), width = 177, height = 177, units = c('mm'), dpi = 'print')
  
}






# make animation
wd = getwd()
setwd(tmp_path)
system("ffmpeg -framerate 16 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p PAIR_NEST.mov")
setwd(wd)

















