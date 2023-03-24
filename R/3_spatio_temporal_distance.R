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
#' Data and code from "Mutual mate guarding and limited sexual conflict in a sex-role reversed shorebird"
#' Contributor: Johannes Krietsch
#' 📍 This script runs relative to the project's root directory and contains all steps to get from the data to
#' the presented results and figures presented in this study.  
#' The order follows the appearance in the manuscript (as much as possible).  
#' Data were extracted from our database (see script) and are in the DATA folder.  
#' Outputs are written to OUTPUTS in the FIGURES or TABLES folder.  
#' Each section in the summary below can be run independently.  
#==============================================================================================================



#==============================================================================================================
# Calculate spatio-temporal distance of points
#==============================================================================================================

# Summary
# 1. Apply speed filter 
# 2. Apply distance filter 
# 3. Check altitudes

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'auksRuak', 'sf','foreach', 'doFuture', 'knitr'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
d = d[filtered == TRUE]
d[, datetime_ := as.POSIXct(as.character(datetime_), tz = 'UTC')]
st_transform_DT(d)

#--------------------------------------------------------------------------------------------------------------
#' # Subset ID's with overlapping data
#--------------------------------------------------------------------------------------------------------------

# start and end of the data
d[, ID_year := paste0(ID, '_', year_)]
d[, start := min(datetime_), by = ID_year]
d[, end   := max(datetime_), by = ID_year]
dID = unique(d[, .(ID_year, year_, ID, sex, start, end)], by = 'ID_year')

# all pairwise combinations
dpu = CJ(ID_year1 = dID[, ID_year], ID_year2 = dID[, ID_year], unique = TRUE)

# merge with start and end
dpu = merge(dpu, dID[, .(ID_year1 = ID_year, year_1 = year_, ID1 = ID, sex1 = sex, start1 = start, end1 = end)], 
            by = 'ID_year1')
dpu = merge(dpu, dID[, .(ID_year2 = ID_year, year_2 = year_, ID2 = ID, sex2 = sex, start2 = start, end2 = end)], 
            by = 'ID_year2')

# overlapping intervals
dpu[, overlap := DescTools::Overlap(c(start1, end1), c(start2, end2)), by = 1:nrow(dpu)]
dpu = dpu[overlap > 0] # exclude non-overlapping data
dpu = dpu[ID_year1 != ID_year2] # exclude within-individual data

#--------------------------------------------------------------------------------------------------------------
#' # Distance to all closest positions
#--------------------------------------------------------------------------------------------------------------

# register cores
registerDoFuture()
plan(multisession)

# loop for all pairwise closest data
dp = foreach(i = 1:nrow(dpu), .combine = 'rbind', .packages = c('data.table')) %dopar% {

  # subset pair
  dpus = dpu[i, ]
  
  d1 = d[year_ == dpus[1, year_1]  & ID == dpus[1, ID1], 
         .(ID1 = ID, sex1 = sex, datetime_ = datetime_, lat1 = lat, lon1 = lon, gps_speed1 = gps_speed, 
           altitude1 = altitude)]
  d2 = d[year_ == dpus[1, year_2]  & ID == dpus[1, ID2], 
         .(ID2 = ID, sex2 = sex, datetime_ = datetime_, lat2 = lat, lon2 = lon, gps_speed2 = gps_speed, 
           altitude2 = altitude)]
  
  setkeyv(d1, 'datetime_')
  setkeyv(d2, 'datetime_')
  
  # join with closest date
  dt = d2[, datetime_2 := (datetime_)][d1, roll = 'nearest'] 
  dt = dt[, .(ID1, ID2, datetime_1 = datetime_, datetime_2)]
  dt[, time_btw := abs(as.numeric(difftime(datetime_1, datetime_2, units = 'min')))]
  
  # subset closest position in time
  dt[, time_btw_min := min(time_btw), by = .(ID1, datetime_2)]
  dt = dt[time_btw == time_btw_min]
  dt[, time_btw_min := NULL]
  
  # merge with other data
  dt = merge(dt, d1[, .(datetime_1 = datetime_, sex1, lat1, lon1, gps_speed1, altitude1)], 
             by = 'datetime_1', all.x = TRUE)
  dt = merge(dt, d2[, .(datetime_2 = datetime_, sex2, lat2, lon2, gps_speed2, altitude2)], 
             by = 'datetime_2', all.x = TRUE)

  # calculate distance
  dt[, distance_pair := sqrt(sum((c(lon1, lat1) - c(lon2, lat2))^2)) , by = 1:nrow(dt)]
  
  dt
  
}

# remove rows with time difference bigger 10 min
dp = dp[time_btw < 10]

# set colour order
setcolorder(dp, c('ID1', 'ID2', 'sex1', 'sex2', 'datetime_1', 'datetime_2', 'time_btw', 'lat1', 'lon1', 'lat2', 'lon2', 
                  'distance_pair', 'gps_speed1', 'gps_speed2', 'altitude1', 'altitude2'))

# check data
ggplot(data = dp) +
  geom_histogram(aes(time_btw))

# quantiles
quantile(dp$time_btw, probs = c(5, 50, 95)/100)

# any duplicates?
anyDuplicated(dp, by = c('ID1', 'ID2', 'datetime_1'))
anyDuplicated(dp, by = c('ID1', 'ID2', 'datetime_2')) # okay

# save data
fwrite(dp, './DATA/PAIR_WISE_DIST_CLOSEST.txt', quote = TRUE, sep = '\t', row.names = FALSE)


# version information
sessionInfo()
