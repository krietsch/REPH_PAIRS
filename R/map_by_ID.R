#==============================================================================================================
# Mapy by ID
#==============================================================================================================

# Summary
# 1. Data available
# 2. Data until 
# 3. Data linked to nests

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/2_data_summary.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
d[, datetime_ := anytime(datetime_)]
d[, year_ := year(datetime_)]
d[, yID := paste0(substr(year_, 3,4), '_', ID)]
d[, ID := as.character(ID)]

# merge with sex
d = merge(d, dg[, .(ID, sex)], all.x = TRUE)

#--------------------------------------------------------------------------------------------------------------
#' # Map by ID
#--------------------------------------------------------------------------------------------------------------

# register cores
# require(doFuture)
# registerDoFuture()
# plan(multiprocess)

# loop for all
foreach(i = d[, yID] %>% unique, .packages = c('data.table', 'ggplot2')) %dopar%{
  
  ds = d[yID == i]
  sex = ds[1, sex]
  bm = create_bm(ds, buffer = 1000)
  
  bm + 
    ggtitle(paste0(ds[1 , yID], '_', sex)) +
    geom_path(data = ds, aes(lon, lat, group = ID), size = 0.7, color = 'grey', alpha = 0.5) + 
    geom_point(data = ds, aes(lon, lat, color = datetime_), size = 1) +
    scale_color_viridis( trans = scales::time_trans())
  
  ggsave(paste0('./OUTPUTS/MAP_BY_ID/', i, '.png'), plot = last_plot(),
         width = 177, height = 177, units = c('mm'), dpi = 'print')
  
}


#--------------------------------------------------------------------------------------------------------------
#' # kml by ID
#--------------------------------------------------------------------------------------------------------------

# change projection to lon lat
st_transform_DT(d, from = PROJ, to = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# transform data in table to create kml
d_plot = function(x){
  ds = data.table(id = x$yID,
                  datetime_ = x$datetime_,
                  lat = x$lat,
                  lon = x$lon)
  ds
  
}

# register cores
# require(doFuture)
# registerDoFuture()
# plan(multiprocess)

# loop for all
foreach(i = d[, yID] %>% unique, .packages = c('data.table', 'sdbvis')) %dopar%{
  
  sex = d[yID == i, sex] %>% unique
  
  kml(dat = d_plot(d[yID == i]), 
      file = paste0('./OUTPUTS/KML_BY_ID/', i, '_', sex,'.kml'), scale = 0.5)
  
}
  










