#==============================================================================================================
# Minimum convex polygon
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'windR', 'ggnewscale', 'doFuture', 'patchwork','tdbscan'), 
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
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

# merge with sex
dg[, ID := as.numeric(ID)]
dg = dg[!is.na(ID)]

d = merge(d, dg[, .(ID, sex)], by = 'ID', all.x = TRUE)

# merge with nest data
dID = rbind(dn[, .(year_, ID = male_id, breeder = TRUE, initiation)], dn[, .(year_, ID = female_id, breeder = TRUE, initiation)])
dID[, first_initiation := min(initiation), by = .(year_, ID)]
dIDu = unique(dID, by = c('year_', 'ID'))

d = merge(d, dIDu[, .(year_, ID, breeder, first_initiation)], by = c('year_', 'ID'), all.x = TRUE)
d[is.na(breeder), breeder := FALSE]

#--------------------------------------------------------------------------------------------------------------
#' # Calculate minimum convex polygon
#--------------------------------------------------------------------------------------------------------------

# unique ID by year
d[, ID_year := paste0(ID, '_', substr(year_, 3, 4))]

d_sf = st_as_sf(d, coords = c('lon', 'lat'), crs = PROJ)


# convex hull for each ID
d_ch = d_sf %>% dplyr::group_by(ID_year) %>% dplyr::summarise() %>%  st_convex_hull()


plot(d_ch)

du = unique(d, by = 'ID_year')


du[, area := st_area(d_ch) %>% as.numeric /1000]



ggplot(data = du) +
  geom_boxplot(aes(as.character(year_), area)) +
  theme_classic()


ggplot(data = du[area < 100000]) +
  geom_boxplot(aes(as.character(sex), area/1000)) +
  theme_classic()

















