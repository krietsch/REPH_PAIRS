#==============================================================================================================
# Initiation date and GPS data
#==============================================================================================================

# Summary


# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork', 'windR', 'tdbscan'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, found_datetime := as.POSIXct(found_datetime, tz = 'UTC')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

#--------------------------------------------------------------------------------------------------------------
#' # GPS tagged birds with nest
#--------------------------------------------------------------------------------------------------------------

# tagged breeders
dnb = rbind(dn[, .(year_, nestID, ID = male_id, sex = 'M', initiation, found_datetime, initial_clutch_size, clutch_size, 
                   lat_n = lat, lon_n = lon)],
            dn[, .(year_, nestID, ID = female_id, sex = 'F', initiation, found_datetime, initial_clutch_size, clutch_size, 
                   lat_n = lat, lon_n = lon)])

# unique IDs with tagging data
IDu = unique(d, by = c('ID', 'year_'))

dnb = merge(dnb, IDu[, .(year_, ID, gps_tag = TRUE)], by = c('ID', 'year_'), all.x = TRUE)
dnb = dnb[gps_tag == TRUE]

# merge with nest
d = d[ID %in% dnb$ID]
d = merge(d, dnb, by = c('ID', 'year_'), all = TRUE, allow.cartesian = TRUE)

#--------------------------------------------------------------------------------------------------------------
#' # Distance to nest
#--------------------------------------------------------------------------------------------------------------

# distance to nest 
d[, distance_nest := sqrt(sum((c(lon, lat) - c(lon_n, lat_n))^2)) , by = 1:nrow(d)]
d[, dist_n10 := distance_nest < 10]
d[, dist_n20 := distance_nest < 20]
d[, dist_n30 := distance_nest < 30]

# relative nest initiation date
d[, initiation_rel := difftime(datetime_, initiation, units = 'days') %>% as.numeric()]
d[, initiation_rel0 := round(initiation_rel, 0)]

# found incomplete
d[!is.na(initial_clutch_size), found_incomplete := initial_clutch_size < clutch_size]


ggplot(data = d[sex == 'M']) +
  geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()



ggplot(data = d[sex == 'F'& found_incomplete == TRUE]) +
  geom_tile(aes(initiation_rel, nestID, fill = dist_n20), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'grey70', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  theme_classic()


found_incomplete



