#==============================================================================================================
# Cummulative distance each bird 
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
#' # Calculate cummulative distance
#--------------------------------------------------------------------------------------------------------------

setorder(d, year_, ID, datetime_)

# distance to the next position
d[, lat_next := shift(lat, type = 'lead'), by = .(year_, ID)]
d[, lon_next := shift(lon, type = 'lead'), by = .(year_, ID)]
d[, distance_next := sqrt(sum((c(lon, lat) - c(lon_next, lat_next))^2)), by = 1:nrow(d)]

# cumulative distance
d[, cum_distance := cumsum(distance_next), by = .(year_, ID)]
d[, cum_distance_max := max(cum_distance, na.rm = TRUE), by = .(year_, ID)]

# before initiation
d[datetime_ < first_initiation, initiation_cat := 'before']
d[datetime_ > first_initiation, initiation_cat := 'after']
d[is.na(initiation_cat), initiation_cat := 'none']

d[, initiation_rel := difftime(datetime_, first_initiation, units = 'days') %>% as.numeric()]

d[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]

ggplot(d) +
  geom_line(aes(datetime_y, cum_distance/1000, group = ID, color = sex)) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4')) +
  xlab('Date') + ylab('cumulative distance (km)') +
  facet_grid(year_~.) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/cum_dist_sex.png', plot = last_plot(),  width = 170, height = 250, units = c('mm'), dpi = 'print')




ggplot(d) +
  geom_line(aes(datetime_y, cum_distance/1000, group = ID, color = initiation_cat)) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue4', 'grey50')) +
  xlab('Date') + ylab('cumulative distance (km)') +
  facet_grid(year_~.) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/cum_dist_initiation.png', plot = last_plot(),  width = 170, height = 250, units = c('mm'), dpi = 'print')




ggplot(d[year_ == 2018]) +
  geom_line(aes(datetime_, cum_distance/1000, group = ID, color = breeder)) +
  theme_classic()


ggplot(d[year_ == 2018]) +
  geom_line(aes(datetime_, cum_distance/1000, group = ID, color = initiation_cat)) +
  theme_classic()


ggplot(d[year_ == 2019]) +
  geom_line(aes(datetime_, cum_distance/1000, group = ID, color = initiation_cat)) +
  theme_classic()

ggplot(d[year_ == 2018 & initiation_cat != 'none' & cum_distance_max > 150/1000]) +
  geom_line(aes(datetime_, cum_distance/1000, group = ID, color = initiation_cat)) +
  theme_classic()

ggplot(d[year_ == 2019 & initiation_cat != 'none' & cum_distance_max > 150/1000]) +
  geom_line(aes(datetime_, cum_distance/1000, group = ID, color = initiation_cat)) +
  theme_classic()

ggplot(d[year_ == 2019 & initiation_cat != 'none' & cum_distance_max > 150/1000]) +
  geom_line(aes(datetime_, cum_distance/1000, group = ID, color = sex)) +
  theme_classic()



ggplot(d[initiation_rel > -100]) +
  geom_line(aes(initiation_rel, cum_distance/1000, group = ID, color = as.character(year_))) +
  theme_classic()

ggplot(d[initiation_rel > -100]) +
  geom_line(aes(initiation_rel, cum_distance/1000, group = ID, color = sex)) +
  theme_classic()









