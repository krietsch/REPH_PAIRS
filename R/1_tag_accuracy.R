#' ---
#' title: Analyse the accuracy of the tags
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
#'  Analyse the accuracy of the tags
#==============================================================================================================

# Summary
# 1. Tag accuracy based on test data
# 2. Tag accuracy based on incubation data

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'ggplot2', 'knitr', 'patchwork', 'auksRuak', 'anytime'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/1_tag_accuracy.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#--------------------------------------------------------------------------------------------------------------
#' # Tag accuracy based on test data
#--------------------------------------------------------------------------------------------------------------

# Data
d = fread('./DATA/NANO_TAGS_TEST.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

# change projection
setnames(d, c('lat', 'lon'), c('lat1', 'lon1'))
st_transform_DT(d, lat = 'lat1', lon = 'lon1')
st_transform_DT(d, lat = 'lat_wp', lon = 'lon_wp')
setnames(d, c('lat1', 'lon1'), c('lat', 'lon'))

# summary
d %>% nrow
d[, min(datetime_)]
d[, max(datetime_)]

# calculate difference between WP and Nanotag
d[, dist := sqrt(sum((c(lat, lon) - c(lat_wp, lon_wp))^2)) , by = 1:nrow(d)]

# exclude some data from tag 94 that had many totally wrong positions
d = d[dist < 500]

# median tag position
d[, lat_m := median(lat), by = tagID]
d[, lon_m := median(lon), by = tagID]

# calculate difference between WP and mean Nanotag position
d[, dist_m := sqrt(sum((c(lat_m, lon_m) - c(lat_wp, lon_wp))^2)) , by = 1:nrow(d)]

# calculate difference mean Nanotag position and each Nanotag position
d[, dist_each_m := sqrt(sum((c(lat, lon) - c(lat_m, lon_m))^2)) , by = 1:nrow(d)]
d1 = d[, .SD[1], by = tagID] # table with first point

# plot data 
median_ = median(d$dist_each_m) %>% round(., 1)
q95 = quantile(d$dist_each_m, probs = c(0.95)) %>% round(., 1)

# exclude distance over 50 m for plot
d[dist_each_m > 50] %>% nrow / d %>% nrow * 100
d[dist_each_m > 50] %>% nrow # N
d[, max(dist_each_m)]

pa = 
ggplot(data = d[dist_each_m < 50]) +
  geom_histogram(aes(dist_each_m), bins = 60, fill = 'grey85', color = 'grey50', linewidth = 0.1) +
  geom_vline(xintercept = median_, color = 'black', linetype = 'dashed') +
  geom_vline(xintercept = q95, color = 'black', linetype = 'dotted') +
  geom_text(aes(median_, Inf, label = paste0(median_, '.0 m')), vjust = 1, hjust = -0.1, 
            size = 3, color = 'black') +
  geom_text(aes(q95, Inf, label = paste0(q95, ' m')), vjust = 1, hjust = -0.1, 
            size = 3, color = 'black') +
  scale_x_continuous(limits = c(0, 50), expand = expansion(add = c(0, 0))) +
  scale_y_continuous(expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(plot.margin = unit(c(2, 4, 0, 2), 'pt'), axis.title.x = element_blank()) +
  ylab('Number of locations') 


pa

# all points on map
bm = create_bm(d, buffer = 10)
bm + 
  geom_point(data = d, aes(lon, lat), color = 'firebrick3', size = 0.8, alpha = 0.3) +
  geom_point(data = d, aes(lon_wp, lat_wp), color = 'dodgerblue4', size = 1)

# median vs. GPS waypoint 
ds = d1[, .(tagID, year_, lat_m, lon_m, lat_wp, lon_wp )]
bm = create_bm(ds, lat = 'lat_m', lon = 'lon_m', buffer = 5)
bm +
  geom_point(data = ds, aes(lon_m, lat_m), color = 'firebrick3', size = 2, alpha = 0.5) +
  geom_point(data = ds, aes(lon_wp, lat_wp), color = 'dodgerblue4', size = 2)

#--------------------------------------------------------------------------------------------------------------
#' # Tag accuracy based on incubation data
#--------------------------------------------------------------------------------------------------------------

# Data
d = fread('./DATA/NANO_TAGS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dn = fread('./DATA/NESTS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table


d[, datetime_ := as.POSIXct(as.character(datetime_))]
d = d[tagID == 92]
d = d[datetime_ > as.POSIXct('2018-06-20 12:22:00')]

# change projection
st_transform_DT(d)
dn = dn[!is.na(lon)]
st_transform_DT(dn)

# Incubation data
b = fread('./DATA/R203_2018_07_16_MSR323219_180625_155302.csv', skip = 27, header = FALSE, sep = ';')

b = data.table(datetime_  = as.POSIXct(as.character(b$V1)),
               t_surface = as.numeric(b$V2),
               t_nest    = as.numeric(b$V4))

# subset time on nest
b = b[datetime_ > as.POSIXct('2018-06-25 15:56:00') & datetime_ < as.POSIXct('2018-07-14 12:30:00')]

datetimes_inc = b$datetime_
d[, clostest_inc := closestDatetime(datetime_, datetimes_inc), by = 1:nrow(d)]

closestDatetime(datetime_ = d$datetime_[1000], datetimes = b$datetime_)

datetime_ = d$datetime_[1000]
datetimes = b$datetime_

cN = which(abs(datetimes - datetime_) == min(abs(datetimes - datetime_)))
cD = as.POSIXct(datetimes[cN])
cD[1]

b[t_nest > 30, inc_t := 1]
b[t_nest < 30, inc_t := 0]

d = merge(d, b[, .(datetime_, t_nest, inc_t)], by.x = 'clostest_inc', by.y = 'datetime_', all.x = TRUE)

# assign unknown (before MSR was installed)
d[datetime_ < as.POSIXct('2018-06-25 15:56:00') | datetime_ > as.POSIXct('2018-07-14 12:30:00'), inc_t := NA]

# subset nest
n = dn[nestID == 'R203_18']

# plot data 
bm = create_bm(d[!is.na(inc_t)], buffer = 10, sc_dist = 10)

bm +
  geom_point(data = d[!is.na(inc_t)], aes(lon, lat, color = factor(inc_t)), size = 0.2) +
  geom_point(data = n, aes(lon, lat), color = 'black', size = 2, alpha = 0.5) +
  scale_colour_manual(values = c('dodgerblue4', 'firebrick3'), labels = c('off nest', 'on nest'), 
                      name = c('T>30°C'))

# calculate distance to nest
n[, .(lon, lat)]
d[, dist := sqrt(sum((c(lon, lat) - c(-403.5346, -2076970))^2)) , by = 1:nrow(d)]

# plot distance
median_ = median(d[inc_t == 1]$dist) %>% round(., 1)
q95_ = quantile(d[inc_t == 1]$dist, probs = c(0.95)) %>% round(., 1)

# number of data 
d[inc_t == 1] |> nrow()

# exclude distance over 50 m for plot
d[inc_t == 1 & dist > 50] %>% nrow / d[inc_t == 1] %>% nrow * 100
d[inc_t == 1 & dist > 50] %>% nrow # N
d[inc_t == 1, max(dist)]


pb = 
  ggplot(data = d[inc_t == 1 & dist < 50]) +
  geom_histogram(aes(dist), bins = 60, fill = 'grey85', color = 'grey50', size = 0.1) +
  geom_vline(xintercept = median_, color = 'black', linetype = 'dashed') +
  geom_vline(xintercept = q95_, color = 'black', linetype = 'dotted') +
  geom_text(aes(median_, Inf, label = paste0(median_, '.0 m')), vjust = 1, hjust = -0.1, size = 3, 
            color = 'black') +
  geom_text(aes(q95_, Inf, label = paste0(q95_, ' m')), vjust = 1, hjust = -0.1, size = 3, 
            color = 'black') +
  scale_x_continuous(limits = c(0, 50), expand = expansion(add = c(0, 0))) +
  scale_y_continuous(expand = expansion(add = c(0, 0))) +
  theme_classic(base_size = 10) +
  theme(plot.margin = unit(c(2, 2, 0, 2), 'pt'), axis.title.x = element_blank()) +
  ylab('')

pb 

# all around nest on map
bm = create_bm(d[inc_t == 1 & dist < 50], buffer = 10)
bm + 
  geom_point(data = d[!is.na(inc_t)], aes(lon, lat, color = factor(inc_t)), size = 0.2) +
  geom_point(data = n, aes(lon, lat), color = 'black', size = 3) +
  scale_colour_manual(values = c('dodgerblue4', 'firebrick3'), labels = c('off nest', 'on nest'), 
                      name = c('T>30°C'))


# merge plots
library(grid)
library(gridExtra)

p <- pa + pb + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a')

gt = patchworkGrob(p)
g = arrangeGrob(gt, 
                bottom = textGrob('          Distance between telemetry logger fixes and precise location (m)', 
                                  gp = gpar(fontsize = 10)))


ggsave('./OUTPUTS/FIGURES/Tag_accuracy.tiff', plot = g,  width = 177, height = 89, units = c('mm'), 
       dpi = 'print')



# version information
sessionInfo()
