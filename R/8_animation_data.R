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
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'windR', 'ggnewscale', 'doFuture', 'patchwork', 'magick', 'ggpubr'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
dID = fread('./DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', sep = '\t', header = TRUE) %>% data.table

d = fread('./DATA/NANO_TAGS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
d = d[filtered == TRUE]
st_transform_DT(d)

dp = fread('./DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

dn = fread('./DATA/NESTS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
st_transform_DT(dn) # change projection
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), 
                                format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, nest := substr(nestID, 1, nchar(nestID)-3)]


# add male and female symbol as legend
# library(showtext)
# female = intToUtf8(9792)
# male = intToUtf8(9794)
# 
# showtext_auto(enable = TRUE, record = TRUE)
# 
# ggplot() +
#   annotate("text", x = 1, y = 1, hjust = 1.5, label = female, size = 80, color = 'indianred3') +
#   annotate("text", x = 1, y = 1, hjust = 0, label = male, size = 80, color = 'steelblue4') +
#   theme_bw(base_family = "sans") +
#   theme_void()
# 
# ggsave('./DATA/FM_SYMBOL.png', plot = last_plot(), width = 500, height = 500, units = c('px'), dpi = 'print')
# 
# showtext_auto(enable = FALSE, record = TRUE)

# load as png
fm_symbol = image_read('./DATA/FM_SYMBOL.png')

fm_symbol = ggplot() +
  background_image(fm_symbol) + 
  coord_fixed() +
  theme_void()

# add egg image
reph_egg = image_read('./DATA/REPH_EGG.png')

reph_egg = ggplot() +
  background_image(reph_egg) + 
  coord_fixed() +
  theme_void() + theme(aspect.ratio = 144 / 109)

# cross for predation event

# ggplot() +
#   annotate("text", x = 1, y = 1, hjust = 0.57,  vjust = 0.4, label = 'x', size = 75, color = 'indianred3') +
#   theme_void()
# 
# ggsave('./DATA/X_SYMBOL.png', plot = last_plot(), width = 500, height = 500, units = c('px'), dpi = 'print')

x_symbol = image_read('./DATA/X_SYMBOL.png')

x_symbol = ggplot() +
  background_image(x_symbol) + 
  coord_fixed() +
  theme_void()


#--------------------------------------------------------------------------------------------------------------
#' Connect ID data with pairwise comparison and nest data
#--------------------------------------------------------------------------------------------------------------

# all pairs with overlap
du = unique(dp[, .(pairID, year_, ID1, ID2, sex1, sex2, nestID, initiation, initiation_rel)], by = 'nestID')

# merge with nest location
dID = merge(du, dn[, .(nestID, nest, lat_n = lat, lon_n = lon, clutch_size, egg1, egg2, egg3, egg4, 
                       nest_state_date)], by = 'nestID', all.x = TRUE)

# merge d with defined interactions

# first and last interaction
dp[interaction == TRUE, first_int := min(datetime_1), by = nestID]
dp[, first_int := min(first_int, na.rm = TRUE), by = nestID]
dp[interaction == TRUE, last_int  := max(datetime_1), by = nestID]
dp[, last_int := min(last_int, na.rm = TRUE), by = nestID]

# reshape for merge
dpID = rbind(dp[, .(ID = ID1, sex = sex1, nestID, datetime_ = datetime_1, interaction, first_int, last_int, distance_pair)], 
             dp[, .(ID = ID2, sex = sex2, nestID, datetime_ = datetime_2, interaction, first_int, last_int, distance_pair)])
dpID[, distance_pair := round(distance_pair, 0)]

# merge with d
d = merge(d, dpID[, .(ID, datetime_, nestID, interaction, distance_pair, first_int, last_int)], 
          by = c('ID', 'datetime_'), all.x = TRUE)

# make ID character for plotting
d[, ID := as.character(ID)]

# distance label
d[!is.na(distance_pair), distance_pair_label := paste0(distance_pair, ' m  ')]

# plot all pairs to check
# bm = create_colored_bm(d[interaction == TRUE], lat = 'lat', lon = 'lon', buffer = 1000)
# bm + 
#   geom_point(data = d[interaction == TRUE], aes(lon, lat, group = ID, colour = ID), show.legend = FALSE)
