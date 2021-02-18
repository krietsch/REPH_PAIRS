#' ---
#' title: Filter GPS data using a speed and distance filter
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
# GPS raw data summary 
#==============================================================================================================

# Summary
# 1. Data available
# 2. Data until 
# 3. Data linked to nests

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/2_data_summary.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
d[, datetime_ := anytime(datetime_)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
dr = dbq(con, 'select * FROM RESIGHTINGS')
dr[, year_ := year(datetime_)]
dr = dr[year_ > 2017]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Data available
#--------------------------------------------------------------------------------------------------------------

# change projection
st_transform_DT(dr)

# assign locations in the study area 
point_over_poly_DT(dr, poly = study_site, buffer = 10)
setnames(dr, 'poly_overlap', 'study_site')

point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# date
dr[, date_ := as.Date(datetime_)]
dr[, datetime_ := as.POSIXct(datetime_)]
dr[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, date_ := as.Date(datetime_)]
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

# exclude NA and off plot
dr = dr[!is.na(ID)]
dr = dr[study_site == TRUE]

# rbind obs and gps data
ds = rbind(dr[, .(ID, year_, datetime_y, type = 'seen', study_site)], d[, .(ID, year_, datetime_y, type = 'tagged', study_site)])

# seperate in gps off plot
ds[, type2 := type]
ds[study_site == FALSE & type == 'tagged', type2 := 'tagged_outside']

# first and last seen
ds[, first_obs := min(datetime_y, na.rm = TRUE), by = .(year_, ID)]
ds[, last_obs  := max(datetime_y, na.rm = TRUE), by = .(year_, ID)]

# tenure time
ds[, tenure := as.numeric(difftime(last_obs, first_obs, units = 'days')), by = .(year_, ID)]

# at least 2 days
ds = ds[tenure > 1]

# factor order
ds[, ID := as.factor(ID)]
ds[, ID := factor(ID, levels = unique(ID[order(first_obs)]))]

setorder(ds, -type)


ggplot(data = ds[year_ == 2018]) +
  ggtitle('2018') +
  geom_point(aes(datetime_y, ID, color = type2), size = 0.7) +
  scale_x_datetime(date_breaks = "weeks", date_labels = "%d %b") +
  scale_color_manual(values = c('black', 'cadetblue3', 'tan1')) +
  theme_classic(base_size = 11) +
  xlab('Date') +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
         axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))

# ggsave('./OUTPUTS/FIGURES/ID_tenure_2018.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')

ggplot(data = ds[year_ == 2019]) +
  ggtitle('2019') +
  geom_point(aes(datetime_y, ID, color = type2), size = 0.7) +
  scale_x_datetime(date_breaks = "weeks", date_labels = "%d %b") +
  scale_color_manual(values = c('black', 'cadetblue3', 'tan1')) +
  theme_classic(base_size = 11) +
  xlab('Date') +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
        axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))

# ggsave('./OUTPUTS/FIGURES/ID_tenure_2019.tiff', plot = last_plot(),  width = 177, height = 250, units = c('mm'), dpi = 'print')


# tenure days
du = unique(ds, by = c('ID', 'year_'))

# merge with sex
du = merge(du, dg[, .(ID, sex)], by = 'ID', all.x = TRUE)

ggplot(data = du) +
  geom_density(aes(x = tenure, color = as.character(year_))) +
  facet_grid(.~sex) +
  scale_color_manual(values = c('dodgerblue2', 'firebrick3')) +
  theme_classic(base_size = 11)


# merge with nests
# subset data from tagged birds
dID = unique(d, by = c('year_', 'ID'))

dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]

dnID = rbind(dn[, .(year_, ID = female_id, nestID, initiation, sex = 'F')], dn[, .(year_, ID = male_id, nestID, initiation, sex = 'M')])

# subset nest with tagged birds
dnID = dnID[ID %in% dID$ID]


# merge with nests - will create duplicate ID's
da = merge(d, dnID, by = c('year_', 'ID'), all = TRUE, allow.cartesian = TRUE)

# remove NA
da = da[!is.na(tagID)]

# nest_ID_bird
da[, nestID_ID := paste0(nestID, '_', ID)]
da[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
da[, initiation := as.POSIXct(initiation)]
da[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

ggplot(data = da[year_ == 2018 & !is.na(nestID)]) +
  ggtitle('2018') +
  geom_point(aes(datetime_y, nestID_ID, color = sex), size = 0.7) +
  geom_point(aes(initiation_y, nestID_ID), size = 4, shape = 3) +
  scale_x_datetime(date_breaks = "weeks", date_labels = "%d %b") +
  scale_color_manual(values = c('darkorange', 'dodgerblue3')) +
  theme_classic(base_size = 11) +
  xlab('Date') +
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
        plot.title = element_text(hjust = 0.5))


ggplot(data = da[year_ == 2019 & !is.na(nestID)]) +
  ggtitle('2019') +
  geom_point(aes(datetime_y, nestID_ID, color = sex), size = 0.7) +
  geom_point(aes(initiation_y, nestID_ID), size = 4, shape = 3) +
  scale_x_datetime(date_breaks = "weeks", date_labels = "%d %b") +
  scale_color_manual(values = c('darkorange', 'dodgerblue3')) +
  theme_classic(base_size = 11) +
  xlab('Date') +
  theme(legend.position = c(0.9, 0.9), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
        plot.title = element_text(hjust = 0.5))





