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
# Data available
# Data until 
# Data linked to nests
# Proportion of individuals seen/tagged in the study site 
# GPS tagged breeders and non-breeders
# Summary by ID

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
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Data available
#--------------------------------------------------------------------------------------------------------------

# Number of tags attached on REPH
dc[!is.na(gps_tag)]$gps_tag %>% unique %>% length
dc[!is.na(gps_tag) & year_ == 2018]$gps_tag %>% unique %>% length
dc[!is.na(gps_tag) & year_ == 2019]$gps_tag %>% unique %>% length

# Number of REPH with tags
dc[!is.na(gps_tag)]$ID %>% unique %>% length
dc[!is.na(gps_tag) & year_ == 2018]$ID %>% unique %>% length
dc[!is.na(gps_tag) & year_ == 2019]$ID %>% unique %>% length

# ID's tagged in both years
ID_tagged_18 = dc[!is.na(gps_tag) & year_ == 2018]$ID %>% unique
ID_tagged_19 = dc[!is.na(gps_tag) & year_ == 2019]$ID %>% unique
ID_tagged_19[ID_tagged_19 %in% ID_tagged_18]

# Tags that never send data (while on bird)
d[, ID_tagID := paste0(ID, '_', tagID)]
dc[, ID_tagID := paste0(ID, '_', gps_tag)]

dc_ID_tagged = dc[!is.na(gps_tag)]$ID_tagID %>% unique
d_ID_tagged = d$ID_tagID %>% unique

d_no_data = dc_ID_tagged[!(dc_ID_tagged %in% d_ID_tagged)]
d_no_data %>% length
ds = dc[ID_tagID %in% d_no_data]

# include tags with no data with capture location
ds = ds[, .(year_, tagID = gps_tag, ID, datetime_ = anytime(released_time), lat, lon, ID_tagID)]
d = rbind(d, ds, fill = TRUE, use.names = TRUE)

# timing of tag attachment
setorder(dc, caught_time)
ds = dc[!is.na(gps_tag)] %>% unique(., by = c('ID', 'gps_tag'))
ds[, date_ := as.Date(caught_time)]
ds[, date_y := as.Date(paste0('2021-', substr(date_, 6, 10)))]

dss = ds[, .N, year_]

p1 = 
ggplot(data = ds[year_ == 2018]) +
  geom_bar(aes(date_y), fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_text(aes(min_date, Inf), label = paste0('N = ', dss[year_ == 2018, N]), hjust = 0.2, size = 3, vjust = 1) +
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0), breaks = c(0, 15, 30)) +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('Tags') +
  theme_classic(base_size = 11) +
  theme(axis.title.x = element_blank(), axis.text.x=element_blank())

p2 = 
ggplot(data = ds[year_ == 2019]) +
  geom_bar(aes(date_y), fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_text(aes(min_date, Inf), label = paste0('N = ', dss[year_ == 2019, N]), hjust = 0.2, size = 3, vjust = 1) +
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0), breaks = c(0, 15, 30)) +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('Tags') +
  theme_classic(base_size = 11) +
  theme(axis.title.x = element_blank(), axis.text.x=element_blank(), axis.title.y = element_blank())




ds[, .(first  = min(date_), 
       last   = max(date_), 
       median = median(date_)), by = year_]

# data availabilty 
d[, ID_year := paste0(ID, '_', year_)]

d[, first := min(datetime_), by = ID_year]
d[, last := max(datetime_), by = ID_year]
d[, days_data := as.numeric(difftime(last, first, units = 'days'))]
setorder(d, first, ID, datetime_)

ds = unique(d, by = 'ID_year')

# merge with sex
ds = merge(ds, unique(dc[, .(ID_tagID, sex = sex_observed)]), by = 'ID_tagID', all.x = TRUE)

ds[year_ == 2018]$days_data %>% mean
ds[year_ == 2019]$days_data %>% mean
ds[year_ == 2018]$days_data %>% max
ds[year_ == 2019]$days_data %>% max
ds[year_ == 2019 & days_data > 27] %>% nrow

#--------------------------------------------------------------------------------------------------------------
#' # Data until 
#--------------------------------------------------------------------------------------------------------------

ds = ds[, .(year_, ID_year, sex, first, last, days_data)]

### loop by date
dt = foreach(k = unique(ds$year_), .combine = 'rbind') %do% {
  
  # subset species and year
  ps = ds[year_ == k]
  
  # open matrix
  dt = data.table( date = seq( ps[, min(first)]  %>% as.Date %>% as.POSIXct,
                               ps[, max(last)] %>% as.Date %>% as.POSIXct  + 60*60*24, by = '1 day') )
  
  
  foreach(i = ps$ID_year) %do% {
    
    dt[, as.character(i) := ifelse(date < ps[ID_year == i, last], 1, 0)]
    
  }	
  
  # calculate survival
  dt[, sum      := rowSums(.SD), .SDcols = grep('d', names(dt), invert = TRUE)]
  dt[, survival := sum/max(sum)]
  
  dt[, year_ := k]
  dt = dt[, .(year_, date, survival)]
  dt
  
}  

max_year = dt$year_ %>% max
dt[, date2 := date + c(max_year - year_) *60*60*24*356, by = 1:nrow(dt)]

# plot
ggplot(data = dt, aes(x = date2, y = survival, col = as.character(year_))) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1.5) +
  scale_color_manual(name = 'Year', values = c('firebrick3', 'dodgerblue2')) +
  labs(x = 'Days of data', y = 'Proportion of tagged individuals') +
  theme_classic(base_size = 18)


### loop by days of data
dt = foreach(k = unique(ds$year_), .combine = 'rbind') %do% {
  
  # subset species and year
  ps = ds[year_ == k]
  
  # open matrix
  dt = data.table( day = seq(0, max(ds$days_data) %>% as.integer + 1, by = 0.1) )
  
  
  foreach(i = ps$ID_year) %do% {
    
    dt[, as.character(i) := ifelse(day < ps[ID_year == i, days_data], 1, 0)]
    
  }	
  
  # calculate survival
  dt[, sum      := rowSums(.SD), .SDcols = grep('d', names(dt), invert = TRUE)]
  dt[, survival := sum/max(sum)]
  
  dt[, year_ := k]
  dt = dt[, .(year_, day, survival)]
  dt
  
}  

# plot
p = 
ggplot(data = dt, aes(x = day, y = survival, col = as.character(year_))) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1.5) +
  scale_color_manual(name = 'Year', values = c('firebrick3', 'dodgerblue2')) +
  labs(x = 'Days of data', y = 'Proportion of tagged individuals') +
  theme_classic(base_size = 18)
p


### loop by sex and days of data
dts = foreach(k = unique(ds$sex), .combine = 'rbind') %do% {
  
  # subset species and year
  ps = ds[sex == k]
  
  # open matrix
  dt = data.table( day = seq(0, max(ds$days_data) %>% as.integer + 1, by = 0.1) )
  
  
  foreach(i = ps$ID_year) %do% {
    
    dt[, as.character(i) := ifelse(day < ps[ID_year == i, days_data], 1, 0)]
    
  }	
  
  # calculate survival
  dt[, sum      := rowSums(.SD), .SDcols = grep('d', names(dt), invert = TRUE)]
  dt[, survival := sum/max(sum)]
  
  dt[, sex := k]
  dt = dt[, .(sex, day, survival)]
  dt
  
}  

# plot
ggplot(data = dts, aes(x = day, y = survival, col = as.character(sex))) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 1.5) +
  scale_color_manual(name = 'Year', values = c('firebrick3', 'dodgerblue2')) +
  labs(x = 'Days of data', y = 'Proportion of tagged individuals') +
  theme_classic(base_size = 18)

#--------------------------------------------------------------------------------------------------------------
#' # Data linked to nests
#--------------------------------------------------------------------------------------------------------------

# prepare data for merge
dn = dn[year_ > 2017]
dn[, maleID_year   := paste0(male_id, '_', year_)]
dn[, femaleID_year := paste0(female_id, '_', year_)]
dn[, initiation := as.POSIXct(initiation)]
dn[, nest_state_date := as.POSIXct(nest_state_date)]

du = unique(d, by = 'ID_year')
du[, first := as.POSIXct(first)]
du[, last := as.POSIXct(last)]

# merge nest's with GPS data
ds = merge(dn[, .(year_, nest, male_id, maleID_year, female_id, femaleID_year, initiation, nest_state_date)], 
           du[, .(ID_year, m_first = first, m_last = last, m_days_data = days_data)], by.x = 'maleID_year', by.y = 'ID_year', all.x = TRUE)

ds = merge(ds, du[, .(ID_year, f_first = first, f_last = last, f_days_data = days_data)], by.x = 'femaleID_year', by.y = 'ID_year', all.x = TRUE)

# assign multiple clutches
ds[, male_N_clutch   := .N, by = .(maleID_year, year_)]
ds[, female_N_clutch := .N, by = .(femaleID_year, year_)]

# exclude both NA
ds = ds[!(is.na(m_first) & is.na(f_first))]
ds[, m_first := as.POSIXct(m_first)]
ds[, m_last := as.POSIXct(m_last)]
ds[, f_first := as.POSIXct(f_first)]
ds[, f_last := as.POSIXct(f_last)]

# check what' there
ds[!is.na(m_first)] %>% nrow 
ds[!is.na(f_first)] %>% nrow

# summary 
ds[, m_before_initiation := ifelse(m_first < initiation, TRUE, FALSE)]
ds[, f_before_initiation := ifelse(f_first < initiation, TRUE, FALSE)]
ds[, m_after_initiation := ifelse(initiation < m_last, TRUE, FALSE)]
ds[, f_after_initiation := ifelse(initiation < f_last, TRUE, FALSE)]
ds[, m_after_initiation5 := ifelse(initiation + 5*86400 < m_last, TRUE, FALSE)]
ds[, f_after_initiation5 := ifelse(initiation + 5*86400 < f_last, TRUE, FALSE)]

# before and after initiation
ds[, m_initiation := ifelse(m_before_initiation == TRUE & m_after_initiation == TRUE, TRUE, FALSE)]
ds[, f_initiation := ifelse(f_before_initiation == TRUE & f_after_initiation == TRUE, TRUE, FALSE)]
ds[, m_initiation5 := ifelse(m_before_initiation == TRUE & m_after_initiation5 == TRUE, TRUE, FALSE)]
ds[, f_initiation5 := ifelse(f_before_initiation == TRUE & f_after_initiation5 == TRUE, TRUE, FALSE)]

ds[, .N, by = m_initiation]
ds[, .N, by = f_initiation]

ds[, .N, by = m_initiation5]
ds[, .N, by = f_initiation5]

# both sexes
ds[, mf_initiation := ifelse(m_initiation == TRUE & f_initiation == TRUE, TRUE, FALSE)]
ds[, mf_initiation5 := ifelse(m_initiation5 == TRUE & f_initiation == TRUE, TRUE, FALSE)]

ds[, .N, by = mf_initiation]
ds[, .N, by = mf_initiation5]

ds[male_N_clutch == 2]
ds[female_N_clutch == 2, .(nest, femaleID_year, maleID_year, f_initiation5, m_initiation5, initiation)]

#--------------------------------------------------------------------------------------------------------------
#' # Proportion of individuals seen/tagged in the study site 
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
d[, date_ := as.Date(datetime_)]

# subset study site
drs = dr[study_site == TRUE]
ds = d[study_site == TRUE]
dso = d[study_site == FALSE]

# unique by date
drs = drs[!is.na(ID)]
dru = unique(drs, by = c('ID', 'date_'))
du = unique(ds, by = c('ID', 'date_'))
duo = unique(dso, by = c('ID', 'date_'))

dss = merge(dru[, .(ID, date_, seen = TRUE)], du[, .(ID, date_, tagged = TRUE)], by = c('ID', 'date_'), all = TRUE)

dss[seen == TRUE & tagged == TRUE, type := 'seen/tagged']
dss[is.na(type) & seen == TRUE, type := 'seen']
dss[is.na(type) & tagged == TRUE, type := 'tagged']

# add positions outside study site
# dss = merge(dss, duo[, .(ID, date_, tagged_out = TRUE)], by = c('ID', 'date_'), all = TRUE)
# dss[is.na(type), type := 'only_outside']

dss[, year_ := year(date_)]
dss[, .N, type]
y = format(Sys.Date(), '%Y') # current year
dss[, date_y := as.Date(paste0(y, substr(date_, 5, 10)))]

# plot settings
min_date = dss[, date_y] %>% min
max_date = dss[, date_y] %>% max

# sample size
dssn = unique(dss, by = c('ID', 'year_'))
dssn = dssn[, .N, year_]


p3 = 
  ggplot(data = dss[year_ == 2018]) +
  ggtitle('2018') +
  geom_bar(aes(x = date_y, fill = type), stat = 'count', width = 0.95, color = 'grey50', size = 0.2) +
  geom_text(aes(min_date, Inf), label = paste0('N = ', dssn[year_ == 2018, N]), hjust = 0.2, size = 3, vjust = 1) +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, 160), expand = c(0, 0)) +
  scale_fill_grey(labels = c('Observed without tag', 'Observed with tag', 'With tag but not observed'), start = 0.4, end = 0.8) +
  xlab('Date') + ylab('Number of individuals') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.3, 0.87), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), legend.key.height = unit(0.4, 'cm'), 
        legend.background = element_rect(fill = alpha('white', 0)), axis.title.x = element_blank(), 
        axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5))

p4 = 
ggplot(data = dss[year_ == 2019]) +
  ggtitle('2019') +
  geom_bar(aes(x = date_y, fill = type), stat = 'count', width = 0.95, color = 'grey50', size = 0.2) +
  geom_text(aes(min_date, Inf), label = paste0('N = ', dssn[year_ == 2019, N]), hjust = 0.2, size = 3, vjust = 1) +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, 160), expand = c(0, 0)) +
  scale_fill_grey(labels = c('Observed without tag', 'Observed with tag', 'With tag but not observed'), start = 0.4, end = 0.8) +
  xlab('Date') + ylab('Number of individuals') +
  theme_classic(base_size = 11) +
  theme(legend.position = 'none', axis.title.x = element_blank(), axis.text.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5))

ggplot_build(p4)$data 

# link to nest initiation 
dn = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

# subset data from intensive study site with initiation date
ds = dn[data_type == 'study_site' & !is.na(initiation_y)]
ds[, initiation_date := as.Date(initiation)]

# sample size
dss = ds[, .(median = median(initiation_y), q25 = quantile(initiation_y, probs = c(0.25)),
             q75 = quantile(initiation_y, probs = c(0.75)), .N, max = max(initiation_y)), by = year_]


p5 = 
ggplot(data = ds[year_ == 2018]) +
  geom_violin(aes(initiation_y, as.character(year_)), show.legend = FALSE, fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_point(data = dss[year_ == 2018], aes(median, as.character(year_)), size = 2) +
  geom_linerange(data = dss[year_ == 2018], aes(y = as.character(year_), xmin = q75, xmax = q25), size = 0.5) +
  geom_text(data = dss[year_ == 2018], aes(as.POSIXct(min_date), as.character(year_), label = paste0('N = ', N)), 
            hjust = 0.2, vjust = -1.1, size = 3) +
  scale_x_datetime(limits = c(as.POSIXct(min_date), as.POSIXct(max_date)), date_breaks = "2 weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('Nests') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), 
        legend.background = element_rect(fill = alpha('white', 0)), axis.text.y = element_blank())

p6 = 
ggplot(data = ds[year_ == 2019]) +
  geom_violin(aes(initiation_y, as.character(year_)), show.legend = FALSE, fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_point(data = dss[year_ == 2019], aes(median, as.character(year_)), size = 2) +
  geom_linerange(data = dss[year_ == 2019], aes(y = as.character(year_), xmin = q75, xmax = q25), size = 0.5) +
  geom_text(data = dss[year_ == 2019], aes(as.POSIXct(min_date), as.character(year_), label = paste0('N = ', N)), 
            hjust = 0.2, vjust = -1.1, size = 3) +
  scale_x_datetime(limits = c(as.POSIXct(min_date), as.POSIXct(max_date)), date_breaks = "2 weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('') +
  theme_classic(base_size = 11) +
  theme(axis.text.y = element_blank())


require(patchwork)
patchwork = (p3 + p4) / (p1 + p2) / (p5 + p6)
patchwork + plot_layout(heights = c(3, 0.4, 0.4))

# ggsave('./OUTPUTS/FIGURES/N_ID_observed_tagged.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # GPS tagged breeders and non-breeders
#--------------------------------------------------------------------------------------------------------------

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
d[, datetime_ := anytime(datetime_)]

dn = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

# assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

# date
d[, date_ := as.Date(datetime_)]
y = format(Sys.Date(), '%Y') # current year
d[, date_y := as.Date(paste0(y, substr(date_, 5, 10)))]

# unique by date
du = unique(d, by = c('ID', 'date_'))

# ID breeders
dID = rbind(dn[, .(year_, ID = male_id, breeder = TRUE, polyandrous)], dn[, .(year_, ID = female_id, breeder = TRUE, polyandrous)])

# merge nest info
du = merge(du, dID, by = c('year_', 'ID'), all.x = TRUE)
du[is.na(breeder), breeder := FALSE]


# plot settings
min_date = du[, date_y] %>% min
max_date = du[, date_y] %>% max

# sample size
dss = unique(du, by = c('ID', 'year_'))
dss = dss[, .N, year_]

p1 = 
ggplot(data = du[year_ == 2018]) +
  ggtitle('2018') +
  geom_bar(aes(x = date_y, fill = breeder), stat = 'count', width = 0.95, color = 'grey50', size = 0.2) +
  geom_text(aes(min_date, Inf), label = paste0('N = ', dss[year_ == 2018, N]), hjust = 0.2, size = 3, vjust = 1) +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, 170), expand = c(0, 0)) +
  scale_fill_grey(labels = c('Unknown if breeder', 'Known breeder'), start = 0.4, end = 0.8) +
  xlab('Date') + ylab('Number of individuals') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.3, 0.87), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
        axis.title.x = element_blank(), axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5))

p2 = 
ggplot(data = du[year_ == 2019]) +
  ggtitle('2019') +
  geom_bar(aes(x = date_y, fill = breeder), stat = 'count', width = 0.95, color = 'grey50', size = 0.2) +
  geom_text(aes(min_date, Inf), label = paste0('N = ', dss[year_ == 2019, N]), hjust = 0.2, size = 3, vjust = 1) +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, 170), expand = c(0, 0)) +
  scale_fill_grey(labels = c('Unknown if breeder', 'Known breeder'), start = 0.4, end = 0.8) +
  xlab('Date') + ylab('Number of individuals') +
  theme_classic(base_size = 11) +
  theme(legend.position = 'none', axis.title.x = element_blank(), axis.text.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5))


# link to nest initiation 
dn = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

# subset data from tagged birds
dID = unique(d, by = c('year_', 'ID'))
dn = merge(dn, dID[, .(year_, female_id = ID, female_tagged = TRUE)], by = c('year_', 'female_id'), all.x = TRUE)
dn = merge(dn, dID[, .(year_, male_id = ID, male_tagged = TRUE)], by = c('year_', 'male_id'), all.x = TRUE)

dn[female_tagged == TRUE & male_tagged == TRUE, tagged := 'both']
dn[is.na(female_tagged) & male_tagged == TRUE, tagged := 'male']
dn[is.na(male_tagged) & female_tagged == TRUE, tagged := 'female']
dn[is.na(tagged), tagged := 'none']

# subset nest with at least one bird tagged
ds = dn[tagged != 'none']

# sample size
dss = ds[, .(median = median(initiation_y), q25 = quantile(initiation_y, probs = c(0.25)),
             q75 = quantile(initiation_y, probs = c(0.75)), .N, max = max(initiation_y)), by = year_]

p3 = 
ggplot(data = ds[year_ == 2018]) +
  geom_violin(aes(initiation_y, as.character(year_)), show.legend = FALSE, fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_point(data = dss[year_ == 2018], aes(median, as.character(year_)), size = 2) +
  geom_linerange(data = dss[year_ == 2018], aes(y = as.character(year_), xmin = q75, xmax = q25), size = 0.5) +
  geom_text(data = dss[year_ == 2018], aes(as.POSIXct(min_date), as.character(year_), label = paste0('N = ', N)), 
            hjust = 0.2, vjust = -1.1, size = 3) +
  scale_x_datetime(limits = c(as.POSIXct(min_date), as.POSIXct(max_date)), date_breaks = "2 weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('Nests') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), 
        legend.background = element_rect(fill = alpha('white', 0)), axis.text.y = element_blank())

p4 = 
ggplot(data = ds[year_ == 2019]) +
  geom_violin(aes(initiation_y, as.character(year_)), show.legend = FALSE, fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_point(data = dss[year_ == 2019], aes(median, as.character(year_)), size = 2) +
  geom_linerange(data = dss[year_ == 2019], aes(y = as.character(year_), xmin = q75, xmax = q25), size = 0.5) +
  geom_text(data = dss[year_ == 2019], aes(as.POSIXct(min_date), as.character(year_), label = paste0('N = ', N)), 
            hjust = 0.2, vjust = -1.1, size = 3) +
  scale_x_datetime(limits = c(as.POSIXct(min_date), as.POSIXct(max_date)), date_breaks = "2 weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('Nests') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), 
        legend.background = element_rect(fill = alpha('white', 0)), axis.text.y = element_blank())




patchwork = (p1 + p2) / (p3 + p4)
patchwork + plot_layout(heights = c(3, 0.4))

# ggsave('./OUTPUTS/FIGURES/N_ID_nest_tagged.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')

#--------------------------------------------------------------------------------------------------------------
#' # Summary by ID
#--------------------------------------------------------------------------------------------------------------


# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
d[, datetime_ := anytime(datetime_)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dr = dbq(con, 'select * FROM RESIGHTINGS')
dr[, year_ := year(datetime_)]
dr = dr[year_ > 2017]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

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



# color by breeder / non-breeder
dnID = rbind(dn[, .(year_, ID = female_id, nestID, initiation, sex = 'F')], dn[, .(year_, ID = male_id, nestID, initiation, sex = 'M')])
dnID[, ID := factor(ID)]
dnID = unique(dnID, by = c('year_', 'ID'))

ds = merge(ds, dnID[, .(year_, ID, status = 'breeder')], by = c('year_', 'ID'), all.x = TRUE)
ds[is.na(status), status := 'non-breeder']

dss = unique(ds, by = c('ID', 'year_'))
dss = dss[, .N, by = .(status, year_)]


ggplot(data = ds[year_ == 2018]) +
  ggtitle('2018') +
  geom_point(aes(datetime_y, ID, color = status), size = 0.7) +
  scale_x_datetime(date_breaks = "weeks", date_labels = "%d %b") +
  scale_color_manual(values = c('cadetblue3', 'tan1')) +
  theme_classic(base_size = 11) +
  xlab('Date') +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
        axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))

# ggsave('./OUTPUTS/FIGURES/ID_tenure_2018_breeder.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')

ds[year_ == 2019 & status == 'non-breeder' & tenure > 30]$ID %>% unique


ggplot(data = ds[year_ == 2019]) +
  ggtitle('2019') +
  geom_point(aes(datetime_y, ID, color = status), size = 0.7) +
  scale_x_datetime(date_breaks = "weeks", date_labels = "%d %b") +
  scale_color_manual(values = c('cadetblue3', 'tan1')) +
  theme_classic(base_size = 11) +
  xlab('Date') +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank(), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
        axis.text.y=element_blank(), plot.title = element_text(hjust = 0.5))

# ggsave('./OUTPUTS/FIGURES/ID_tenure_2019_breeder.tiff', plot = last_plot(),  width = 177, height = 250, units = c('mm'), dpi = 'print')

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








