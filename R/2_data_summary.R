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

ggplot(data = ds) +
  geom_bar(aes(date_y), fill = 'grey85', color = 'grey50') +
  facet_grid(.~year_, scales = 'fixed') +
  theme_classic()

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
drs = dr[!is.na(ID)]
dru = unique(drs, by = c('ID', 'date_'))
du = unique(ds, by = c('ID', 'date_'))
duo = unique(dso, by = c('ID', 'date_'))

dss = merge(dru[, .(ID, date_, seen = TRUE)], du[, .(ID, date_, tagged = TRUE)], by = c('ID', 'date_'), all = TRUE)

dss[seen == TRUE & tagged == TRUE, type := 'seen/tagged']
dss[is.na(type) & seen == TRUE, type := 'seen']
dss[is.na(type) & tagged == TRUE, type := 'tagged']

dss = merge(dss, duo[, .(ID, date_, tagged_out = TRUE)], by = c('ID', 'date_'), all = TRUE)
dss[is.na(type), type := 'only_outside']

dss[, year_ := year(date_)]
dss[, .N, type]

ggplot(data = dss[year_ == 2019]) +
  geom_bar(aes(x = date_, fill = type), stat = 'count') +
  scale_x_date(date_breaks = "weeks", date_labels = "%d-%m") +
  theme_classic()


ggplot(data = dss[year_ == 2018]) +
  geom_bar(aes(x = date_, fill = type), stat = 'count') +
  scale_x_date(date_breaks = "weeks", date_labels = "%d-%m")



