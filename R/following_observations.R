#' ---
#' title: Calculate pair-wise interactions
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
# Look at observation data
#==============================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'auksRuak', 'foreach', 'knitr', 'windR'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM RESIGHTINGS')
dg = dbq(con, 'select * FROM SEX')
dpa = dbq(con, 'select * FROM PATERNITY')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]

#--------------------------------------------------------------------------------------------------------------
#' # Following behaviour
#--------------------------------------------------------------------------------------------------------------

# ID_year
d[, datetime_ := as.POSIXct(datetime_)]
d[, date_ := as.POSIXct(format(datetime_, format = '%y-%m-%d'), format = '%y-%m-%d')]
d[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, year_ := year(datetime_)]
d[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]

# nestID
dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# ID_year
d[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dn[!is.na(male_id), male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
dn[!is.na(female_id), female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]

# unique nest information by ID
dm = dn[, .(year_, nestID, ID_year = male_id_year, initiation, nest_state_date)]
df = dn[, .(year_, nestID, ID_year = female_id_year, initiation, nest_state_date)]

dnID = rbind(dm, df)
dnID = dnID[!is.na(ID_year) & year_ > 2000]

# any nest in study site?
dnID[, any_nest := TRUE]
# dnID[, any_nest_study_site := any(study_site == TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(initiation), first_initiation := min(initiation, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(initiation) & initiation != first_initiation, 
     second_initiation := min(initiation, na.rm = TRUE), by = ID_year]
dnID[, second_initiation := min(second_initiation, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(nest_state_date), first_nest_state_date := min(nest_state_date, na.rm = TRUE), by = ID_year]
dnID[!is.na(ID_year) & !is.na(nest_state_date) & nest_state_date != first_nest_state_date, 
     second_nest_state_date := min(nest_state_date, na.rm = TRUE), by = ID_year]
dnID[, second_nest_state_date := min(second_nest_state_date, na.rm = TRUE), by = ID_year]
dnID[, N_clutches := .N, by = ID_year]
dnID = unique(dnID, by = 'ID_year')

# table with pairs with nest
dnp1 = dn[!is.na(male_id) & !is.na(female_id), .(ID1 = male_id_year, ID2 = female_id_year, nestID, initiation)]
dnp2 = dnp1[, .(ID1 = ID2, ID2 = ID1, nestID, initiation)]
dnp = rbind(dnp1, dnp2)
dnp[, nest_together := 1]
dnp[!is.na(initiation), first_initiation_together := min(initiation, na.rm = TRUE), by = .(ID1, ID2)]
dnp = unique(dnp, by = c('ID1', 'ID2'))

# how many nest partners?
dnp[, N_partners := .N, by = ID1]
dnp[, .N, by = N_partners]
setorder(dnp, initiation)
dnp[, partner := seq_len(.N), by = ID1]

# first & second partner partner
ds = dnp[partner == 1]
dnp = merge(dnp, ds[, .(ID1, ID1_1st_partner = ID2)], by = 'ID1', all.x = TRUE)
ds = dnp[partner == 2]
dnp = merge(dnp, ds[, .(ID1, ID1_2nd_partner = ID2)], by = 'ID1', all.x = TRUE)
dnp[, c('N_partners', 'partner') := NULL]

# exclude unknown ID's
d = d[!is.na(ID)]

# ID's per obs_id
d[, N := .N, by = obs_id]
d2 = d[N > 1]

# reshape data in long format
d2 = d2[, data.table(t(combn(ID_year, 2))), obs_id]
setnames(d2, c('obs_id', 'ID1', 'ID2'))

# have everything also in the ID
dup = d2[, .(obs_id, ID1 = ID2, ID2 = ID1)]
d2 = rbind(d2, dup)

di = copy(d2)

di = merge(di, d[, .(obs_id, ID_year, ID2sex = sex)], 
           by.x = c('obs_id', 'ID2'), by.y = c('obs_id', 'ID_year'), all.x = TRUE)

di = merge(di, d[, .(obs_id, ID_year, ID1sex = sex, author, year_, datetime_, 
                     datetime_y, date_)], 
           by.x = c('obs_id', 'ID1'), by.y = c('obs_id', 'ID_year'), all.x = TRUE)

# any nest? 
di = merge(di, dnID[, .(ID_year, any_nest, first_initiation, second_initiation, N_clutches)], 
           by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)
di[is.na(any_nest), any_nest := FALSE]
# di[is.na(any_nest_study_site), any_nest_study_site := FALSE]

# any nest together?
di = merge(di, dnp[, .(ID1, ID2, nestID, initiation, nest_together, first_initiation_together)], by = c('ID1', 'ID2'), all.x = TRUE)
di[is.na(nest_together), nest_together := 0]

# merge partner to all
di = merge(di, dnp[, .(ID1, ID1_1st_partner, ID1_2nd_partner)], by = 'ID1', all.x = TRUE)



d[flight %like%('F'), .N, by = sex]


d[is.na(min_dist)]
