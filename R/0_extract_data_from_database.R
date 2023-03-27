#==============================================================================================================
# Data and code from "Mutual mate guarding and limited sexual conflict in a sex-role reversed shorebird"
# Contributor: Johannes Krietsch
# ❗This script is provided as reference only. It contains links to the internal database of the Max Planck 
# Institute for Ornithology, from which it pulls the data and exports all the collected data to ./DATA
#==============================================================================================================

### Summary
# NANO_TAGS_TEST data
# NANO_TAGS data
# CAPTURES
# NESTS
# OBSERVATIONS

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'auksRuak', 'ggplot2'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#--------------------------------------------------------------------------------------------------------------
# NANO_TAGS_TEST data
#--------------------------------------------------------------------------------------------------------------

# All test data from Nano tags on the roof

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID == 999] # ID = 999 are the test data, tags where on the BASC building 
d[, datetime_ := as.POSIXct(datetime_, tz = 'UTC')]
g = dbq(con, "SELECT gps_id, gps_point, datetime_ gps_time, 
               lat, lon FROM FIELD_2018_REPHatBARROW.GPS_POINTS")
DBI::dbDisconnect(con)

# table with tagID and GPS waypoints
dl = data.table(gps_id = rep(2, 10),
                gps_point = rep(85:89, each = 2),
                tagID = 91:100)

dl = merge(dl, g, by = c('gps_point', 'gps_id') )

# merge actual location with all points
d = merge(d, dl[, .(tagID, lat_wp = lat, lon_wp = lon)], by = 'tagID', all.x = TRUE)

# subset data relevant for this study
d = d[, .(year_, tagID, datetime_, lat, lon, lat_wp, lon_wp)]

# check data
summary(d)
sapply(d, function(x) sum(is.na(x)))

# save data
# write.table(d, './DATA/NANO_TAGS_TEST.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------
# NANO_TAGS data
#--------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID != 999] # exclude test data
d[is.na(lon)] # check that no NA
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

# merge with sex
dg[, ID := as.numeric(ID)]
dg = dg[!is.na(ID)]


d = merge(d, dg, by = c('ID'), all.x = TRUE)


# subset relevant data
d = d[, .(year_, tagID, ID, sex, datetime_, lat, lon, gps_speed, altitude, batvolt)]

# set order
setorder(d, ID, tagID, datetime_)

# save data
# fwrite(d, './DATA/NANO_TAGS.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------
# CAPTURES
#--------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM CAPTURES')
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

# subset years relevant for this study 
d = d[year_ %in% c(2017:2019)]

# Change projection
d[, lat_dec := lat]
d[, lon_dec := lon]

ds = d[is.na(lon)] # separate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

point_over_poly_DT(d, poly = study_site, buffer = 60) # buffer including birds followed flying off plot
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d = rbind(d, ds)

# exclude birds caught before intensive study in this site
d[year_ < 2017, study_site := FALSE]

# assign data type
d[study_site == TRUE, data_type := 'study_site']
d[study_site == FALSE & external == 0, data_type := 'own_off_site']
d[external == 1, data_type := 'survey_plot']
d[comments %like% 'clutch_removal_exp', data_type := 'clutch_removal_exp']
d = d[!is.na(data_type)]

# any without metal band?
d[is.na(ID)]

# exclude chick
d = d[!is.na(sex_observed)]

# assign first capture
d[, caught_time := as.POSIXct(caught_time)]
setorder(d, year_, caught_time)
d[, capture_id := seq_len(.N), by = ID]
d[, .N, capture_id]

# subset only adults that were genotyped from long-term monitoring data
dg[, genotyped := TRUE]
d[, ID := as.character(ID)]
d = merge(d, dg[, .(ID, genotyped, sex)], by = 'ID', all.x = TRUE)
d[is.na(genotyped), genotyped := FALSE]
d = d[!(genotyped == FALSE & external == 1)]

# check data
bm = create_bm(d[study_site == TRUE], buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = study_site))

bm = create_bm(d, buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = data_type))

# subset birds from MPI study
d = d[external == 0]

# subset data relevant for this study
d = d[, .(year_, ID, UL, UR, LL, LR, gps_tagID = gps_tag, sex = sex_observed, 
          lat = lat_dec, lon = lon_dec, caught_time, dead)]

# check data
summary(d)
sapply(d, function(x) sum(is.na(x)))

# save data
# write.table(d, './DATA/CAPTURES.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------
# NESTS
#--------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
dt = dbq(con, 'select * FROM NANO_TAGS')
DBI::dbDisconnect(con)

# subset years relevant for this study 
d = d[year_ %in% c(2017:2019)]

# Change projection
d[, lat_dec := lat]
d[, lon_dec := lon]

ds = d[is.na(lon)] # separate data without position
d = d[!is.na(lon)]
st_transform_DT(d)

point_over_poly_DT(d, poly = study_site, buffer = 0)
setnames(d, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := FALSE]
d = rbind(d, ds)

# nestID
d[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# nests with paternity data
dp = dp[!is.na(EPY)]
# EPY father 
dp[EPY == 1, .N, by = nestID] # more than 1 EPY father? No
dp[EPY == 1, EPY_father := IDfather]
dps = dp[, .(EPY = sum(EPY, na.rm = TRUE), N = .N, EPY_father = min(EPY_father, na.rm = TRUE)), by = .(nestID, year_)]
dps[, anyEPY := ifelse(EPY > 0, 1, 0)]
dps[is.infinite(EPY_father), EPY_father := NA]
dps[!is.na(EPY_father)] %>% nrow # known EPY fathers

# merge with nests
d = merge(d, dps[, .(nestID, N_parentage = N, N_EPY = EPY, anyEPY, EPY_father)], by = 'nestID', all.x = TRUE)

# assign all nests with parentage data
d[, parentage := ifelse(!is.na(N_parentage), TRUE, FALSE)]
d[, any_parentage_year := any(parentage == TRUE), by = year_]

# data sets and data available
d[study_site == TRUE, data_type := 'study_site']
d[parentage == TRUE & study_site == FALSE & external == 0, data_type := 'own_off_site']

# plots with parentage data, total nests in years with data
plot_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$plot %>% unique
year_R = d[parentage == TRUE & study_site == FALSE & external == 1 & plot %like% 'brw']$year_ %>% unique
d[plot %in% plot_R & year_ %in% year_R, data_type := 'survey_plot']
d[parentage == TRUE & external == 1 & !(plot %like% 'brw') & year_ %in% year_R, data_type := 'clutch_removal_exp']

d[, .N, data_type]

# fill gaps
d[is.na(data_type) & nest %like% 'REPH', data_type := 'survey_plot']
d[is.na(data_type), data_type := 'own_off_site']

# check data
bm = create_bm(d[study_site == TRUE], buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = study_site))

bm = create_bm(d, buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = data_type))

# rates of social polyandry
d[, initiation := as.POSIXct(initiation)]
setorder(d, year_, initiation)

# assign clutch order
d[!is.na(male_id) & !is.na(female_id), clutch_together := seq_len(.N), by = .(year_, male_id, female_id)]

# first and second clutches by females
d[, female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
d[, N_female_clutch := .N, by = female_id]
d[, female_clutch := seq_len(.N), by = .(female_id, year_)]
d[is.na(female_id), female_clutch := 1]
d[!is.na(female_id), N_female_clutch := max(female_clutch), by = .(female_id, year_)]
d[is.na(female_id), N_female_clutch := 1]
d[, .N, by = .(year_, female_clutch)]
d[, .N, by = .(female_clutch, external)]

# polyandrous clutches (second clutch with different partner)
ID2c = d[female_clutch == 2]$female_id_year
dx = d[female_id_year %in% ID2c]

dr = merge(dx[female_clutch == 1, .(year1 = year_, nestID1 = nestID, female_id_year, m1 = male_id, anyEPY1 = anyEPY, 
                                    ss1 = study_site, initiation1 = initiation)], 
           dx[female_clutch == 2, .(year2 = year_, nestID2 = nestID, female_id_year, m2 = male_id, anyEPY2 = anyEPY, 
                                    ss2 = study_site, initiation2 = initiation)],  
           by = 'female_id_year', all = TRUE)

dr[, same_male := m1 == m2]
dr[is.na(same_male), same_male := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, female_id_year)
dr

# polyandrous females
dr = dr[same_male == FALSE, .(female_id_year, polyandrous = TRUE, polyandry_study_site = both_study_site)]
d = merge(d, dr, by = 'female_id_year', all.x = TRUE)
d[is.na(polyandrous), polyandrous := FALSE]
d[is.na(polyandry_study_site), polyandry_study_site := FALSE]

setorder(d, year_, initiation)

# males renesting
d[, male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
d[, N_male_clutch := .N, by = male_id]
d[, male_clutch := seq_len(.N), by = male_id_year]
d[is.na(male_id), male_clutch := 1]
d[!is.na(male_id), N_male_clutch := max(male_clutch), by = male_id_year]
d[is.na(male_id), N_male_clutch := 1]
d[, .N, by = .(year_, male_clutch)]
d[, .N, by = .(male_clutch, external)]

# males renesting with same or different partner
ID2c = d[male_clutch == 2]$male_id_year
dx = d[male_id_year %in% ID2c]

dr = merge(dx[male_clutch == 1, .(year1 = year_, nestID1 = nestID, male_id_year, f1 = female_id, anyEPY1 = anyEPY, 
                                  mfc1 = male_clutch, ss1 = study_site, initiation1 = initiation)], 
           dx[male_clutch == 2, .(year2 = year_, nestID2 = nestID, male_id_year, f2 = female_id, anyEPY2 = anyEPY, 
                                  fc2 = male_clutch, ss2 = study_site, initiation2= initiation)],  
           by = 'male_id_year', all = TRUE)

dr[, same_female := f1 == f2]
dr[is.na(same_female), same_female := FALSE]
dr[, both_study_site := ss1 == ss2]
dr[, diff_initiation := difftime(initiation2, initiation1, units = 'days') %>% as.numeric]
setorder(dr, male_id_year)
dr

# renesting males
dr = dr[, .(male_id_year, renesting_male = TRUE, same_female, renesting_study_site = both_study_site)]
d = merge(d, dr, by = 'male_id_year', all.x = TRUE)
d[is.na(renesting_male), renesting_male := FALSE]
d[is.na(renesting_study_site), renesting_study_site := FALSE]

# start and end of the data
dt[, start := min(datetime_), by = ID]
dt[, end   := max(datetime_), by = ID]
dID = unique(dt, by = 'ID')

# check if data overlap
d = merge(d, dID[, .(year_, male_id = ID, start_m = start, end_m = end)], by = c('male_id', 'year_'), all.x = TRUE)
d = merge(d, dID[, .(year_, female_id = ID, start_f = start, end_f = end)], by = c('female_id', 'year_'), all.x = TRUE)

# subset nests from MPIO study or with any bird tagged
d[, m_tagged := !is.na(start_m), by = nestID]
d[, f_tagged := !is.na(start_f), by = nestID]
d[, any_tagged := any(!is.na(start_m), !is.na(start_f)), by = nestID]
d[, both_tagged := !is.na(start_m) & !is.na(start_f), by = nestID]

d = d[external == 0 | any_tagged == TRUE]

# check
bm = create_bm(d, buffer = 500)
bm +
  geom_point(data = d, aes(lon, lat, color = data_type))

# check NA
d[study_site == TRUE & initiation_method == 'none', .(year_, nest)]

# subset data relevant for this study
d = d[, .(data_type, year_, nestID, male_id, female_id, male_assigned, female_assigned, found_datetime, 
          clutch_size, initiation, initiation_method, egg1, egg2, egg3, egg4, hatching_datetime, nest_state, 
          nest_state_date, lat = lat_dec, lon = lon_dec, parentage, anyEPY, N_parentage, N_EPY, female_clutch, 
          N_female_clutch, polyandrous, male_clutch, N_male_clutch, clutch_together, renesting_male, m_tagged, 
          f_tagged)]

# check data
summary(d)
sapply(d, function(x) sum(is.na(x)))

# save data
# write.table(d, './DATA/NESTS.txt', quote = TRUE, sep = '\t', row.names = FALSE)


#--------------------------------------------------------------------------------------------------------------
# OBSERVATIONS
#--------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW') 
d  = dbq(con, 'select * FROM RESIGHTINGS')
dc = dbq(con, 'select * FROM CAPTURES')
dn = dbq(con, 'select * FROM NESTS')
dp = dbq(con, 'select * FROM PATERNITY')
DBI::dbDisconnect(con)

# Change projection
ds = dn[is.na(lon)] # separate data without position
dn = dn[!is.na(lon)]
st_transform_DT(dn)
st_transform_DT(d)


# Assign locations in the study area 
point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

point_over_poly_DT(dn, poly = study_site, buffer = 10)
setnames(dn, 'poly_overlap', 'study_site')

# merge with data without position
ds[, study_site := NA]
dn = rbind(dn, ds)

# datetime
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_y := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, year_ := year(datetime_)]
dn[, initiation := as.POSIXct(initiation)]

# first REPH and first copulation
d[, min(datetime_), by = .(year_, study_site)]
d[!is.na(cop), min(datetime_), by = year_]
d[!is.na(cop), max(datetime_), by = year_]
d[!is.na(cop)]

# nestID
dn[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]
dp[, nestID := paste0(nest, '_', substr(year_, 3,4 ))]

# ID_year
d[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dc[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]
dn[!is.na(male_id), male_id_year := paste0(male_id, '_', substr(year_, 3,4 ))]
dn[!is.na(female_id), female_id_year := paste0(female_id, '_', substr(year_, 3,4 ))]
dp[!is.na(IDfather), IDfather_year := paste0(IDfather, '_', substr(year_, 3,4 ))]
dp[!is.na(IDmother), IDmother_year := paste0(IDmother, '_', substr(year_, 3,4 ))]

# unique nest information by ID
dm = dn[, .(year_, nestID, ID_year = male_id_year, study_site, initiation, nest_state_date)]
df = dn[, .(year_, nestID, ID_year = female_id_year, study_site, initiation, nest_state_date)]

dnID = rbind(dm, df)
dnID = dnID[!is.na(ID_year) & year_ > 2000]

# any nest in study site?
dnID[, any_nest := TRUE]
dnID[, any_nest_study_site := any(study_site == TRUE), by = ID_year]
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

# individuals seen at least once in study site
d[, seen_in_study_site := any(study_site == TRUE), by = ID_year]

# first and last seen &  tenure time
d[!is.na(ID), first_obs := min(datetime_, na.rm = TRUE), by = ID_year]
d[!is.na(ID), last_obs  := max(datetime_, na.rm = TRUE), by = ID_year]
d[!is.na(ID), tenure := as.numeric(difftime(last_obs, first_obs, units = 'days')), by = ID_year]

# number of observations per year
d[!is.na(ID), N_obs := .N, by = ID_year]

# number of copulations
d[, .N, by = cop]
d[, copAS := ifelse(!is.na(cop), 1, 0), by = 1:nrow(d)]
d[!is.na(ID), N_cop := sum(copAS, na.rm = TRUE), by = ID_year]

# on how many days seen?
d[, date_ := as.POSIXct(format(datetime_, format = '%y-%m-%d'), format = '%y-%m-%d')]
du = unique(d[!is.na(ID)], by = c('date_', 'ID_year'))
du = du[, .(N_obs_days = .N), by = ID_year]
d = merge(d, du[, .(ID_year, N_obs_days)], by = 'ID_year', all.x = TRUE)

# exclude NOBA and birds never seen in study site
d = d[!is.na(ID)]
d = d[seen_in_study_site == TRUE]

# ID's per obs_id
d[, N := .N, by = obs_id]

# check if sex in resightings fits captures
d_sex = unique(d[!is.na(ID)], by = 'ID')
dc_sex = unique(dc, by = 'ID')

dx = merge(d_sex[, .(ID, sex)], dc_sex[, .(ID, sex_observed)], by = 'ID', all.x = TRUE)
dx[, identical(sex, sex_observed)]
dx[is.na(sex_observed)]

drs = unique(d, by = 'ID_year')
drs = drs[, .(ID_year, sex)]

# females that had EPY in a clutch
dpm = dp[, any_EPY := any(EPY == 1), IDmother_year] %>% unique(., by = 'IDmother_year')
dpm = dpm[, .(year_, ID_year = IDmother_year, any_EPY)]

# males that sired EPY
dpf = dp[, any_EPY := any(EPY == 1), IDfather_year] %>% unique(., by = 'IDfather_year')
dpf = dpf[, .(year_, ID_year = IDfather_year, any_EPY)]

dpmf = rbind(dpm, dpf)
dpmf = dpmf[!is.na(ID_year)]

# EPY together
dpEPY1 = dp[EPY == 1 & !is.na(IDmother) & !is.na(IDfather), .(ID1 = IDfather_year, ID2 = IDmother_year)]
dpEPY2 = dpEPY1[, .(ID1 = ID2, ID2 = ID1)]
dpEPY = rbind(dpEPY1, dpEPY2)
dpEPY[, EPY_together := 1]
dpEPY = unique(dpEPY, by = c('ID1', 'ID2'))

# split data in obs_id with only one individual
d1 = d[N == 1, .(obs_id, ID1 = ID_year, ID2 = NA)]
d2 = d[N > 1]

# reshape data in long format
d2 = d2[, data.table(t(combn(ID_year, 2))), obs_id]
setnames(d2, c('obs_id', 'ID1', 'ID2'))

# have everything also in the ID
dup = d2[, .(obs_id, ID1 = ID2, ID2 = ID1)]
d2 = rbind(d2, dup)

di = rbind(d1, d2)

di = merge(di, d[, .(obs_id, ID_year, ID2sex = sex, ID2copAS = copAS)], 
           by.x = c('obs_id', 'ID2'), by.y = c('obs_id', 'ID_year'), all.x = TRUE)

di = merge(di, d[, .(obs_id, ID_year, ID1sex = sex, ID1copAS = copAS, author, year_, datetime_, 
                     datetime_y, date_, lat, lon, seen_in_study_site, N, N_obs, N_obs_days, N_cop, first_obs, last_obs)], 
           by.x = c('obs_id', 'ID1'), by.y = c('obs_id', 'ID_year'), all.x = TRUE)

# any nest? 
di = merge(di, dnID[, .(ID_year, any_nest, any_nest_study_site, first_initiation, second_initiation, N_clutches)], 
           by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)
di[is.na(any_nest), any_nest := FALSE]
di[is.na(any_nest_study_site), any_nest_study_site := FALSE]

# any nest together?
di = merge(di, dnp[, .(ID1, ID2, nestID, initiation, nest_together, first_initiation_together)], by = c('ID1', 'ID2'), all.x = TRUE)
di[is.na(nest_together), nest_together := 0]

# merge partner to all
di = merge(di, dnp[, .(ID1, ID1_1st_partner, ID1_2nd_partner)], by = 'ID1', all.x = TRUE)

# any EPY?
di = merge(di, dpmf[, .(ID_year, any_EPY)], by.x = 'ID1', by.y = 'ID_year', all.x = TRUE)

# EPY together?
di = merge(di, dpEPY, by = c('ID1', 'ID2'), all.x = TRUE)
di[is.na(EPY_together), EPY_together := 0]

# type of interactions
di[, interaction_ := ifelse(!is.na(ID2), 1, 0), by = 1:nrow(di)]
di[, any_interaction := any(interaction_ == 1), by = ID1]
di[, same_sex := ifelse(ID1sex == ID2sex, 1, 0), by = 1:nrow(di)]
di[, any_same_sex := any(same_sex == 1), by = ID1]
di[, any_opp_sex := any(same_sex == 0), by = ID1]
di[, copAS := ifelse(ID1copAS == 1 & ID2copAS == 1, 1, 0), by = 1:nrow(di)]
di[, N_cop_ID := sum(copAS, na.rm = TRUE), by = ID1]

# number of interactions
di[, N_interactions := sum(interaction_, na.rm = TRUE), by = ID1]

# number of unique interactions
diu = unique(di, by = c('ID1', 'ID2'))
diu[, N_interactions_unique := sum(interaction_, na.rm = TRUE), by = ID1]
diu = unique(diu, by = 'ID1')
di = merge(di, diu[, .(ID1, N_interactions_unique)], by = 'ID1', all.x = TRUE)

# number of unique copulation partner
diu = unique(di, by = c('ID1', 'ID2'))
diu = diu[copAS == 1]
diu[, N_copAS_unique := sum(copAS, na.rm = TRUE), by = ID1]
diu = unique(diu, by = 'ID1')
di = merge(di, diu[, .(ID1, N_copAS_unique)], by = 'ID1', all.x = TRUE)

# first and last time seen together
di[!is.na(ID1) & !is.na(ID2), first_obs_together := min(datetime_, na.rm = TRUE), by = .(ID1, ID2)]
di[!is.na(ID1) & !is.na(ID2), last_obs_together  := max(datetime_, na.rm = TRUE), by = .(ID1, ID2)]
di[!is.na(ID1) & !is.na(ID2), tenure_together := as.numeric(difftime(last_obs_together, first_obs_together, units = 'days')), by = .(ID1, ID2)]

# first and last time seen after nest initation 
di[any_nest == TRUE, first_obs_first_initiation := as.numeric(difftime(first_obs, first_initiation, units = 'days')), by = ID1]
di[any_nest == TRUE, last_obs_first_initiation := as.numeric(difftime(last_obs, first_initiation, units = 'days')), by = ID1]

# first_paired assuming paired at least a day before initiation
di[nest_together == 1, first_obs_together_cor_initiation := dplyr::if_else(first_obs_together < c(first_initiation_together - 86400), 
                                                                           first_obs_together, c(first_initiation_together - 86400)), by = ID1] 

# last_paired assuming paired at least 3 days after initiation (during egg laying)
di[nest_together == 1, last_obs_together_cor_initiation := dplyr::if_else(last_obs_together > c(first_initiation_together + 3*86400), 
                                                                          last_obs_together, c(first_initiation_together + 3*86400)), by = ID1]

# tenure together using also initiation data
di[!is.na(ID1) & !is.na(ID2), tenure_together_cor_initiation := as.numeric(difftime(last_obs_together_cor_initiation, 
                                                                                    first_obs_together_cor_initiation, units = 'days')), by = .(ID1, ID2)]

# seen any other birds of opposite sex while tenure together? 
ds = unique(di[ID2 == ID1_1st_partner], by = 'ID1')
di = merge(di, ds[, .(ID1, first_obs_1st_partner = first_obs_together_cor_initiation, 
                      last_obs_1st_partner = last_obs_together_cor_initiation)], by = 'ID1', all.x = TRUE)

di[, paired_1st_partner := datetime_ >= first_obs_1st_partner & datetime_ <= last_obs_1st_partner]
di[, not_1st_partner_opp_sex := same_sex == 0 & ID2 != ID1_1st_partner]
di[, contact_other_than_1st_partner_while_paired := paired_1st_partner == TRUE & not_1st_partner_opp_sex == TRUE]
di[, copulation_other_than_1st_partner_while_paired := paired_1st_partner == TRUE & not_1st_partner_opp_sex == TRUE & ID1copAS == 1]

di[contact_other_than_1st_partner_while_paired == TRUE, .N, by = ID1]
di[copulation_other_than_1st_partner_while_paired == TRUE, .N, by = .(ID1, ID1sex)]

# copulation with other than first partner? 
di[ID1copAS == 1 & ID2copAS == 1 & ID2 != ID1_1st_partner, copAS_not_1st_partner := TRUE]
di[ID1copAS == 1 & ID2copAS == 1 & ID2 == ID1_2nd_partner, copAS_2nd_partner := TRUE]
di[ID1copAS == 1 & ID2copAS == 1 & ID2 != ID1_2nd_partner, copAS_not_2nd_partner := TRUE]

# timing of this copulation
di[copAS_not_1st_partner == TRUE, copEPC_timing := difftime(datetime_, first_initiation, units = 'days') %>% as.numeric]
di[copAS_not_1st_partner == TRUE, copEPC_first := min(copEPC_timing, na.rm = TRUE), by = .(ID1, ID2)]
di[copAS_not_1st_partner == TRUE, copEPC_last := max(copEPC_timing, na.rm = TRUE), by = .(ID1, ID2)]

# second partner?
unique(di[copAS_not_1st_partner == TRUE, .(ID1, ID2, copAS_2nd_partner)], by = 'ID1')

# seen with anybody except first partner? 
di[ID2 != ID1_1st_partner & same_sex == 0, seen_with_other_than_1st_partner := TRUE]
di[ID2 == ID1_2nd_partner, seen_with_2nd_partner := TRUE]

di[ID2 != ID1_2nd_partner & same_sex == 0, seen_with_other_than_2nd_partner := TRUE]

unique(di[seen_with_other_than_1st_partner == TRUE, .(ID1, ID2, seen_with_2nd_partner)], by = c('ID1', 'ID2'))

di[, diff_obs_1st_initiation := difftime(datetime_, first_initiation, units = 'days') %>% as.numeric %>% round(., 0)]
di[, diff_obs_2nd_initiation := difftime(datetime_, second_initiation, units = 'days') %>% as.numeric %>% round(., 0)]

# turn inf values in NA
invisible(lapply(names(di),function(.name) set(di, which(is.infinite(di[[.name]])), j = .name,value = NA)))

# select columns of interest
ds = di[, .(year_, datetime_, obs_id, ID1, ID2, ID1sex, ID2sex, ID1copAS, ID2copAS, same_sex, ID1_1st_partner, ID1_2nd_partner, 
            diff_obs_1st_initiation, diff_obs_2nd_initiation, seen_with_other_than_1st_partner, 
            seen_with_other_than_2nd_partner, copAS_not_1st_partner, copAS_not_2nd_partner)]

# check data
summary(ds)
sapply(ds, function(x) sum(is.na(x)))

# save data
write.table(ds, './DATA/OBSERVATIONS.txt', quote = TRUE, sep = '\t', row.names = FALSE)



