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
d = dbq(con, 'select * FROM RESIGHTINGS')
DBI::dbDisconnect(con)

# Change projection
d[, lat_dec := lat]
d[, lon_dec := lon]
st_transform_DT(d)

point_over_poly_DT(d, poly = study_site, buffer = 0)
setnames(d, 'poly_overlap', 'study_site')

setorder(d, datetime_)

# subset data relevant for this study
d = d[, .(obs_id, datetime_, lat = lat_dec, lon = lon_dec, study_site, sex, UL, UR, LL, LR, ID, habitat, 
          aggres, displ, cop, cop_inv, flight, voc, maint, spin, comments)]

# check data
summary(d)
sapply(d, function(x) sum(is.na(x)))

# save data
# write.table(d, './DATA/OBSERVATIONS.txt', quote = TRUE, sep = '\t', row.names = FALSE)

