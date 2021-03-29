#=========================================================================================================================
# REPHatBARROW NANO_TAGS Check missing data
#=========================================================================================================================

# settings, con
options(stringsAsFactors = FALSE)
sapply( c('data.table', 'sdb','readxl','foreach', 'xlsx', 'wadeR', 'sdbvis', 'sp', 'rgeos', 'anytime'),
        require, character.only = TRUE)

# database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
f = dbq(con, 'select * FROM NANO_TAGS WHERE FALSE') %>% names
d = dbq(con, q = 'SELECT * FROM NANO_TAGS') %>% as.data.table
cdb = dbq(con, q = 'SELECT * FROM REPHatBARROW.CAPTURES') %>% as.data.table

drdb18 =  dbq(con, q = 'SELECT * FROM FIELD_2018_REPHatBARROW.NANO_TAGS') %>% as.data.table
drdb19 =  dbq(con, q = 'SELECT * FROM FIELD_2019_REPHatBARROW.NANO_TAGS') %>% as.data.table
drdb = rbind(drdb18, drdb19, fill = TRUE) 

# raw data
f18 = list.files('//ds/raw_data_kemp/FIELD/Barrow/2018/DATA/RAW_DATA/NANO_TAG_DATA', pattern = '.csv') %>% data.table
f18[, year_ := 2018]
f19 = list.files('//ds/raw_data_kemp/FIELD/Barrow/2019/DATA/RAW_DATA/NANO_TAG_DATA', pattern = '.csv') %>% data.table
f19[, year_ := 2019]
fr = rbind(f18, f19)
setnames(fr, '.', 'file_name')

#-------------------------------------------------------------------------------------------------------------------------
# Load raw data
#-------------------------------------------------------------------------------------------------------------------------

# file path 
fr[, file_path := paste0('//ds/raw_data_kemp/FIELD/Barrow/', year_, '/DATA/RAW_DATA/NANO_TAG_DATA/', file_name)]

# merge all files
o = foreach(i = 1:nrow(fr), .combine = 'rbind') %do% {
  
  x = fread(fr[i, file_path], fill = TRUE)
  x[, year_ := fr[i, year_]]
  x[, file_name := fr[i, file_name]]
  x
  
}

# rename table
dr = data.table(tagID = (as.integer(o[['Device ID']]) - 967000),
                datetime_ = as.character(anytime(o[, Timestamp], asUTC = TRUE, tz = "UTC")),    
                lat = o[['Latitude decimal']],   
                lon = o[['Longitude decimal']],
                altitude = o[, Altitude],
                gps_speed = o[, Speed], 
                batvolt = o[['Battery voltage']],                                   
                filename = o[, file_name],
                pk = NA)

#-------------------------------------------------------------------------------------------------------------------------
# Compare database with raw data
#-------------------------------------------------------------------------------------------------------------------------

# change timezone to AKDT
dr[, datetime_ := anytime(datetime_, asUTC = TRUE, tz = 'America/Anchorage')]
dr[, datetime_ := anytime(as.character(datetime_))]
setorder(dr, tagID, datetime_)

# subset REPH
dr = dr[tagID < 301]

# correct tag with tagID given twice
dr[tagID == 146 & datetime_ > anytime('2019-06-25 00:00:00'), tagID := 148]

# exclude NA
# dr = dr[!is.na(lon)]

# Information on tag deployment
cdb[, IDC :=  paste0(UL, '-', UR, '/', LL, '-', LR)]
cdb[, tag_attached := anytime(released_time)]

c_on = cdb[!is.na(gps_tag),  .(tag_attached, ID, IDC), by = gps_tag]
dl = dr[, .(last_position = max(datetime_, na.rm = TRUE)), by = tagID]

ds = merge(c_on, dl, by.x = 'gps_tag', by.y = 'tagID')
ds = ds[, .(gps_tag, ID, IDC, tag_attached, last_position, last_on_bird = NA, found = NA)]

# load data
c_on = read_excel('//ds/raw_data_kemp/FIELD/Barrow/2019/DATA/RAW_DATA/NANO_TAGS_LAST_ON_BIRD_FILLED.xlsx') %>% data.table 
c_on[is.na(found), found := FALSE]

# merge with new data
c_on = merge(c_on, ds[, .(gps_tag, ID, last_position_new = last_position)], by = c('gps_tag', 'ID'))

c_on[, tag_attached  :=  anytime(as.character(tag_attached))]
c_on[, last_position :=  anytime(as.character(last_position))]
c_on[, last_on_bird  :=  anytime(as.character(last_on_bird))]

# compare last position
c_on[, last_position_compared := last_position == last_position_new]

# tim additional data
c_on[, difftime_additional := as.numeric(difftime(last_position_new, last_position, units = 'days'))]
c_on[last_position_compared == FALSE]
c_on[last_position_compared == FALSE, sum(difftime_additional)]

# transform data in table to create kml
d_plot = function(x){
  ds = data.table(id = x$tagID,
                  datetime_ = x$datetime_,
                  lat = x$lat,
                  lon = x$lon)
  ds
  
}

# loop for each by ID with more data
# bird_id = c_on[last_position_compared == FALSE]$gps_tag
# 
# foreach(i = bird_id) %do%
#   kml(dat = d_plot(dr[tagID == i]), file = paste0('//ds/raw_data_kemp/FIELD/Barrow/2019/DATA/KLM_BY_ID/ID_', i,'_DATA_new.kml'), scale = 0.5)


# load data
c_on = read_excel('//ds/raw_data_kemp/FIELD/Barrow/2019/DATA/RAW_DATA/NANO_TAGS_LAST_ON_BIRD_FILLED_2.xlsx') %>% data.table 
c_on[is.na(found), found := FALSE]

c_on[, tag_attached  :=  anytime(as.character(tag_attached))]
c_on[, last_position :=  anytime(as.character(last_position))]
c_on[, last_on_bird  :=  anytime(as.character(last_on_bird))]

# time sending data
c_on[, difftime_ := as.numeric(difftime(last_on_bird, tag_attached, units = 'days'))]

plot(difftime_ ~ tag_attached, c_on)
hist(c_on$difftime_, breaks = c(30))

setorder(c_on, -difftime_)

# merge data with metal ID
dr = merge(dr, c_on[, .(gps_tag, ID, tag_attached, last_on_bird)], by.x = 'tagID', by.y = 'gps_tag', all.x = TRUE, allow.cartesian = TRUE)
dr[, tag_on := datetime_ >= tag_attached & datetime_ <= last_on_bird, by = 1:nrow(dr)]
drx = dr[tag_on == TRUE]

d = d[ID != 999]

# add merge for 2018!




