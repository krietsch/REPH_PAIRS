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

# raw data+
f18 = list.files('//ds/raw_data_kemp/FIELD/Barrow/2018/DATA/RAW_DATA/NANO_TAG_DATA', pattern = '.csv') %>% data.table
f18[, year_ := 2018]
f19 = list.files('//ds/raw_data_kemp/FIELD/Barrow/2019/DATA/RAW_DATA/NANO_TAG_DATA', pattern = '.csv') %>% data.table
f19[, year_ := 2019]
fr = rbind(f18, f19)
setnames(fr, '.', 'file_name')

#-------------------------------------------------------------------------------------------------------------------------
# Load raw data
#-------------------------------------------------------------------------------------------------------------------------

# merge all files
o = foreach(i = 1:nrow(fr), .combine = 'rbind') %do% {
  
  x = fread(fr[i, file_path], fill = TRUE)
  x[, year_ := fr[i, year_]]
  x[, file_name := fr[i, file_name]]
  x
  
}


# file path 
fr[, file_path := paste0('//ds/raw_data_kemp/FIELD/Barrow/', year_, '/DATA/RAW_DATA/NANO_TAG_DATA/', file_name)]


fr[13,]

xt = fread(fr[13, file_path])


<<967274;2019;6;13;4;49;33;0;;;;;;;;;;;;;71194002N;156396590W;;0.0;0;4.050;;;;71.3233327+/156.6609833;;1T;2019-06-13T04:49:33.000>>
#-------------------------------------------------------------------------------------------------------------------------
# Compare database with raw data
#-------------------------------------------------------------------------------------------------------------------------




function (dir = "~/ownCloud/RAW_DATA/NANO_TAG_DATA/", db) 
{
  if (missing(db)) 
    db = yy2dbnam(data.table::year(Sys.Date()))
  allff = data.table(f = list.files(dir, pattern = ".csv"))
  doneff = idbq(q = "select distinct filename f from NANO_TAGS")[, 
                                                                 `:=`(done, 1)]
  newff = merge(allff, doneff, byx = "f", all.x = TRUE)[is.na(done)]
  con = dbConnect(RMySQL::MySQL(), host = ip(), user = getOption("wader.user"), 
                  db = db, password = pwd())
  on.exit(dbDisconnect(con))
  O = foreach(i = 1:nrow(newff), .combine = rbind, .errorhandling = "remove") %do% 
    {
      di = fread(paste(dir, newff[i, f], sep = "/"))
      setnames(di, make.names(names(di)))
      di = di[, .(Device.ID, Timestamp, Latitude.decimal, 
                  Longitude.decimal, Altitude, Speed, Battery.voltage)]
      di[, `:=`(Timestamp, anytime(Timestamp, asUTC = TRUE, 
                                   tz = "UTC"))]
      di[, `:=`(filenam, newff[i, f])]
      di
    }
  if (inherits(O, "data.table")) {
    O[, `:=`(pk, NA)]
    O[, `:=`(Device.ID, as.integer(Device.ID) - 967000)]
    dbnam = idbq(q = "select * from NANO_TAGS where false") %>% 
      names
    setnames(O, dbnam)
    out = dbWriteTable(con, "NANO_TAGS", O, append = TRUE, 
                       row.names = FALSE)
    return(out)
  }
  else FALSE
}










