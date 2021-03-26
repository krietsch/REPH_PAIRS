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
f18 = list.files('//ds/raw_data_kemp/FIELD/Barrow/2019/DATA/RAW_DATA/NANO_TAG_DATA', pattern = '.csv')
f19 = list.files('//ds/raw_data_kemp/FIELD/Barrow/2019/DATA/RAW_DATA/NANO_TAG_DATA', pattern = '.csv')

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










