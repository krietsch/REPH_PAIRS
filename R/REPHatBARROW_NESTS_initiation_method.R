#=========================================================================================================================
# REPHatBARROW NESTS initiation date method
#=========================================================================================================================

### Summary
# 1. Assign initiation date method external data
# 2. Assign initiation date method our data

# settings, con
options(stringsAsFactors = FALSE)
sapply( c('data.table', 'sdb','readxl','foreach', 'xlsx', 'anytime', 'magrittr', 'DBI'),
        require, character.only = TRUE)

#-------------------------------------------------------------------------------------------------------------------------
# 1. Assign initiation date method external data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, "select * FROM NESTS")
DBI::dbDisconnect(con)

# subset external data
d = d[external == 1]

# nests found incomplete
d[, found_incomplete := initial_clutch_size < clutch_size]

# initiation date methods
d[found_incomplete == TRUE, initiation_method := 0]
d[is.na(initiation_method) & !is.na(hatching_datetime), initiation_method := 1]
d[is.na(initiation_method) & !is.na(est_hatching_datetime), initiation_method := 2]
d[is.na(initiation_method) & !is.na(initiation), initiation_method := 2]

# UPDATE db
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
du = d[!is.na(initiation_method), .(initiation_method, pk)]

# save new values from d in a temp table
dbWriteTable(con, 'temp', du , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.initiation_method = t.initiation_method where n.pk = t.pk")
dbExecute(con,"drop table temp")

DBI::dbDisconnect(con)


#-------------------------------------------------------------------------------------------------------------------------
# 2. Assign initiation date method our data
#-------------------------------------------------------------------------------------------------------------------------

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, "select * FROM NESTS")
DBI::dbDisconnect(con)

# subset external data
d = d[external == 0]

# nests found incomplete
d[, found_incomplete := initial_clutch_size < clutch_size]

# initiation date methods
d[found_incomplete == TRUE, initiation_method := 0]
d[is.na(initiation_method) & !is.na(hatching_datetime), initiation_method := 1]
d[is.na(initiation_method) & !is.na(est_hatching_datetime), initiation_method := 2]

# by detailed observations
d[year_ == 2019 & nest == 'R612', initiation_method := 5]
d[year_ == 2019 & nest == 'R401', initiation_method := 5]

d[is.na(initiation_method) & !is.na(initiation), initiation_method := 3]

d[comments %like% 'GPS' & initiation_method != 3, initiation_method := 4]

# UPDATE db
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
du = d[!is.na(initiation_method), .(initiation_method, pk)]

# save new values from d in a temp table
dbWriteTable(con, 'temp', du , row.names = FALSE)

# update target table based on values in temp table
dbExecute(con, "update NESTS n, temp t set n.initiation_method = t.initiation_method where n.pk = t.pk")
dbExecute(con,"drop table temp")

DBI::dbDisconnect(con)





