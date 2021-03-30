#=========================================================================================================================
# REPHatBARROW NANO_TAGS delete duplicates in test data 
#=========================================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr'), 
        require, character.only = TRUE)

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID == 999] # exclude test data
DBI::dbDisconnect(con)

# duplicates (added by mistake when adding missing data)
d[, duplicated := duplicated(d, by = c('lat', 'lon', 'datetime_'))]
ds = d[duplicated == TRUE]

# check only 999
ds$ID %>% unique

# delete rows in db
pktoupdate = paste(ds$pk, collapse = ",")
dbExecute(con, paste("DELETE FROM NANO_TAGS WHERE pk IN (", pktoupdate, ")")  )


