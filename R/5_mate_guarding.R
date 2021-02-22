#==============================================================================================================
# Mateguarding
#==============================================================================================================

# Summary
# 1. Data available
# 2. Data until 
# 3. Data linked to nests

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# read data
# dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
# dpn = fread('./DATA/PAIR_WISE_DIST_NESTS.txt', sep = '\t', header = TRUE) %>% data.table
# d = rbind(dp, dpn)
# d[, datetime_ := as.POSIXct(datetime_10min)]
# d[, datetime_y := as.POSIXct(format(datetime_10min, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
# d[, date_ := as.Date(datetime_10min)]
# # bird or nest interaction
# d[, type := ifelse(ID2 %like% 'R', 'nest', 'individual')]
# fwrite(d, './DATA/PAIR_WISE_DIST_NESTS_AND_ID_DUP.txt', quote = TRUE, sep = '\t', row.names = FALSE)
d = fread('./DATA/PAIR_WISE_DIST_NESTS_AND_ID_DUP.txt', sep = '\t', header = TRUE) %>% data.table

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

# interactions
d[, interaction := distance < 10]

# nest data
dnID = rbind(dn[, .(year_, ID = female_id, nestID, initiation, sex = 'F')], dn[, .(year_, ID = male_id, nestID, initiation, sex = 'M')])
dnID[, ID := factor(ID)]
dnID = unique(dnID[!is.na(ID)], by = c('year_', 'ID'))

# which where tagged?
IDu = unique(d[, ID1])
dnID = dnID[ID %in% IDu]

# both or only one?
dnID[, N_tagged := .N, by = nestID]
dnID = dnID[N_tagged == 2]


ds = merge(ds, dnID[, .(year_, ID, status = 'breeder')], by = c('year_', 'ID'), all.x = TRUE)
ds[is.na(status), status := 'non-breeder']












