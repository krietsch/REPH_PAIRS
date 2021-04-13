#==============================================================================================================
# How to best define interactions
#==============================================================================================================

# Summary
# 1. Data available
# 2. Data until 
# 3. Data linked to nests

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Comparison 10 min intervals vs. closest time
#--------------------------------------------------------------------------------------------------------------

# Data
dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
dp[, time_btw := abs(as.numeric(difftime(datetime_1, datetime_2, units = 'min')))]

dpn = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table

# comparisions
dp %>% nrow
dpn %>% nrow

# difference between positions in time
ggplot(data = dp) +
  geom_histogram(aes(time_btw))

ggplot(data = dpn) +
  geom_histogram(aes(time_btw))

# number of interactions
dp[, interaction := distance < 30]
dpn[, interaction := distance_pair < 30]

dp[interaction == TRUE] %>% nrow
dpn[interaction == TRUE] %>% nrow

# subset pair
dps = dp[ID1 == 270170746 & ID2 == 270170747] # R304_18
dpns = dpn[ID1 == 270170746 & ID2 == 270170747] # R304_18

dps[interaction == TRUE] %>% nrow / dps %>% nrow * 100
dpns[interaction == TRUE] %>% nrow / dpns %>% nrow * 100

#--------------------------------------------------------------------------------------------------------------
#' # Define breeding pairs with both sexes tagged
#--------------------------------------------------------------------------------------------------------------

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = 'ID')

# ID as character
d[, ID := as.character(ID)]
dp[, ID1 := as.character(ID1)]
dp[, ID2 := as.character(ID2)]
dID[, ID := as.character(ID)]
dn[, male_id := as.character(male_id)]
dn[, female_id := as.character(female_id)]

# check if data overlap
dn = merge(dn, dID[, .(male_id = ID, start_m = start, end_m = end)], by = 'male_id', all.x = TRUE)
dn = merge(dn, dID[, .(female_id = ID, start_f = start, end_f = end)], by = 'female_id', all.x = TRUE)

# subset nests with both IDs tagged
dn = dn[!is.na(start_m) & !is.na(start_f)]

# subset nests with both IDs tagged and overlapping time intervals
dn[, overlap := DescTools::Overlap(c(start_m, end_m), c(start_f, end_f)), by = nestID]
dn = dn[overlap > 0]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y)]
dnID = unique(dnID, by = 'nestID')

#--------------------------------------------------------------------------------------------------------------
#' # Comparison 10 min intervals vs. closest time
#--------------------------------------------------------------------------------------------------------------

# Data
dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table










