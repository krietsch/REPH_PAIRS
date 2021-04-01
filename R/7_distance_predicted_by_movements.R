#==============================================================================================================
# Does the distance between a pair can be explained by the distance before 
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
dp[, year_ := year(datetime_10min)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Define breeding pairs with both sexes tagged
#--------------------------------------------------------------------------------------------------------------

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
#' # Calculate delta distances
#--------------------------------------------------------------------------------------------------------------

# merge with nests
dp = merge(dp, dnID, by.x = c('year_', 'ID1', 'ID2'), by.y = c('year_', 'male_id', 'female_id'), all.x = TRUE, allow.cartesian=TRUE)

# subset data of breeding pairs
dp = dp[!is.na(nestID)]

# assign sex
dp[, sex1 := 'M']
dp[, sex2 := 'F']

# shift positions
dp[, lat1_next := shift(lat1, type = 'lead'), by = nestID]
dp[, lon1_next := shift(lon1, type = 'lead'), by = nestID]
dp[, lat2_next := shift(lat2, type = 'lead'), by = nestID]
dp[, lon2_next := shift(lon2, type = 'lead'), by = nestID]

# distances pair next
dp[, distance_pair_next := sqrt(sum((c(lon1_next, lat1_next) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]

# distance male and female next
dp[, delta_male_distance := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)) , by = 1:nrow(dp)]
dp[, delta_female_distance := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]

# delta difference in pair distance
dp[, delta_pair_distance := distance - distance_pair_next, by = 1:nrow(dp)]
dp[, delta_pair_distance := distance - distance_pair_next, by = 1:nrow(dp)]


#--------------------------------------------------------------------------------------------------------------
#' # Model change in within-pair distance 
#--------------------------------------------------------------------------------------------------------------

# Packages
sapply(c('lme4', 'effects', 'multcomp', 'gtools', 'emmeans', 'broom', 'MuMIn', 'nlme'),
       function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ) )

# exclude NA
dp = dp[!is.na(delta_pair_distance)]

# model within pair distance change
fm = lme(delta_pair_distance ~ delta_male_distance + delta_female_distance, random =  (~1 | nestID), data = dp)

plot(ACF(fm, resType = 'normalized'), alpha=0.01)

# runs forever
# fm1 = update(fm, correlation = corARMA(  form = ~ 1 | nestID, p = 2, q = 2) )
# fm2 = update(fm, correlation = corAR1(  form = ~ 1 | nestID) )
# 
# model.sel(fm, fm1, fm2)
# 
# plot(ACF(fm, resType = 'normalized'), alpha=0.01)


plot(allEffects(fm))
glht(fm) %>% summary
summary(fm)

# look at raw data
ggplot(data = dp) +
  geom_point(aes(delta_male_distance, delta_pair_distance, color = nestID), show.legend = FALSE)

ggplot(data = dp) +
  geom_point(aes(delta_female_distance, delta_pair_distance, color = nestID), show.legend = FALSE)


# distribution delta within-pair distance
ggplot(data = dp) +
  geom_density(aes(delta_pair_distance)) +
  xlim(-200, 200)

# define together based on distance threshold
dp[, interaction := distance < 30]
dp[, interaction_next := distance_pair_next < 30]

# define movements in the same way
dp[, male_movement := delta_male_distance > 30]
dp[, female_movement := delta_female_distance > 30]

# subset splits
ds = dp[interaction == TRUE & interaction_next == FALSE]

# subset data when only one moved
ds = ds[male_movement != female_movement]

ds[, .N, by = male_movement]


# splits relative to initiation date
ds[, datetime_y := as.POSIXct(format(datetime_10min, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
ds[, datetime_rel := difftime(datetime_y, initiation_y, units = 'days') %>% as.numeric() %>% round(0)]

dss = ds[, .N, by = datetime_rel]

ggplot(data = dss) +
  geom_bar(aes(datetime_rel, N), stat = 'identity')


# model within pair distance change for splits
fm = lme(delta_pair_distance ~ delta_male_distance + delta_female_distance, random =  (~1 | nestID), data = ds)

plot(allEffects(fm))
glht(fm) %>% summary
summary(fm)


ggplot(data = ds) +
  geom_point(aes(delta_male_distance, delta_pair_distance, color = nestID), show.legend = FALSE)

ggplot(data = ds) +
  geom_point(aes(delta_female_distance, delta_pair_distance, color = nestID), show.legend = FALSE)


# subset merges
ds = dp[interaction == FALSE & interaction_next == TRUE]

# subset data when only one moved
ds = ds[male_movement != female_movement]

ds[, .N, by = male_movement]













