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
dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, year_ := year(datetime_1)]

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
dp[, distance_btw_1 := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)) , by = 1:nrow(dp)]
dp[, distance_btw_2 := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]

# delta difference in pair distance
dp[, distance_btw_pair := distance_pair - distance_pair_next, by = 1:nrow(dp)]

#--------------------------------------------------------------------------------------------------------------
#' # Define dynamic interaction based on speed
#--------------------------------------------------------------------------------------------------------------

# time between consecutive points
dp[, datetime_1_next := data.table::shift(datetime_1, type = 'lead'), by = ID1]
dp[, datetime_2_next := data.table::shift(datetime_2, type = 'lead'), by = ID2]
dp[, time_btw_1 := as.numeric(difftime(datetime_1_next, datetime_1, units = 'sec'))]
dp[, time_btw_2 := as.numeric(difftime(datetime_2_next, datetime_2, units = 'sec'))]
dp[, time_btw_pair := abs(as.numeric(difftime(datetime_1, datetime_2, units = 'sec')))]

# ground speed 
dp[, speed_1 := distance_btw_1/ time_btw_1]
dp[, speed_2 := distance_btw_2/ time_btw_2]

# how far could bird have moved in between points
dp[, distance_travelled_1 := time_btw_pair * speed_1]
dp[, distance_travelled_2 := time_btw_pair * speed_2]
dp[, distance_travelled_pair := time_btw_pair * max(speed_1, speed_2), by = 1:nrow(dp)]

# interaction based on distance threshold
dp[, interaction := distance_pair < 30]
dp[, interaction_time_btw := distance_pair < 30 + distance_travelled_pair]

# first and last interaction
dp[interaction == TRUE, first_int := min(datetime_1), by = nestID]
dp[, first_int := min(first_int, na.rm = TRUE), by = nestID]
dp[interaction == TRUE, last_int  := max(datetime_1), by = nestID]
dp[, last_int := max(last_int, na.rm = TRUE), by = nestID]

# subset period with interactions
dp = dp[datetime_1 > first_int & datetime_1 < last_int]

# relative nest initiation date
dp[, initiation_rel := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# subset four days before and after
dp = dp[initiation_rel > -4 & initiation_rel < 4]

#--------------------------------------------------------------------------------------------------------------
#' # Model change in within-pair distance 
#--------------------------------------------------------------------------------------------------------------

# Packages
sapply(c('lme4', 'effects', 'multcomp', 'gtools', 'emmeans', 'broom', 'MuMIn', 'nlme'),
       function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE) ) )

# exclude NA
dp = dp[!is.na(distance_btw_pair)]

# # model within pair distance change
# fm = lme(distance_btw_pair ~ distance_btw_1 + distance_btw_2, random =  (~1 | nestID), data = dp)
# 
# plot(ACF(fm, resType = 'normalized'), alpha = 0.01)
# 
# # runs forever
# fm1 = update(fm, correlation = corARMA(  form = ~ 1 | nestID, p = 2, q = 2) )
# fm2 = update(fm, correlation = corAR1(  form = ~ 1 | nestID) )
# 
# model.sel(fm, fm1, fm2)
# 
# plot(ACF(fm1, resType = 'normalized'), alpha=0.01)

# # selected model
# fm1 = lme(distance_btw_pair ~ distance_btw_1 + distance_btw_2, random =  (~1 | nestID), 
#          correlation = corARMA(form = ~ 1 | nestID, p = 2, q = 2), data = dp)
# 
# plot(ACF(fm1, resType = 'normalized'), alpha=0.01)
# 
# plot(allEffects(fm1))
# glht(fm1) %>% summary
# summary(fm1)


hist(dp$distance_btw_1)
hist(dp$distance_btw_2)

fm1 = lm(distance_btw_pair ~ distance_btw_1 + distance_btw_2, data = dp)

plot(allEffects(fm1))
glht(fm1) %>% summary
summary(fm1)

# look at raw data
ggplot(data = dp) +
  geom_point(aes(distance_btw_1, distance_btw_pair, color = nestID), show.legend = FALSE)

ggplot(data = dp) +
  geom_point(aes(distance_btw_2, distance_btw_pair, color = nestID), show.legend = FALSE)







# subset < 1000 m
ds = dp[distance_btw_1 < 1000 & distance_btw_2 < 1000]

fm1 = lm(distance_btw_pair ~ distance_btw_1 + distance_btw_2, data = ds)

plot(allEffects(fm1))
glht(fm1) %>% summary
summary(fm1)


# look at raw data
ggplot(data = ds) +
  geom_point(aes(distance_btw_1, distance_btw_pair), alpha = 0.1, show.legend = FALSE) +
  theme_classic()

ggplot(data = ds) +
  geom_point(aes(distance_btw_2, distance_btw_pair), alpha = 0.1, show.legend = FALSE) +
  theme_classic()

ggplot(data = ds) +
  geom_point(aes(distance_btw_1, distance_btw_pair), color = 'dodgerblue2', alpha = 0.1, show.legend = FALSE) +
  geom_point(aes(distance_btw_2, distance_btw_pair), color = 'darkorange', alpha = 0.1, show.legend = FALSE) +
  theme_classic()


ggplot(data = ds) +
  geom_hex(aes(distance_btw_1, distance_btw_pair), show.legend = TRUE) +
  scale_fill_continuous(type = 'viridis', limits = c(0, 100)) 

ggplot(data = ds) +
  geom_hex(aes(distance_btw_2, distance_btw_pair), show.legend = TRUE) +
  scale_fill_continuous(type = 'viridis', limits = c(0, 100)) 




# distribution delta within-pair distance
ggplot(data = dp) +
  geom_density(aes(distance_btw_pair)) +
  xlim(-200, 200)

# define together based on distance threshold
dist_t = 200

dp[, interaction := distance < dist_t]
dp[, interaction_next := distance_pair_next < dist_t]

# define movements in the same way
dp[, male_movement := distance_btw_1 > dist_t]
dp[, female_movement := distance_btw_2 > dist_t]

# subset splits
ds = dp[interaction == TRUE & interaction_next == FALSE]

# subset data when only one moved
ds = ds[male_movement != female_movement]

ds[, .N, by = male_movement]


# splits relative to initiation date
ds[, datetime_y := as.POSIXct(format(datetime_1, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
ds[, datetime_rel := difftime(datetime_y, initiation_y, units = 'days') %>% as.numeric() %>% round(0)]

dss = ds[, .N, by = datetime_rel]

ggplot(data = dss) +
  geom_bar(aes(datetime_rel, N), stat = 'identity')


# model within pair distance change for splits
fm = lme(distance_btw_pair ~ distance_btw_1 + distance_btw_2, random =  (~1 | nestID), data = ds)

plot(allEffects(fm))
glht(fm) %>% summary
summary(fm)


ggplot(data = ds) +
  geom_point(aes(distance_btw_1, distance_btw_pair, color = nestID), show.legend = FALSE)

ggplot(data = ds) +
  geom_point(aes(distance_btw_2, distance_btw_pair, color = nestID), show.legend = FALSE)


# subset merges
ds = dp[interaction == FALSE & interaction_next == TRUE]

# subset data when only one moved
ds = ds[male_movement != female_movement]

ds[, .N, by = male_movement]













