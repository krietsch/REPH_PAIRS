#==============================================================================================================
# Subset breeders and merge with nest data
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'viridis', 'auksRuak', 'foreach', 'knitr', 
          'stringr', 'ggnewscale'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp = fread('./DATA/PAIR_WISE_INTERACTIONS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp[, year_ := year(datetime_1)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
dpa = dbq(con, 'select * FROM PATERNITY')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

#--------------------------------------------------------------------------------------------------------------
#' Sired or received EPP?
#--------------------------------------------------------------------------------------------------------------

# any EPP in nest?
dpa[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dpa[, any_EPY := any(EPY == 1), by = nestID]

# social male sired EPP in other nest
dpam = dpa[EPY == 1 & !is.na(IDfather)]
dpam[, m_sired_EPY := TRUE]

# merge with social nests
dpas = dpa[EPY == 0 & !is.na(IDfather)]
dpas = merge(dpas, dpam[, .(year_, IDfather, m_sired_EPY)], by = c('IDfather', 'year_'), all.x = TRUE )

# merge with nest data
dpau = unique(dpa, by = 'nestID')
dn = merge(dn, dpas[, .(nestID, any_EPY, m_sired_EPY)], by = 'nestID', all.x = TRUE)


#--------------------------------------------------------------------------------------------------------------
#' Define breeding pairs
#--------------------------------------------------------------------------------------------------------------

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = 'ID')

# check if data overlap
dn = merge(dn, dID[, .(year_, male_id = ID, start_m = start, end_m = end)], by = c('male_id', 'year_'), all.x = TRUE)
dn = merge(dn, dID[, .(year_, female_id = ID, start_f = start, end_f = end)], by = c('female_id', 'year_'), all.x = TRUE)

# subset nests with both IDs tagged
dn[, both_tagged := !is.na(start_m) & !is.na(start_f), by = nestID]

# subset nests with both IDs tagged and overlapping time intervals
dn[, overlap := DescTools::Overlap(c(start_m, end_m), c(start_f, end_f)), by = nestID]
dn[, both_tagged_overlapping := overlap > 0, by = nestID]
dn[is.na(both_tagged_overlapping), both_tagged_overlapping := FALSE]

# check overlap with initiation date
dn[, overlap_initiation_m := DescTools::Overlap(c(start_m, end_m), c(initiation - 86400, initiation + 86400)), by = nestID]
dn[, overlap_initiation_f := DescTools::Overlap(c(start_f, end_f), c(initiation - 86400, initiation + 86400)), by = nestID]
dn[, both_tagged_at_initiation := overlap_initiation_m > 0 & overlap_initiation_f > 0, by = nestID]
dn[is.na(both_tagged_at_initiation), both_tagged_at_initiation := FALSE]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date, any_EPY, m_sired_EPY, 
              lat_n = lat, lon_n = lon, overlap, both_tagged_overlapping)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# assign clutch order
setorder(dnID, male_id, initiation)
dnID[!is.na(male_id) & !is.na(female_id), clutch_together := seq_len(.N), by = .(year_, male_id, female_id)]
dnID[!is.na(male_id), male_clutch     := seq_len(.N), by = .(year_, male_id)]
dnID[!is.na(female_id), female_clutch := seq_len(.N), by = .(year_, female_id)]

# relative timing 
di = dn[!is.na(year_) & plot == 'NARL', .(initiation_mean = mean(initiation, na.rm = TRUE)), by = year_]

dp = merge(dp, di, by = 'year_', all.x = TRUE)
dp[, datetime_rel_season := difftime(datetime_1, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

# for plots subset study site nests
di = dn[!is.na(year_) & plot == 'NARL']
di[, initiation_mean := mean(initiation, na.rm = TRUE), by = year_]
di[, initiation_rel := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

# data available relative to clutch initiation for each ID
dnIDu = rbind(dnID[!is.na(male_id), .(year_, ID = male_id, sex = 'M', nestID, initiation, both_tagged_overlapping)],
              dnID[!is.na(female_id), .(year_, ID = female_id, sex = 'F', nestID, initiation, both_tagged_overlapping)])

# merge all with nest data
dID = merge(d, dnIDu, by = c('year_', 'ID'), all.x = TRUE, allow.cartesian = TRUE)
dID = dID[!is.na(nestID)]

# relative timing
dID[, datetime_rel_pair := difftime(datetime_, initiation, units = 'days') %>% as.numeric()]
dID[, datetime_rel_pair0 := round(datetime_rel_pair, 0)]

# unique by datetime_rel_pair0
dID = dID[, .(N = .N), by = c('year_', 'nestID', 'ID', 'sex', 'datetime_rel_pair0', 'both_tagged_overlapping')]

# check max
dID[, N] |> max()
142 # max possible 

# proportion of day with data
dID[, Np := N / 142]

# save data
fwrite(dID, './DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------
#' How many breeders with overlap? Divide first and second clutch nest data
#--------------------------------------------------------------------------------------------------------------

ds = dnID[!is.na(male_id) & !is.na(female_id) & year_ > 2017 & !is.na(overlap) & overlap > 0] %>% 
  unique(., by = c('male_id', 'female_id', 'nestID'))

ds[, N := .N, by = .(male_id, female_id)]
ds[N > 1]

# number of pairs with data for both at the same time
ds %>% unique(., by = c('male_id', 'female_id')) %>% nrow
ds[year_ == 2018] %>% unique(., by = c('male_id', 'female_id')) %>% nrow
ds[year_ == 2019] %>% unique(., by = c('male_id', 'female_id')) %>% nrow

# by nests 
ds %>% unique(., by = c('male_id', 'female_id', 'nestID')) %>% nrow

# look at pairs with two clutches 
ggplot(data = dp[ID1 == 270170620 & ID2 == 273145050]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-18 10:37:00')), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('Nest') +
  theme_classic()


ggplot(data = dp[ID1 == 270170938 & ID2 == 270170935]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-21 17:55:00')), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('Nest') +
  theme_classic()


ggplot(data = dp[ID1 == 273145126 & ID2 == 270170942]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-14 00:02:36')), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('Nest') +
  theme_classic()


ggplot(data = dp[ID1 == 273145139 & ID2 == 270170970]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = as.POSIXct('2019-06-16 15:33:59')), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('Nest') +
  theme_classic()

# decided to split based on date first clutch failed

# delete column 
dnID[, overlap := NULL]

#--------------------------------------------------------------------------------------------------------------
#' Merge dp with nest data
#--------------------------------------------------------------------------------------------------------------

# merge with first nest number
dp = merge(dp, dnID[clutch_together == 1, .(male_id, female_id, year_, clutch_together)], 
           by.x = c('ID1', 'ID2', 'year_'), by.y = c('male_id', 'female_id', 'year_'), all.x = TRUE)

# assign second clutches
dp[ID1 == 270170620 & ID2 == 273145050 & datetime_1 > as.POSIXct('2019-06-18 10:37:00'), clutch_together := 2]
dp[ID1 == 270170938 & ID2 == 270170935 & datetime_1 > as.POSIXct('2019-06-21 17:55:00'), clutch_together := 2]
dp[ID1 == 273145126 & ID2 == 270170942 & datetime_1 > as.POSIXct('2019-06-14 00:02:36'), clutch_together := 2]
dp[ID1 == 273145139 & ID2 == 270170970 & datetime_1 > as.POSIXct('2019-06-16 15:33:59'), clutch_together := 2]

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2', 'year_', 'clutch_together'), 
           by.y = c('male_id', 'female_id', 'year_', 'clutch_together'), all.x = TRUE)

# datetime relative to nest initiation date
dp[, datetime_rel_pair := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# date
dp[, date_ := as.Date(datetime_1)]

# same sex interaction?
dp[, same_sex := sex1 == sex2]

# breeding pair
dp[, breeding_pair := !is.na(nestID)]

#--------------------------------------------------------------------------------------------------------------
#' Nest attendance 
#--------------------------------------------------------------------------------------------------------------

# distance to nest
dps = dp[!is.na(nestID)]
dps[, distance_nest_1 := sqrt(sum((c(lon1, lat1) - c(lon_n, lat_n))^2)), by = 1:nrow(dps)]
dps[, at_nest1 := distance_nest_1 < 15]

dps[, distance_nest_2 := sqrt(sum((c(lon2, lat2) - c(lon_n, lat_n))^2)), by = 1:nrow(dps)]
dps[, at_nest2 := distance_nest_2 < 15]

# merge back with data
dp = merge(dp, dps[, .(ID1, ID2, datetime_1, nestID, at_nest1, at_nest2)], 
           by = c('ID1', 'ID2', 'datetime_1', 'nestID'), all.x = TRUE)

#--------------------------------------------------------------------------------------------------------------
#' Extra-pair interactions
#--------------------------------------------------------------------------------------------------------------

# summary by unique pair excluding pair wise duplicates
dps = dp[same_sex == FALSE & sex1 == 'M'] # because nests are merged with ID1 = male

# any interaction with other than breeding partner?
dps[, ID1_any_ep_int := any(interaction == TRUE & breeding_pair == FALSE), by = .(year_, ID1, datetime_1)]
dps1 = unique(dps, by = c('year_', 'ID1', 'datetime_1'))

dps[, ID2_any_ep_int := any(interaction == TRUE & breeding_pair == FALSE), by = .(year_, ID2, datetime_2)]
dps2 = unique(dps, by = c('year_', 'ID2', 'datetime_2'))

# merge with dp
dp = merge(dp, dps1[, .(year_, ID1, datetime_1, ID1_any_ep_int)], 
           by = c('year_', 'ID1', 'datetime_1'), all.x = TRUE)

dp = merge(dp, dps2[, .(year_, ID2, datetime_2, ID2_any_ep_int)], 
           by = c('year_', 'ID2', 'datetime_2'), all.x = TRUE)

#--------------------------------------------------------------------------------------------------------------
#' Same sex interactions
#--------------------------------------------------------------------------------------------------------------

# summary by unique pair excluding pair wise duplicates
dps = dp[same_sex == TRUE] # because nests are merged with ID1 = male

# any interaction with other than breeding partner?
dps[, ID1_any_same_int := any(interaction == TRUE & breeding_pair == FALSE), by = .(year_, ID1, datetime_1)]
dps1 = unique(dps, by = c('year_', 'ID1', 'datetime_1'))

dps[, ID2_any_same_int := any(interaction == TRUE & breeding_pair == FALSE), by = .(year_, ID2, datetime_2)]
dps2 = unique(dps, by = c('year_', 'ID2', 'datetime_2'))

# merge with dp
dp = merge(dp, dps1[, .(year_, ID1, datetime_1, ID1_any_same_int)], 
           by = c('year_', 'ID1', 'datetime_1'), all.x = TRUE)

dp = merge(dp, dps2[, .(year_, ID2, datetime_2, ID2_any_same_int)], 
           by = c('year_', 'ID2', 'datetime_2'), all.x = TRUE)

#--------------------------------------------------------------------------------------------------------------
#' Subset relevant data
#--------------------------------------------------------------------------------------------------------------

# round to days
dp[, datetime_rel_pair0 := round(datetime_rel_pair, 0)]
dp[, datetime_rel_season0 := round(datetime_rel_season, 0)]

# N pairwise data per day
dpn = dp[!is.na(datetime_rel_pair0), .N, by = .(pairID, nestID, datetime_rel_pair0)]

# check max
dpn[, N] |> max()

# proportion of locations send
dpn[, Np := N / 142]

# merge with dp
dp = merge(dp, dpn, by = c('pairID', 'nestID', 'datetime_rel_pair0'), all.x = TRUE)

# subset 
dps = 
dp[breeding_pair == TRUE & sex1 == 'M', 
   .(pairID, year_, ID1, ID2, sex1, sex2, datetime_1, datetime_2, N, Np, date_, datetime_rel_season, 
     datetime_rel_season0, datetime_rel_pair, datetime_rel_pair0, interaction, split, merge, nestID, 
     at_nest1, at_nest2, female_clutch, any_EPY, ID1_any_ep_int, ID2_any_ep_int, ID1_any_same_int, ID2_any_same_int, 
     breeding_pair, type = 'breeding pair')]

# save data
fwrite(dps, './DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------
#' # Randomization for interaction base line
#--------------------------------------------------------------------------------------------------------------

# unique data
du = unique(dp, by = c('pairID'))
ds = unique(dp, by = c('nestID', 'datetime_rel_pair0'))
ds = ds[!is.na(datetime_rel_pair0)]

# nests to exclude (second clutches)
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]

# data needed for null model
d0 = ds[, .(datetime_rel_pair0, date_)] %>% unique

# subset non-breeders pairs within breeders
breeding_males = du[breeding_pair == TRUE]$ID1
breeding_females = du[breeding_pair == TRUE]$ID2

dps = dp[breeding_pair == FALSE & ID1 %in% breeding_males & ID2 %in% breeding_females]

# subset all interactions on days with relative initiation date for breeders
x = d0$datetime_rel_pair0 %>% unique

d0a = foreach(i = x, .combine = 'rbind') %do% {
  
  # subset relative day
  dss = d0[datetime_rel_pair0 == i]
  
  # subset all pairwise interactions from this date 
  dsms = dps[date_ %in% dss$date_]
  
  # assign type
  dsms[, datetime_rel_pair0 := i]
  
  dsms
  
}

d0a[, datetime_rel_pair0_type := 'null_model']


# subset pairwise data with at least one interaction per day

# pairwise positions non-breeder pairs
dpn = d0a[, .N, by = .(pairID, date_, datetime_rel_pair0)]
setnames(dpn, 'N', 'Nnb')

# proportion of locations send
dpn[, Np_nb := Nnb / 142]

d0a = merge(d0a, dpn, by = c('pairID', 'date_', 'datetime_rel_pair0'), all.x = TRUE)

# Proportion of time together
dps = d0a[interaction == TRUE, .(N_int_nb = .N), by = .(pairID, date_, datetime_rel_pair0)]
du = unique(d0a, by = c('pairID', 'date_', 'datetime_rel_pair0'))
du = merge(du, dps, by = c('pairID', 'date_', 'datetime_rel_pair0'), all.x = TRUE)
du[is.na(N_int_nb), N_int_nb := 0]
du[, int_prop_nb := N_int_nb / Nnb]

# merge back with full table
d0a = merge(d0a, du[, .(pairID, date_, datetime_rel_pair0, N_int_nb, int_prop_nb)], 
            by = c('pairID', 'date_', 'datetime_rel_pair0'), all.x = TRUE)

# subset only pairs with at least one interaction
d0a = d0a[int_prop_nb > 0]

# subset pairs with at least 50% data
d0a = d0a[Np_nb >= 0.5]

# select 31 random pairs for each day 
ds = unique(d0a, by = c('pairID', 'date_', 'datetime_rel_pair0'))

# shuffle data
ds = ds[sample(dim(ds)[1])]

# subset 31 pairs
ds[, datetime_rel_pair0_id := seq_len(.N), by = datetime_rel_pair0]
ds = ds[datetime_rel_pair0_id <= 50]
ds[, sub := paste0(pairID, '_', date_, '_', datetime_rel_pair0)]
d0a[, sub := paste0(pairID, '_', date_, '_', datetime_rel_pair0)]

d0as = d0a[sub %in% ds$sub]

ds[datetime_rel_pair0 >= -10 & datetime_rel_pair0 <= 10, .N, datetime_rel_pair0]

# rename order and save
d0as = d0as[, 
        .(pairID, year_, ID1, ID2, sex1, sex2, datetime_1, datetime_2, N = Nnb, Np = Np_nb, date_, 
          datetime_rel_season, datetime_rel_season0, datetime_rel_pair, datetime_rel_pair0, interaction, 
          split, merge, nestID, at_nest1, at_nest2, any_EPY, ID1_any_ep_int, ID2_any_ep_int, breeding_pair, 
          type = 'randomization')]

# save data
fwrite(d0as, './DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS_RANDOM.txt', quote = TRUE, sep = '\t', row.names = FALSE)



