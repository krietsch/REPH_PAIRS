#==============================================================================================================
# Subset breeders and merge with nest data
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'viridis', 'auksRuak', 'sf', 'foreach', 'knitr', 
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
d = fread('./DATA/NANO_TAGS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
d = d[filtered == TRUE]
st_transform_DT(d)

dp = fread('./DATA/PAIR_WISE_INTERACTIONS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table
dp[, year_ := year(datetime_1)]

dn = fread('./DATA/NESTS.txt', sep = '\t', header = TRUE, nThread = 20) %>% data.table

# change projection
st_transform_DT(dn)


#--------------------------------------------------------------------------------------------------------------
#' Define breeding pairs
#--------------------------------------------------------------------------------------------------------------

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = c('ID', 'year_'))

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
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, nest_state_date, anyEPY, m_tagged, f_tagged,
              lat_n = lat, lon_n = lon, female_clutch, male_clutch, clutch_together, overlap, both_tagged_overlapping)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# data available relative to clutch initiation for each ID
dnIDu = rbind(dnID[!is.na(male_id), .(year_, ID = male_id, nestID, initiation, both_tagged_overlapping)],
              dnID[!is.na(female_id), .(year_, ID = female_id, nestID, initiation, both_tagged_overlapping)])


# nests of tagged birds
ds = unique(dnID[m_tagged == TRUE | f_tagged == TRUE], by = c('male_id', 'female_id', 'nestID'))
ds[, .N, by = year_]
ds[m_tagged == TRUE, .N, by = year_]
ds[f_tagged == TRUE, .N, by = year_]
ds[f_tagged == TRUE & m_tagged == TRUE, .N, by = year_]
ds[both_tagged_overlapping == TRUE, .N, by = year_]

# by ID
ds = unique(dnID[m_tagged == TRUE | f_tagged == TRUE], by = c('male_id', 'female_id'))
ds[both_tagged_overlapping == TRUE, .N, by = year_] # four replacement clutches of same pair

# merge all with nest data
dID = merge(d, dnIDu, by = c('year_', 'ID'), all.x = TRUE, allow.cartesian = TRUE)
dID = dID[!is.na(nestID)]

# relative timing
dID[, initiation_day := as.Date(initiation)]
dID[, date_ := as.Date(datetime_)]
dID[, date_rel_pair := difftime(date_, initiation_day, units = 'days') %>% as.numeric()]

# unique by date_rel_pair
dID = dID[, .(N = .N), by = c('year_', 'nestID', 'ID', 'sex', 'date_rel_pair', 'both_tagged_overlapping')]

# check max
dID[, N] |> max()
142 # max possible 

# proportion of day with data
dID[, Np := N / 142]

# save data
# fwrite(dID, './DATA/NANO_TAGS_UNIQUE_BY_DAY.txt', quote = TRUE, sep = '\t', row.names = FALSE)

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

unique(dp[!is.na(nestID)], by = 'nestID') |> nrow()

# datetime relative to nest initiation date
dp[, datetime_rel_pair := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# datetime relative to nest initiation date
dp[, date_ := as.Date(datetime_1)]
dp[, initiation_day := as.Date(initiation)]
dp[, date_rel_pair := difftime(date_, initiation_day, units = 'days') %>% as.numeric()]

# same sex interaction?
dp[, same_sex := sex1 == sex2]

# breeding pair
dp[, breeding_pair := !is.na(nestID)]

# Assign polyandry
# polyandrous females
xf = dn[polyandrous == TRUE]$female_id |> unique()
dp[ID2 %in% xf, f_polyandrous := TRUE]
dp[is.na(f_polyandrous), f_polyandrous := FALSE]

# first nests
x1 = dn[polyandrous == TRUE & female_clutch == 1]$nestID |> unique()
dp[nestID %in% x1, f_polyandrous_first := TRUE]
dp[is.na(f_polyandrous_first), f_polyandrous_first := FALSE]

# second nest
x2 = dn[polyandrous == TRUE & female_clutch == 2]$nestID |> unique()
dp[nestID %in% x2, f_polyandrous_second := TRUE]
dp[is.na(f_polyandrous_second), f_polyandrous_second := FALSE]

# renesting females
# first nests of renesting females
x1 = dn[polyandrous == FALSE & female_clutch == 1 & N_female_clutch > 1 |
          polyandrous == TRUE & female_clutch == 2 & N_female_clutch > 2]$nestID |> unique()
dp[nestID %in% x1, f_renesting_first := TRUE]

# second nests of renesting females
x2 = dn[polyandrous == FALSE & female_clutch == 2 & N_female_clutch > 1 |
          polyandrous == TRUE & female_clutch == 3 & N_female_clutch > 2]$nestID |> unique()
dp[nestID %in% x2, f_renesting_second := TRUE]

# relative timing 
dn[, initiation_day := as.Date(initiation)]
di = dn[!is.na(year_) & data_type == 'study_site', .(initiation_mean = mean(initiation_day, na.rm = TRUE)), by = year_]
di[, initiation_mean := as.character(initiation_mean) |> as.Date()]

dp = merge(dp, di, by = 'year_', all.x = TRUE)

# relative initiation date
dp[, initiation_rel := difftime(initiation_day, initiation_mean, units = 'days') %>% as.numeric]

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
#' Subset relevant data
#--------------------------------------------------------------------------------------------------------------

# N pairwise data per day
dpn = dp[!is.na(date_rel_pair), .N, by = .(pairID, nestID, date_rel_pair)]

# check max
dpn[, N] |> max()

# proportion of locations send
dpn[, Np := N / 142]

# merge with dp
dp = merge(dp, dpn, by = c('pairID', 'nestID', 'date_rel_pair'), all.x = TRUE)

# subset 
dps = 
dp[breeding_pair == TRUE & sex1 == 'M', 
   .(pairID, year_, ID1, ID2, sex1, sex2, datetime_1, datetime_2, N, Np, date_, date_rel_pair, datetime_rel_pair, 
     distance_pair, interaction, split, merge, nestID, initiation, initiation_day, initiation_rel, at_nest1, 
     at_nest2, female_clutch, anyEPY, breeding_pair, f_polyandrous, f_polyandrous_first,
     f_polyandrous_second, f_renesting_first, f_renesting_second, type = 'breeding pair')]

# save data
# fwrite(dps, './DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS.txt', quote = TRUE, sep = '\t', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------
#' # Randomization for interaction base line
#--------------------------------------------------------------------------------------------------------------

# unique data
du = unique(dp, by = c('pairID'))
ds = unique(dp, by = c('nestID', 'date_rel_pair'))
ds = ds[!is.na(date_rel_pair)]

# nests to exclude (second clutches)
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]

# data needed for null model
d0 = ds[, .(date_rel_pair, date_)] %>% unique

# subset non-breeders pairs within breeders
breeding_males = du[breeding_pair == TRUE]$ID1
breeding_females = du[breeding_pair == TRUE]$ID2

dps = dp[breeding_pair == FALSE & ID1 %in% breeding_males & ID2 %in% breeding_females]

# subset all interactions on days with relative initiation date for breeders
x = d0$date_rel_pair %>% unique

d0a = foreach(i = x, .combine = 'rbind') %do% {

  # subset relative day
  dss = d0[date_rel_pair == i]

  # subset all pairwise interactions from this date
  dsms = dps[date_ %in% dss$date_]

  # assign type
  dsms[, date_rel_pair := i]

  dsms

}

d0a[, date_rel_pair_type := 'null_model']


# subset pairwise data with at least one interaction per day

# pairwise positions non-breeder pairs
dpn = d0a[, .N, by = .(pairID, date_, date_rel_pair)]
setnames(dpn, 'N', 'Nnb')

# proportion of locations send
dpn[, Np_nb := Nnb / 142]

d0a = merge(d0a, dpn, by = c('pairID', 'date_', 'date_rel_pair'), all.x = TRUE)

# Proportion of time together
dps = d0a[interaction == TRUE, .(N_int_nb = .N), by = .(pairID, date_, date_rel_pair)]
du = unique(d0a, by = c('pairID', 'date_', 'date_rel_pair'))
du = merge(du, dps, by = c('pairID', 'date_', 'date_rel_pair'), all.x = TRUE)
du[is.na(N_int_nb), N_int_nb := 0]
du[, int_prop_nb := N_int_nb / Nnb]

# merge back with full table
d0a = merge(d0a, du[, .(pairID, date_, date_rel_pair, N_int_nb, int_prop_nb)],
            by = c('pairID', 'date_', 'date_rel_pair'), all.x = TRUE)

# subset only pairs with at least one interaction
d0a = d0a[int_prop_nb > 0]

# subset pairs with at least 50% data
d0a = d0a[Np_nb >= 0.5]

# select random pairs for each day
ds = unique(d0a, by = c('pairID', 'date_', 'date_rel_pair'))

# shuffle data
set.seed(21)
ds = ds[sample(dim(ds)[1])]

# subset pairs
ds[, date_rel_pair_id := seq_len(.N), by = date_rel_pair]
ds = ds[date_rel_pair_id <= 50]
ds[, sub := paste0(pairID, '_', date_, '_', date_rel_pair)]
d0a[, sub := paste0(pairID, '_', date_, '_', date_rel_pair)]

d0as = d0a[sub %in% ds$sub]

ds[date_rel_pair >= -10 & date_rel_pair <= 10, .N, date_rel_pair]

# merge with nest data from female
duf = unique(dp[!is.na(nestID)], by = c('ID2', 'year_'))

# delete columns in d0as
d0as[, nestID := NULL]
d0as[, initiation := NULL]
d0as[, initiation_rel := NULL]

d0as = merge(d0as, duf[, .(year_, ID2, nestID, initiation, initiation_rel)],
             by = c('year_', 'ID2'), all.x = TRUE)

d0as[is.na(nestID)]

# rename order and save
d0as = d0as[,
        .(pairID, year_, ID1, ID2, sex1, sex2, datetime_1, datetime_2, N = Nnb, Np = Np_nb, date_, 
          date_rel_pair, datetime_rel_pair, distance_pair, interaction, split, merge, nestID, initiation, 
          initiation_day, initiation_rel, at_nest1, at_nest2, female_clutch, anyEPY, breeding_pair, 
          f_polyandrous, f_polyandrous_first, f_polyandrous_second, f_renesting_first, f_renesting_second, 
          type = 'randomization')]

# save data
fwrite(d0as, './DATA/PAIR_WISE_INTERACTIONS_BREEDING_PAIRS_RANDOM.txt', quote = TRUE, sep = '\t', row.names = FALSE)



