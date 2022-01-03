#==============================================================================================================
# Mate guarding
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'ggnewscale', 'doFuture', 'patchwork', 'activity', 'glmmTMB', 'effects'), 
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
#' # Sired or received EPP?
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
#' # Define breeding pairs
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
              lat_n = lat, lon_n = lon, overlap)]
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
dp[, datetime_rel := difftime(datetime_1, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

# for plots subset study site nests
di = dn[!is.na(year_) & plot == 'NARL']
di[, initiation_mean := mean(initiation, na.rm = TRUE), by = year_]
di[, initiation_rel := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

#--------------------------------------------------------------------------------------------------------------
#' # How many breeders with overlap?
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
#' # Percentage of daily interactions
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

# relative initiation date
dp[, initiation_rel := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

# early and late clutches?
dp[initiation_rel < -2, initiation_type := 'early']
dp[initiation_rel > 2, initiation_type := 'late']
dp[!is.na(initiation) & is.na(initiation_type), initiation_type := 'peak']

# datetime relative to nest initiation date
dp[, datetime_rel_initiation := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# median daily 
dp[, date_ := as.Date(datetime_1)]
# dp[, median_dist := median(distance_pair, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# median corrected
# distance_threshold = 30
# dp[, distance_pair_cor := ifelse(interaction == TRUE & distance_pair > distance_threshold, distance_threshold, distance_pair)]
# dp[interaction == FALSE, distance_pair_cor := distance_pair]
# dp[, median_dist_cor := median(distance_pair_cor, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# total number of interactions and percentage 
dp[, N_pairwise_positions := .N, by = .(year_, pairID, nestID)]
dp[interaction == TRUE, N_pairwise_interactions := .N, by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions := mean(N_pairwise_interactions, na.rm = TRUE), by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions_per := N_pairwise_interactions / N_pairwise_positions * 100]

# number of daily interactions and percentage 
dp[, N_pairwise_positions_daily := .N, by = .(year_, pairID, nestID, date_)]
dp[interaction == TRUE, N_pairwise_interactions_daily := .N, by = .(year_, pairID, nestID, date_)]
dp[, N_pairwise_interactions_daily := mean(N_pairwise_interactions_daily, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, N_pairwise_interactions_daily_per := N_pairwise_interactions_daily / N_pairwise_positions_daily * 100]
dp[, N_pairwise_interactions_daily_per_50 := any(N_pairwise_interactions_daily_per > 50), by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions_daily_per_90 := any(N_pairwise_interactions_daily_per > 90), by = .(year_, pairID, nestID)]

# non interactions
dp[, N_pairwise_no_interactions_daily := N_pairwise_positions_daily - N_pairwise_interactions_daily]

# number of nest attendance and percentage
dps = dp[!is.na(nestID)]
dps[, distance_nest_1 := sqrt(sum((c(lon1, lat1) - c(lon_n, lat_n))^2)), by = 1:nrow(dps)]
dps[, at_nest := distance_nest_1 < 15]

dps[at_nest == TRUE, N_pairwise_positions_daily_at_nest := .N, by = .(year_, pairID, nestID, date_)]
dps[, N_pairwise_positions_daily_at_nest := mean(N_pairwise_positions_daily_at_nest, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dps[, N_pairwise_positions_daily_at_nest_per := N_pairwise_positions_daily_at_nest / N_pairwise_positions_daily * 100]

dps[at_nest == TRUE & interaction == TRUE, N_pairwise_positions_daily_at_nest_with_female := .N, by = .(year_, pairID, nestID, date_)]
dps[, N_pairwise_positions_daily_at_nest_with_female := mean(N_pairwise_positions_daily_at_nest_with_female, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dps[, N_pairwise_positions_daily_at_nest_with_female_per := N_pairwise_positions_daily_at_nest_with_female / N_pairwise_positions_daily * 100]

# no interaction and at nest? 
dps[, no_interaction_at_nest := interaction == FALSE & at_nest == TRUE]
dps[no_interaction_at_nest == TRUE, N_no_interaction_at_nest := .N, by = .(year_, pairID, nestID, date_)]
dps[, N_no_interaction_at_nest := mean(N_no_interaction_at_nest, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dps[, percent_at_nest_when_no_interaction := N_no_interaction_at_nest / N_pairwise_no_interactions_daily * 100]

# interaction and at nest? 
dps[, interaction_at_nest := interaction == TRUE & at_nest == TRUE]
dps[interaction_at_nest == TRUE, N_interaction_at_nest := .N, by = .(year_, pairID, nestID, date_)]
dps[, N_interaction_at_nest := mean(N_interaction_at_nest, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dps[, percent_at_nest_when_interaction := N_interaction_at_nest / N_pairwise_interactions_daily * 100]

# merge back with data
dp = merge(dp, dps[, .(ID1, ID2, datetime_1, nestID, at_nest, N_pairwise_positions_daily_at_nest_per, 
                       N_no_interaction_at_nest, N_pairwise_positions_daily_at_nest_with_female_per, percent_at_nest_when_no_interaction, percent_at_nest_when_interaction)], 
           by = c('ID1', 'ID2', 'datetime_1', 'nestID'), all.x = TRUE)

# longest bout together
dp[, bout_start := min(c(datetime_1, datetime_2)), by = .(year_, pairID, bout)]
dp[, bout_end := max(c(datetime_1, datetime_2)), by = .(year_, pairID, bout)]
dp[, bout_length := difftime(bout_end, bout_start, units = 'mins') %>% as.numeric]
dp[interaction == TRUE, bout_max := max(bout_length, na.rm = TRUE), by = .(year_, pairID, nestID)]
dp[, bout_max := mean(bout_max, na.rm = TRUE), by = .(year_, pairID, nestID)]

# longest bout together daily 
dp[interaction == TRUE, bout_max_daily := max(bout_length, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, bout_max_daily := mean(bout_max_daily, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# pairs with known nest and mate guarding data before clutch initiation
dp[, mg_before_initiation := any(datetime_rel_initiation < 0), by = .(year_, pairID, nestID)]
dp[nestID == 'R812_18' | nestID == 'R604_18', mg_before_initiation := FALSE] # pair with data before paired

# same sex interaction?
dp[, same_sex := sex1 == sex2]

# breeding pair
dp[, breeding_pair := !is.na(nestID)]

# summary by unique pair excluding pair wise duplicates
dsm = dp[same_sex == FALSE & sex1 == 'M'] # because nests are merged with ID1 = male
dss = dp[same_sex == TRUE & ID1 > ID2] 

dps = rbind(dsm, dss)

# assign all datetimes in which the males was interacting with the breeding partner
dpib = dps[interaction == TRUE & breeding_pair == TRUE]
dpib = dpib[, .(ID1, datetime_1, interaction_with_partner = TRUE)]
dpib = unique(dpib)

# merge with data
dps = merge(dps, dpib, by = c('ID1', 'datetime_1'), all.x = TRUE)
dps[is.na(interaction_with_partner), interaction_with_partner := FALSE]

# Males interacting with other females
dpi = dps[same_sex == FALSE & interaction == TRUE & breeding_pair == FALSE]
dpi = unique(dpi, by = c('ID1', 'pairID', 'date_'))

dpia = dpi[, .(N_ID1_other_interactions_daily = .N), by = .(ID1, date_)]

# Males interacting with other females while not with breeding partner
dpi = dps[same_sex == FALSE & interaction == TRUE & breeding_pair == FALSE & interaction_with_partner == FALSE]
dpi = unique(dpi, by = c('ID1', 'pairID', 'date_'))

dpibn = dpi[, .(N_ID1_other_interactions_daily_without_partner = .N), by = .(ID1, date_)]

# Males interacting with other females while with breeding partner
dpi = dps[same_sex == FALSE & interaction == TRUE & breeding_pair == FALSE & interaction_with_partner == TRUE]
dpi = unique(dpi, by = c('ID1', 'pairID', 'date_'))

dpiby = dpi[, .(N_ID1_other_interactions_daily_with_partner = .N), by = .(ID1, date_)]

# merge data
dpia = merge(dpia, dpibn, by = c('ID1', 'date_'), all.x = TRUE)
dpia = merge(dpia, dpiby, by = c('ID1', 'date_'), all.x = TRUE)
dpia[is.na(N_ID1_other_interactions_daily_without_partner), N_ID1_other_interactions_daily_without_partner := 0]
dpia[is.na(N_ID1_other_interactions_daily_with_partner), N_ID1_other_interactions_daily_with_partner := 0]

# percentage of daily interactions with and without partner
dpia[, N_ID1_other_interactions_daily_without_partner_per := N_ID1_other_interactions_daily_without_partner / N_ID1_other_interactions_daily * 100]
dpia[, N_ID1_other_interactions_daily_with_partner_per := N_ID1_other_interactions_daily_with_partner / N_ID1_other_interactions_daily * 100]

# merge back with dps
dps = merge(dps, dpia[, .(date_, ID1, N_ID1_other_interactions_daily, 
                          N_ID1_other_interactions_daily_without_partner, N_ID1_other_interactions_daily_without_partner_per, 
                          N_ID1_other_interactions_daily_with_partner, N_ID1_other_interactions_daily_with_partner_per)], 
            by = c('date_', 'ID1'), all.x = TRUE)

# females interacting with other females
dpi = dps[same_sex == FALSE & interaction == TRUE & breeding_pair == FALSE]
dpi = unique(dpi, by = c('ID2', 'pairID', 'date_'))

dpia = dpi[, .(N_ID2_other_interactions_daily = .N), by = .(ID2, date_)]

# females interacting with other females while not with breeding partner
dpi = dps[same_sex == FALSE & interaction == TRUE & breeding_pair == FALSE & interaction_with_partner == FALSE]
dpi = unique(dpi, by = c('ID2', 'pairID', 'date_'))

dpibn = dpi[, .(N_ID2_other_interactions_daily_without_partner = .N), by = .(ID2, date_)]

# males interacting with other females while with breeding partner
dpi = dps[same_sex == FALSE & interaction == TRUE & breeding_pair == FALSE & interaction_with_partner == TRUE]
dpi = unique(dpi, by = c('ID2', 'pairID', 'date_'))

dpiby = dpi[, .(N_ID2_other_interactions_daily_with_partner = .N), by = .(ID2, date_)]

# merge data
dpia = merge(dpia, dpibn, by = c('ID2', 'date_'), all.x = TRUE)
dpia = merge(dpia, dpiby, by = c('ID2', 'date_'), all.x = TRUE)
dpia[is.na(N_ID2_other_interactions_daily_without_partner), N_ID2_other_interactions_daily_without_partner := 0]
dpia[is.na(N_ID2_other_interactions_daily_with_partner), N_ID2_other_interactions_daily_with_partner := 0]

# percentage of daily interactions with and without partner
dpia[, N_ID2_other_interactions_daily_without_partner_per := N_ID2_other_interactions_daily_without_partner / N_ID2_other_interactions_daily * 200]
dpia[, N_ID2_other_interactions_daily_with_partner_per := N_ID2_other_interactions_daily_with_partner / N_ID2_other_interactions_daily * 200]

# merge back with dps
dps = merge(dps, dpia[, .(date_, ID2, N_ID2_other_interactions_daily, 
                          N_ID2_other_interactions_daily_without_partner, N_ID2_other_interactions_daily_without_partner_per, 
                          N_ID2_other_interactions_daily_with_partner, N_ID2_other_interactions_daily_with_partner_per)], 
            by = c('date_', 'ID2'), all.x = TRUE)


# round to days
dps[, datetime_rel_initiation0 := round(datetime_rel_initiation, 0)]
dp[, datetime_rel_initiation0 := round(datetime_rel_initiation, 0)]

du = unique(dps, by = c('year_', 'pairID', 'nestID'))
dud = unique(dps, by = c('year_', 'pairID', 'nestID', 'date_'))

#--------------------------------------------------------------------------------------------------------------
#' # Look at variation over the season
#--------------------------------------------------------------------------------------------------------------

# extract day and month
dud[, month_day := substr(date_, 7, 10)]
dud[, datetime_y := as.POSIXct(format(datetime_1, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dud[, date_y := as.Date(format(date_, format = '%m-%d'), format = '%m-%d')]

# Period with data
ggplot(data = dud) +
  geom_density(aes(x = datetime_y, group = year_, color = as.factor(year_))) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3')) +
  theme_classic(base_size = 12)



# percentage together of breeders in relation to clutch initiation

# check what scale works best
min(dud$date_y)
min(di$initiation_y, na.rm = TRUE)

max(dud$date_y)
max(di$initiation_y, na.rm = TRUE)



p1 = 
ggplot(data = dud[breeding_pair == TRUE]) +
  geom_boxplot(aes(as.POSIXct(date_y), N_pairwise_interactions_daily_per, color = as.factor(year_), 
                   group = interaction(year_, as.POSIXct(date_y))), varwidth = TRUE) +
  geom_smooth(aes(as.POSIXct(date_y), N_pairwise_interactions_daily_per, group = as.factor(year_), color = as.factor(year_))) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Year') +
  xlab('Date') + ylab('Percentage of positions together') +
  scale_x_datetime(date_labels = "%d %b", limits = c(as.POSIXct('2021-06-02 00:00:00'), as.POSIXct('2021-07-02 00:00:00'))) +
  scale_y_continuous(limits = c(0, 110)) +
  theme_classic(base_size = 12)

p2 = 
ggplot(data = di) +
  geom_violin(aes(as.POSIXct(as.Date(initiation_y)), as.character(year_), color = as.character(year_), fill = as.character(year_)), 
              show.legend = FALSE, alpha = 0.5) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3')) +
  scale_fill_manual(values = c('darkorange', 'dodgerblue3')) +
  xlab('Initiation date') + ylab('Year') +
  scale_x_datetime(date_labels = "%d %b", limits = c(as.POSIXct('2021-06-02 00:00:00'), as.POSIXct('2021-07-02 00:00:00'))) +
  theme_classic(base_size = 12)

p1 + p2 + plot_layout(ncol = 1, heights = c(3, 1))

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')



# same for non-breeders
p1 = 
  ggplot(data = dud[breeding_pair == FALSE]) +
  geom_boxplot(aes(as.POSIXct(date_y), N_pairwise_interactions_daily_per, color = as.factor(year_), 
                   group = interaction(year_, as.POSIXct(date_y))), varwidth = TRUE) +
  geom_smooth(aes(as.POSIXct(date_y), N_pairwise_interactions_daily_per, group = as.factor(year_), color = as.factor(year_))) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Year') +
  xlab('Date') + ylab('Percentage of positions together') +
  scale_x_datetime(date_labels = "%d %b", limits = c(as.POSIXct('2021-06-02 00:00:00'), as.POSIXct('2021-07-02 00:00:00'))) +
  scale_y_continuous(limits = c(0, 110)) +
  theme_classic(base_size = 12)

p2 = 
  ggplot(data = di) +
  geom_violin(aes(as.POSIXct(as.Date(initiation_y)), as.character(year_), color = as.character(year_), fill = as.character(year_)), 
              show.legend = FALSE, alpha = 0.5) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3')) +
  scale_fill_manual(values = c('darkorange', 'dodgerblue3')) +
  xlab('Initiation date') + ylab('Year') +
  scale_x_datetime(date_labels = "%d %b", limits = c(as.POSIXct('2021-06-02 00:00:00'), as.POSIXct('2021-07-02 00:00:00'))) +
  theme_classic(base_size = 12)

p1 + p2 + plot_layout(ncol = 1, heights = c(3, 1))

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_non_breeders.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


# relative to clutch initiation date
p1 = 
  ggplot(data = dud[breeding_pair == TRUE]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, color = as.factor(year_), 
                   group = interaction(year_, datetime_rel_initiation0)), varwidth = TRUE) +
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, group = as.factor(year_), color = as.factor(year_))) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Year') +
  xlab('Date') + ylab('Percentage of positions together') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(0, 110)) +
  theme_classic(base_size = 12)

p2 = 
  ggplot(data = di) +
  geom_violin(aes(initiation_rel, as.character(year_), color = as.character(year_), fill = as.character(year_)), 
              show.legend = FALSE, alpha = 0.5) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3')) +
  scale_fill_manual(values = c('darkorange', 'dodgerblue3')) +
  xlab('Initiation date') + ylab('Year') +
  scale_x_continuous(limits = c(-15, 15)) +
  theme_classic(base_size = 12)

p1 + p2 + plot_layout(ncol = 1, heights = c(3, 1))

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_rel.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')

# early, peak or late breeders
ggplot(data = dud[breeding_pair == TRUE & !is.na(datetime_rel_initiation0) & !is.na(initiation_type)]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, color = initiation_type, 
                   group = interaction(datetime_rel_initiation0, initiation_type)), varwidth = TRUE) +
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, group = initiation_type, color = initiation_type)) +
  scale_color_manual(values = c('firebrick3', 'dodgerblue3', 'darkgreen'), name = 'Initiation timing') +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  # geom_text(data = dss, aes(as.factor(datetime_rel_initiation0), Inf, label = N), 
  #           position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_initiation_type.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')

du[, .N, by = initiation_type]

#--------------------------------------------------------------------------------------------------------------
#' # Randomization for interaction base line
#--------------------------------------------------------------------------------------------------------------

# unique data
ds = unique(dp, by = c('nestID', 'datetime_rel_initiation0'))
ds = ds[!is.na(datetime_rel_initiation)]

# nests to exclude
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]

# data needed for null model
d0 = ds[, .(datetime_rel_initiation0, date_)] %>% unique

# subset non-breeders pairs within breeders
breeding_males = du[breeding_pair == TRUE]$ID1
breeding_females = du[breeding_pair == TRUE]$ID2

duds = dud[breeding_pair == FALSE & ID1 %in% breeding_males & ID2 %in% breeding_females]

# subset all interactions on days with relative initiation date for breeders
x = d0$datetime_rel_initiation0 %>% unique

d0a = foreach(i = x, .combine = 'rbind') %do% {
  
  # subset relative day
  dss = d0[datetime_rel_initiation0 == i]
  
  # subset all pairwise interactions from this date 
  dsms = duds[date_ %in% dss$date_]
  
  # assign type
  dsms[, datetime_rel_initiation0 := i]
  
  dsms
  
}

d0a[, datetime_rel_initiation0_type := 'null_model']


# merge with breeding pairs 
duds = dud[breeding_pair == TRUE]
duds[, datetime_rel_initiation0_type := 'breeding_pair']

dud0 = rbind(duds, d0a)


ggplot(data = dud0) +
  geom_boxplot(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, color = datetime_rel_initiation0_type, 
                   group = interaction(datetime_rel_initiation0_type, datetime_rel_initiation0)), varwidth = TRUE) +
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, group = datetime_rel_initiation0_type, color = datetime_rel_initiation0_type)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Data type') +
  xlab('Date') + ylab('Percentage of positions together') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(0, 110)) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model_all.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


dud0[, .N, by = .(datetime_rel_initiation0_type, datetime_rel_initiation0)]

# same thing, but with only 50 random pairs in comparision
d0a = d0a[!is.na(N_pairwise_interactions_daily_per)]

# shuffle data
d0ar <- d0a[sample(dim(d0a)[1])]

# name rows by datetime_rel_initiation0
d0ar[, datetime_rel_initiation0_id := seq_len(.N), by = datetime_rel_initiation0]
d0ar = d0ar[datetime_rel_initiation0_id < 50]
d0ar[, datetime_rel_initiation0_id := NULL]

dud0 = rbind(duds, d0ar)


ggplot(data = dud0) +
  geom_boxplot(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, color = datetime_rel_initiation0_type, 
                   group = interaction(datetime_rel_initiation0_type, datetime_rel_initiation0)), varwidth = TRUE) +
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, group = datetime_rel_initiation0_type, 
                  color = datetime_rel_initiation0_type)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Data type') +
  xlab('Date') + ylab('Percentage of positions together') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(0, 110)) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model_50breeders.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')





# statistics


# individual level relative date-time: relative to pair initiation
dps[, datetime_rel_pair := difftime(datetime_1, initiation) |> as.numeric() ]

# MODEL 1: probability of interactions for breeding pairs
x = dps[!is.na(initiation) &
         datetime_rel_pair / 3600 / 24  <= 10 &   # subset to 10 days before nest initiated and 
         datetime_rel_pair / 3600 / 24  >= -10   # 10 days after nest is initiated
]

x[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
x[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]


fm1 <- glmmTMB(interaction ~ poly(datetime_rel_pair, 2) +
                 scale(sin_time) + scale(cos_time) +
                 (1 + poly(datetime_rel_pair, 2) | pairID),
               family = binomial, data = x,
               REML = FALSE,
               control = glmmTMBControl(parallel = 15)
)

summary(fm1)






e <- allEffects(fm1, xlevels = 100)$"poly(datetime_rel_pair,2)" |>
  data.frame() |>
  setDT()



dss = unique(dud[breeding_pair == TRUE], by = c('nestID', 'datetime_rel_initiation0'))
dss = dss[, .N, by = datetime_rel_initiation0]
dss



# only breeding pairs 
ggplot(e, aes(y = fit, x = datetime_rel_pair / 3600 / 24)) +
  geom_rect(aes(xmin = 0, xmax = 4, ymin = 0, ymax = 1), fill = 'grey80') +
  geom_boxplot(data = dud[breeding_pair == TRUE], 
               aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per/100, 
               group = interaction(datetime_rel_initiation0))) +
  
  geom_line(size = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(datetime_rel_initiation0, Inf, label = N), vjust = 1, size = 3) +
  scale_x_continuous(limits = c(-10.5, 10.5)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 12) +
 
  ylab('Proportion / probability of interactions') +
  xlab('Day relative to clutch initiation (= 0)')


# breeding pairs and randomization
ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey80') +
  geom_boxplot(data = dud0, 
               aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per/100, color = datetime_rel_initiation0_type, 
                   group = interaction(datetime_rel_initiation0_type, datetime_rel_initiation0)), 
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('firebrick4', 'dodgerblue4'), name = '', 
                     labels = c('Breeding pair', 'Male-female pair')) +
  
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair / 3600 / 24), size = 0.8, color = 'firebrick4') +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair / 3600 / 24, ymin = lower, ymax = upper), 
              fill = 'firebrick4', alpha = 0.2) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(datetime_rel_initiation0, Inf, label = N), vjust = 1, size = 3) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0', '0.2', '0.4', '0.6', '0.8', '1'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank()) +
  ylab('Proportion / probability of interactions') +
  xlab('Day relative to clutch initiation (= 0)')



# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_over_season_null_model_50breeders_new.tiff', plot = last_plot(),  width = 180, height = 120, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Look at variation in connection to EPP
#--------------------------------------------------------------------------------------------------------------

# EPP in clutch of female
dud[is.na(any_EPY), any_EPY := FALSE]

ggplot(data = dud[same_sex == FALSE & !is.na(datetime_rel_initiation0)]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, color = any_EPY, 
                   group = interaction(datetime_rel_initiation0, any_EPY)), varwidth = TRUE) +
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, group = any_EPY, color = any_EPY)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  # geom_text(data = dss, aes(as.factor(datetime_rel_initiation0), Inf, label = N), 
  #           position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Female with EPY') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(0, 110)) +
  theme_classic(base_size = 12)


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_female_with_EPY.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')

du[any_EPY == TRUE, .(nestID, initiation_type, initiation_rel)]

# Epp sired by male
dud[is.na(m_sired_EPY), m_sired_EPY := FALSE]

ggplot(data = dud[same_sex == FALSE & !is.na(datetime_rel_initiation0)]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, color = m_sired_EPY, 
                   group = interaction(datetime_rel_initiation0, m_sired_EPY)), varwidth = TRUE) +
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per, group = m_sired_EPY, color = m_sired_EPY)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  # geom_text(data = dss, aes(as.factor(datetime_rel_initiation0), Inf, label = N), 
  #           position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Male sired EPY') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(0, 110)) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_male_with_EPY.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


du[m_sired_EPY == TRUE, .(nestID, initiation_type, initiation_rel)]


#--------------------------------------------------------------------------------------------------------------
#' # Look at variation in connection to males at nest
#--------------------------------------------------------------------------------------------------------------

# males at nest
ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey80') +
  geom_boxplot(data = dud[breeding_pair == TRUE], 
               aes(datetime_rel_initiation0, N_pairwise_positions_daily_at_nest_per/100, 
                   group = interaction(datetime_rel_initiation0))) +
  
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair / 3600 / 24), size = 0.8, color = 'firebrick4') +
  geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair / 3600 / 24, ymin = lower, ymax = upper), 
              fill = 'firebrick4', alpha = 0.2) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(datetime_rel_initiation0, Inf, label = N), vjust = 1, size = 3) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0', '0.2', '0.4', '0.6', '0.8', '1'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.9), legend.background = element_blank()) +
  ylab('Proportion / probability at nest') +
  xlab('Day relative to clutch initiation (= 0)')




d0a[, datetime_rel_initiation0_type := 'null_model']


# at nest total and with female
duds = dud[breeding_pair == TRUE]
duds[, N_pairwise_positions_daily_at_nest_total_and_with_female_per := N_pairwise_positions_daily_at_nest_per]
duds[, at_nest_type := 'at_nest']

dudss = dud[breeding_pair == TRUE]
dudss[, N_pairwise_positions_daily_at_nest_total_and_with_female_per := N_pairwise_positions_daily_at_nest_with_female_per]
dudss[, at_nest_type := 'at_nest_with_female']

dudn = rbind(duds, dudss)



# MODEL 2: probability to be at the nest
# x = dps[!is.na(initiation) & !is.na(at_nest) &
#           datetime_rel_pair / 3600 / 24  <= 10 &   # subset to 7 days before nest initiated and 
#           datetime_rel_pair / 3600 / 24  >= -7   # 10 days after nest is initiated
# ]
# 
# x[, sin_time := sin(gettime(datetime_1, "radian")) |> as.numeric()]
# x[, cos_time := cos(gettime(datetime_1, "radian")) |> as.numeric()]
# 
# 
# fm2 <- glmmTMB(at_nest ~ datetime_rel_pair +
#                  scale(sin_time) + scale(cos_time) +
#                  (1 | pairID), # runs forever with 1 + datetime_rel_pair
#                family = binomial, data = x,
#                REML = FALSE,
#                control = glmmTMBControl(parallel = 15)
# )
# 
# summary(fm2)
# 
# 
# e2 <- allEffects(fm2, xlevels = 100)$"datetime_rel_pair" |>
#   data.frame() |>
#   setDT()
# 
# 
# ggplot() +
#   geom_line(data = e2, aes(y = fit, x = datetime_rel_pair / 3600 / 24), size = 0.8, color = 'firebrick4') +
#   geom_ribbon(data = e2, aes(y = fit, x = datetime_rel_pair / 3600 / 24, ymin = lower, ymax = upper), 
#               fill = 'firebrick4', alpha = 0.2) 




# males at nest with female
ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 1), fill = 'grey80') +
  geom_boxplot(data = dudn, 
               aes(datetime_rel_initiation0, N_pairwise_positions_daily_at_nest_total_and_with_female_per/100, 
                   group = interaction(at_nest_type, datetime_rel_initiation0), color = at_nest_type),
               lwd = 0.4, outlier.size = 0.7) +
  scale_color_manual(values = c('firebrick4', 'dodgerblue4'), name = '', 
                     labels = c('Total', 'With female')) +
  
  geom_smooth(data = dudn,
              aes(datetime_rel_initiation0, N_pairwise_positions_daily_at_nest_total_and_with_female_per/100,
                  group = at_nest_type, color = at_nest_type)) +
  geom_line(data = e, aes(y = fit, x = datetime_rel_pair / 3600 / 24), size = 0.8, 
            color = 'black') +
  # geom_ribbon(data = e, aes(y = fit, x = datetime_rel_pair / 3600 / 24, ymin = lower, ymax = upper), 
  #             fill = 'black', alpha = 0.2) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(datetime_rel_initiation0, Inf, label = N), vjust = 1, size = 3) +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0', '0.2', '0.4', '0.6', '0.8', '1'),
                     expand = expansion(add = c(0, 0.05))) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.1, 0.9), legend.background = element_blank()) +
  ylab('Proportion / probability at nest') +
  xlab('Day relative to clutch initiation (= 0)')


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_male_at_nest_new.tiff', plot = last_plot(), width = 180, height = 120, units = c('mm'), dpi = 'print')



















ggplot(data = dud[breeding_pair == TRUE & datetime_rel_initiation0 > -3 & datetime_rel_initiation0 < 4]) +
  geom_point(aes(N_pairwise_positions_daily_at_nest_per, N_pairwise_interactions_daily_per)) +
  geom_smooth(aes(N_pairwise_positions_daily_at_nest_per, N_pairwise_interactions_daily_per), method = 'lm') +
  theme_classic(base_size = 12)




# Plot for correlation time at the nest with mate guarding

dx = dud[breeding_pair == TRUE & datetime_rel_initiation0 > -3 & datetime_rel_initiation0 < 4]

ggplot(data = dx) +

  geom_point(aes(N_pairwise_positions_daily_at_nest_per/100, N_pairwise_interactions_daily_per/100, 
               color = as.factor(datetime_rel_initiation0))) +
  geom_smooth(aes(N_pairwise_positions_daily_at_nest_per/100, N_pairwise_interactions_daily_per/100, 
                  color = as.factor(datetime_rel_initiation0), fill = as.factor(datetime_rel_initiation0)), method = 'lm', alpha = 0.2) +

  scale_colour_manual(name = 'Egg date',
                      values=c('firebrick4', 'firebrick2', 'tomato', 'steelblue1', 'steelblue3', 'dodgerblue4')) +
  scale_fill_manual(name = 'Egg date',
                    values=c('firebrick4', 'firebrick2', 'tomato', 'steelblue1', 'steelblue3', 'dodgerblue4')) +
  
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0', '0.2', '0.4', '0.6', '0.8', '1'),
                     expand = expansion(add = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     labels = c('0', '0.2', '0.4', '0.6', '0.8', '1'),
                     expand = expansion(add = c(0, 0.05))) +
  # guides(color = guide_legend('Egg date ')) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.9, 0.75), legend.background = element_blank()) +
  ylab('Proportion of interactions with female') +
  xlab('Proportion of locations at nest')

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_male_at_nest_cor.tiff', plot = last_plot(),  width = 190, height = 190, units = c('mm'), dpi = 'print')


# When the male is not with the female is it at the nest?
ggplot(data = dud[breeding_pair == TRUE & datetime_rel_initiation0 > -3 & datetime_rel_initiation0 < 4]) +
  geom_boxplot(aes(datetime_rel_initiation0, percent_at_nest_when_no_interaction, 
                 color = as.factor(datetime_rel_initiation0))) +
  geom_smooth(aes(datetime_rel_initiation0, percent_at_nest_when_no_interaction, 
                  color = as.factor(datetime_rel_initiation0)), method = 'lm') +
  guides(color = guide_legend('Initiation std')) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/MG_male_at_nest_When_no_interaction.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Do males and females interact with other opposite sex IDs?
#--------------------------------------------------------------------------------------------------------------


colors = c('all' = 'black', 'with partner' = 'dodgerblue3', 'without partner' = 'firebrick3')

# males interacting with other females  - population level
duds = dud[breeding_pair == TRUE, .(N_ID1_other_interactions_daily = sum(N_ID1_other_interactions_daily, na.rm = TRUE), 
                                    N_ID1_other_interactions_daily_with_partner = sum(N_ID1_other_interactions_daily_with_partner, na.rm = TRUE),
                                    N_ID1_other_interactions_daily_without_partner = sum(N_ID1_other_interactions_daily_without_partner, na.rm = TRUE)),
           by = datetime_rel_initiation0]



ggplot(data = duds) +
  geom_bar(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily), stat = 'identity', fill = 'grey90') +
  geom_point(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_with_partner, color = 'with partner')) +
  geom_point(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_without_partner, color = 'without partner')) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily, color = 'all'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_with_partner, color = 'with partner'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_without_partner, color = 'without partner'), se = FALSE) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = colors, name = 'Type of interaction') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Total number of extra-pair interactions (male)') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-10, 210)) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/EP_interactions_total_males.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


# females interacting with other females - population level
duds = dud[breeding_pair == TRUE, .(N_ID2_other_interactions_daily = sum(N_ID2_other_interactions_daily, na.rm = TRUE), 
                                    N_ID2_other_interactions_daily_with_partner = sum(N_ID2_other_interactions_daily_with_partner, na.rm = TRUE),
                                    N_ID2_other_interactions_daily_without_partner = sum(N_ID2_other_interactions_daily_without_partner, na.rm = TRUE)),
           by = datetime_rel_initiation0]


ggplot(data = duds) +
  geom_bar(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily), stat = 'identity', fill = 'grey90') +
  geom_point(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_with_partner, color = 'with partner')) +
  geom_point(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_without_partner, color = 'without partner')) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily, color = 'all'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_with_partner, color = 'with partner'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_without_partner, color = 'without partner'), se = FALSE) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = colors, name = 'Type of interaction') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Total number of extra-pair interactions (female)') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-10, 210)) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/EP_interactions_total_females.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


# male over season 
ggplot(data = dud[breeding_pair == TRUE]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily, 
                   group = datetime_rel_initiation0)) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_without_partner, color = 'without partner'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_with_partner, color = 'with partner'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily, color = 'all'), se = FALSE) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = colors, name = 'Type of interaction') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Number of extra-pair interactions (male)') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic(base_size = 12)


# merge for plot
dxs = dud[breeding_pair == TRUE]
dxs[, N_ID1_other_interactions_daily := N_ID1_other_interactions_daily_with_partner]
dxs[, type := 'with partner']

dxss = dud[breeding_pair == TRUE]
dxss[, N_ID1_other_interactions_daily := N_ID1_other_interactions_daily_without_partner]
dxss[, type := 'without partner']

dx = rbind(dxs, dxss)

pm = 
ggplot(data = dx) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = Inf), fill = 'grey80') +
  geom_boxplot(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily,
                   group = interaction(type, datetime_rel_initiation0), color = type)) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily, 
                  color = type), se = FALSE) +
  scale_color_manual(values = c('firebrick4', 'dodgerblue4'), name = '') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Number of extra-pair interactions (male)') +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(2, 2), legend.background = element_blank())
  # theme(legend.position = c(0.85, 0.9), legend.background = element_blank())


# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/EP_interactions_perID_males.tiff', plot = last_plot(), width = 180, height = 120, units = c('mm'), dpi = 'print')


# females over season
ggplot(data = dud[breeding_pair == TRUE]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily, 
                   group = datetime_rel_initiation0)) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_without_partner, color = 'without partner'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_with_partner, color = 'with partner'), se = FALSE) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily, color = 'all'), se = FALSE) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = colors, name = 'Type of interaction') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Number of extra-pair interactions (female)') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic(base_size = 12)



# merge for plot
dxs = dud[breeding_pair == TRUE]
dxs[, N_ID2_other_interactions_daily := N_ID2_other_interactions_daily_with_partner]
dxs[, type := 'with partner']

dxss = dud[breeding_pair == TRUE]
dxss[, N_ID2_other_interactions_daily := N_ID2_other_interactions_daily_without_partner]
dxss[, type := 'without partner']

dx = rbind(dxs, dxss)

pf = 
ggplot(data = dx) +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = Inf), fill = 'grey80') +
  geom_boxplot(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily,
                   group = interaction(type, datetime_rel_initiation0), color = type)) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily, 
                  color = type), se = FALSE) +
  scale_color_manual(values = c('firebrick4', 'dodgerblue4'), name = '') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Number of extra-pair interactions (female)') +
  scale_x_continuous(limits = c(-10.4, 10.4), breaks = seq(-10, 10, 1), 
                     labels = c('-10', '', '', '', '', '-5', '', '', '', '', '0', 
                                '', '', '', '', '5', '', '', '', '', '10'),
                     expand = expansion(add = c(0.2, 0.2))) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.85, 0.9), legend.background = element_blank())

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/EP_interactions_perID_females.tiff', plot = last_plot(), width = 180, height = 120, units = c('mm'), dpi = 'print')


pm + pf

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/EP_interactions_perID_both.tiff', plot = last_plot(), width = 180, height = 120, units = c('mm'), dpi = 'print')




# males: proportion of interactions with or without partner 
colors = c('with partner' = 'dodgerblue3', 'without partner' = 'firebrick3')

ggplot(data = dud[breeding_pair == TRUE]) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_without_partner_per, color = 'without partner')) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID1_other_interactions_daily_with_partner_per, color = 'with partner')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = colors, name = 'Type of interaction') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Proportion of extra-pair interactions (male)') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-10, 110)) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/EP_interactions_perID_males_prop.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


# females: proportion of interactions with or without partner 
ggplot(data = dud[breeding_pair == TRUE]) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_without_partner_per, color = 'without partner')) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_with_partner_per, color = 'with partner')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = colors, name = 'Type of interaction') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Proportion of extra-pair interactions (female)') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-10, 110)) +
  theme_classic(base_size = 12)


ggplot(data = dud[breeding_pair == TRUE]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_without_partner_per, 
                   group = interaction(datetime_rel_initiation0), color = 'without partner')) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_without_partner_per, color = 'without partner')) +
  geom_smooth(aes(datetime_rel_initiation0, N_ID2_other_interactions_daily_with_partner_per, color = 'with partner')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = colors, name = 'Type of interaction') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Proportion of extra-pair interactions (female)') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-10, 110)) +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/FIGURES/MATE_GUARDING/EP_interactions_perID_females_prop.tiff', plot = last_plot(),  width = 280, height = 190, units = c('mm'), dpi = 'print')


#--------------------------------------------------------------------------------------------------------------
#' # Proportion of extra-pair interactions guarded vs unguarded
#--------------------------------------------------------------------------------------------------------------

dud[, prop_epi_with_partner := ]





















