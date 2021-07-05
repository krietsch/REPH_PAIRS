#==============================================================================================================
# Interaction analysis for all
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'windR', 'ggnewscale', 'doFuture', 'patchwork'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
dp = fread('./DATA/PAIR_WISE_INTERACTIONS.txt', sep = '\t', header = TRUE) %>% data.table
dp[, year_ := year(datetime_1)]

do = fread('./DATA/PAIR_WISE_SPACE_USE.txt', sep = '\t', header = TRUE) %>% data.table

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

# any EPP in nest?
dpa[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dpa[, any_EPY := any(EPY == 1), by = nestID]

dpau = unique(dpa, by = 'nestID')

dn = merge(dn, dpau[, .(nestID, any_EPY)], by = 'nestID', all.x = TRUE)

# males that sired EPY 


#--------------------------------------------------------------------------------------------------------------
#' # Define breeding pairs
#--------------------------------------------------------------------------------------------------------------

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = 'ID')

# check if data overlap
dn = merge(dn, dID[, .(male_id = ID, start_m = start, end_m = end)], by = 'male_id', all.x = TRUE)
dn = merge(dn, dID[, .(female_id = ID, start_f = start, end_f = end)], by = 'female_id', all.x = TRUE)

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
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date, any_EPY, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# assign clutch order
setorder(dnID, male_id, initiation)
dnID[!is.na(male_id) & !is.na(female_id), clutch_together := seq_len(.N), by = .(year_, male_id, female_id)]
dnID[!is.na(male_id), male_clutch     := seq_len(.N), by = .(year_, male_id)]
dnID[!is.na(female_id), female_clutch := seq_len(.N), by = .(year_, female_id)]

#--------------------------------------------------------------------------------------------------------------
#' # Define interactions
#--------------------------------------------------------------------------------------------------------------

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2', 'year_'), by.y = c('male_id', 'female_id', 'year_'), all.x = TRUE, allow.cartesian = TRUE)

# relative nest initiation date
dp[, initiation_rel := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

# mean and median daily 
dp[, date_ := as.Date(datetime_1)]
dp[, mean_dist := mean(distance_pair, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, median_dist := median(distance_pair, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# median corrected
distance_threshold = 30
dp[, distance_pair_cor := ifelse(interaction == TRUE & distance_pair > distance_threshold, distance_threshold, distance_pair)]
dp[interaction == FALSE, distance_pair_cor := distance_pair]
dp[, median_dist_cor := median(distance_pair_cor, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# Number of interactions and percentage 
dp[, N_pairwise_positions := .N, by = .(year_, pairID, nestID)]
dp[interaction == TRUE, N_pairwise_interactions := .N, by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions := mean(N_pairwise_interactions, na.rm = TRUE), by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions_per := N_pairwise_interactions / N_pairwise_positions * 100]

# Number of daily interactions and percentage 
dp[, N_pairwise_positions_daily := .N, by = .(year_, pairID, nestID, date_)]
dp[interaction == TRUE, N_pairwise_interactions_daily := .N, by = .(year_, pairID, nestID, date_)]
dp[, N_pairwise_interactions_daily := mean(N_pairwise_interactions_daily, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, N_pairwise_interactions_daily_per := N_pairwise_interactions_daily / N_pairwise_positions_daily * 100]
dp[, N_pairwise_interactions_daily_per_50 := any(N_pairwise_interactions_daily_per > 50), by = .(year_, pairID, nestID)]
dp[, N_pairwise_interactions_daily_per_90 := any(N_pairwise_interactions_daily_per > 90), by = .(year_, pairID, nestID)]

# Longest bout together
dp[, bout_start := min(c(datetime_1, datetime_2)), by = .(year_, pairID, bout)]
dp[, bout_end := max(c(datetime_1, datetime_2)), by = .(year_, pairID, bout)]
dp[, bout_length := difftime(bout_end, bout_start, units = 'mins') %>% as.numeric]
dp[interaction == TRUE, bout_max := max(bout_length, na.rm = TRUE), by = .(year_, pairID, nestID)]
dp[, bout_max := mean(bout_max, na.rm = TRUE), by = .(year_, pairID, nestID)]

# Longest bout together daily 
dp[interaction == TRUE, bout_max_daily := max(bout_length, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dp[, bout_max_daily := mean(bout_max_daily, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]

# pairs with known nest and mate guarding data before clutch initiation
dp[, mg_before_initiation := any(initiation_rel < 0), by = .(year_, pairID, nestID)]
dp[nestID == 'R812_18' | nestID == 'R604_18', mg_before_initiation := FALSE] # pair with data before paired

# same sex interaction?
dp[, same_sex := sex1 == sex2]

# breeding pair
dp[, breeding_pair := !is.na(nestID)]

# summary by unique pair excluding pair wise duplicates
dsm = dp[same_sex == FALSE & sex1 == 'M'] # because nests are merged with ID1 = male
dss = dp[same_sex == TRUE & ID1 > ID2] 

dps = rbind(dsm, dss)

du = unique(dps, by = c('year_', 'pairID', 'nestID'))
dud = unique(dps, by = c('year_', 'pairID', 'nestID', 'date_'))


#--------------------------------------------------------------------------------------------------------------
#' # Define "null model" 
#--------------------------------------------------------------------------------------------------------------

# N interactions of unpaired birds for a given day relative to the initiation date 

# Identify all dates linked to relative initiation dates 
# Use N interaction of these unpaired birds 














# look at data same sex vs. opposite sex
ggplot(data = du) +
  geom_boxplot(aes(same_sex, N_pairwise_interactions)) +
  theme_classic()

# look at data known breeders vs. same sex vs. opposite sex
du[breeding_pair == TRUE, pair_type := 'breeding_pair']
du[breeding_pair == TRUE & mg_before_initiation == TRUE, pair_type := 'breeding_pair_mg']
du[breeding_pair == FALSE, pair_type := 'non_breeding_sex']
du[same_sex == TRUE, pair_type := 'same_sex_pair']

dud[breeding_pair == TRUE, pair_type := 'breeding_pair']
dud[breeding_pair == TRUE & mg_before_initiation == TRUE, pair_type := 'breeding_pair_mg']
dud[breeding_pair == FALSE, pair_type := 'non_breeding_sex']
dud[same_sex == TRUE, pair_type := 'same_sex_pair']


ggplot(data = du) +
  geom_boxplot(aes(pair_type, N_pairwise_interactions)) +
  xlab('Pair type') + ylab('Number of pair-wise interactions') +
  theme_classic(base_size = 14)

# ggsave('./OUTPUTS/ALL_PAIRS/N_pair_wise_interactions.png', plot = last_plot(),  width = 170, height = 150, units = c('mm'), dpi = 'print')


ggplot(data = du) +
  geom_boxplot(aes(pair_type, N_pairwise_interactions_per)) +
  xlab('Pair type') + ylab('Number of pair-wise interactions (%)') +
  theme_classic(base_size = 14)

# ggsave('./OUTPUTS/ALL_PAIRS/N_pair_wise_interactions_per.png', plot = last_plot(),  width = 170, height = 150, units = c('mm'), dpi = 'print')


ggplot(data = du) +
  geom_boxplot(aes(pair_type, bout_max/60/24)) +
  theme_classic(base_size = 14)



ggplot(data = dud) +
  geom_boxplot(aes(pair_type,N_pairwise_interactions_daily_per)) +
  xlab('Pair type') + ylab('Number of pair-wise interactions daily (%)') +
  theme_classic(base_size = 14)

# ggsave('./OUTPUTS/ALL_PAIRS/N_pair_wise_interactions_per_daily.png', plot = last_plot(),  width = 170, height = 150, units = c('mm'), dpi = 'print')



ggplot(data = dud[pair_type == 'non_breeding_sex' & year_ == 2019 | pair_type == 'breeding_pair_mg' & year_ == 2019]) +
  geom_point(aes(as.POSIXct(date_), N_pairwise_interactions_daily_per, color = pair_type)) +
  geom_smooth(aes(as.POSIXct(date_), N_pairwise_interactions_daily_per, group = pair_type)) +
  xlab('Pair type') + ylab('Number of pair-wise interactions daily (%)') +
  theme_classic(base_size = 14)


# number of days with more than 50% together
dud[N_pairwise_interactions_daily_per > 50, N_pi_daily_50 := .N, by = .(year_, pairID, nestID)]
dud[is.na(N_pi_daily_50), N_pi_daily_50 := 0]


dud_max = dud[, .(N_pairwise_interactions_daily_per_max = max(N_pairwise_interactions_daily_per, na.rm = TRUE),
                  N_pi_daily_50 = max(N_pi_daily_50)), 
              by = .(year_, pairID, nestID, pair_type)]

ggplot(data = dud_max) +
  geom_boxplot(aes(pair_type, N_pi_daily_50)) +
  theme_classic(base_size = 14)

ggplot(data = dud_max) +
  geom_boxplot(aes(pair_type, N_pairwise_interactions_daily_per_max)) +
  xlab('Pair type') + ylab('Number of pair-wise interactions daily MAX (%)') +
  theme_classic(base_size = 14)

# ggsave('./OUTPUTS/ALL_PAIRS/N_pair_wise_interactions_per_daily_max.png', plot = last_plot(),  width = 170, height = 150, units = c('mm'), dpi = 'print')



dud_max[pair_type == 'non_breeding_sex' & N_pairwise_interactions_daily_per_max > 50 | 
        pair_type == 'same_sex_pair' & N_pairwise_interactions_daily_per_max > 50]


ds = dud_max[pair_type == 'non_breeding_sex' & N_pi_daily_50 > 1 | 
             pair_type == 'same_sex_pair' & N_pi_daily_50 > 1]


setorder(ds, N_pi_daily_50)


pairs = ds[pair_type == 'non_breeding_sex' & N_pi_daily_50 > 2, pairID]
pairs = ds[pair_type == 'same_sex_pair' & N_pi_daily_50 > 2, pairID]


pairs = dud_max[pair_type == 'breeding_pair_mg' | pair_type == 'breeding_pair', pairID]

p1 = 
ggplot(data = dp[pairID %in% pairs & year_ == 2018]) +
  ggtitle('2018') +
  geom_tile(aes(datetime_1, pairID, fill = interaction), show.legend = FALSE, width = 2500) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('PairID') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

p2 = 
ggplot(data = dp[pairID %in% pairs & year_ == 2019]) +
  ggtitle('2019') +
  geom_tile(aes(datetime_1, pairID, fill = interaction), show.legend = FALSE, width = 2500) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('PairID') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

p1 + p2 + plot_layout(ncol = 1, heights = c(4, 1))

# ggsave('./OUTPUTS/ALL_PAIRS/Non_breeders_interactions.png', plot = last_plot(),  width = 170, height = 250, units = c('mm'), dpi = 'print')
# ggsave('./OUTPUTS/ALL_PAIRS/same_sex_interactions.png', plot = last_plot(),  width = 170, height = 250, units = c('mm'), dpi = 'print')



pairs = dud_max[pair_type == 'breeding_pair_mg' | pair_type == 'breeding_pair', pairID]

p1 = 
  ggplot(data = dp[pairID %in% pairs & year_ == 2018]) +
  ggtitle('2018') +
  geom_tile(aes(datetime_1, pairID, fill = interaction), show.legend = FALSE, width = 2500) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('PairID') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

p2 = 
  ggplot(data = dp[pairID %in% pairs & year_ == 2019]) +
  ggtitle('2019') +
  geom_tile(aes(datetime_1, pairID, fill = interaction), show.legend = FALSE, width = 2500) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date') + ylab('PairID') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

p1 + p2 + plot_layout(ncol = 1, heights = c(1, 4))

# ggsave('./OUTPUTS/ALL_PAIRS/breeders_interactions.png', plot = last_plot(),  width = 170, height = 250, units = c('mm'), dpi = 'print')


ggplot(data = dp[pairID %in% pairs & year_ == 2018 & distance_pair < 100]) +
  geom_point(aes(datetime_1, distance_pair, color = interaction))



ggplot(data = dp[pairID == '270170055_270170704']) +
  geom_tile(aes(datetime_1, pairID, fill = N_pairwise_interactions_daily_per), show.legend = FALSE, width = 2500) +
  scale_fill_viridis() +
  xlab('Date relative to initiation') + ylab('Nest') +
  theme_classic()

ggplot(data = dp[pairID == '270170055_270170704']) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), show.legend = FALSE, width = 500) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  xlab('Date relative to initiation') + ylab('Nest') +
  theme_classic()



dus = du[pair_type == 'breeding_pair_mg']

setorder(dus, N_pairwise_interactions_per)

dus[, .(nestID, N_pairwise_interactions, N_pairwise_interactions_per)]


# first bout no interaction
dp[bout == 1, bout1_interaction := interaction == TRUE, by = pairID]

# longest separation before initiation

dp[datetime_1 < initiation & interaction == FALSE, longest_split_before_initiation := max(bout_length, na.rm = TRUE), by = pairID]

dp[longest_split_before_initiation > 360, last_long_split := max(bout_end), by = pairID]
dp[, last_long_split := max(last_long_split, na.rm = TRUE), by = pairID]



dp[nestID == 'R232_19']

dss = unique(dp[!is.na(longest_split_before_initiation)], by = 'nestID')
setorder(dss, -longest_split_before_initiation)

dss[, .(nestID, longest_split_before_initiation)]

#--------------------------------------------------------------------------------------------------------------
#' # Plot for each nest
#--------------------------------------------------------------------------------------------------------------

# sort by most time together before initiation
ds = dp[initiation_rel < 0 & interaction == TRUE, .N, by = nestID]
setorder(ds, -N)

# order nest ID
# dp[, nestID := factor(nestID %>% as.factor, levels = ds[, nestID])]



ggplot(data = dp[datetime_1 < nest_state_date & bout1_interaction != FALSE | is.na(bout1_interaction) & 
                   datetime_1 > last_long_split | is.na(last_long_split)]) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  scale_x_continuous(limits = c(-13, 1)) +
  theme_classic()

ggplot(data = dp[datetime_1 < nest_state_date & bout1_interaction != FALSE | is.na(bout1_interaction) & 
                   datetime_1 > last_long_split | is.na(last_long_split)]) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  facet_grid(.~any_EPY) +
  theme_classic()

ggplot(data = dp[datetime_1 < nest_state_date]) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  scale_x_continuous(limits = c(-13, 1)) +
  theme_classic()











ggplot(data = dp[datetime_1 < nest_state_date & bout1_interaction != FALSE | is.na(bout1_interaction)]) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

ggplot(data = dp[datetime_1 < nest_state_date]) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/Barplot_interactions.png', plot = last_plot(),  width = 250, height = 170, units = c('mm'), dpi = 'print')


ggplot(data = dp[datetime_1 < nest_state_date]) +
  geom_tile(aes(initiation_rel, nestID, fill = N_pairwise_interactions_daily_per), show.legend = FALSE, width = 0.5) +
  scale_fill_viridis() +
  # scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/Barplot_interactions_daily_per.png', plot = last_plot(),  width = 250, height = 170, units = c('mm'), dpi = 'print')


ggplot(data = dp[datetime_1 < nest_state_date & mg_before_initiation == TRUE]) +
  geom_tile(aes(initiation_rel, nestID, fill = N_pairwise_interactions_daily_per), show.legend = FALSE, width = 0.5) +
  scale_fill_viridis() +
  # scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

ggplot(data = dp[ID1 < ID2 & N_pairwise_interactions_daily_per_50 == TRUE & year_ == 2019 & breeding_pair == FALSE]) +
  geom_tile(aes(datetime_1, pairID, fill = N_pairwise_interactions_daily_per), show.legend = FALSE, width = 2500) +
  scale_fill_viridis() +
  # scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()


ggplot(data = dp[ID1 < ID2 & N_pairwise_interactions_daily_per_90 == TRUE & year_ == 2019 & breeding_pair == FALSE]) +
  geom_tile(aes(datetime_1, pairID, fill = N_pairwise_interactions_daily_per), show.legend = FALSE, width = 2500) +
  scale_fill_viridis() +
  # scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

ggplot(data = dps[ID1 < ID2 & N_pairwise_interactions_daily_per_50 == TRUE & year_ == 2019 & breeding_pair == FALSE]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), show.legend = FALSE, width = 2500) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()


dp[ID1 == 273145065 & ID2 == 270170834 & breeding_pair == FALSE]


ggplot(data = dp[clutch_together == 2]) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()


dp[, any_second_clutch := any(clutch_together == 2), by = pairID]

ggplot(data = dp[any_second_clutch == TRUE]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 2500, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_point(aes(initiation, pairID), size = 4) +
  geom_point(aes(nest_state_date, pairID), size = 4, color = 'grey') +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

ggplot(data = dp[year_ == 2019]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 2500, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_point(aes(initiation, pairID), size = 4) +
  geom_point(aes(nest_state_date, pairID), size = 4, color = 'grey') +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()


ggplot(data = dp[year_ == 2018]) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()



ggplot(data = dp[nestID == 'R231_19']) +
  geom_tile(aes(datetime_1, nestID, fill = interaction), width = 400, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()








dp = dp[!is.na(nestID)]

# round to days
dp[, initiation_rel0 := round(initiation_rel, 0)]

# daily points of both individuals
dp[, N_daily := .N, by = .(nestID, initiation_rel0)]

# daily interactions
dp[interaction == TRUE, N_together := .N, by = .(nestID, initiation_rel0)]
dp[interaction == FALSE, N_together := NA]
dp[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, initiation_rel0)]
dp[is.na(N_together), N_together := 0]

# unique data
ds = unique(dp, by = c('nestID', 'initiation_rel0'))
ds[, per_together := N_together / N_daily * 100]

# nests to exclude
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]

# exclude pairs before mate guarding started
# ds = ds[!(initiation_rel < 0 & per_together < 50)]

setorder(ds, initiation_rel0)

ggplot(data = ds[datetime_1 < nest_state_date]) +
  geom_point(aes(initiation_rel0, per_together, color = N_daily, group = nestID), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel0, per_together, color = N_daily, group = nestID), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, name = 'N positions') +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  scale_x_continuous(limits = c(-10, 16)) +
  theme(legend.position = c(0.8, 0.7))

# ggsave('./OUTPUTS/ALL_PAIRS/Percentage_daily_interactions.png', plot = last_plot(),  width = 177, height = 150, units = c('mm'), dpi = 'print')


ggplot(data = ds[datetime_1 < nest_state_date]) +
  geom_point(data = ds[datetime_1 < nest_state_date], aes(initiation_rel0, per_together, group = nestID), 
             color = 'grey75', size = 2, alpha = 1) +
  geom_point(data = ds[datetime_1 < nest_state_date & any_EPY == TRUE], aes(initiation_rel0, per_together, group = nestID), 
             color = 'firebrick3', size = 3, alpha = 1) +
  geom_path(data = ds[datetime_1 < nest_state_date], aes(initiation_rel0, per_together, group = nestID), 
            color = 'grey75', size = 1, alpha = 0.5) +
  geom_path(data = ds[datetime_1 < nest_state_date & any_EPY == TRUE], aes(initiation_rel0, per_together, group = nestID), 
            color = 'firebrick3', size = 1, alpha = 0.5) +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  geom_vline(aes(xintercept = 3), color = 'firebrick2', size = 1, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  scale_x_continuous(limits = c(-10, 16)) +
  theme(legend.position = c(0.8, 0.7))




ds[initiation_rel0 == -3 & any_EPY == TRUE, .(nestID, per_together)]




# split and merges

ggplot(data = dp[split == TRUE]) +
  geom_bar(aes(initiation_rel0, fill = split_ID)) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/splits.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


ggplot(data = dp[merge == TRUE]) +
  geom_bar(aes(initiation_rel0, fill = merge_ID)) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/merges.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')



ds = dp[split == TRUE]
ds = unique(ds, by = c('ID1', 'ID2', 'bout'))

ds[, bout_seq_max_factor := factor(bout_seq_max)]
ds[bout_seq_max > 18, bout_seq_max_factor := '>18']

ggplot(data = ds) +
  geom_bar(aes(initiation_rel0, fill = bout_seq_max_factor)) +
  scale_fill_viridis(discrete = TRUE, direction = -1) +
  scale_x_continuous(limits = c(-10, 16)) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/bout_lenght_split.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')




# N daily interactions
ds = unique(dp, by = c('nestID', 'date_'))
ds[, per_together := N_together / N_daily * 100]
ds[, per_sampled := N_daily / 140 * 100]

ds = ds[initiation_rel0 > -9 & initiation_rel0 < 17]

# nests to exclude
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]


dss = ds[, .N, by = initiation_rel0]

ggplot(data = ds) +
  geom_boxplot(aes(as.factor(initiation_rel0), per_together), varwidth = TRUE) +
  geom_vline(aes(xintercept = '0'), color = 'firebrick2', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(as.factor(initiation_rel0), Inf, label = N), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/ALL_PAIRS/per_together.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


ggplot(data = ds) +
  geom_boxplot(aes(as.factor(initiation_rel0), median_dist), varwidth = TRUE) +
  geom_vline(aes(xintercept = '0'), color = 'firebrick2', size = 1, alpha = 0.3) +
  geom_hline(aes(yintercept = 30), color = 'grey50', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(as.factor(initiation_rel0), Inf, label = N), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  scale_y_continuous(limits = c(0, 1000)) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Within-pair median distance') +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/ALL_PAIRS/median_dist.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')



ggplot(data = ds) +
  geom_boxplot(aes(as.factor(initiation_rel0), median_dist_cor), varwidth = TRUE) +
  geom_vline(aes(xintercept = '0'), color = 'firebrick2', size = 1, alpha = 0.3) +
  geom_hline(aes(yintercept = 30), color = 'grey50', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(as.factor(initiation_rel0), Inf, label = N), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  scale_y_continuous(limits = c(0, 1000)) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Within-pair median distance') +
  theme_classic(base_size = 12)

# ggsave('./OUTPUTS/ALL_PAIRS/median_dist_cor.png', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


























#--------------------------------------------------------------------------------------------------------------
#' # Space use
#--------------------------------------------------------------------------------------------------------------

# merge with spatio-temporal clusters
do[, ID := as.integer(ID)]

dp = merge(dp, do[, .(ID, datetime_, nestID, c_start1 = start, c_end1 = end, clustID1 = clustID, 
                      s_clustID1 = s_clustID, st_clustID1 = st_clustID)], 
           by.x = c('ID1', 'datetime_1', 'nestID'), by.y = c('ID', 'datetime_', 'nestID'), all.x = TRUE)

dp = merge(dp, do[, .(ID, datetime_, nestID, c_start2 = start, c_end2 = end, clustID2 = clustID, 
                      s_clustID2 = s_clustID, st_clustID2 = st_clustID)], 
           by.x = c('ID2', 'datetime_2', 'nestID'), by.y = c('ID', 'datetime_', 'nestID'), all.x = TRUE)

# subset breeders
ds = dp[!is.na(nestID)]


# positions at nest
ds[, distance_nest1 := sqrt(sum((c(lon1, lat1) - c(lon_n, lat_n))^2)) , by = 1:nrow(ds)]
ds[, distance_nest2 := sqrt(sum((c(lon2, lat2) - c(lon_n, lat_n))^2)) , by = 1:nrow(ds)]

# at nest?
ds[, ID1_at_nest := distance_nest1 < 30]
ds[, ID2_at_nest := distance_nest2 < 30]

ds1 = ds[ID1_at_nest == TRUE & !is.na(s_clustID1), .N, .(year_, ID1, s_clustID1, nestID)]
ds2 = ds[ID2_at_nest == TRUE & !is.na(s_clustID2), .N, .(year_, ID1, s_clustID2, nestID)]

# check if unique
ds1 %>% nrow == unique(ds1, by = c('year_', 'ID1', 'nestID')) %>% nrow
ds2 %>% nrow == unique(ds2, by = c('year_', 'ID1', 'nestID')) %>% nrow

setnames(ds1, 's_clustID1', 'nest_clust')

ds = merge(ds, ds1[, .(nestID, nest_clust)], by = 'nestID', all.x = TRUE)

# Times a site was used
ds1 = unique(ds[!is.na(clustID1) & !is.na(s_clustID1), .(ID1, nestID, clustID1, s_clustID1)])
ds1 = ds1[, .N, by = .(ID1, nestID, s_clustID1)]
setnames(ds1, 'N', 's_clustID1_N')

ds = merge(ds, ds1[, .(nestID, s_clustID1, s_clustID1_N)], by = c('nestID', 's_clustID1'), all.x = TRUE)

ds[, visited_once := s_clustID1_N == 1]
ds[is.na(s_clustID1), visited_once := NA]
ds[is.na(s_clustID1_N), visited_once := NA]

ds[is.na(s_clustID2), visited_once := NA]
ds[is.na(s_clustID2_N), visited_once := NA]

# 




ds[, st_overlap := st_clustID1 == st_clustID2]
dss = ds[nestID == 'R219_19']


dss = ds[nestID == 'R304_18']
dos = do[nestID == 'R304_18']

dss = ds[nestID == 'R605_19']

ggplot(data = dss) +
  geom_hline(yintercept = dss[1, nest_clust]) +
  geom_vline(xintercept = dss[1, initiation]) +
  geom_point(aes(datetime_1, s_clustID1, color = visited_once)) +
  geom_point(aes(datetime_2, s_clustID2+0.1, color = visited_once))

ggplot(data = dos) +
  geom_hline(yintercept = dss[1, nest_clust]) +
  geom_vline(xintercept = dss[1, initiation]) +
  geom_point(data = dss[interaction == TRUE], aes(datetime_1, s_clustID1+0.05), color = 'grey70', size = 7, alpha = 0.5) +
  geom_point(data = dos[sex == 'M'], aes(datetime_, s_clustID), color = 'dodgerblue3') +
  geom_point(data = dos[sex == 'F'], aes(datetime_, s_clustID+0.1), color = 'firebrick3') +
  ggtitle(dss[1, nestID]) +
  xlab('Datetime') + ylab('Spatial cluster') +
  theme_classic()








foreach(i = ds[, nestID] %>% unique, .packages = c('data.table', 'ggplot2')) %do%{
  
  # subset data
  dss = ds[nestID == i]
  dos = do[nestID == i]
  
  # plot for each nest
  ggplot(data = dos) +
    geom_hline(yintercept = dss[1, nest_clust]) +
    geom_vline(xintercept = dss[1, initiation]) +
    geom_point(data = dss[interaction == TRUE], aes(datetime_1, s_clustID1+0.05), color = 'grey70', size = 7, alpha = 0.5) +
    geom_point(data = dos[sex == 'M'], aes(datetime_, s_clustID), color = 'dodgerblue3') +
    geom_point(data = dos[sex == 'F'], aes(datetime_, s_clustID+0.1), color = 'firebrick3') +
    ggtitle(dss[1, nestID]) +
    xlab('Datetime') + ylab('Spatial cluster') +
    theme_classic()
  
  ggsave(paste0('./OUTPUTS/SPACE_USE_PAIRS/', i, '.png'), plot = last_plot(),
         width = 250, height = 177, units = c('mm'), dpi = 'print')

}









ggplot(data = dss) +
  geom_point(aes(datetime_1, s_clustID1, color = interaction)) +
  geom_point(aes(datetime_2, s_clustID2+0.1, color = interaction))

ggplot(data = dss) +
  geom_point(aes(datetime_1, s_clustID1, color = ID1_at_nest)) +
  geom_point(aes(datetime_2, s_clustID2+0.1, color = ID2_at_nest))


ggplot(data = dss) +
  geom_point(aes(datetime_1, st_clustID1), color = 'blue') +
  geom_point(aes(datetime_2, st_clustID2+0.1), color = 'red')



bm = create_bm(dss, lat = 'lat1', lon = 'lon1')

bm + 
  geom_point(data = dss[1, ], aes(lon_n, lat_n), color = 'black', size = 15) +
  geom_point(data = dss, aes(lon1, lat1, color = as.character(st_clustID1))) +
  geom_point(data = dss, aes(lon2, lat2, color = as.character(st_clustID2)))
  
bm + 
  geom_point(data = dss[1, ], aes(lon_n, lat_n), color = 'black', size = 15) +
  geom_point(data = dss, aes(lon1, lat1, color = as.character(visited_once))) +
  geom_point(data = dss, aes(lon2, lat2, color = as.character(visited_once)))


  
bm + 
  geom_point(data = ds, aes(lon1, lat1, color = as.character(visited_once))) +
  geom_point(data = ds, aes(lon2, lat2, color = as.character(visited_once)))


bm + 
  geom_point(data = ds[visited_once == TRUE], aes(lon1, lat1, color = as.character(visited_once))) +
  geom_point(data = ds[visited_once == TRUE], aes(lon2, lat2, color = as.character(visited_once)))
  
  
bm + 
  geom_point(data = ds[visited_once == FALSE & year_ == 2019], aes(lon1, lat1, color = datetime_1)) +
  geom_point(data = ds[visited_once == FALSE & year_ == 2019], aes(lon2, lat2, color = datetime_2))



