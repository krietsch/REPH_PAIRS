#==============================================================================================================
# Mate guarding
#==============================================================================================================

# Summary

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 
          'stringr', 'ggnewscale', 'doFuture', 'patchwork'), 
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
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date, any_EPY, m_sired_EPY, 
              lat_n = lat, lon_n = lon)]
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
#' # Percentage of daily interactions
#--------------------------------------------------------------------------------------------------------------

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2', 'year_'), by.y = c('male_id', 'female_id', 'year_'), all.x = TRUE, allow.cartesian = TRUE)

# unique data
dp = unique(dp)

# relative initiation date
dp[, initiation_rel := difftime(initiation, initiation_mean, units = 'days') %>% as.numeric %>% round(., 0)]

# early and late clutches?
dp[initiation_rel < -3, initiation_type := 'early']
dp[initiation_rel > 3, initiation_type := 'late']
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
dps[, at_nest := distance_nest_1 < 10]

dps[at_nest == TRUE, N_pairwise_positions_daily_at_nest := .N, by = .(year_, pairID, nestID, date_)]
dps[, N_pairwise_positions_daily_at_nest := mean(N_pairwise_positions_daily_at_nest, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dps[, N_pairwise_positions_daily_at_nest_per := N_pairwise_positions_daily_at_nest / N_pairwise_positions_daily * 100]

# no interaction and at nest? 
dps[, no_interaction_at_nest := interaction == FALSE & at_nest == TRUE]
dps[no_interaction_at_nest == TRUE, N_no_interaction_at_nest := .N, by = .(year_, pairID, nestID, date_)]
dps[, N_no_interaction_at_nest := mean(N_no_interaction_at_nest, na.rm = TRUE), by = .(year_, pairID, nestID, date_)]
dps[, percent_at_nest_when_no_interaction := N_no_interaction_at_nest / N_pairwise_no_interactions_daily * 100]

# merge back with data
dp = merge(dp, dps[, .(ID1, ID2, datetime_1, nestID, at_nest, N_pairwise_positions_daily_at_nest_per, 
                       N_no_interaction_at_nest, percent_at_nest_when_no_interaction)], 
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


#--------------------------------------------------------------------------------------------------------------
#' # Null model for interactions
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



ggplot(data = dud[breeding_pair == TRUE]) +
  geom_boxplot(aes(datetime_rel_initiation0, N_pairwise_positions_daily_at_nest_per, 
                   group = interaction(datetime_rel_initiation0)), varwidth = TRUE) +
  
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_positions_daily_at_nest_per)) +
  
  geom_smooth(aes(datetime_rel_initiation0, N_pairwise_interactions_daily_per)) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 1, alpha = 0.3) +
  scale_color_manual(values = c('darkorange', 'dodgerblue3'), name = 'Male sired EPY') +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  scale_x_continuous(limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-10, 110)) +
  theme_classic(base_size = 12)


ggplot(data = dud[breeding_pair == TRUE & datetime_rel_initiation0 > -3 & datetime_rel_initiation0 < 4]) +
  geom_point(aes(N_pairwise_positions_daily_at_nest_per, N_pairwise_interactions_daily_per)) +
  geom_smooth(aes(N_pairwise_positions_daily_at_nest_per, N_pairwise_interactions_daily_per), method = 'lm') +
  theme_classic(base_size = 12)

ggplot(data = dud[breeding_pair == TRUE & datetime_rel_initiation0 > -3 & datetime_rel_initiation0 < 4]) +
  geom_point(aes(N_pairwise_positions_daily_at_nest_per, N_pairwise_interactions_daily_per, 
                 color = as.factor(datetime_rel_initiation0))) +
  geom_smooth(aes(N_pairwise_positions_daily_at_nest_per, N_pairwise_interactions_daily_per, 
                  color = as.factor(datetime_rel_initiation0)), method = 'lm') +
  guides(color = guide_legend('Initiation std')) +
  theme_classic(base_size = 12)


# When the male is not with the female is it at the nest?
ggplot(data = dud[breeding_pair == TRUE & datetime_rel_initiation0 > -3 & datetime_rel_initiation0 < 4]) +
  geom_boxplot(aes(datetime_rel_initiation0, percent_at_nest_when_no_interaction, 
                 color = as.factor(datetime_rel_initiation0))) +
  geom_smooth(aes(datetime_rel_initiation0, percent_at_nest_when_no_interaction, 
                  color = as.factor(datetime_rel_initiation0)), method = 'lm') +
  guides(color = guide_legend('Initiation std')) +
  theme_classic(base_size = 12)



