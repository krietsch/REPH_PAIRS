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
dp = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dg = dbq(con, 'select * FROM SEX')
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation, tz = 'UTC')]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S', tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

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

# check overlap with initiation date
dn[, overlap_initiation_m := DescTools::Overlap(c(start_m, end_m), c(initiation - 86400, initiation + 86400)), by = nestID]
dn[, overlap_initiation_f := DescTools::Overlap(c(start_f, end_f), c(initiation - 86400, initiation + 86400)), by = nestID]
dn = dn[overlap_initiation_m > 0 & overlap_initiation_f > 0]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# create directory for each of these breeding pairs
dnID[, directory := paste0('//ds/grpkempenaers/Hannes/temp/PAIRS_ANIMATION_EACH/', nestID)]
dnID[, dir.create(file.path(directory), showWarnings = FALSE), by = 1:nrow(dnID)]

# unique IDs
IDu = unique(c(dnID[,  male_id], dnID[,  female_id]))

# subset d
d = d[ID %in% IDu]

# merge with nest and initiation date
dnIDu = rbind(dnID[, .(year_, ID = female_id, nestID, initiation, sex = 'F')], 
              dnID[, .(year_, ID = male_id, nestID, initiation, sex = 'M')])

d = merge(d, dnIDu[, .(year_, ID, nestID, initiation, sex)], by = c('ID', 'year_'), all.x = TRUE, allow.cartesian = TRUE)
d = d[!is.na(nestID)]

#--------------------------------------------------------------------------------------------------------------
#' # Define interactions
#--------------------------------------------------------------------------------------------------------------

distance_threshold = 30

# merge with nests
dp = merge(dp, dnID, by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'))

# relative nest initiation date
dp[, initiation_rel := difftime(datetime_1, initiation, units = 'days') %>% as.numeric()]

### positions before and after

# shift positions
dp[, lat1_before := shift(lat1, type = 'lag'), by = nestID]
dp[, lon1_before := shift(lon1, type = 'lag'), by = nestID]
dp[, lat2_before := shift(lat2, type = 'lag'), by = nestID]
dp[, lon2_before := shift(lon2, type = 'lag'), by = nestID]

dp[, lat1_next := shift(lat1, type = 'lead'), by = nestID]
dp[, lon1_next := shift(lon1, type = 'lead'), by = nestID]
dp[, lat2_next := shift(lat2, type = 'lead'), by = nestID]
dp[, lon2_next := shift(lon2, type = 'lead'), by = nestID]

# distance to position before and after
dp[, distance1_before := sqrt(sum((c(lon1, lat1) - c(lon1_before, lat1_before))^2)) , by = 1:nrow(dp)]
dp[, distance1_next := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)) , by = 1:nrow(dp)]
dp[, distance2_before := sqrt(sum((c(lon2, lat2) - c(lon2_before, lat2_before))^2)) , by = 1:nrow(dp)]
dp[, distance2_next := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)) , by = 1:nrow(dp)]

# interactions
dp[, interaction := distance_pair < c(distance1_before + distance2_before + distance_threshold), by = 1:nrow(dp)]
# dp[, interaction := distance_pair < c(max(distance1_before, distance2_before) + distance_threshold), by = 1:nrow(dp)]

# simple interactions
dp[, interaction_threshold := distance_pair < distance_threshold]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]

dp[, any_interaction_threshold := any(interaction_threshold == TRUE), by = .(nestID, bout)]
dp[any_interaction_threshold == FALSE, interaction := FALSE]

# split points and merging points
dp[, interaction_next := shift(interaction, type = 'lead'), by = nestID]
dp[, interaction_before := shift(interaction, type = 'lag'), by = nestID]

# correct for true splits
dp[interaction == TRUE & interaction_next == FALSE & distance_pair > distance_threshold, interaction := FALSE]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = nestID]
dp[, bout_seq := seq_len(.N), by = .(nestID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(nestID, bout)]

# split points and merging points
dp[, interaction_next := shift(interaction, type = 'lead'), by = nestID]
dp[, interaction_before := shift(interaction, type = 'lag'), by = nestID]
dp[, split := interaction_before == TRUE & interaction == FALSE]
dp[, merge := interaction_before == FALSE & interaction == TRUE]

# which ID split?
dp[split == TRUE, split_ID := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]

# which ID approached?
dp[merge == TRUE, merge_ID := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]

# mean and median
dp[, date_ := as.Date(datetime_1)]
dp[, mean_dist := mean(distance_pair, na.rm = TRUE), by = .(nestID, date_)]
dp[, median_dist := median(distance_pair, na.rm = TRUE), by = .(nestID, date_)]

# median corrected
dp[, distance_pair_cor := ifelse(interaction == TRUE & distance_pair > distance_threshold, distance_threshold, distance_pair)]
dp[interaction == FALSE, distance_pair_cor := distance_pair]
dp[, median_dist_cor := median(distance_pair_cor, na.rm = TRUE), by = .(nestID, date_)]


#--------------------------------------------------------------------------------------------------------------
#' # Plot for each nest
#--------------------------------------------------------------------------------------------------------------

# sort by most time together before initiation
ds = dp[initiation_rel < 0 & interaction == TRUE, .N, by = nestID]
setorder(ds, -N)

# order nest ID
dp[, nestID := factor(nestID %>% as.factor, levels = ds[, nestID])]


ggplot(data = dp) +
  geom_tile(aes(initiation_rel, nestID, fill = interaction), width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

# ggsave('./OUTPUTS/ALL_PAIRS/Barplot_interactions.png', plot = last_plot(),  width = 177, height = 170, units = c('mm'), dpi = 'print')


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
ds = ds[!(initiation_rel < 0 & per_together < 50)]

setorder(ds, initiation_rel0)

ggplot(data = ds) +
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


