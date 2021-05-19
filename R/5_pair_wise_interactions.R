#' ---
#' title: Calculate pair-wise interactions
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
# Calculate spatio-temporal distance of points
#==============================================================================================================

# Summary
# 1. Apply speed filter 
# 2. Apply distance filter 
# 3. Check altitudes

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'auksRuak', 'foreach', 'knitr'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Lines to run to create html output
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# rmarkdown::render('./R/3_spatio_temporal_distance.R', output_dir = './OUTPUTS/R_COMPILED')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
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
dn[, nest_state_date := as.POSIXct(nest_state_date, tz = 'UTC')]
DBI::dbDisconnect(con)

# change projection
st_transform_DT(dn)

#--------------------------------------------------------------------------------------------------------------
#' # Define interactions
#--------------------------------------------------------------------------------------------------------------

distance_threshold = 30

# pair ID
dp[, pairID := paste0(ID1, '_', ID2)]

### positions before and after

# shift positions
dp[, lat1_before := shift(lat1, type = 'lag'), by = pairID]
dp[, lon1_before := shift(lon1, type = 'lag'), by = pairID]
dp[, lat2_before := shift(lat2, type = 'lag'), by = pairID]
dp[, lon2_before := shift(lon2, type = 'lag'), by = pairID]

dp[, lat1_next := shift(lat1, type = 'lead'), by = pairID]
dp[, lon1_next := shift(lon1, type = 'lead'), by = pairID]
dp[, lat2_next := shift(lat2, type = 'lead'), by = pairID]
dp[, lon2_next := shift(lon2, type = 'lead'), by = pairID]

# distance to position before and after
dp[, distance1_before := sqrt(sum((c(lon1, lat1) - c(lon1_before, lat1_before))^2)), by = 1:nrow(dp)]
dp[, distance1_next   := sqrt(sum((c(lon1, lat1) - c(lon1_next, lat1_next))^2)),     by = 1:nrow(dp)]
dp[, distance2_before := sqrt(sum((c(lon2, lat2) - c(lon2_before, lat2_before))^2)), by = 1:nrow(dp)]
dp[, distance2_next   := sqrt(sum((c(lon2, lat2) - c(lon2_next, lat2_next))^2)),     by = 1:nrow(dp)]

# interactions
dp[, interaction := distance_pair < c(distance1_before + distance2_before + distance_threshold), by = 1:nrow(dp)]

# simple interactions
dp[, interaction_threshold := distance_pair < distance_threshold]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = pairID]
dp[, bout_seq := seq_len(.N), by = .(pairID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(pairID, bout)]

dp[, any_interaction_threshold := any(interaction_threshold == TRUE), by = .(pairID, bout)]
dp[any_interaction_threshold == FALSE, interaction := FALSE]

# split points and merging points
dp[, interaction_next := shift(interaction, type = 'lead'), by = pairID]
dp[, interaction_before := shift(interaction, type = 'lag'), by = pairID]

# correct for true splits
dp[interaction == TRUE & interaction_next == FALSE & distance_pair > distance_threshold, interaction := FALSE]

# count bouts of split and merge
dp[, bout := bCounter(interaction), by = pairID]
dp[, bout_seq := seq_len(.N), by = .(pairID, bout)]
dp[, bout_seq_max := max(bout_seq), by = .(pairID, bout)]

# split points and merging points
dp[, interaction_next := shift(interaction, type = 'lead'), by = pairID]
dp[, interaction_before := shift(interaction, type = 'lag'), by = pairID]
dp[, split := interaction_before == TRUE & interaction == FALSE]
dp[, merge := interaction_before == FALSE & interaction == TRUE]

# which ID split?
dp[split == TRUE, split_ID := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]

# which ID approached?
dp[merge == TRUE, merge_ID := ifelse(distance1_before > distance2_before, 'ID1', 'ID2')]


dps = dp[interaction == TRUE, .(N_interactions = .N), by = .(pairID, ID1, ID2)]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')

# as integer
dnID[, male_id := as.integer(male_id)]
dnID[, female_id := as.integer(female_id)]

# merge with nests
dps = merge(dps, dnID[, .(male_id, female_id, nestID1 = nestID)], by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'), all.x = TRUE)
dps = merge(dps, dnID[, .(male_id, female_id, nestID2 = nestID)], by.x = c('ID1', 'ID2'), by.y = c('female_id', 'male_id'), all.x = TRUE)


dps[!is.na(nestID1) | !is.na(nestID2), breeding_pair := TRUE]
dps[is.na(breeding_pair), breeding_pair := FALSE]



ggplot(data = dps) +
  geom_boxplot(aes(breeding_pair, N_interactions))


dps[N_interactions > 600 & breeding_pair == FALSE]


dp[interaction == TRUE, N_interactions := .N, by = .(pairID, ID1, ID2)]
dp[, N_interactions := mean(N_interactions, na.rm = TRUE), by = .(pairID, ID1, ID2)]

# merge with nests
dp = merge(dp, dnID[, .(male_id, female_id, nestID1 = nestID)], by.x = c('ID1', 'ID2'), by.y = c('male_id', 'female_id'), all.x = TRUE, allow.cartesian = TRUE)
dp = merge(dp, dnID[, .(male_id, female_id, nestID2 = nestID)], by.x = c('ID1', 'ID2'), by.y = c('female_id', 'male_id'), all.x = TRUE, allow.cartesian = TRUE)

dp[!is.na(nestID1) | !is.na(nestID2), breeding_pair := TRUE]
dp[is.na(breeding_pair), breeding_pair := FALSE]


dp[, year_ := year(datetime_1)]

ggplot(data = dp[N_interactions > 600 & breeding_pair == FALSE & year_ == 2019]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()

ggplot(data = dp[N_interactions > 600 & breeding_pair == FALSE & year_ == 2018]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()



dp[pairID == '270170055_270170704' & interaction == TRUE]
