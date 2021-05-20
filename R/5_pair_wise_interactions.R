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

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'auksRuak', 'foreach', 'knitr', 'windR'), 
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

# save data
fwrite(dp, './DATA/PAIR_WISE_INTERACTIONS.txt', quote = TRUE, sep = '\t', row.names = FALSE)

