#==============================================================================================================
# Interaction on a population level
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
#' # Interactions on a population level
#--------------------------------------------------------------------------------------------------------------


dps = dp[interaction == TRUE, .(N_interactions = .N), by = .(pairID, ID1, ID2)]

# nest data
dnID = dn[, .(year_, nestID, male_id, female_id, initiation, initiation_y, nest_state_date, lat_n = lat, lon_n = lon)]
dnID = unique(dnID, by = 'nestID')
dnID = unique(dnID, by = c('male_id', 'female_id'))

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
dp = merge(dp, dnID[, .(year_, male_id, female_id, nestID1 = nestID)], by.x = c('ID1', 'ID2', 'year_'), by.y = c('male_id', 'female_id', 'year_'), all.x = TRUE)
dp = merge(dp, dnID[, .(year_, male_id, female_id, nestID2 = nestID)], by.x = c('ID1', 'ID2', 'year_'), by.y = c('female_id', 'male_id', 'year_'), all.x = TRUE)

dp[!is.na(nestID1) | !is.na(nestID2), breeding_pair := TRUE]
dp[is.na(breeding_pair), breeding_pair := FALSE]


dp[, year_ := year(datetime_1)]

ggplot(data = dp[ID1 < ID2 & N_interactions > 200 & breeding_pair == FALSE & year_ == 2019]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()


ggplot(data = dp[ID1 < ID2 & N_interactions > 100 & year_ == 2019]) +
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



# interactions per day
dp[, date_1 := as.Date(datetime_1)]

dp[, N_pair_wise_positions := .N, by = .(pairID, ID1, ID2, date_1)]
dps = dp[interaction == TRUE, .(N_interactions = .N), by = .(pairID, breeding_pair, ID1, ID2, date_1, N_pair_wise_positions)]

dps[, percent_together := N_interactions / N_pair_wise_positions * 100]
dps[, any_above_50_together := any(percent_together > 50), by = pairID]

dps = unique(dps, by = c('pairID'))

dp = merge(dp, dps[, .(pairID, any_above_50_together)], by = 'pairID', all.x = TRUE)


ggplot(data = dp[ID1 < ID2 & any_above_50_together == TRUE & breeding_pair == FALSE & year_ == 2019]) +
  geom_tile(aes(datetime_1, pairID, fill = interaction), width = 900, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = 'green4', 'FALSE' = 'firebrick3', 'NA' = 'grey50')) +
  # geom_vline(aes(xintercept = 0), color = 'black', size = 3, alpha = 0.5) +
  # geom_vline(aes(xintercept = 3), color = 'black', size = 3, alpha = 0.5) +
  xlab('Date relative to initiation') + ylab('Nest') +
  # scale_x_continuous(limits = c(-12, 12)) +
  theme_classic()



