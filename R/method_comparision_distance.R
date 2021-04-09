#==============================================================================================================
# Method comparison 10 min intervals vs. closest time
#==============================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'auksRuak', 'foreach', 'knitr'), 
        require, character.only = TRUE)

# Data
dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
dp[, time_btw := abs(as.numeric(difftime(datetime_1, datetime_2, units = 'min')))]

dpn = fread('./DATA/PAIR_WISE_DIST_CLOSEST.txt', sep = '\t', header = TRUE) %>% data.table

# comparisions
dp %>% nrow
dpn %>% nrow

# difference between positions in time
ggplot(data = dp) +
  geom_histogram(aes(time_btw))

ggplot(data = dpn) +
  geom_histogram(aes(time_btw))

# number of interactions
dp[, interaction := distance < 30]
dpn[, interaction := distance_pair < 30]

dp[interaction == TRUE] %>% nrow
dpn[interaction == TRUE] %>% nrow

# subset pair
dps = dp[ID1 == 270170746 & ID2 == 270170747] # R304_18
dpns = dpn[ID1 == 270170746 & ID2 == 270170747] # R304_18

dps[interaction == TRUE] %>% nrow / dps %>% nrow * 100
dpns[interaction == TRUE] %>% nrow / dpns %>% nrow * 100

