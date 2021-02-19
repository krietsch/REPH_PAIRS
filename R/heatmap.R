#==============================================================================================================
# Mapy by ID
#==============================================================================================================

# Summary
# 1. Data available
# 2. Data until 
# 3. Data linked to nests

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# read data
dp = fread('./DATA/PAIR_WISE_DIST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_10min)]
dpn = fread('./DATA/PAIR_WISE_DIST_NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dpn[, date_ := as.Date(datetime_10min)]

# interactions
dp[, interaction := distance < 10]
dpn = dpn[distance < 10]

ds = dpn[, .N, by = .(ID1, ID2)]
ds[, ID1 := as.character(ID1)]

ds[, N_total := sum(N), by = ID1]
ds[, N_rel := N / N_total * 100]

hist(ds[N > 50]$N)

ggplot(data = ds) + 
  geom_tile(aes(ID2, ID1, fill = N_rel)) +
  scale_fill_viridis(direction = -1)



dp[, year_ := year(datetime_10min)]

ds = dp[year_ == 2018 & interaction == TRUE, .N, by = .(ID1, ID2)]
ds[, ID1 := as.character(ID1)]
ds[, ID2 := as.character(ID2)]


ds[, N_total := sum(N), by = ID1]
ds[, N_rel := N / N_total * 100]

hist(ds[N > 50]$N)

ggplot(data = ds) + 
  geom_tile(aes(ID1, ID2, fill = N_rel)) +
  scale_fill_viridis(direction = -1)






















