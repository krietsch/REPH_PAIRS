
# read data
dp = fread('./DATA/PAIR_WISE_DIST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_10min)]

# interactions
dp[, interaction := distance < 10]

ds = unique(dp, by = c('ID1', 'datetime_10min'))
ds_N = ds[, .(N_obs_day = .N), by = .(ID1, date_)]
ds = dp[interaction == TRUE, .(Np_obs_day = .N), by = .(ID1, ID2, date_)]

ds = merge(ds, ds_N, by = c('ID1', 'date_'), all.x = TRUE)

ds[, Np_obs_day_per := Np_obs_day / N_obs_day]


hist(ds$N_obs_day)

ds[, year_ := year(date_)]

ggplot(data = ds[year_ == 2019]) +
  geom_point(aes(date_, Np_obs_day_per)) +
  scale_x_date(date_breaks = "weeks", date_labels = "%d-%m")


ggplot(data = ds[year_ == 2019]) +
  geom_boxplot(aes(as.character(date_), Np_obs_day_per)) 


# number of daily interactions


