# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'patchwork'),
        require, character.only = TRUE)

# read data
dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table

ds = dp[, .(closest = min(distance, na.rm = TRUE)), by = .(ID1, datetime_10min)]
ds[, date_ := as.Date(datetime_10min)]

ds[closest < 5, type := '<05 m']
ds[is.na(type) & closest < 10, type := '<10 m']
ds[is.na(type) & closest < 15, type := '<15 m']
ds[is.na(type) & closest < 20, type := '<20 m']
ds[is.na(type) & closest < 30, type := '<30 m']
ds[is.na(type) & closest < 50, type := '<50 m']
ds[is.na(type), type := '>50 m']

ds = ds[, .N, by = .(date_, type)]
ds[, type := factor(type, levels = unique(type[rev(order(type))]))]
ds[, year_ := year(date_)]
y = format(Sys.Date(), '%Y') # current year
ds[, date_y := as.Date(paste0(y, substr(date_, 5, 10)))]

# plot settings
min_date = ds[, date_y] %>% min
max_date = ds[, date_y] %>% max


p1 = 
ggplot(data = ds[year_ == 2018]) +
  ggtitle('2018') +
  geom_bar(aes(date_y, N/1000, fill = type), width = 0.95, color = 'grey80', size = 0.1, position = 'stack', stat = 'identity') +
  scale_fill_viridis(discrete = TRUE, direction = -1, name = 'Closest bird') +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, 18), expand = c(0, 0)) +
  xlab('') + ylab('Number of positions (thousand)') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.8, 0.80), legend.key.width = unit(0.4, 'cm'), 
        legend.key.height = unit(0.4, 'cm'), legend.background = element_rect(fill = alpha('white', 0)), 
        axis.title.x = element_blank(), axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5))

p2 = 
ggplot(data = ds[year_ == 2019]) +
  ggtitle('2019') +
  geom_bar(aes(date_y, N/1000, fill = type), width = 0.95, color = 'grey80', size = 0.1, position = 'stack', stat = 'identity') +
  scale_fill_viridis(discrete = TRUE, direction = -1, name = 'Closest bird') +
  scale_x_date(limits = c(min_date, max_date), date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, 18), expand = c(0, 0)) +
  xlab('') + ylab('Number of positions (thousand)') +
  theme_classic(base_size = 11) +
  theme(legend.position = 'none', axis.title.x = element_blank(), axis.text.x=element_blank(),
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5))


# link to nest initiation 
dn = read.table('./DATA/NESTS.txt', sep = '\t', header = TRUE) %>% data.table
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

# subset data from tagged birds
dID = unique(d, by = c('year_', 'ID'))
dn = merge(dn, dID[, .(year_, female_id = ID, female_tagged = TRUE)], by = c('year_', 'female_id'), all.x = TRUE)
dn = merge(dn, dID[, .(year_, male_id = ID, male_tagged = TRUE)], by = c('year_', 'male_id'), all.x = TRUE)

dn[female_tagged == TRUE & male_tagged == TRUE, tagged := 'both']
dn[is.na(female_tagged) & male_tagged == TRUE, tagged := 'male']
dn[is.na(male_tagged) & female_tagged == TRUE, tagged := 'female']
dn[is.na(tagged), tagged := 'none']

# subset nest with at least one bird tagged
ds = dn[tagged != 'none']

# sample size
dss = ds[, .(median = median(initiation_y), q25 = quantile(initiation_y, probs = c(0.25)),
             q75 = quantile(initiation_y, probs = c(0.75)), .N, max = max(initiation_y)), by = year_]

p3 = 
  ggplot(data = ds[year_ == 2018]) +
  geom_violin(aes(initiation_y, as.character(year_)), show.legend = FALSE, fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_point(data = dss[year_ == 2018], aes(median, as.character(year_)), size = 2) +
  geom_linerange(data = dss[year_ == 2018], aes(y = as.character(year_), xmin = q75, xmax = q25), size = 0.5) +
  geom_text(data = dss[year_ == 2018], aes(as.POSIXct(min_date), as.character(year_), label = paste0('N = ', N)), 
            hjust = 0.2, vjust = -1.1, size = 3) +
  scale_x_datetime(limits = c(as.POSIXct(min_date), as.POSIXct(max_date)), date_breaks = "2 weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('Nests') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), 
        legend.background = element_rect(fill = alpha('white', 0)), axis.text.y = element_blank())

p4 = 
  ggplot(data = ds[year_ == 2019]) +
  geom_violin(aes(initiation_y, as.character(year_)), show.legend = FALSE, fill = 'grey80', color = 'grey50', size = 0.2) +
  geom_point(data = dss[year_ == 2019], aes(median, as.character(year_)), size = 2) +
  geom_linerange(data = dss[year_ == 2019], aes(y = as.character(year_), xmin = q75, xmax = q25), size = 0.5) +
  geom_text(data = dss[year_ == 2019], aes(as.POSIXct(min_date), as.character(year_), label = paste0('N = ', N)), 
            hjust = 0.2, vjust = -1.1, size = 3) +
  scale_x_datetime(limits = c(as.POSIXct(min_date), as.POSIXct(max_date)), date_breaks = "2 weeks", date_labels = "%d %b") +
  xlab('Date') + ylab('Nests') +
  theme_classic(base_size = 11) +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), 
        legend.background = element_rect(fill = alpha('white', 0)), axis.text.y = element_blank())


patchwork = (p1 + p2) / (p3 + p4)
patchwork + plot_layout(heights = c(3, 0.4))

ggsave('./OUTPUTS/FIGURES/N_positions_distance.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')







# read data
dp = fread('./DATA/PAIR_WISE_DIST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_10min)]

# interactions
dp[, interaction := distance < 30]

ds1 = unique(dp, by = c('ID1', 'datetime_10min'))
ds2 = unique(dp[interaction == TRUE], by = c('ID1', 'datetime_10min'))
ds = merge(ds1, ds2[, .(ID1, datetime_10min, type = 'interaction')], all.x = TRUE)
ds[is.na(type), type := 'alone']
ds = ds[, .N, by = .(date_, type)]

ggplot(data = ds) +
  geom_bar(aes(as.factor(date_), N, fill = type), position = 'stack', stat = 'identity')




ds = unique(dp, by = c('ID1', 'datetime_10min'))
ds_N = ds[, .(N_obs_day = .N), by = .(ID1, date_)]
ds = dp[interaction == TRUE, .(Np_obs_day = .N), by = .(ID1, ID2, date_)]

ds = merge(ds, ds_N, by = c('ID1', 'date_'), all.x = TRUE)

ds[, Np_obs_day_per := Np_obs_day / N_obs_day]


hist(ds$N_obs_day)

ds[, year_ := year(date_)]

ggplot(data = ds[year_ == 2019]) +
  geom_point(aes(date_, Np_obs_day)) +
  scale_x_date(date_breaks = "weeks", date_labels = "%d-%m")


ggplot(data = ds[year_ == 2019]) +
  geom_boxplot(aes(as.character(date_), Np_obs_day)) 



# number of daily interactions

# read data
dp = fread('./DATA/PAIR_WISE_DIST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_10min)]

# interactions
dp[, interaction := distance < 20]

dp[, N_per_day := .N, by = date_]
dp[interaction == TRUE, N_int_per_day := .N, by = date_]
dp[, N_int_per_day := mean(N_int_per_day, na.rm = TRUE), by = date_]
dp[is.na(N_int_per_day), N_int_per_day := 0]

# relative
dp[, N_int_per_day_rel := N_int_per_day / N_per_day * 100]


ds = unique(dp, by = c('date_'))
ds[, year_ := year(date_)]

ggplot(data = ds[year_ == 2019]) +
  geom_boxplot(aes(as.character(date_), N_int_per_day_rel)) 





ds_N = ds[, .(N_obs_day = .N), by = .(ID1, date_)]
ds = dp[interaction == TRUE, .(Np_obs_day = .N), by = .(ID1, ID2, date_)]


















