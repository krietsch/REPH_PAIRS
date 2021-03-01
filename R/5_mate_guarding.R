#==============================================================================================================
# Mateguarding
#==============================================================================================================

# Summary
# 1. Data available
# 2. Data until 
# 3. Data linked to nests

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '



#--------------------------------------------------------------------------------------------------------------
#' # Mmate guarding and nest distance
#--------------------------------------------------------------------------------------------------------------


# read data
# dp = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
# dpn = fread('./DATA/PAIR_WISE_DIST_NESTS.txt', sep = '\t', header = TRUE) %>% data.table
# d = rbind(dp, dpn)
# d[, datetime_ := as.POSIXct(datetime_10min)]
# d[, datetime_y := as.POSIXct(format(datetime_10min, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
# d[, date_ := as.Date(datetime_10min)]
# # bird or nest interaction
# d[, type := ifelse(ID2 %like% 'R', 'nest', 'individual')]
# fwrite(d, './DATA/PAIR_WISE_DIST_NESTS_AND_ID_DUP.txt', quote = TRUE, sep = '\t', row.names = FALSE)
d = fread('./DATA/PAIR_WISE_DIST_NESTS_AND_ID_DUP.txt', sep = '\t', header = TRUE) %>% data.table

d_ = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table
d_[, datetime_ := as.POSIXct(datetime_)]
d_[, datetime_ := as.POSIXct(format(datetime_, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

# interactions
d[, interaction := distance < 15]

# nest data
dnID = rbind(dn[, .(year_, ID = female_id, nestID, initiation, initiation_y, sex = 'F')], 
             dn[, .(year_, ID = male_id, nestID, initiation, initiation_y, sex = 'M')])
dnID[, ID := factor(ID)]
dnID = unique(dnID[!is.na(ID)], by = c('year_', 'ID', 'nestID'))

# which where tagged?
IDu = unique(d[, ID1])
dnID = dnID[ID %in% IDu]

# both or only one?
dnID[, N_tagged := .N, by = nestID]
dnID = dnID[N_tagged == 2]

# only distance to nests with tagged birds
IDn = dnID[, ID] %>% unique
d = d[ID1 %in% IDn]

d[, ID1 := as.character(ID1)]
d[, ID2 := as.character(ID2)]
dnID[, ID := as.character(ID)]


setkey(d, ID1)
i = unique(dnID[, nestID])
i = "R304_18"
i = "R312_19"

o = foreach(i = unique(dnID[, nestID]), .combine = 'rbind') %do%{

  ds = dnID[nestID == i]
  
  dm = d[ID1 == ds[sex == 'M', ID]]
  dm = unique(dm, by = c('datetime_10min'))
  df = d[ID1 == ds[sex == 'F', ID]]
  df = unique(df, by = c('datetime_10min'))
  
  dmf = d[ID1 == ds[sex == 'M', ID] & ID2 == ds[sex == 'F', ID] & interaction == TRUE]
  dfm = d[ID1 == ds[sex == 'F', ID] & ID2 == ds[sex == 'M', ID] & interaction == TRUE]
  dmn = d[ID1 == ds[sex == 'M', ID] & ID2 == ds[1, nestID] & interaction == TRUE]
  dfn = d[ID1 == ds[sex == 'F', ID] & ID2 == ds[1, nestID] & interaction == TRUE]
  
  # prepare for plot
  dm[, type2 := 'Male']
  df[, type2 := 'Female']
  dmf[, type2 := 'Male-Female']
  dfm[, type2 := 'Female-Male']
  dmn[, type2 := 'Male-Nest']
  dfn[, type2 := 'Female-Nest']
  
  # how many positions a day?
  dmfd = dmf[, .N, .(ID1, date_)]
  dfmd = dmf[, .N, .(ID1, date_)]
  dmnd = dmn[, .N, .(ID1, date_)]
  dfnd = dfn[, .N, .(ID1, date_)]
  
  dmd = dm[, .N, .(ID1, date_)]
  dfd = df[, .N, .(ID1, date_)]
  
  # male
  dmd = merge(dmd, dmfd[, .(date_, N_mf = N)], by = 'date_', all.x = TRUE)
  dmd[is.na(N_mf), N_mf := 0]
  dmd[, N_mf_per := N_mf / N * 100]
  dmd = merge(dmd, dmnd[, .(date_, N_nest = N)], by = 'date_', all.x = TRUE)
  dmd[is.na(N_nest), N_nest := 0]
  dmd[, N_nest_per := N_nest / N * 100]
  dmd[, N_pos_per := N / 144 * 100]
  
  # female
  dfd = merge(dfd, dfmd[, .(date_, N_fm = N)], by = 'date_', all.x = TRUE)
  dfd[is.na(N_fm), N_fm := 0]
  dfd[, N_fm_per := N_fm / N * 100]
  dfd = merge(dfd, dfnd[, .(date_, N_nest = N)], by = 'date_', all.x = TRUE)
  dfd[is.na(N_nest), N_nest := 0]
  dfd[, N_nest_per := N_nest / N * 100]
  dfd[, N_pos_per := N / 144 * 100]
  
  d_per = rbindlist(list(dmd[, .(ID1, date_, type2 = 'Male', percentage = N_pos_per)],
                         dmd[, .(ID1, date_, type2 = 'Male-Female', percentage = N_mf_per)],
                         dmd[, .(ID1, date_, type2 = 'Male-Nest', percentage = N_nest_per)],
                         dfd[, .(ID1, date_, type2 = 'Female', percentage = N_pos_per)],
                         dfd[, .(ID1, date_, type2 = 'Female-Male', percentage = N_fm_per)],
                         dfd[, .(ID1, date_, type2 = 'Female-Nest', percentage = N_nest_per)]
                         ))
  
  # rbind everything
  dss = rbindlist(list(dm, df, dmf, dfm, dmn, dfn))
  dss = merge(dss, d_per, by = c('ID1', 'date_', 'type2'), all.x = TRUE)
  dss[, type2 := factor(type2, levels = c('Male', 'Female', 'Male-Female', 'Female-Male', 'Male-Nest', 'Female-Nest'))]
  
  # subset relevat data
  dss[, nestID := i]
  dss[, .(ID1, datetime_10min, date_, datetime_y, type, type2, percentage)]
  dss

}


dss = o[nestID == 'R310_19']

ggplot(data = dss) +
  geom_point(aes(datetime_y, type2, color = percentage), shape = '|', size = 6) +
  geom_vline(data = ds[1, ], aes(xintercept = initiation_y), color = 'grey50', size = 3, alpha = 0.3) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  theme_classic()


# merge positions with sex
d_[, ID := as.character(ID)]
d_ = merge(d_, dg[, .(ID, sex)], by = 'ID', all.x = TRUE)


# loop for nest attendance and mate guarding 

foreach(i = unique(dnID[, nestID])) %do%{

  ds = dnID[nestID == i]
  
  # map with tracks
  dm = d_[ID %in% ds[, ID]]
  bm = create_bm(dm, buffer = 1000)
  
  p1 = 
  bm + 
    ggtitle(i) +
    geom_path(data = dm, aes(lon, lat, group = ID, color = datetime_), size = 0.7, alpha = 0.5) + 
    geom_point(data = dm, aes(lon, lat, color = datetime_, fill = sex), size = 1, shape = 21) +
    scale_color_viridis( trans = scales::time_trans(), name = 'Date')
  
  
  # bars with overlap
  dss = o[nestID == i]

  p2 = 
  ggplot(data = dss) +
  geom_point(aes(datetime_y, type2, color = percentage), shape = '|', size = 5) +
  geom_vline(data = ds[1, ], aes(xintercept = initiation_y), color = 'firebrick2', size = 3, alpha = 0.3) +
  scale_color_viridis(direction = -1, limits = c(0, 100), name = '%') +
  xlab('Date') + ylab('') +
  theme_classic()
  
  patchwork = p1 / p2 
  patchwork + plot_layout(heights = c(4, 1))
  
  
  ggsave(paste0('./OUTPUTS/MAP_PAIRS/', i, '.png'), plot = last_plot(),
         width = 177, height = 177, units = c('mm'), dpi = 'print')
  
}












ini_u = unique(dnID, by = 'nestID')
o = merge(o, ini_u[, .(nestID, initiation_y)], by = 'nestID', all.x = TRUE)

# relative nest initiation date
o[, datetime_rel := difftime(datetime_y, initiation_y, units = 'days') %>% as.numeric()]


o[, nestID := factor(nestID, levels = unique(nestID[order(initiation_y)]))]

ggplot(data = o[type2 == 'Female-Nest']) +
  geom_point(aes(datetime_rel, nestID, color = percentage), shape = '|', size = 6) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()


ggplot(data = o[type2 == 'Male-Nest']) +
  geom_point(aes(datetime_rel, nestID, color = percentage), shape = '|', size = 6) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()

ggplot(data = o[type2 == 'Male-Female']) +
  geom_point(aes(datetime_rel, nestID, color = percentage), shape = '|', size = 5) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()


o[, ID1 := factor(ID1, levels = unique(ID1[order(initiation_y)]))]
ggplot(data = o[type2 == 'Male-Female']) +
  geom_point(aes(datetime_y, ID1, color = percentage), shape = '|', size = 5) +
  geom_point(aes(initiation_y, ID1), color = 'firebrick3', shape = '|', size = 5) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()


o[, data_during_initiation := any(percentage > 0 & datetime_y < initiation_y), by = nestID]



ds = o[type2 == 'Male-Female' & data_during_initiation == TRUE]
ds2 = o[type2 == 'Female' & data_during_initiation == TRUE]
ds[, ID1 := factor(ID1, levels = unique(ID1[order(initiation_y)]))]

ggplot(data = ds) +
  geom_point(aes(datetime_y, ID1, color = percentage), shape = '|', size = 5) +
  geom_point(aes(initiation_y, ID1), color = 'firebrick3', shape = '|', size = 5) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()


ggplot() +
  geom_point(data = ds2, aes(datetime_y, nestID), color = 'grey90', shape = '|', size = 5) +
  geom_point(data = ds, aes(datetime_y, nestID, color = percentage), shape = '|', size = 5) +
  geom_point(data = ds, aes(initiation_y, nestID), color = 'firebrick3', shape = '|', size = 5) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()








o[nestID == 'R320_19']


ds = o [type2 == 'Female-Male']
ds = o [type2 == 'Male-Female']
ds = unique(ds, by = c('nestID', 'ID1', 'date_'))


ggplot(data = ds, aes(datetime_rel, percentage, color = percentage)) +
  geom_point() +
  geom_smooth(data = ds, aes(datetime_rel, percentage), method = 'loess') +
  geom_path(aes(group = nestID)) +
  scale_color_viridis(direction = -1, limits = c(0, 100)) +
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()







#--------------------------------------------------------------------------------------------------------------
#' # Only mate guarding
#--------------------------------------------------------------------------------------------------------------

# Data
d = fread('./DATA/PAIR_WISE_DIST_DUP.txt', sep = '\t', header = TRUE) %>% data.table
d[, date_ := as.Date(datetime_10min)]
d[, year_ := year(date_)]
  
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)


# interactions
d[, interaction := distance < 20]

# nest data
dnID = dn[, .(year_, IDm = male_id, IDf = female_id, nestID, initiation, initiation_y)]

# subset both tagged 
IDu = unique(d[, ID1])
dnID = dnID[IDm %in% IDu]
dnID = dnID[IDf %in% IDu]

# subset birds with nest
IDu = unique(c(dnID[, IDm], dnID[, IDf]))
d = d[ID1 %in% IDu & ID2 %in% IDu]

# merge with nest data
setnames(d, c('ID1', 'ID2'), c('IDm', 'IDf'))
d = merge(d, dnID, by = c('year_', 'IDm', 'IDf'), all = TRUE, allow.cartesian = TRUE)

# subset actually breeding pairs
d = d[!is.na(nestID)]

# date without year
d[, datetime_y := as.POSIXct(format(datetime_10min, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
d[, date_y := as.Date(datetime_y)]
d[, initiation_dy := as.Date(initiation_y)]

# relative nest initiation date
d[, datetime_rel := difftime(datetime_y, initiation_y, units = 'days') %>% as.numeric()]
d[, initiation_rel := difftime(date_y, initiation_dy, units = 'days') %>% as.numeric()]

# daily points of both individuals
d[, N_daily := .N, by = .(nestID, date_)]

# daily interactions
d[interaction == TRUE, N_together := .N, by = .(nestID, date_)]
d[, N_together := mean(N_together, na.rm = TRUE), by = .(nestID, date_)]
d[is.na(N_together), N_together := 0]

# any within three days around initiation
d[, any_before_initiation := any(initiation_rel < 8), by = nestID]
d[, any_after_initiation  := any(initiation_rel > 0), by = nestID]
d[, any_around_initiation := any_before_initiation == TRUE & any_after_initiation == TRUE, by = nestID]

# mean and median
d[, mean_dist := mean(distance, na.rm = TRUE), by = .(nestID, date_)]
d[, median_dist := median(distance, na.rm = TRUE), by = .(nestID, date_)]

# N daily interactions
ds = unique(d[any_around_initiation == TRUE], by = c('nestID', 'date_'))
ds[, per_together := N_together / N_daily * 100]
ds[, per_sampled := N_daily / 140 * 100]

# nests to exclude
n2 = c('R201_19', 'R231_19', 'R905_19', 'R502_19')
ds = ds[!(nestID %in% n2)]


dss = ds[, .N, by = initiation_rel]


ggplot(data = ds) +
  geom_point(aes(initiation_rel, per_together, group = nestID, color = per_sampled), size = 2, alpha = 1) +
  geom_path(aes(initiation_rel, per_together, group = nestID, color = per_sampled), size = 1, alpha = 0.5) +
  scale_color_viridis(direction = -1, limits = c(0, 100), name = '% day sampled') +
  geom_vline(aes(xintercept = 0), color = 'firebrick2', size = 3, alpha = 0.3) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8) +
  theme(legend.position = c(0.8, 0.8))

# ggsave('./OUTPUTS/FIGURES/Within_pair_per_together_path.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


ggplot(data = ds) +
  geom_boxplot(aes(as.factor(initiation_rel), per_together), varwidth = TRUE) +
  geom_vline(aes(xintercept = '0'), color = 'firebrick2', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(as.factor(initiation_rel), Inf, label = N), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Percentage of positions together') +
  theme_classic(base_size = 8)

# ggsave('./OUTPUTS/FIGURES/Within_pair_per_together.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')


ggplot(data = ds) +
  geom_boxplot(aes(as.factor(initiation_rel), mean_dist), varwidth = TRUE) +
  geom_vline(aes(xintercept = '0'), color = 'firebrick2', size = 1, alpha = 0.3) +
  geom_hline(aes(yintercept = 20), color = 'grey50', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(as.factor(initiation_rel), Inf, label = N), 
            position = position_dodge(width = 0.9), vjust = 1, size = 4) +
  scale_y_continuous(limits = c(0, 1000)) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Within-pair mean distance') +
  theme_classic(base_size = 8)

ggplot(data = ds) +
  geom_boxplot(aes(as.factor(initiation_rel), median_dist), varwidth = TRUE) +
  geom_vline(aes(xintercept = '0'), color = 'firebrick2', size = 1, alpha = 0.3) +
  geom_hline(aes(yintercept = 20), color = 'grey50', size = 1, alpha = 0.3) +
  geom_text(data = dss, aes(as.factor(initiation_rel), Inf, label = N), 
            position = position_dodge(width = 0.9), vjust = 1, size = 2) +
  scale_y_continuous(limits = c(0, 1000)) +
  xlab('Day relative to clutch initiation (= 0)') + ylab('Within-pair median distance') +
  theme_classic(base_size = 8)


# ggsave('./OUTPUTS/FIGURES/Within_pair_median_distanced.tiff', plot = last_plot(),  width = 177, height = 120, units = c('mm'), dpi = 'print')



#--------------------------------------------------------------------------------------------------------------
# split points


ds = d[nestID == 'R909_18']

# split points and merging points
ds[, interaction_before := shift(interaction, type = 'lag'), by = nestID]
ds[, split := interaction_before == TRUE & interaction == FALSE]
ds[, merge := interaction_before == FALSE & interaction == TRUE]


ggplot(data = ds) +
  geom_point(aes(datetime_y, split))

# map with tracks
dt_ = as.POSIXct('2021-06-22 09:50:00')
dss = ds[datetime_y > c(dt_ - 3600*3) & datetime_y < c(dt_ + 3600*3)]
bm = create_bm(dss, buffer = 10, lat = 'lat1', lon = 'lon1')

bm + 
  geom_path(data = dss, aes(lon1, lat1, group = IDm, color = split), size = 0.7, alpha = 0.5) + 
  geom_point(data = dss, aes(lon1, lat1, color = split), size = 1, shape = 21) +
  geom_path(data = dss, aes(lon2, lat2, group = IDf, color = split), size = 0.7, alpha = 0.5) + 
  geom_point(data = dss, aes(lon2, lat2, color = split), size = 1, shape = 21)







































