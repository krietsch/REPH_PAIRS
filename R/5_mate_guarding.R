#==============================================================================================================
# Mateguarding
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


o[nestID == 'R320_19']


ds = o [type2 == 'Female-Male']
ds = o [type2 == 'Male-Female']
ds = unique(ds, by = c('nestID', 'ID1', 'date_'))


ggplot(data = ds, aes(datetime_rel, percentage)) +
  geom_point() +
  geom_smooth(data = ds, aes(datetime_rel, percentage), method = 'loess') +
  geom_path(aes(group = nestID)) +
  
  geom_vline(aes(xintercept = 0), color = 'grey50', size = 3, alpha = 0.3) +
  theme_classic()

