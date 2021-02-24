#' ---
#' title: Social network
#' subtitle: 
#' author: Johannes Krietsch
#' output:
#'    html_document:
#'      toc: true
#'      highlight: tango
#' ---

#==============================================================================================================
# Social network
#==============================================================================================================

# Summary
# 1. 
# 2. 
# 3. 

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 
          'knitr', 'asnipe', 'igraph'), 
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
d[, datetime_ := as.POSIXct(datetime_)]

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dr = dbq(con, 'select * FROM RESIGHTINGS')
dr[, year_ := year(datetime_)]
dr = dr[year_ > 2017]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Social network with observations
#--------------------------------------------------------------------------------------------------------------

# change projection
st_transform_DT(dr)

# assign locations in the study area 
point_over_poly_DT(dr, poly = study_site, buffer = 10)
setnames(dr, 'poly_overlap', 'study_site')

point_over_poly_DT(d, poly = study_site, buffer = 10)
setnames(d, 'poly_overlap', 'study_site')

dr = dr[!is.na(ID)]
dr[, year_ := year(datetime_)]
dr[, ID_year := paste0(ID, '_', substr(year_, 3,4 ))]

# 2018 all interactions
ds = dr[year_ == 2018 & study_site == TRUE, .(ID, obs_id)]

# create matrix with observation ID_year by individual
gbi = get_group_by_individual(ds[, .(ID, obs_id)], data_format = 'individuals')

# calculate a network
netw = get_network(gbi, data_format = 'GBI')

# plot network
pn = graph.adjacency(netw, mode = 'undirected',weighted = TRUE, diag = FALSE)
plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black')

# assign sex
ID_netw = unique(ds$ID)
dcn = dg[ID %in% ID_netw, .(ID, sex)]
dcn = unique(dcn)
dcn[, ID := as.character(ID)]

ID_pn = data.table(ID = V(pn)$name)
ID_pn[, order := 1:nrow(ID_pn)]
ID_pn = merge(ID_pn, dcn, by = 'ID')
setorder(ID_pn, order)

V(pn)$sex = ID_pn$sex

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black',
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])


# point size by number of observations
ds_N = ds[, .N, by = ID]
ds_N[, ID := as.character(ID)]

ID_pn = merge(ID_pn, ds_N, by = 'ID')
setorder(ID_pn, order)

V(pn)$size = as.numeric(ID_pn$N) %>% log

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, edge.color = 'grey30', vertex.size = V(pn)$size+2,
     edge.color = 'black', vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])


#--------------------------------------------------------------------------------------------------------------
#' # Social network with GPS data
#--------------------------------------------------------------------------------------------------------------

# read data
dp = fread('./DATA/PAIR_WISE_DIST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_10min)]
dp[, year_ := year(date_)]
dp = dp[year_ == 2019]

# interactions
dp[, interaction := distance < 10]
# du = unique(dp[interaction == TRUE], by = c('ID1', 'ID2', 'date_'))

ds = copy(dp[interaction == TRUE])

# dss = ds[, .N, by = .(ID1, ID2)]
# setorder(dss, N)
# dss[, N := log(N)]
# 
# hist(dss[N > 30]$N)
# dss[N > 200]
# dss[N == 1]

ds[, obs_id := paste0('int_', 1:nrow(ds))]
ds = rbind(ds[, .(ID = ID1, obs_id)], ds[, .(ID = ID2, obs_id)])

# include single observations
dsf = dp[interaction == FALSE]
dsf = unique(dsf, by = c('ID1', 'date_'))
dsf[, obs_id := paste0('lone_', 1:nrow(dsf))]

ds = rbind(ds, dsf[, .(ID = ID1, obs_id)])


# an <- with(dss, sort(unique(c(as.character(ID1),
#                               as.character(ID2)))))
# M <- array(0, c(length(an), length(an)), list(an, an))
# i <- match(dss$ID1, an)
# j <- match(dss$ID2, an)
# M[cbind(i,j)] <- M[cbind(j,i)] <- dss$N
# 
# 
# # plot network
# pn = graph.adjacency(M, mode = 'undirected', weighted = TRUE, diag = FALSE)
# plot(pn, vertex.label = NA, edge.width = E(pn)$weight/15, vertex.size = 4, edge.color = 'black')
# 
# l <- layout_in_circle(pn)
# plot(pn, layout=l)

# create matrix with observation ID by individual
gbi = get_group_by_individual(ds[, .(ID, obs_id)], data_format = 'individuals')

# calculate a network
netw = get_network(gbi, data_format = 'GBI', association_index = 'SRI')

# plot network
pn = graph.adjacency(netw, mode = 'undirected', weighted = TRUE, diag = FALSE)
plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black')



# assign sex
ID_netw = unique(ds$ID)
dcn = dg[ID %in% ID_netw, .(ID, sex)]
dcn = unique(dcn)
dcn[, ID := as.character(ID)]

ID_pn = data.table(ID = V(pn)$name)
ID_pn[, order := 1:nrow(ID_pn)]
ID_pn = merge(ID_pn, dcn, by = 'ID')
setorder(ID_pn, order)

V(pn)$sex = ID_pn$sex

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black',
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])


# point size by number of observations
ds_N = ds[, .N, by = ID]
ds_N[, ID := as.character(ID)]

ID_pn = merge(ID_pn, ds_N, by = 'ID')
setorder(ID_pn, order)

V(pn)$size = as.numeric(ID_pn$N) %>% log

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, edge.color = 'grey30', edge.curved = .2, 
     vertex.size = V(pn)$size+2,
     edge.color = 'black', vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])


plot(pn, vertex.label = NA, edge.width = 120*E(pn)$weight, edge.color = 'grey30', edge.curved = .2, 
     vertex.size = V(pn)$size+2,
     edge.color = 'black', vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])




plot(pn, vertex.label = NA, edge.width = 20*E(pn)$weight, edge.color = 'grey30', vertex.size = V(pn)$size+2,
     edge.color = c('red', 'blue')[1+(E(pn)$sex == 'M')], 
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])


plot(pn, vertex.label = NA, edge.width = 20*E(pn)$weight, edge.color = 'grey30', vertex.size = V(pn)$size+2,
     edge.color = c(E(pn)$color), 
     vertex.color = c('red', 'blue')[1+(V(pn)$sex == 'M')])

E(pn)$color[E(pn)$weight > 0.5] <- 'red'
E(pn)$color[E(pn)$weight < 0.5] <- 'blue'



E(g)$color[E(g)$weight == 8] <- 'green'
E(g)$lty[E(g)$weight == 8] <- 1
E(g)$color[E(g)$weight == 3] <- 'green'
E(g)$lty[E(g)$weight == 3] <- 2



# pair-wise table
dw = as.table(netw) %>% data.table
setnames(dw, c('ID1', 'ID2', 'association'))

dss = dp[interaction == TRUE]
dss[, ID1 := as.character(ID1)]
dss[, ID2 := as.character(ID2)]
dss = dss[, .N, by = .(ID1, ID2)]

dw = merge(dw, dss, by = c('ID1', 'ID2'))

ggplot(data = dw) +
  geom_point(aes(N, association))


hist(dw$association)








#--------------------------------------------------------------------------------------------------------------
#' # Social network animation
#--------------------------------------------------------------------------------------------------------------

# read data
dp = fread('./DATA/PAIR_WISE_DIST.txt', sep = '\t', header = TRUE) %>% data.table
dp[, date_ := as.Date(datetime_10min)]
dp[, year_ := year(date_)]
dp = dp[year_ == 2018]

# interactions
dp[, interaction := distance < 10]
# du = unique(dp[interaction == TRUE], by = c('ID1', 'ID2', 'date_'))

ds = copy(dp[interaction == TRUE])
ds[, year_ := year(date_)]

ds[, obs_id := paste0('int_', 1:nrow(ds))]
ds = rbind(ds[, .(ID = ID1, obs_id)], ds[, .(ID = ID2, obs_id)])

# include single observations
dsf = dp[interaction == FALSE]
dsf = unique(dsf, by = c('ID1', 'date_'))
dsf[, obs_id := paste0('lone_', 1:nrow(dsf))]

ds = rbind(ds, dsf[, .(ID = ID1, obs_id)])

# create matrix with observation ID by individual
gbi = get_group_by_individual(ds[, .(ID, obs_id)], data_format = 'individuals')

# calculate a network
netw = get_network(gbi, data_format = 'GBI', association_index = 'SRI')

# plot network
pn = graph.adjacency(netw, mode = 'undirected', weighted = TRUE, diag = FALSE)
plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black')

# assign sex
ID_netw = unique(ds$ID)
dcn = dg[ID %in% ID_netw, .(ID, sex)]
dcn = unique(dcn)
dcn[, ID := as.character(ID)]

ID_pn = data.table(ID = V(pn)$name)
ID_pn[, order := 1:nrow(ID_pn)]
ID_pn = merge(ID_pn, dcn, by = 'ID')
setorder(ID_pn, order)

V(pn)$sex = ID_pn$sex

plot(pn, vertex.label = NA, edge.width = 10*E(pn)$weight^2, vertex.size = 4, edge.color = 'black',
     vertex.color = c('firebrick3', 'dodgerblue3')[1+(V(pn)$sex == 'M')])

######## subset
dss = copy(dp[interaction == TRUE])
dss = dss[date_ == as.Date('2019-06-10')]

dss[, obs_id := paste0('int_', 1:nrow(dss))]
dss = rbind(dss[, .(ID = ID1, obs_id)], dss[, .(ID = ID2, obs_id)])

# create matrix with observation ID by individual
gbi_s = get_group_by_individual(dss[, .(ID, obs_id)], data_format = 'individuals')

# calculate a network
netw_s = get_network(gbi_s, data_format = 'GBI', association_index = 'SRI')

# plot network
pn_s = graph.adjacency(netw_s, mode = 'undirected', weighted = TRUE, diag = FALSE)
plot(pn_s, vertex.label = NA, edge.width = 10*E(pn_s)$weight^2, vertex.size = 4, edge.color = 'black')

# assign sex
ID_netw = unique(dss$ID)
dcn = dg[ID %in% ID_netw, .(ID, sex)]
dcn = unique(dcn)
dcn[, ID := as.character(ID)]

ID_pn = data.table(ID = V(pn_s)$name)
ID_pn[, order := 1:nrow(ID_pn)]
ID_pn = merge(ID_pn, dcn, by = 'ID')
setorder(ID_pn, order)

V(pn_s)$sex = ID_pn$sex

plot(pn_s, vertex.label = NA, edge.width = 10*E(pn_s)$weight^2, vertex.size = 4, edge.color = 'black',
     vertex.color = c('firebrick3', 'dodgerblue3')[1+(V(pn_s)$sex == 'M')])



# graph layouts
# g1
set.seed(1)
layg1 <- layout.fruchterman.reingold(pn)

# g2
set.seed(2)
layg2 <- layout.fruchterman.reingold(pn_s)
# overwrite coords for shared nodes
layg2[which(V(pn_s)$name %in% V(pn)$name), ] <- 
  layg1[which(V(pn)$name %in% V(pn_s)$name),]

xlim <- range(c(layg1[,1], layg2[,1]))
ylim <- range(c(layg1[,2], layg2[,2]))




plot(pn , vertex.size=50, layout=layg1, xlim=xlim, ylim=ylim, rescale=FALSE, vertex.label = NA,
     edge.width = 10*E(pn)$weight^2,
     vertex.color = c('firebrick3', 'dodgerblue3')[1+(V(pn)$sex == 'M')])


plot(pn_s , vertex.size=50, layout=layg2, xlim=xlim, ylim=ylim, rescale=FALSE, vertex.label = NA,
     edge.width = 10*E(pn_s)$weight^2,
     vertex.color = c('firebrick3', 'dodgerblue3')[1+(V(pn_s)$sex == 'M')])





######## loop for each date 

# register cores
require(doFuture)
registerDoFuture()
plan(multiprocess)



foreach(i = dp[interaction == TRUE & year_ == 2018, date_] %>% unique, .packages = c('asnipe', 'igraph')) %dopar%{
  
  
  dss = dp[interaction == TRUE & date_ == i]
  dss[, obs_id := paste0('int_', 1:nrow(dss))]
  dss = rbind(dss[, .(ID = ID1, obs_id)], dss[, .(ID = ID2, obs_id)])
  
  # include single observations
  dsf = dp[interaction == FALSE & date_ == i]
  dsf = unique(dsf, by = c('ID1', 'date_'))
  dsf[, obs_id := paste0('lone_', 1:nrow(dsf))]

  dss = rbind(dss, dsf[, .(ID = ID1, obs_id)])
  
  # create matrix with observation ID by individual
  gbi_s = get_group_by_individual(dss[, .(ID, obs_id)], data_format = 'individuals')
  
  # calculate a network
  netw_s = get_network(gbi_s, data_format = 'GBI', association_index = 'SRI')
  
  # plot network
  pn_s = graph.adjacency(netw_s, mode = 'undirected', weighted = TRUE, diag = FALSE)
  
  # assign sex
  ID_netw = unique(V(pn_s)$name)
  dcn = dg[ID %in% ID_netw, .(ID, sex)]
  dcn = unique(dcn)
  dcn[, ID := as.character(ID)]
  
  ID_pn = data.table(ID = V(pn_s)$name)
  ID_pn[, order := 1:nrow(ID_pn)]
  ID_pn = merge(ID_pn, dcn, by = 'ID')
  setorder(ID_pn, order)
  
  V(pn_s)$sex = ID_pn$sex
  
  
  # graph layouts
  # g1
  set.seed(1)
  layg1 <- layout.fruchterman.reingold(pn)
  
  # g2
  set.seed(2)
  layg2 <- layout.fruchterman.reingold(pn_s)
  # overwrite coords for shared nodes
  layg2[which(V(pn_s)$name %in% V(pn)$name), ] <- 
    layg1[which(V(pn)$name %in% V(pn_s)$name),]
  
  xlim <- range(c(layg1[,1], layg2[,1]))
  ylim <- range(c(layg1[,2], layg2[,2]))
  
  
  
  
  png(paste0('./OUTPUTS/NETWORK_BY_DAY/', i, '.png'), 600, 600)
  
  plot(pn_s , vertex.size=50, layout=layg2, xlim=xlim, ylim=ylim, rescale=FALSE, vertex.label = NA,
       edge.width = 10*E(pn_s)$weight^2,
       vertex.color = c('firebrick3', 'dodgerblue3')[1+(V(pn_s)$sex == 'M')])
  title(i, cex.main = 2, col.main = "black")
  
  dev.off()
  
  
}



# make animation
wd = getwd()
setwd(paste0(wd, '/OUTPUTS/NETWORK_BY_DAY'))
system("ffmpeg -framerate 1 -pattern_type glob -i '*.png' -y -c:v libx264 -profile:v high -crf 1 -pix_fmt yuv420p Socialnetwork.mov")
setwd(wd)






# pair-wise table
dw = as.table(netw_s) %>% data.table
setnames(dw, c('ID1', 'ID2', 'association'))

dd[, ID1 := as.character(ID1)]
dd[, ID2 := as.character(ID2)]

dd = dss[, .N, by = .(ID1, ID2)]

dw = merge(dw, dd, by = c('ID1', 'ID2'))

ggplot(data = dw) +
  geom_point(aes(N, association))


hist(dw$association)









