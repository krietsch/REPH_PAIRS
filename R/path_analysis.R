#==============================================================================================================
# Path analysis
#==============================================================================================================

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'sf', 'foreach', 'auksRuak', 'knitr', 'foreach',
          'sdbvis', 'viridis', 'patchwork', 'adehabitatLT', 'wildlifeDI', 'DescTools'),
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table

con = dbcon('jkrietsch', db = 'REPHatBARROW')  
dn = dbq(con, 'select * FROM NESTS')
dn[, nestID := paste0(nest, '_', substr(year_, 3, 4))]
dn = dn[year_ > 2017]
dn[, initiation := as.POSIXct(initiation)]
dn[, initiation_y := as.POSIXct(format(initiation, format = '%m-%d %H:%M:%S'), format = '%m-%d %H:%M:%S')]
dg = dbq(con, 'select * FROM SEX')
DBI::dbDisconnect(con)

#--------------------------------------------------------------------------------------------------------------
#' # Round to 10 min intervals
#--------------------------------------------------------------------------------------------------------------

# round times to 10 min intervalls
d[, datetime_ := as.POSIXct(datetime_)]
d[, datetime_10min := round(datetime_, '10 mins')]
d[, datetime_ := datetime_ %>% as.character %>% as.POSIXct]
d[, datetime_10min := datetime_10min %>% as.character %>% as.POSIXct]

# check for duplicates by ID
d[, duplicated := duplicated(d, by = c('ID', 'datetime_10min'))]
d[duplicated == TRUE] %>% nrow

# mean of these instances
d = d[, .(year_ = mean(year_), datetime_ = mean(datetime_), lat = mean(lat), lon = mean(lon), 
          gps_speed = mean(gps_speed), altitude = mean(altitude), batvolt = mean(batvolt)), 
      by = .(ID, datetime_10min)]

anyDuplicated(d, by = c('ID', 'datetime_10min'))

# shift time of track by 10 min
d[, datetime_shift := datetime_ + 600]

# start and end of the data
d[, start := min(datetime_), by = ID]
d[, end   := max(datetime_), by = ID]
dID = unique(d, by = 'ID')

#--------------------------------------------------------------------------------------------------------------
#' # Define breeding pairs with both sexes tagged
#--------------------------------------------------------------------------------------------------------------

# ID as character
d[, ID := as.character(ID)]
dID[, ID := as.character(ID)]
dn[, male_id := as.character(male_id)]
dn[, female_id := as.character(female_id)]

# check if data overlap
dn = merge(dn, dID[, .(male_id = ID, start_m = start, end_m = end)], by = 'male_id', all.x = TRUE)
dn = merge(dn, dID[, .(female_id = ID, start_f = start, end_f = end)], by = 'female_id', all.x = TRUE)

# subset nests with both IDs tagged
dn = dn[!is.na(start_m) & !is.na(start_f)]

# subset nests with both IDs tagged and overlapping time intervals
dn[, overlap := Overlap(c(start_m, end_m), c(start_f, end_f)), by = nestID]
dn = dn[overlap > 0]

# nest data
dnID = rbind(dn[, .(year_, ID = female_id, nestID, initiation, initiation_y, sex = 'F')], 
             dn[, .(year_, ID = male_id, nestID, initiation, initiation_y, sex = 'M')])
dnID[, ID := factor(ID)]
dnID = unique(dnID[!is.na(ID)], by = c('year_', 'ID', 'nestID'))

#--------------------------------------------------------------------------------------------------------------
#' # Global track characteristics
#--------------------------------------------------------------------------------------------------------------

# register cores
require(doFuture)
registerDoFuture()
plan(multiprocess)


do = foreach(i = dnID[, nestID] %>% unique, .combine = 'rbind', .packages = c('data.table', 'wildlifeDI', 'adehabitatLT')) %dopar% {

  # subset pair
  dns = dnID[nestID == i] 
  
  # create track for each sex
  IDm = as.ltraj(xy = d[ID == dns[sex == 'M', ID], .(lon, lat)], 
                 date = d[ID == dns[sex == 'M', ID], datetime_], 
                 id = d[ID == dns[sex == 'M', ID], ID], typeII = TRUE)
  
  IDf = as.ltraj(xy = d[ID == dns[sex == 'F', ID], .(lon, lat)], 
                 date = d[ID == dns[sex == 'F', ID], datetime_], 
                 id = d[ID == dns[sex == 'F', ID], ID], typeII = TRUE)
  
  # create shifted track for each sex
  IDms = as.ltraj(xy = d[ID == dns[sex == 'M', ID], .(lon, lat)], 
                 date = d[ID == dns[sex == 'M', ID], datetime_shift], 
                 id = d[ID == dns[sex == 'M', ID], ID], typeII = TRUE)
  
  IDfs = as.ltraj(xy = d[ID == dns[sex == 'F', ID], .(lon, lat)], 
                 date = d[ID == dns[sex == 'F', ID], datetime_shift], 
                 id = d[ID == dns[sex == 'F', ID], ID], typeII = TRUE)
  
  
  # Proximity
  prox = Prox(IDm, IDf, tc = 5*60, dc = 30)
  
  # Correlation coefficient
  corr = Cr(IDm, IDf, tc = 5*60)
  
  # Correlation coefficient male shifted
  corr_ms = Cr(IDms, IDf, tc = 5*60)
  
  # Correlation coefficient female shifted
  corr_fs = Cr(IDm, IDfs, tc = 5*60)
  
  # Dynamic interaction index
  di = DI(IDm, IDf, tc = 5*60) %>% as.data.table
  
  
  o = cbind(year_ = dns[1, year_], 
            nestID = dns[1, nestID], 
            IDm = dns[sex == 'M', ID],
            IDf = dns[sex == 'F', ID],
            prox, corr, corr_ms, corr_fs, di)
  
  o

}


# fwrite(do, './DATA/Path_analyis.txt', quote = TRUE, sep = '\t', row.names = FALSE)

do[, delta_corr_ms := corr - corr_ms]
do[, delta_corr_fs := corr - corr_fs]

ds = rbind(do[, .(type = 'Closest time', corr)],
           do[, .(type = 'Male +10 min', corr = corr_ms)],
           do[, .(type = 'Female +10 min', corr = corr_fs)])

ggplot(data = ds) +
  geom_boxplot(aes(type, corr), notch = TRUE) +
  xlab('Type') + ylab('Correlation coefficient') +
  theme_classic()

# ggsave('./OUTPUTS/FIGURES/Path_correlation.tiff', plot = last_plot(),  width = 177, height = 177, units = c('mm'), dpi = 'print')



#--------------------------------------------------------------------------------------------------------------
#' # Local track characteristics
#--------------------------------------------------------------------------------------------------------------


i = dnID[, nestID] %>% unique
i = i[20]

# subset pair
dns = dnID[nestID == i] 

# create track for each sex
IDm = as.ltraj(xy = d[ID == dns[sex == 'M', ID], .(lon, lat)], 
               date = d[ID == dns[sex == 'M', ID], datetime_10min], 
               id = d[ID == dns[sex == 'M', ID], ID], typeII = TRUE)

IDf = as.ltraj(xy = d[ID == dns[sex == 'F', ID], .(lon, lat)], 
               date = d[ID == dns[sex == 'F', ID], datetime_10min], 
               id = d[ID == dns[sex == 'F', ID], ID], typeII = TRUE)





# shift time of track by 10 min
d[, datetime_10min_shift := datetime_10min + 600]

dt = as.ltraj(xy = d[ ,c('lon', 'lat')], date = d$datetime_10min_shift, id = d$ID, typeII = TRUE)

# seperate by ID
ID1s = dt[1]
ID2s = dt[2]




