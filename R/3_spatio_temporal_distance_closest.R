#' ---
#' title: Calculate spatio-temporal distance of points
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

# Summary
# 1. Apply speed filter 
# 2. Apply distance filter 
# 3. Check altitudes

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr', 'windR'), 
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
d[, datetime_ := as.POSIXct(as.character(datetime_))]

#--------------------------------------------------------------------------------------------------------------
#' # Subset ID's with overlapping data
#--------------------------------------------------------------------------------------------------------------

# start and end of the data
d[, ID_year := paste0(ID, '_', year_)]
d[, start := min(datetime_), by = ID_year]
d[, end   := max(datetime_), by = ID_year]
dID = unique(d[, .(ID_year, year_, ID, start, end)], by = 'ID_year')

# all pairwise combinations
dpu = CJ(ID_year1 = dID[, ID_year], ID_year2 = dID[, ID_year], unique = TRUE)

# merge with start and end
dpu = merge(dpu, dID[, .(ID_year1 = ID_year, year_1 = year_, ID1 = ID, start1 = start, end1 = end)], by = 'ID_year1')
dpu = merge(dpu, dID[, .(ID_year2 = ID_year, year_2 = year_, ID2 = ID, start2 = start, end2 = end)], by = 'ID_year2')

# overlapping intervals
dpu[, overlap := DescTools::Overlap(c(start1, end1), c(start2, end2)), by = 1:nrow(dpu)]
dpu = dpu[overlap > 0] # exclude non-overlapping data
dpu = dpu[ID_year1 != ID_year2] # exclude within-individual data

#--------------------------------------------------------------------------------------------------------------
#' # Distance to all closest positions
#--------------------------------------------------------------------------------------------------------------

# example
d1 = d[ID == 270170746, .(ID1 = ID, datetime_ = datetime_)]
d2 = d[ID == 270170747, .(ID2 = ID, datetime_ = datetime_)]

setkeyv(d1, 'datetime_')
setkeyv(d2, 'datetime_')

x = d2[, nearest := (datetime_)][d1, roll = 'nearest'] #closest date
x[, Tbtw := abs(as.numeric(difftime(datetime_, nearest, units = 'min')))]




hist(x[Tbtw < 6, Tbtw], breaks = 20)

x[, point_id := seq_along(ID1)]
x[Tbtw > 5 & Tbtw < 6]


x[point_id > 486 & point_id < 494]

xx = x[, .(ID2 = ID2[c(1)], datetime_ = datetime_[c(1)], Tbtw = min(Tbtw)), by = .(ID1, nearest)]


xx[, duplicated := duplicated(nearest)]
xx[duplicated == TRUE]
xx[Tbtw > 5]

hist(xx[Tbtw < 30, Tbtw], breaks = 20)


xx[, point_id := seq_along(ID1)]
xx[point_id > 469 - 5 & point_id < 469 + 5]

d1 = merge(d1, x, by = c('ID1', 'datetime_'), all.x = TRUE)

d1[datetime_ == '2018-06-19 09:24:30']
d1[nearest == '2018-06-19 09:24:30']
d2[datetime_ == '2018-06-19 09:24:30']

hist(x[Tbtw < 30, Tbtw], breaks = 20)

d1[, nearest := (datetime_)][d2, roll = Inf] 

d2[, nearest := (datetime_)][d1, roll = Inf] 

datetimem = d2$datetime_

d1[, closest_dtm := closestDatetime(datetime_, datetimem), by = datetime_]
d1[, TbtwPoints_m_f := as.numeric(difftime(datetime_, closest_dtm, units = 'min'))]
d1[, TbtwPoints_m_f := abs(TbtwPoints_m_f)]

plot(TbtwPoints_m_f ~ datetime_, df[TbtwPoints_m_f < 10])
df = df[TbtwPoints_m_f < 10]

dm[, datetime_m := datetime_]







d1 <- data.table(val=seq(1:121), dates = seq.Date(as.Date('2018-12-01'), as.Date('2019-03-31'), "days"))
d2 <- data.table(val=c(1,2,4,5), dates = c(as.Date('2018-12-14'), as.Date('2019-1-2'), as.Date('2019-2-3')))

setkeyv(d1,"dates")
setkeyv(d2,"dates")

x = d2[,nearest:=(dates)][d1, roll = 'nearest'] #closest date
x

d2












