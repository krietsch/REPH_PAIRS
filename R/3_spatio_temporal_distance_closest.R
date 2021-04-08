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
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr'), 
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
dpu = dpu[overlap > 0] # exlude non-overlapping data
dpu = dpu[ID_year1 != ID_year2] # exclude within-individual data

#--------------------------------------------------------------------------------------------------------------
#' # Distance to all closest positions
#--------------------------------------------------------------------------------------------------------------


























