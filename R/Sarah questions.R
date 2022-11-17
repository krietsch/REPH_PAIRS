# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'sf', 'auksRuak', 'ggplot2'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Projection
PROJ = '+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '

#--------------------------------------------------------------------------------------------------------------
# CAPTURES
#--------------------------------------------------------------------------------------------------------------

# Database
con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NESTS')

# How many tinytag nests did you have?
d[msr == 1, .N, by = .(year_, external)]


# Did you think that birds adjusted well to the tags after attachment?
# Could you share your IACUC and permitting info please? Or when you see a draft in the future you can add it then.
# Were all your birds tagged pre-breeding?



#--------------------------------------------------------------------------------------------------------------
# Examples for Sarah
#--------------------------------------------------------------------------------------------------------------


con = dbcon('jkrietsch', db = 'REPHatBARROW')  
d = dbq(con, 'select * FROM NANO_TAGS')
d = d[ID != 999] # exclude test data
d[is.na(lon)] # check that no NA
d[, datetime_ := as.POSIXct(datetime_, tz = 'UTC')]
DBI::dbDisconnect(con)






# R203_19
d1 = d[ID == 270170736 & year_ == 2019]

# R321_19
d2 = d[ID == 270170835 & year_ == 2019]

# R221_19
d3 = d[ID == 270170929 & year_ == 2019]

# R308_19
d4 = d[ID == 273145031 & year_ == 2019]

# R213_19
d5 = d[ID == 273145071 & year_ == 2019]


d = rbindlist(list(d1, d2, d3, d4, d5))

# save data
fwrite(d, './DATA/NANO_TAGS_examples_method_paper.txt', quote = TRUE, sep = '\t', row.names = FALSE)

# d = fread('./DATA/NANO_TAGS_examples_method_paper.txt', sep = '\t', header = TRUE) %>% data.table




