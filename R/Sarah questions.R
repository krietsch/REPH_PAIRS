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
  