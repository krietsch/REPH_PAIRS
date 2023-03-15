# Check how long everybody was in Barrow

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2'),
        function(x) suppressPackageStartupMessages(require(x , character.only = TRUE, quietly = TRUE)))

# Data
con = dbcon('jkrietsch', db = 'FIELD_2017_REPHatBARROW')  
d17 = dbq(con, 'select * FROM AUTHORS')
DBI::dbDisconnect(con)

con = dbcon('jkrietsch', db = 'FIELD_2018_REPHatBARROW')  
d18 = dbq(con, 'select * FROM AUTHORS')
DBI::dbDisconnect(con)

con = dbcon('jkrietsch', db = 'FIELD_2019_REPHatBARROW')  
d19 = dbq(con, 'select * FROM AUTHORS')
DBI::dbDisconnect(con)

d = rbindlist(list(d17, d18, d19), fill=TRUE)

# days in the field
d[, START := as.POSIXct(START, tz = 'UTC')]
d[, STOP := as.POSIXct(STOP, tz = 'UTC')]
d[, days_in_field := difftime(STOP, START, units = 'days') |> as.numeric()]
ds = d[, .(days_in_field = sum(days_in_field)), by = surname]
ds = ds[!is.na(days_in_field)]

# order
setorder(ds, -days_in_field, surname)
ds[, surname := factor(surname, levels = c(ds$surname))]
ds

# plot
ggplot(data = ds) +
  geom_bar(aes(x = surname, y = days_in_field), stat="identity") +
  theme_classic(base_size = 12)


