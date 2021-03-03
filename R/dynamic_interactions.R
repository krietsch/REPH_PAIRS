#--------------------------------------------------------------------------------------------------------------
#' # Transform data in trajectory
#--------------------------------------------------------------------------------------------------------------

library(adehabitatLT)

data(puechabonsp)
locs <- puechabonsp$relocs
head(locs)
xy <- coordinates(locs)
df <- as.data.frame(locs)
id <- df[,1]


## Example of a trajectory of type II (time recorded)

### Conversion of the date to the format POSIX
da <- as.character(df$Date)
da <- as.POSIXct(strptime(as.character(df$Date),"%y%m%d", tz="Europe/Paris"))


### Creation of an object of class "ltraj", with for 
### example the first animal
(tr1 <- as.ltraj(xy[id=="Brock",],
                 date = da[id=="Brock"],
                 id="Brock"))

## The components of the object of class "ltraj"
head(tr1[[1]])

## With all animals
(litr <- as.ltraj(xy, da, id = id))

## Change something manually in the first burst:
head(litr[[1]])
litr[[1]][3,"x"] <- 700000

## Recompute the trajectory
litr <- rec(litr)
## Note that descriptive statistics have changed (e.g. dx)
head(litr[[1]])



######################################################
##
## Example of a trajectory of type II (time recorded)
## with an infolocs attribute:

data(capreochiz)
head(capreochiz)

## Create an object of class "ltraj"
cap <- as.ltraj(xy = capreochiz[,c("x","y")], date = capreochiz$date,
                id = "Roe.Deer", typeII = TRUE,
                infolocs = capreochiz[,4:8])
cap







#--------------------------------------------------------------------------------------------------------------
#' # try with REPH data 
#--------------------------------------------------------------------------------------------------------------

library(wildlifeDI)
data(deer)
deer

# seperate data
deer37 <- deer[1]
deer38 <- deer[2]

# DI - Dynamic interaction index
DI(deer37, deer38, tc=7.5*60)

#obtain the local di analysis data-frame
di.df <- DI(deer37, deer38, tc=7.5*60, local=TRUE)

#Examine the temporal dynamics of local di
plot(di.df$date, di.df$di,type="l")


#Smoothed version of local di
di.df$smooth <- 0
#4 fixes/hour x 6 hours on either side of 12 hour centered window
w <- 4*6 
n <- dim(di.df)[1]   #no. of fixes

for (i in (w+1):(n-1-w)){
  di.temp <- di.df$di[(i-w):(i+w)]
  di.df$smooth[i] <- mean(di.temp,na.rm=T)
}

plot(di.df$date, di.df$smooth,type="l")



#--------------------------------------------------------------------------------------------------------------
#' # try with REPH data 
#--------------------------------------------------------------------------------------------------------------

# Packages
sapply( c('data.table', 'magrittr', 'sdb', 'ggplot2', 'anytime', 'viridis', 'auksRuak', 'foreach', 'sf', 'knitr'), 
        require, character.only = TRUE)

# Functions
source('./R/0_functions.R')

# Data
d = fread('./DATA/NANO_TAGS_FILTERED.txt', sep = '\t', header = TRUE) %>% data.table

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

# subset data from breeding pair R909_18
# male: 270170763
# female: 270170764

d = d[ID %in% c(270170763, 270170764)]


dt <- as.ltraj(xy = d[,c("lon","lat")], date = d$datetime_10min,
                id = d$ID, typeII = TRUE)



# seperate data
deer37 <- dt[1]
deer38 <- dt[2]

# DI - Dynamic interaction index
DI(deer37, deer38, tc = 5*60)

#obtain the local di analysis data-frame
di.df <- DI(deer37, deer38, tc = 5*60, local = TRUE)

#Examine the temporal dynamics of local di
plot(di.df$date, di.df$di,type="l")


#Smoothed version of local di
di.df$smooth <- 0
#4 fixes/hour x 6 hours on either side of 12 hour centered window
w <- 4*6 
n <- dim(di.df)[1]   #no. of fixes

for (i in (w+1):(n-1-w)){
  di.temp <- di.df$di[(i-w):(i+w)]
  di.df$smooth[i] <- mean(di.temp,na.rm=T)
}

plot(di.df$date, di.df$smooth,type="l")


di.df = data.table(di.df)
di.df[, datetime_10min := date]



df <- IAB(deer37, deer38, dc=50, tc=5*60, local=TRUE)
plot(df$date, df$Iab,type='l')


prox.df <- Prox(deer37, deer38, tc=5*60, dc=50, local=TRUE)
plot(prox.df$date1,prox.df$prox,type="l")

