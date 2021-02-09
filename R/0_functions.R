#==============================================================================================================
# Functions 
#==============================================================================================================


#' Track characteristics
#'
#' @param DT 
#' @param ID 
#' @param lat 
#' @param lon 
#' @param datetime_ 
#'
#' @return
#' @export
#'
#' @examples

track_characteristics <- function(DT, ID, lat = "lat", lon = "lon", datetime_ = "datetime_"){
  
  if(nrow(DT) > 0) {
    setnames(DT, c(ID, lat, lon, datetime_), c('IDu', 'lat', 'lon', 'datetime_'))
    
    setorder(DT, IDu, datetime_)
    
    # distance between consecutive points
    DT[, lon2         := data.table::shift(lon, type = 'lead'), by = IDu]
    DT[, lat2         := data.table::shift(lat, type = 'lead'), by = IDu]
    DT[, distance_btw := sqrt(sum((c(lon, lat) - c(lon2, lat2))^2)) , by = 1:nrow(DT)]
    
    # time between consecutive points
    DT[, datetime_2   := data.table::shift(datetime_, type = 'lead'), by = IDu]
    DT[, time_btw     := as.numeric(difftime(datetime_2, datetime_, units = 'sec'))]
    
    # speed 
    DT[, speed        := distance_btw / time_btw]
    
    setnames(DT, c('IDu', 'lat', 'lon', 'datetime_'), c(ID, lat, lon, datetime_))
    
  }
}




#' Speed filter function
#'
#' @param DT 
#' @param ID 
#' @param speed 
#' @param max_speed 
#'
#' @return
#' @export
#'
#' @examples

speed_filter <- function(DT, ID, speed, max_speed){
  
  if(nrow(DT) > 0) {
    setnames(DT, c(ID, speed), c('IDu', 'speed'))
    
    # speed over may
    DT[, speed_over_threshold := speed > max_speed]
    
    # select couts
    bCounter <- function(x){
      n = length(x)
      y = x[-1] != x[-n]
      i = c(which(y | is.na(y)), n)
      lengths = diff(c(0L, i))
      bout_length = rep(lengths, lengths)
      ids = 1:length(lengths)
      bout_id = rep(ids, lengths)
      bout_id 
    }
    
    DT[, bout := bCounter(speed_over_threshold), by = IDu]
    
    # select only true outlier
    DT[, seq := seq_len(.N), by = .(IDu, bout)]
    DT[, error := seq == 2 & speed_over_threshold == TRUE]
    
    # delete unwanted columns
    DT[, c('bout','seq', 'speed_over_threshold') := NULL]
    
    setnames(DT, c('IDu', 'speed'), c(ID, speed))
    
  }
}