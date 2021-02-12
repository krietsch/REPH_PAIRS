#==============================================================================================================
# Functions 
#==============================================================================================================

#' Create base map (Function adjusted from package auksRuak: https://github.com/krietsch/auksRuak)
#'
#' Use open street map data to create a basemap with the extent of the data
#'
#' @param DT         Name of the data.table
#' @param lat        Name of the column with latitude (as.character)
#' @param lon        Name of the column with longitude (as.character)
#' @param buffer     Buffer around the data
#' @param sc_dist    Distance of the scale
#' @param squared    If true buffer is created around the centre and outline squared
#' @param projection Projection of the data (default is equal area with centre Barrow)
#'
#' @return           bm, a ggplot2 base map
#' @export
#'
#' @import           data.table
#' @importFrom       sf st_as_sf st_transform st_join st_buffer st_intersects st_geometry st_intersection st_bbox st_as_sfc st_crs
#' @importFrom       magrittr %>%
#' @importFrom       ggplot2 ggplot geom_sf coord_sf aes theme element_line element_rect element_blank unit
#' @importFrom       ggspatial annotation_scale
#'
#' @examples
#' # create table with two points
#' DT = data.table(name = c('NARL', 'Utqiagvik'),
#'                 lat  = c(71.320854, 71.290246),
#'                 lon  = c(-156.648210, -156.788622))
#'
#' # change projection
#' st_transform_DT(DT)
#'
#' # create base map
#' bm = create_bm(DT)
#' bm

create_bm = function(DT, lat = 'lat', lon = 'lon', buffer = 1000, sc_dist, squared = FALSE, 
                     projection = paste0('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0',
                                         ' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 ')){
  
  if(nrow(DT) > 0) {
    
    setnames(DT, c(lat, lon), c('lat', 'lon'))
    
    if(squared == TRUE){
      center_lon = min(DT$lon) + (max(DT$lon) - min(DT$lon))/2
      center_lat = min(DT$lat) + (max(DT$lat) - min(DT$lat))/2
      center_point = st_point(x = c(center_lon, center_lat)) %>% st_sfc(crs = projection)
      rs_extent = center_point %>% st_buffer(buffer) %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_geometry
      rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
      bb = st_bbox(rs_extent) %>% data.table
      
    } else {
      
      st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
      rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_geometry
      rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
      bb = st_bbox(rs_extent) %>% data.table
    }
    
    # crop data
    land      = st_intersection(osm_land, rs_extent)
    lakes     = st_intersection(osm_lakes, rs_extent)
    rivers    = st_intersection(osm_rivers, rs_extent)
    roads     = st_intersection(osm_roads, rs_extent)
    buildings = st_intersection(osm_buildings, rs_extent)
    
    bm =
      ggplot() +
      geom_sf(data = land, fill = 'grey92', colour = 'grey80') +
      geom_sf(data = lakes[lakes$fclass == 'water', ], fill = 'grey85', colour = 'grey80') +
      geom_sf(data = roads, color = 'grey60') +
      geom_sf(data = buildings, fill = 'grey50', color = 'grey50', size = 0.3) +
      coord_sf(expand = FALSE, xlim = c(bb$.[1], bb$.[3]), ylim = c(bb$.[2], bb$.[4])) +
      ggspatial::annotation_scale(aes(location = 'bl'), text_cex = 0.8, height = unit(0.2, 'cm'),
                                  pad_x = unit(0.9, 'cm'), pad_y = unit(0.2, 'cm')) +
      ggspatial::annotation_north_arrow(aes(location = 'bl'), which_north = "true",
                                        height = unit(0.6, "cm"), width = unit(0.4, "cm"),
                                        style = ggspatial::north_arrow_orienteering(text_size = 6, fill = c("black", "black")),
                                        pad_x = unit(0.2, 'cm'), pad_y = unit(0.25, 'cm')) +
      theme(panel.grid.major = element_line(colour = "transparent"),
            panel.grid.minor = element_line(colour = "transparent"),
            panel.background = element_rect(fill = 'white'), 
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.border = element_rect(fill = NA, colour = "black"),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
            axis.title = element_blank(), plot.margin = unit(c(0, 0, -0.2, -0.2), "lines"),
            legend.background = element_rect(fill = 'transparent'), legend.key = element_blank()
      )
    
    setnames(DT, c('lat', 'lon'), c(lat, lon))
    
    bm
    
  }
}



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




distance_filter <- function(DT, ID, distance_btw, max_distance, max_distance_before_after){
  
  if(nrow(DT) > 0) {
    setnames(DT, c(ID, distance_btw), c('IDu', 'distance_btw'))
    
    # speed over may
    DT[, distance_over_threshold := distance_btw > max_distance]
    
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
    
    DT[, bout := bCounter(distance_over_threshold), by = IDu]
    
    # select only true outlier
    DT[, bout_lengths := .N, by = .(IDu, bout)]
    DT[, seq := seq_len(.N), by = .(IDu, bout)]
    
    # distance between point before and after
    DT[, lon_before := data.table::shift(lon, type = 'lag'), by = IDu]
    DT[, lat_before := data.table::shift(lat, type = 'lag'), by = IDu]
    DT[, lon_after  := data.table::shift(lon, type = 'lead'), by = IDu]
    DT[, lat_after  := data.table::shift(lat, type = 'lead'), by = IDu]
    DT[, distance_btw_ab := sqrt(sum((c(lon_after, lat_after) - c(lon_before, lat_before))^2)), by = 1:nrow(DT)]
    
    DT[, distance_over_threshold_ab := distance_btw_ab < max_distance_before_after]
    
    # error
    DT[, error := seq == 2 & bout_lengths == 2 & distance_over_threshold == TRUE & distance_over_threshold_ab == TRUE]
    
    # delete unwanted columns
    DT[, c('bout','bout_lengths', 'seq', 'distance_over_threshold', 'lon_before', 'lat_before', 
           'lon_after', 'lat_after', 'distance_btw_ab', 'distance_over_threshold_ab') := NULL]
    
    setnames(DT, c('IDu', 'distance_btw'), c(ID, distance_btw))
    
  }
}



#' Round datetimes
#'
#' Written by Rui Barradas: https://stat.ethz.ch/pipermail/r-help/2012-June/315336.html
#'
#' @param x 
#' @param units 
#'
#' @return
#' @export
#'
#' @examples
#' x <- as.POSIXct(Sys.time() + 1:10*60, tz = "UTC")
#' x
#' round(x, "10 mins")

round.POSIXct <- function(x, units = c("mins", "5 mins", "10 mins", "15 mins", "quarter hours", "30 mins", "half hours", "hours")){
  if(is.numeric(units)) units <- as.character(units)
  units <- match.arg(units)
  r <- switch(units,
              "mins" = 60,
              "5 mins" = 60*5,
              "10 mins" = 60*10,
              "15 mins"=, "quarter hours" = 60*15,
              "30 mins"=, "half hours" = 60*30,
              "hours" = 60*60)
  H <- as.integer(format(x, "%H"))
  M <- as.integer(format(x, "%M"))
  S <- as.integer(format(x, "%S"))
  D <- format(x, "%Y-%m-%d")
  secs <- 3600*H + 60*M + S
  as.POSIXct(round(secs/r)*r, origin=D, tz = "UTC")
}














