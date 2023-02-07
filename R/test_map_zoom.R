
buffer = 100


DT = data.table(name = c('NARL', 'Utqiagvik'),
                lat  = c(71.320854, 71.290246),
                lon  = c(-156.648210, -156.788622))

# change projection
st_transform_DT(DT)

st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox_ext(asp = '16:9', crs = projection) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb = st_bbox(rs_extent) %>% data.table

bb$.[1]

bb$.[2]

dm1 = data.table(x1 = bb$.[1], x2 = bb$.[2], y1 = bb$.[3], y2 = bb$.[4])


dm2 = data.table(x1 = bb2$.[1], x2 = bb2$.[2], y1 = bb2$.[3], y2 = bb2$.[4])

seq(dm1[1, x1], dm2[1, x1], by = 100)


bm =
  ggplot() +
  geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
  geom_sf(data = osm_roads, color = 'grey70') +
  geom_sf(data = osm_buildings, color = 'grey30') +
  coord_sf(expand = FALSE, xlim = c(bb$.[1], bb$.[3]), ylim = c(bb$.[2], bb$.[4])) +
  ggspatial::annotation_scale(aes(location = 'br'), text_cex = 2) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.background = element_rect(fill = '#D7E7FF'),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))



bm









buffer = 1000


DT = data.table(name = c('NARL', 'Utqiagvik'),
                lat  = c(71.320854, 71.290246),
                lon  = c(-156.648210, -156.788622))

# change projection
st_transform_DT(DT)

st_d = st_as_sf(DT[!is.na(lon), .(lon, lat)], coords = c('lon','lat'), crs = projection)
rs_extent = st_d %>% st_bbox(crs = projection) %>% st_as_sfc %>% st_buffer(buffer) %>% st_bbox_ext(asp = '16:9', crs = projection) %>% st_as_sfc %>% st_geometry
rs_extent = st_transform(rs_extent, crs = st_crs(osm_land))
bb2 = st_bbox(rs_extent) %>% data.table



bm =
  ggplot() +
  geom_sf(data = osm_land, fill = '#faf5ef') + #f6eee2  #f8f2e9
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'wetland', ], fill = '#faf5ef', alpha = 0.6, colour = '#faf5ef') +
  geom_sf(data = osm_lakes[osm_lakes$fclass == 'water', ], fill = '#f3fafd', colour = 'grey80') + # #D7E7FF
  geom_sf(data = osm_roads, color = 'grey70') +
  geom_sf(data = osm_buildings, color = 'grey30') +
  coord_sf(expand = FALSE, xlim = c(bb2$.[1], bb2$.[3]), ylim = c(bb2$.[2], bb2$.[4])) +
  ggspatial::annotation_scale(aes(location = 'br'), text_cex = 2) +
  theme(panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.background = element_rect(fill = '#D7E7FF'),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title = element_blank(), plot.margin = unit(rep(0, 4), "lines"))



bm