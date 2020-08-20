tmap_mode("view")

# Map with Traffic counts and Road reference codes

traffic_map <- traffic %>% filter(road != "C" & road != "U")
osm_map <- osm[!is.na(osm$ref),]

tm_shape(osm_map[osm_map$highway %in% c("motorway","motorway_link","primary","primary_link","trunk","trunk_link"),])+
  tm_lines(lwd = 3) +
  tmap_options(max.categories = 36) +
  tm_shape(traffic_map) +
  tm_view(view.legend.position = c("right", "top")) +
  tm_dots(col = "aadt", size = 0.05)

tm_shape(st_make_valid(zones))+
  tm_polygons(col = "id", title = "Zone ID") +
  tm_shape(osm_minor) +
  tm_lines(col = "highway", lwd = 1.5, title.col = "Minor Roads", palette = "div")+
  tm_shape(osm_map[osm_map$highway %in% c("motorway","motorway_link","primary","primary_link","trunk","trunk_link"),]) +
  tm_lines(lwd = 3)

# Traffic over roadtype
tm_shape(osm_roadtype)+
  tm_lines(col = "roadtype", lwd = 3) +
  tmap_options(max.categories = 36) +
  tm_shape(traffic_map) +
  tm_view(view.legend.position = c("right", "top")) +
  tm_dots(col = "aadt", size = 0.05)
