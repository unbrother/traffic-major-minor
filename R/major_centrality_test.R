major_centrality <- weight_streetnet(osm, wt_profile = "motorcar")
major_centrality <- dodgr_centrality(major_centrality)

major_cent <- dodgr_to_sf(major_centrality)
major_cent <- stplanr::overline2(major_cent, attrib = "centrality")

#g = major_centrality
g <- dodgr_centrality(major_centrality)
major_geom <- major_centrality[,c("from_lon","from_lat","to_lon","to_lat")]
g <- major_centrality[,!names(major_centrality) %in% c("from_lon","from_lat","to_lon","to_lat","from_id","to_id")]
major_geom <- as.matrix(major_geom)
major_geom <- split(major_geom, 1:nrow(major_geom))
major_geom <- lapply(major_geom, function(x){
  sf::st_linestring(matrix(x, ncol = 2, byrow = TRUE))
})
major_geom <- sf::st_as_sfc(major_geom)
g$geometry <- major_geom
g <- st_as_sf(g, crs = 4326)
g <- stplanr::overline2(g, attrib = "centrality")



osm_major <- left_join(osm_major, major_centrality, by = c("osm_id" = "way_id"))
