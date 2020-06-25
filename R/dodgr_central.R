dodgr_central = function(g){
  g <- dodgr_centrality(graph_sub)
  geom <- g[,c("from_lon","from_lat","to_lon","to_lat")]
  g <- g[,!names(g) %in% c("from_lon","from_lat","to_lon","to_lat","from_id","to_id")]
  geom <- as.matrix(geom)
  geom <- split(geom, 1:nrow(geom))
  geom <- lapply(geom, function(x){
    sf::st_linestring(matrix(x, ncol = 2, byrow = TRUE))
  })
  geom <- sf::st_as_sfc(geom)
  g$geometry <- geom
  g <- st_as_sf(g, crs = 4326)

  g <- stplanr::overline2(g, attrib = "centrality")
  return(g)
}



