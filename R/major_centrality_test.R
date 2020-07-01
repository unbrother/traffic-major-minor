osm <- st_transform(osm, 4326)
osm_major <- st_transform(osm, 4326)

major_centrality <- weight_streetnet(osm, wt_profile = "motorcar")
major_centrality <- dodgr_centrality(major_centrality)
major_centrality <- merge_directed_graph(major_centrality)
clear_dodgr_cache()
major_centrality <- dodgr_to_sf(major_centrality)

summary(major_centrality$centrality)
tm_shape(major_centrality) +
  tm_lines(col = "centrality", lwd = 3)

graphs_major <- left_join(major_centrality,
                    st_drop_geometry(osm_major[,c("osm_id","aadt")]),
                    by = c("way_id" = "osm_id"))
graphs_major <- graphs_major[graphs_major$highway %in% c("motorway","motoway_link","primary","primary_link","trunk","trunk_link"),]
colnames(graphs_major)[which(names(graphs_major) == "aadt")] <- "major_aadt"


traffic_major <- traffic[!traffic$road %in% c("C","U"),]

traffic_major <- st_buffer(traffic_major, 1000)
traffic_major <- st_transform(traffic_major, 4326)

traffic_major <- st_join(graphs_major, traffic_major)

qtm(traffic_major, lines.col = "aadt", lines.lwd = 3)
