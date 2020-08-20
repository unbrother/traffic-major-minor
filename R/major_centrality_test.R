# 1) Make dodgr graph of minor roads
graph_major <- weight_streetnet(osm, wt_profile = "motorcar")

major_centrality <- dodgr_centrality(graph_major)
major_centrality <- merge_directed_graph(major_centrality)
clear_dodgr_cache()
major_centrality <- dodgr_to_sf(major_centrality)

major_centrality <- major_centrality %>%
  mutate(roadtype = ifelse(highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),
                           "major", "minor"))
major_centrality <- st_transform(osm_roadtype, 4326)
major_centrality <- major_centrality %>% filter(roadtype == "major")


major_graphs <- left_join(major_centrality,
                    st_drop_geometry(osm_major[,c("osm_id")]),
                    by = c("way_id" = "osm_id"))

osm_roadtype <- right_join(major_graphs,
                          st_drop_geometry(graphs[,c("way_id")]),
                          by = c("way_id" = "way_id"))


summary(unique(osm_roadtype$way_id) %in% unique(osm_roadtype$osm_id))
summary(duplicated(osm_roadtype$way_id)) # some osm_ids have been split

tm_shape(osm_roadtype) +
  tm_lines()

tm_shape(osm_roadtype) +
  tm_lines(col = "centrality", lwd = 3, style = "jenks")



