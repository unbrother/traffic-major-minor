# Find the AADT on the major road ofr each junction point -----------------
junc_majmi <- st_as_sf(junc_majmi, coords = c("X","Y"), crs = 4326, remove = FALSE)
osm_major <- st_transform(osm_major, 4326)
junc_majmi <- st_join(junc_majmi, osm_major[,"aadt"])

osm_minor$aadt2 <- junc_majmi$aadt[match(osm_minor$nearst_junction ,junc_majmi$from_id)]
qtm(osm_minor, lines.col = "aadt2", lines.lwd = 3) # Relative flows number

osm_minor <- osm_minor %>% st_drop_geometry() %>% dplyr::select(osm_id, centrality, aadt2)
osm_aadt_maj <- left_join(osm %>% as.data.frame(), osm_minor %>% as.data.frame(), by = "osm_id")
osm_aadt_maj <- osm_aadt_maj %>% st_sf(sf_column_name = 'geom')

for(i in 1:nrow(osm_aadt_maj)){
  if(is.na(osm_aadt_maj$aadt2[i])){
    osm_aadt_maj$aadt2[i] = osm_aadt_maj$aadt[i]
  }
}

qtm(osm_aadt_maj, lines.lwd = 3, lines.col = "aadt2")
