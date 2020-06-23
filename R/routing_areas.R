# Make subgraphs
road_cut <- st_cast(osm_major$geom, "LINESTRING")

minor_points <- st_cast(osm_minor$geom, "POINT")
minor_points <- st_transform(minor_points, 27700)
all_points <- st_cast(osm$geom, "POINT")

minor_hull <- concaveman::concaveman(st_as_sf(all_points), concavity = 2)
minor_hull <- st_make_valid(minor_hull)
minor_hull <- st_collection_extract(minor_hull)

qtm(minor_hull) +
  qtm(osm_major)

zones <- lwgeom::st_split(minor_hull, road_cut)
zones <- st_collection_extract(zones)
zones <- st_as_sf(zones)
zones_inter <- st_contains_properly(zones, minor_points)
zones$npoints <- lengths(zones_inter)
zones <- zones[zones$npoints > 5, ]
zones$id <- 1:nrow(zones)
zones <- st_transform(zones, 4326)
qtm(zones, fill = "id")

