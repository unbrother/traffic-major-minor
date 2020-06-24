# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

# Load Packages -----------------------------------------------------------

library(sf)
library(dodgr)
library(tmap)
library(dplyr)
library(concaveman)
tmap_mode("view")


# Load Data ---------------------------------------------------------------

osm = st_read("data/iow_osm_all.gpkg")
points = st_read("data/iow_points.gpkg")
traffic = st_read("data/iow_traffic_points.gpkg")

# Find Junctions between minor and major roads ----------------------------

osm_major <- osm[osm$highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),]
osm_minor <- osm[!osm$highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),]

minor_int <- st_intersects(points, osm_minor)
major_int <- st_intersects(points, osm_major)

minor_int = lengths(minor_int)
major_int = lengths(major_int)
both_int = ifelse(minor_int > 0 & major_int > 0, TRUE, FALSE)

junc_majmi = points[both_int,]

# Convert to 4326 for dodgr
osm_minor <- st_transform(osm_minor, 4326)
junc_majmi <- st_transform(junc_majmi, 4326)

# 5) Get mid-point of minor roads, i.e. centroid on the line
minor_cent <- as.data.frame(st_coordinates(osm_minor))
minor_cent <- group_by(minor_cent, L1) %>%
  summarise(X = nth(X, n()/2),
            Y = nth(Y, n()/ 2))

# 6) Make dodgr graph of minor roads
graph <- weight_streetnet(osm_minor, wt_profile = "motorcar")
graph_ids <- graph[,c("from_id","from_lon","from_lat")]
graph_ids <- unique(graph_ids)

junc_majmi <- left_join(as.data.frame(st_coordinates(junc_majmi)), graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))
minor_cent <- left_join(minor_cent, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))

#swtich to fastest rather than shortest
dists <- dodgr_times(graph,
                     from = junc_majmi$from_id,
                     to = minor_cent$from_id,
                     shortest = FALSE)

nearst_junction <- list()
for(i in 1:ncol(dists)){
  sub <- dists[,i]
  sub <- sub[!is.na(sub)]
  if(length(sub) == 0){
    nearst_junction[[i]] <- NA
  } else {
    mindist <- names(sub)[sub == min(sub, na.rm = TRUE)]
    if(length(mindist) == 0){
      mindist <- NA
    }
    if(length(mindist) > 1){
      mindist <- mindist[1]
    }
    nearst_junction[[i]] <- mindist
  }

}
nearst_junction <- unlist(nearst_junction)

osm_minor$nearst_junction <- nearst_junction
qtm(osm_minor, lines.col = "nearst_junction", lines.lwd = 3) # plot each road colored by it nearest junction

# Make subgraphs
road_cut <- st_cast(osm_major$geom, "LINESTRING")

minor_points <- st_cast(osm_minor$geom, "POINT")
minor_points <- st_transform(minor_points, 27700)
all_points <- st_cast(osm$geom, "POINT")

minor_hull <- concaveman::concaveman(st_as_sf(all_points), concavity = 2)
minor_hull <- st_make_valid(minor_hull)
minor_hull <- st_collection_extract(minor_hull)

# qtm(minor_hull) +
#   qtm(osm_major)

zones <- lwgeom::st_split(minor_hull, road_cut)
zones <- st_collection_extract(zones)
zones <- st_as_sf(zones)
zones_inter <- st_contains_properly(zones, minor_points)
zones$npoints <- lengths(zones_inter)
zones <- zones[zones$npoints > 5, ]
zones$id <- 1:nrow(zones)
zones <- st_transform(zones, 4326)
qtm(zones, fill = "id")

graphs <- list()

minor_cent <- st_as_sf(minor_cent, coords = c("X","Y"), crs = 4326, remove = FALSE)

# Free up memmory
rm(road_cut, points, zones_inter, both_int,minor_int, minor_hull,
   minor_points, all_points, dists, traffic, graph, graph_ids, nearst_junction)
gc()
stop()

od_to_odmatrix <- function(flow, attrib = 3, name_orig = 1, name_dest = 2) {
  out <- matrix(
    nrow = length(unique(flow[[name_orig]])),
    ncol = length(unique(flow[[name_dest]])),
    dimnames = list(unique(flow[[name_orig]]), unique(flow[[name_dest]]))
  )
  out[cbind(flow[[name_orig]], flow[[name_dest]])] <- flow[[attrib]]
  out
}

graphs <- list()
for(i in zones$id){
  message(paste0("Doing Zone ",i))
  zone_sub <- st_buffer(zones[zones$id == i, ], 0.0001)
  osm_sub <- osm_minor[zone_sub, , op = st_within]
  cents_sub <- minor_cent[zone_sub, , op = st_within]
  # qtm(zone_sub) +
  #   qtm(osm_sub) +
  #   qtm(cents_sub)
  if(nrow(osm_sub) > 0){
    graph_sub <- weight_streetnet(osm_sub, wt_profile = "motorcar")
    pts = graph_sub[,c("from_id","component")]
    pts = unique(pts)
    cents_sub = left_join(cents_sub, pts, "from_id")
    cents_sub = st_drop_geometry(cents_sub)
    cents_sub = cents_sub[,c("from_id","component")]
    flow = cents_sub[rep(1:nrow(cents_sub), times = nrow(cents_sub)),]
    flow = cbind(flow, cents_sub[rep(1:nrow(cents_sub), each = nrow(cents_sub)),])
    names(flow) = c("from_id","component1","to_id","component2")
    flow$flow <- ifelse(flow$component1 == flow$component2, 1, NA)
    flow <- flow[,c("from_id","to_id","flow")]

    # Simiply the matrix as dodgr_flows_aggregate doesn't support pairwise
    flow = od_to_odmatrix(flow)
    graph_sub <- dodgr_flows_aggregate(graph_sub,
                                      from = pts$from_id,
                                      to = pts$from_id,
                                      flows = flow,
                                      contract = TRUE, quiet = TRUE, tol = 0,
                                      norm_sums = FALSE)
    class(graph_sub) <- "data.frame"
    graphs[[i]] <- graph_sub
  }


}

graphs <- bind_rows(graphs)

summary(graphs$flow)
#graph_undir <- merge_directed_graph(graphs)
geoms <- dodgr_to_sfc (graphs)
# graph_cont <- dodgr_contract_graph (graph_undir)
gsf <- sf::st_sf (geoms)
qtm(gsf, lines.col = "dat.flow", lines.lwd = 3) # Relative flows number



# Match Up osm_minor_mod with gsf -----------------------------------------
summary(unique(gsf$dat.way_id) %in% unique(osm_minor$osm_id))

imp_score <- st_drop_geometry(gsf[,c("dat.way_id","dat.flow")])
osm_minor <- left_join(osm_minor, imp_score, by = c("osm_id" = "dat.way_id"))


# Find the AADT on the major road ofr each junction point -----------------
junc_majmi <- st_as_sf(junc_majmi, coords = c("X","Y"), crs = 4326, remove = FALSE)
osm_major <- st_transform(osm_major, 4326)
junc_majmi <- st_join(junc_majmi, osm_major[,"aadt"])

osm_minor$aadt2 <- junc_majmi$aadt[match(osm_minor$nearst_junction ,junc_majmi$from_id)]


osm_minor$aadt3 <- osm_minor$aadt2 * (osm_minor$dat.flow / max(osm_minor$dat.flow, na.rm = TRUE))

tm_shape(osm_minor) +
tm_lines(lwd = 3, col = "aadt3",
    breaks = c(0,100,500,1000,2000,30000)) # Plot AADT of minor roads
