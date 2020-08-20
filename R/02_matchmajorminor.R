# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned


# Load Packages -----------------------------------------------------------

library(sf)
library(dodgr)
library(tmap)
library(dplyr)
tmap_mode("view")


# Load Data ---------------------------------------------------------------

osm = st_read("data/iow_osm_all.gpkg")
points = st_read("data/iow_points.gpkg")
traffic = st_read("data/iow_traffic_points.gpkg")

# Find Junctions between minor and major roads ----------------------------

osm_major <- osm[osm$highway %in% c("data/grump/grump-v1-urban-ext-polygons-rev01-documentation.pdfrway","motowray_link","primary","primary_link","trunk","trunk_link"),]
osm_minor <- osm[!osm$highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),]

minor_int <- st_intersects(points, osm_minor)
major_int <- st_intersects(points, osm_major)

minor_int = lengths(minor_int)
major_int = lengths(major_int)
both_int = ifelse(minor_int > 0 & major_int > 0, TRUE, FALSE)

junc_majmi = points[both_int,]
junc_majmi = as.data.frame(st_coordinates(junc_majmi))
rm(major_int, minor_int)

# Break cross roads
# Otherwise flows can go around the major roads
osm_major_buff <- st_buffer(osm_major, 1)
osm_major_buff <- st_union(osm_major_buff)
osm_minor_mod <- st_difference(osm_minor, osm_major_buff)
osm_minor_mod <- st_cast(osm_minor_mod, "MULTILINESTRING")
osm_minor_mod <- st_cast(osm_minor_mod, "LINESTRING")

# Make new junction intersection points
# Which are fractionally offset form the original junction points
# So match up with the broken up road network.

junc_majmi2 = st_buffer(points[both_int,], 1.1)
osm_minor_points = st_cast(osm_minor_mod, "POINT")
junc_majmi2 = osm_minor_points[junc_majmi2,,op = st_within]

# Convert to 4326 for dodgr
osm_minor_mod <- st_transform(osm_minor_mod, 4326)
junc_majmi2 <- st_transform(junc_majmi2, 4326)

# 5) Get mid-point of minor roads, i.e. centroid on the line
minor_cent <- as.data.frame(st_coordinates(osm_minor_mod))
minor_cent <- group_by(minor_cent, L1) %>%
  summarise(X = nth(X, n()/2),
            Y = nth(Y, n()/ 2))

# 6) Make dodgr graph of minor roads
graph <- weight_streetnet(osm_minor_mod, wt_profile = "motorcar")
graph_ids <- graph[,c("from_id","from_lon","from_lat")]
graph_ids <- unique(graph_ids)

junc_majmi2 <- left_join(as.data.frame(st_coordinates(junc_majmi2)), graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))
minor_cent <- left_join(minor_cent, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))

dists <- dodgr_dists(graph,
                     from = junc_majmi2$from_id,
                     to = minor_cent$from_id)

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
#nearst_junction <- data.frame(from_id = minor_cent$from_id, nearst_junction = nearst_junction)

osm_minor_mod$nearst_junction <- nearst_junction
qtm(osm_minor_mod, lines.col = "nearst_junction", lines.lwd = 3) # plot each road colored by it nearest junction

# Do the routing again and aggregate flows
from = osm_minor_mod$nearst_junction
to = minor_cent$from_id
to = to[!is.na(from)]
from = from[!is.na(from)]

# Simiply the matrix as dodgr_flows_aggregate doesn't support pairwise

od_to_odmatrix <- function(flow, attrib = 3, name_orig = 1, name_dest = 2) {
  out <- matrix(
    nrow = length(unique(flow[[name_orig]])),
    ncol = length(unique(flow[[name_dest]])),
    dimnames = list(unique(flow[[name_orig]]), unique(flow[[name_dest]]))
  )
  out[cbind(flow[[name_orig]], flow[[name_dest]])] <- flow[[attrib]]
  out
}

flow = od_to_odmatrix(data.frame(from = from, to = to, flow = 1))
# flow2 = flow
# flow2[is.na(flow2)] <- 0
# dodgr_flows_aggregate  now seems to work on Windows
agg_flow <- dodgr_flows_aggregate(graph,
                                  from = row.names(flow),
                                  to = colnames(flow),
                                  flows = flow,
                                  contract = TRUE, quiet = FALSE, tol = 0, norm_sums = FALSE )
summary(agg_flow$flow)
graph_undir <- merge_directed_graph(agg_flow)
geoms <- dodgr_to_sfc (graph_undir)
graph_cont <- dodgr_contract_graph (graph_undir)
gsf <- sf::st_sf (geoms)
qtm(gsf, lines.col = "dat.flow", lines.lwd = 3) # Relative flows number



# Match Up osm_minor_mod with gsf -----------------------------------------
summary(unique(gsf$dat.way_id) %in% unique(osm_minor_mod$osm_id))

#imp_score <- st_drop_geometry(gsf[,"dat.way_id","dat.flow"])
#imp_score <- cbind(gsf$dat.way_id, as.numeric(gsf$dat.flow))
#colnames(imp_score) <- c("dat.way_id", "score")
#imp_score <- data.frame(imp_score)
#osm_minor_mod <- left_join(osm_minor_mod, imp_score, by = c("osm_id" = "dat.way_id"))
#osm_mod <- left_join(osm, imp_score, by = c("osm_id" = "dat.way_id"))
#
#str(osm_mod)

qtm(osm_minor_mod, lines.col = "score", lines.lwd = 3) # Relative flows number

# Find the AADT on the major road ofr each junction point -----------------

#junc_majmi2 <- st_as_sf(junc_majmi2, coords = c("X","Y"), crs = 4326)
osm_major <- st_transform(osm_major, 4326)
osm_major_points <- st_cast(osm_major, "POINT")
osm_major_points <- cbind(st_drop_geometry(osm_major_points), st_coordinates(osm_major_points))

# find nearst neighbours

maj_nn <- RANN::nn2(osm_major_points[,c("X","Y")], junc_majmi2[,c("X","Y")], k = 1)
maj_nn_idx <- maj_nn$nn.idx
maj_nn_dist <- maj_nn$nn.dists
summary(maj_nn$nn.dists) # should be around 1m in degrees ~ 0.00001

junc_majmi2$aadt <- osm_major_points$aadt[maj_nn$nn.idx]

osm_minor_mod$aadt2 <- junc_majmi2$aadt[match(osm_minor_mod$nearst_junction ,junc_majmi2$from_id)]

qtm(osm_minor_mod, lines.lwd = 3, lines.col = "aadt2") # Plot AADT of nearest major road assigned to minor road
