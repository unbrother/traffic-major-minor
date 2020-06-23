# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

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

dists <- dodgr_dists(graph,
                     from = junc_majmi$from_id,
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

osm_minor$nearst_junction <- nearst_junction
qtm(osm_minor, lines.col = "nearst_junction", lines.lwd = 3) # plot each road colored by it nearest junction

# Do the routing again and aggregate flows
from = osm_minor$nearst_junction
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
                                  contract = TRUE, quiet = FALSE, tol = 0,
                                  norm_sums = FALSE)
summary(agg_flow$flow)
graph_undir <- merge_directed_graph(agg_flow)
geoms <- dodgr_to_sfc (graph_undir)
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

qtm(osm_minor, lines.lwd = 3, lines.col = "aadt2") # Plot AADT of nearest major road assigned to minor road

osm_minor$aadt3 <- osm_minor$aadt2 * osm_minor$
