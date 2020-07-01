# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

# Load Packages -----------------------------------------------------------

library(ggplot2)
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

# Match Major Road AADT onto junctions
junc_majmi <- st_join(junc_majmi, osm_major[,"aadt"])
junc_majmi <- junc_majmi[!duplicated(junc_majmi$geom),]
qtm(junc_majmi, dots.col = "aadt")


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

junc_majmi <- cbind(junc_majmi, st_coordinates(junc_majmi))
junc_majmi <- left_join(junc_majmi, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))
minor_cent <- left_join(minor_cent, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))

# For each minor road centroid, fin the nearest (in time) junction  --------
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
osm_minor$major_aadt <- junc_majmi$aadt[match(osm_minor$nearst_junction, junc_majmi$from_id)]
# plot each road colored by the AADT on the nearest (in time) major road
qtm(osm_minor, lines.col = "major_aadt", lines.lwd = 3)



# Split the minor roads into zones divided by the major road network --------

# Make subgraphs
road_cut <- st_cast(osm_major$geom, "LINESTRING")

minor_points <- st_cast(osm_minor$geom, "POINT")
minor_points <- st_transform(minor_points, 27700)
all_points <- st_cast(osm$geom, "POINT")

minor_hull <- concaveman::concaveman(st_as_sf(all_points), concavity = 2)
minor_hull <- st_make_valid(minor_hull)
minor_hull <- st_collection_extract(minor_hull)

zones <- lwgeom::st_split(minor_hull, road_cut)
zones <- st_collection_extract(zones)
zones <- st_as_sf(zones)
zones_inter <- st_contains_properly(zones, minor_points)
zones$npoints <- lengths(zones_inter)
zones <- zones[zones$npoints > 5, ]
zones$id <- 1:nrow(zones)
zones <- st_transform(zones, 4326)

# plot the zones created
qtm(zones, fill = "id")

minor_cent <- st_as_sf(minor_cent, coords = c("X","Y"), crs = 4326, remove = FALSE)

# Free up memory
rm(road_cut, points, zones_inter, both_int,minor_int, minor_hull,
   minor_points, all_points, dists, graph, graph_ids, nearst_junction,
   major_int, mindist, sub)
gc()


# Loop over each zone and find the centrality of the minor road ne --------
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
    graph_sub <- dodgr_centrality(graph_sub)
    graph_sub <- merge_directed_graph(graph_sub)
    clear_dodgr_cache()
    graph_sub <- dodgr_to_sf(graph_sub)
    #graph_sub <- dodgr_central(graph_sub)
    graphs[[i]] <- graph_sub
  }


}

graphs <- bind_rows(graphs)

# Plot the centrality of all minor roads
summary(graphs$centrality)
tm_shape(graphs) +
  tm_lines(col = "centrality", lwd = 3, style = "jenks")


# Match Up centrality with major aadt -----------------------------------------
summary(unique(graphs$way_id) %in% unique(osm_minor$osm_id))
summary(duplicated(graphs$way_id)) # some osm_ids have been split

graphs <- left_join(graphs,
                       st_drop_geometry(osm_minor[,c("osm_id","major_aadt")]),
                       by = c("way_id" = "osm_id"))



# Graphs is a sf df of minor roads with a major_aadt and centrality --------
head(graphs)


# Compare measured AADT and centrality ------------------------------------
traffic_minor <- traffic[traffic$road %in% c("C","U"),]
# Buffer as traffic points are away from roads
traffic_minor <- st_buffer(traffic_minor, 33)
traffic_minor <- st_transform(traffic_minor, 4326)

traffic_minor <- st_join(traffic_minor, graphs)
summary(is.na(traffic_minor$centrality))

# some simple plots
plot(traffic_minor$centrality, traffic_minor$aadt)
plot(traffic_minor$major_aadt, traffic_minor$aadt)

traffic_minor %>% ggplot(aes(x = aadt, y = centrality)) +
  geom_point(aes(shape = highway)) +
  geom_smooth(method="lm", se=F, fullrange=FALSE, level=0.95)

# simple model
m1 <- lm(aadt ~ centrality + major_aadt, data = traffic_minor)
summary(m1)
plot(traffic_minor$aadt[!is.na(traffic_minor$centrality)], predict(m1))
abline(0,1, col = "red")


# log model
m2 <- lm(aadt ~ log(centrality) + log(major_aadt), data = traffic_minor)
summary(m2)
plot(traffic_minor$aadt[!is.na(traffic_minor$centrality)], predict(m2))
cor(traffic_minor$aadt[!is.na(traffic_minor$centrality)], predict(m2))
abline(0,1, col = "red")

# Assign area type (urban and rural) ---------------------------------------

# Import Strategi shp

strategi <- st_read("data/strategi/urban_region.shp") #Find if this can be obtained directly
strategi <- st_transform(strategi, 4326)

# Classify as urban or rural

traffic_areatype <- st_join(traffic_minor, strategi, by = c("geom" = "geometry"))
traffic_areatype <- traffic_areatype %>%
  mutate(areatype = ifelse(LEGEND %in% c("Large Urban Area polygon"), "urban", "rural"))

# log model with added variable
m2a <- lm(log(aadt) ~ log(centrality) + log(major_aadt) + areatype, data = traffic_areatype)
summary(m2a)
plot(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)], exp(predict(m2a)))
abline(0,1, col = "red")
cor(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)], exp(predict(m2a)))

# log model poisson
m3 <- glm(aadt ~ log(centrality) + log(major_aadt) + areatype, data = traffic_areatype, family = "poisson")
summary(m3)
plot(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)], exp(predict(m3)))
cor(traffic_areatype$aadt[!is.na(traffic_areatype$centrality)], exp(predict(m3)))
abline(0,1, col = "red")

rmse <- sum(sqrt((exp(predict(m2a))-traffic_minor$aadt)^2))/22

data.frame(exp(predict(m2a)), traffic_minor$aadt)

4.80/mean(traffic_minor$aadt)
