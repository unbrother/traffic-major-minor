# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory


### Load libraries
library(RANN)
library(igraph)
library(cppRouting)
library(dodgr)
library(stplanr)
library(sandwich)

# ################################################################# #
#### Add AADT to roads                                           ####
# ################################################################# #

get.aadt.class <- function(e){
  #message(paste0("doing ",e))
  traffic.sub <- traffic.class[traffic.class$road == roadnames[e],]
  traffic.sub <- traffic.sub[!duplicated(traffic.sub$geometry),]
  osm.sub <- osm.nona[osm.nona$ref == roadnames[e],]

  ### need at least 2 points to make voronoi polygons
  if(nrow(traffic.sub) > 1){
    ### Make voronoi polygons and convert to SF
    voronoi <- dismo::voronoi(xy = st_coordinates(traffic.sub))
    voronoi <- as(voronoi, "sf")
    st_crs(voronoi) <- st_crs(traffic.sub)
  }else{
    ### Make a big buffer around the point
    voronoi <- st_buffer(traffic.sub, 1000)
  }

  ### Find Intersections of roads with vernoi polygons
  inter <- st_intersects(osm.sub,voronoi)
  ### Get aadt and ncycle values
  osm.sub$aadt <- lapply(1:nrow(osm.sub),function(x){as.numeric(round(mean(traffic.sub$aadt[inter[[x]]])),0)})
  osm.sub$ncycles <- lapply(1:nrow(osm.sub),function(x){as.numeric(round(mean(traffic.sub$ncycles[inter[[x]]])),0)})

  ### Remove Unneded Data
  osm.sub <- as.data.frame(osm.sub)
  osm.sub <- osm.sub[,c("osm_id","aadt","ncycles")]

  return(osm.sub)
}


### Separate Calssified and Unclassified Roads
traffic.class <- traffic[!substr(traffic$road,1,1) %in% c("U","C"),]
traffic.unclass <- traffic[substr(traffic$road,1,1) %in% c("U","C"),]


### Start with the classified
roadnames <- unique(traffic.class$road)
roadnames <- roadnames[roadnames %in% osm$ref]
osm.nona <- osm[!is.na(osm$ref),] #Create a working dataset without nas
osm.nona <- osm.nona[,c("osm_id","ref")] #Dump unneeded data
res.class <- lapply(1:length(roadnames),get.aadt.class)
res.class <- do.call("rbind",res.class)
res.class <- res.class[!is.na(res.class$osm_id),]
rm(osm.nona,roadnames)


### remove any duplicates
res.class <- res.class[!duplicated(res.class$osm_id),]
res.class$aadt <- as.numeric(res.class$aadt)

### Join onto the original osm data
osm <- left_join(osm,res.class, by = c("osm_id" = "osm_id"))
rm(res.class)


qtm(points_mp)
# ################################################################# #
#### Assign Road Type Variable (major and minor)                 ####
# ################################################################# #
# Based on the Morley SQL algorithm

# CLassify road type
osm_class <- osm %>%
  mutate(roadtype = ifelse(highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),
                           "major", "minor"))

# ################################################################# #
#### Assign Road importance with dodgr                           ####
# ################################################################# #

# CLassify by road importance
# start.time <- Sys.time()

osm_graph <- weight_streetnet(osm_class, wt_profile = "motorcar", type_col = "highway",
                              id_col = "osm_id")
#
# I believe this goes into the assignment part

# rnet_contracted = dodgr_contract_graph(osm_graph)
#
# v <- dodgr_vertices (rnet_contracted) # 500 vertices
# fmat <- array (1, dim = rep (nrow (v), 2))
# rnet_f <- dodgr_flows_aggregate (rnet_contracted, from = v$id, to = v$id,
#                                  flows = fmat)
# rnet_directed <- merge_directed_graph(rnet_f)
# rnet_f = dodgr_to_sf(rnet_directed)
# summary(rnet_f$flow)
#
#
#
# traffic_data_buffer = stplanr::geo_projected(traffic.class, st_buffer, dist = 200)
# traffic_estimates = aggregate(rnet_f["flow"], traffic_data_buffer, max)
#
# #> although coordinates are longitude/latitude, st_intersects assumes that they are planar
# traffic_data_sf$pcu_estimated = traffic_estimates$flow
# tm_shape(rnet_f) +
#   tm_lines(lwd = "flow", scale = 9) +
#   tm_shape(traffic_data_sf) +
#   tm_dots(size = "pcu", alpha = 0.2) +
#   tm_shape(traffic_data_sf) +
#   tm_dots(size = "pcu_estimated", col = "blue") +
#   tm_scale_bar()
#


osm_weighted <- osm_graph %>%
  group_by(way_id) %>%
  summarize(road_importance = sum(time_weighted))


osm_imp <- left_join(osm1, osm_weighted, by = c("osm_id" = "way_id"))

osm_imp <- osm_imp %>%
  mutate(road_importance = cut(road_importance,breaks = quantile(road_importance, probs = seq(0, 1, 0.2)),
                               labels=c("1","2","3","4","5")))


#osm2$road_importance <- unfactor(osm2$road_importance)

#osm2$road_importance <- as.numeric(factor(osm2$road_importance))

class(osm2$road_importance)

summary(osm2$road_importance)

#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken


osm_imp %>% filter(!is.na(road_importance)) %>% group_by(roadtype) %>%
  summarize(mean = mean(road_importance))

#osm_graph <- osm_graph[,c("from_id", "to_id", "time")]

#nodes <- unique(c(osm_graph$from_id, osm_graph$to_id))

#osm_graph <- makegraph(osm_graph, directed = T)
#osm_simple <- cpp_simplify(osm_graph, keep = NULL, rm_loop = TRUE, iterate = FALSE,
#             silent = TRUE)
#
#osm_graph <- dodgr_to_igraph(osm_graph)

#start.time <- Sys.time()
#dodgr_paths(osm_graph, from = nodes, to = nodes, pairwise = T)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken


#start.time <- Sys.time()
#get_multi_paths(osm_graph, from = nodes, to = nodes, keep = NULL, long = FALSE)
#
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

# ################################################################# #
#### Assign area type (urban and rural)                          ####
# ################################################################# #

# Classify as urban or rural

osm_urban <- st_join(osm_imp, strategi)
osm_urban <- osm_urban %>%
  mutate(urban = ifelse(LEGEND %in% c("Large Urban Area polygon"), "urban", "rural"))

qtm(osm_urban, lines.col = "roadtype", lines.lwd = 1, style = "col_blind")
qtm(osm_urban, lines.col = "urban", lines.lwd = 1, style = "col_blind")
# qtm(osm_urban)


osm_filtered <- osm_urban[,c("osm_id","name","ref","highway","aadt","roadtype","road_importance","urban")]

osm_filtered <- st_set_geometry(osm_filtered, NULL)
osm_filtered <- osm_filtered %>% filter(!is.na(aadt))

write.csv(osm_filtered, "data/osmurban.csv", row.names = FALSE)



rm(osm, osm1, osm2, osm_graph, osm_weighted, strategi, grump)

# ################################################################# #
#### Assign AADT from nearest major road                         ####
# ################################################################# #

# filter points inside bounds

points_major <- st_line_sample(osm_major, 1, density = 0.5, type = "regular", sample = NULL)

qtm(points_major)

#points_mp <- sf::st_intersection(points, bounds)
#qtm(points_mp)


points_major <- sf::st_combine(points_major)
voronoi <- try(sf::st_voronoi(points_major, envelope = bounds$geometry), silent = TRUE)
#if("try-error" %in% class(voronoi)){
#  ennv <- sf::st_buffer(bounds, 2000)
#  voronoi <- sf::st_voronoi(points_mp, envelope = ennv$geometry)
#}
voronoi <- sf::st_collection_extract(voronoi)
sf::st_crs(voronoi) <- 27700
voronoi <- sf::st_as_sf(data.frame(id = 1:length(voronoi), geometry = voronoi))
voronoi <- sf::st_join(voronoi, points)

qtm(voronoi) +
qtm(osm_urban, lines.col = "aadt", lines.lwd = 1)
# ################################################################# #
#### Apply Generalised Linear Model                              ####
# ################################################################# #

# Can't manage to do this yet.

# AADT = log(route_importance) + (OSM Road Type) + log(AADT on nearest major road) + (Urban or rural)

osm_urban$highway[osm2$highway %in% "primary_link"] <- "primary"
osm_urban$highway[osm2$highway %in% "tertiary_link"] <- "tertiary"
osm_urban$highway[osm2$highway %in% "secondary_link"] <- "secondary"

osm_model <- osm_urban %>% filter(!is.na(aadt))

class(osm_model$road_importance)

osm_model$road_importance <- as.numeric(factor(osm_model$road_importance))

summary(fit <- glm(urban ~ roadtype + log(road_importance) + log(aadt), family="poisson", data = osm_model))




LLR = -2 * (fit$null.deviance - fit$deviance)
pchisq(LLR, 4651, lower.tail = FALSE)



cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

predict(m1, s1, type="response", se.fit=TRUE)


glm()





