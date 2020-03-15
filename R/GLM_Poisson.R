# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(len, rowsum, inter)

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
rm(traffic)

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

# ################################################################# #
#### Assign Road Type Variable (major and minor)                 ####
# ################################################################# #

# CLassify road type
osm1 <- osm %>%
  mutate(roadtype = ifelse(highway %in% c("primary","secondary","motorway","trunk"), "major", "minor"))

# ################################################################# #
#### Assign Road importance with Djikstra algorithm              ####
# ################################################################# #

# CLassify by road importance
start.time <- Sys.time()

osm_graph <- weight_streetnet(osm1, wt_profile = "motorcar", type_col = "highway",
                              id_col = "osm_id")

osm_weighted <- osm_graph %>%
  group_by(way_id) %>%
  summarize(road_importance = sum(1/time_weighted))


osm2 <- left_join(osm1, osm_weighted, by = c("osm_id" = "way_id"))


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


osm2 %>% group_by(roadtype) %>%
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

#ni idea


# ################################################################# #
#### Apply Generalised Linear Model                              ####
# ################################################################# #

# AADT = log(route_importance) + (OSM Road Type) + log(AADT on nearest major road) + (Urban or rural)

osm2$highway[osm2$highway %in% "primary_link"] <- "primary"
osm2$highway[osm2$highway %in% "tertiary_link"] <- "tertiary"
osm2$highway[osm2$highway %in% "secondary_link"] <- "secondary"

osm_model <- osm2 %>% filter(!is.na(aadt))



summary(fit <- glm(road_importance ~ roadtype + highway, family="poisson", data = osm_model))





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

osm3 <- osm2 %>%
  mutate(AADT_pred = log(road_importance) +



