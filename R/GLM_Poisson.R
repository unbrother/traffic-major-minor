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


# Use RANN to get the 20 nearest roads to each road
# If the reference is missining and the nearest road has the same highway type
# copy the reference. Else type next nearest road
# Repeate the whole process twice to allow propogation along roads
osm_major_cents <- st_coordinates(st_centroid(osm_major))

nn = RANN::nn2(osm_major_cents, k = 20) ### TRY TO FIX THIS LOOP ###

for(k in 1:2){
  for(i in 1:nrow(osm_major)){
    if(is.na(osm_major$ref[i])){
      for(j in 2:20){
        idx <- nn$nn.idx[i,j]
        if(osm_major$highway[idx] == osm_major$highway[i]){
          if(!is.na(osm_major$ref[idx])){
            osm_major$ref[i] <- osm_major$ref[idx]
            break
          }
        }
      }
    }
  }
}

qtm(osm_major, lines.col = "ref", lines.lwd = 3)
#rm(nn,osm_major_cents)

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
    voronoi <- st_buffer(traffic.sub, 500)
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
osm_graph <- weight_streetnet(osm_class, wt_profile = "motorcar", type_col = "highway",
                              id_col = "osm_id")

#osm_graph <- osm_graph %>%
#  mutate(road_importance = cut(road_importance,breaks = quantile(road_importance, probs = seq(0, 1, 0.2)),
#                               labels=c("1","2","3","4","5")))

osm_weighted <- osm_graph %>%
  group_by(way_id) %>%
  summarize(road_importance = round(mean(d)/mean(time)))

#graph <- dodgr_contract_graph (osm_graph)
#graph <- dodgr_centrality (osm_graph)

#osm_weighted <- osm_graph %>%
#  group_by(way_id) %>%
#  summarize(road_importance = sum(time_weighted))

#osm_weighted <- graph %>%
#  group_by(way_id) %>%
#  summarize(road_importance = mean(centrality))

osm_imp <- left_join(osm_class, osm_weighted, by = c("osm_id" = "way_id"))

# ################################################################# #
#### Assign area type (urban and rural)                          ####
# ################################################################# #

# Classify as urban or rural

osm_urban <- st_join(osm_imp, strategi)
osm_urban <- osm_urban %>%
  mutate(areatype = ifelse(LEGEND %in% c("Large Urban Area polygon"), "urban", "rural"))

# ################################################################# #
#### Assign AADT from nearest major road                         ####
# ################################################################# #

osm_cents <- st_coordinates(st_centroid(osm_urban))

nn_aadt = RANN::nn2(osm_cents, k = 50)

osm_urban$aadt[is.nan(osm_urban$aadt)] <- NA # Treat NaN as NA

osm_urban$aadt_major = osm_urban$aadt # Duplicate AADT column for control

is.na(osm_urban$aadt[2171])

#Make sure major roads have the right original value

for(a in 1:nrow(osm_urban)){
  if(osm_urban$roadtype[a] == "major"){
    osm_urban$aadt_major[a] = osm_urban$aadt[a]
  }
}

# Finish major roads (populate only major road)

for(k in 1:50){
  for(i in 1:nrow(osm_urban)){
    if(osm_urban$roadtype[i] == "major"){
      if(is.na(osm_urban$aadt_major[i])){
        for(j in 1:ncol(nn_aadt$nn.idx)){
         idx <- nn_aadt$nn.idx[i,j]
          if(osm_urban$roadtype[idx] == "major"){
           if(!is.na(osm_urban$aadt_major[idx])){
             osm_urban$aadt_major[i] <- osm_urban$aadt_major[idx]
             break
            }
          }
        }
      }
    }
  }
}

qtm(osm_urban, lines.col = "aadt_major", lines.lwd = 3)

# Populate minor roads from nearest major road

for(k in 1:50){
  for(i in 1:nrow(osm_urban)){
    if(osm_urban$roadtype[i] == "minor"){
        for(j in 1:ncol(nn_aadt$nn.idx)){
          idx <- nn_aadt$nn.idx[i,j]
          if(osm_urban$roadtype[idx] == "major"){
            if(!is.na(osm_urban$aadt_major[idx])){
              osm_urban$aadt_major[i] <- osm_urban$aadt_major[idx]
              break
          }
        }
      }
    }
  }
}

qtm(osm_urban, lines.col = "aadt_major", lines.lwd = 3)

#for(k in 1:50){
#  for(i in 1:nrow(osm_urban)){
#    if(osm_urban$roadtype[i] == "minor"){
#      for(j in 2:ncol(nn_aadt$nn.idx)){
#        idx <- nn_aadt$nn.idx[i,j]
#        if(osm_urban$roadtype[i] != osm_urban$roadtype[idx]){
#          if(!is.na(osm_urban$aadt_major[idx])){
#            osm_urban$aadt_major[i] <- osm_urban$aadt[idx]
#            break
#          }
#        }
#      }
#    }
#  }
#}


#qtm(osm_urban, lines.col = "aadt_major", lines.lwd = 3)
#qtm(osm_urban, lines.col = "aadt", lines.lwd = 3)
# Populate remaining minor roads

for(k in 1:20){
  for(i in 1:nrow(osm_urban)){
    if(is.na(osm_urban$aadt_major[i])){
      for(j in 1:ncol(nn_aadt$nn.idx)){
        idx <- nn_aadt$nn.idx[i,j]
        if(osm_urban$roadtype[idx] == osm_urban$roadtype[i]){
          if(!is.na(osm_urban$aadt[idx])){
            if(osm_urban$roadtype[idx] == "minor"){
              osm_urban$aadt_major[i] <- osm_urban$aadt[idx]
              break
            }
          }
        }
      }
    }
  }
}

# Populate remaining unclassified roads

for(k in 1:20){
  for(i in 1:nrow(osm_urban)){
    if(is.na(osm_urban$aadt[i])){
      for(j in 1:ncol(nn_aadt$nn.idx)){
        idx <- nn_aadt$nn.idx[i,j]
        if(osm_urban$roadtype[idx] == osm_urban$roadtype[i]){
          if(!is.na(osm_urban$aadt[idx])){
              osm_urban$aadt[i] <- osm_urban$aadt[idx]
              break
          }
        }
      }
    }
  }
}

#nn_aadt$nn.idx[1,2:5]
#nn_aadt$nn.idx[,j]
#nn_aadt$nn.idx[i,]

osm_glm <- osm_urban

# Confirm only minor roads were assigned
#aadt_major <- osm_glm %>% filter(roadtype == "major")


#rm(nn, nn_aadt, osm, osm_cents, osm_class, osm_graph, osm_imp, osm_major_cents, osm_urban, points, q, strategi)

# ################################################################# #
#### Mapping results                                             ####
# ################################################################# #

#st_write(osm_glm, "glm.shp")

#qtm(osm_urban, lines.col = "aadt", lines.lwd = 3)
#qtm(osm_urban, lines.col = "aadt_major", lines.lwd = 3)
#qtm(osm_urban, lines.col = "road_importance", lines.lwd = 3)
qtm(osm_urban, lines.col = "road_importance", lines.lwd = 3)

#tm_shape(osm_urban) +
#  tm_lines(col = "areatype", scale = 1, palette = c("red", "blue")) +
#  tm_facets(by = "roadtype")
#
# ################################################################# #
#### Apply Generalised Linear Model                              ####
# ################################################################# #

# AADT = log(route_importance) + (OSM Road Type) + log(AADT on nearest major road) + (Urban or rural)

osm_model <- osm_glm %>% filter(!is.na(aadt_major))

# Normalize continuous variables (AADT/Road Importance)

#osm_model$aadt <- log(osm_model$aadt)
#osm_model$road_importance <- log(osm_model$road_importance)
#
#osm_model$aadt <- (osm_model$aadt-min(osm_model$aadt))/(max(osm_model$aadt)-min(osm_model$aadt))
#osm_model$road_importance <- (osm_model$road_importance-min(osm_model$road_importance))/(max(osm_model$road_importance)-min(osm_model$road_importance))
#
#osm_model$road_importance <- as.numeric(factor(osm_model$road_importance))
#osm_model$areatype <- factor(osm_model$areatype)
#osm_model$roadtype <- factor(osm_model$roadtype)

str(osm_model)

summary(fit <- glm(aadt_major ~ log(road_importance) + roadtype + log(aadt) + areatype, family="poisson", data = osm_model))

osm_proj <-predict(fit, osm_glm,
        type = "response")

osm_glm$osm_predict <- osm_proj

aadt_minor <- osm_glm %>% filter(roadtype == "minor", !is.na(aadt_major))

# ################################################################# #
#### Accuracy tests                                              ####
# ################################################################# #

#GEH


#RMSE
Metrics::rmse(aadt_minor$aadt_major, aadt_minor$osm_predict)




