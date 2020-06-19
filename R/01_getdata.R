# 01 Getdata
# Aim this script get the OSM data for an area, cleans it and assigns
# AADT values where available



# ################################################################# #
#### LOAD LIBRARIES AND DEFINE CORE SETTINGS                     ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load libraries
library(sf)
library(tmap)
library(osmdata)
library(dplyr)
library(dismo)
library(deldir)

tmap_mode("view")

# ################################################################# #
#### GET ROAD OSM DATA                                           ####
# ################################################################# #

### Get Roads
q = opq(getbb("isle of wight, UK")) %>%
  add_osm_feature(key = "highway")

osm_raw = osmdata_sf(q = q)

# ################################################################# #
#### PREP INPUT DATA                                             ####
# ################################################################# #

###
osm <- osm_raw$osm_lines
points <- osm_raw$osm_points
polys <- osm_raw$osm_polygons # needed for roundabouts

polys <- polys[polys$highway %in% c("living_street","primary","primary_link",
                                    "residential","secondary", "secondary_link",
                                    "tertiary",  "tertiary_link", "unclassified"),]

polys <- st_cast(polys, "LINESTRING")
polys <- polys[,c("osm_id","name","ref","highway",
                  "junction","maxspeed","geometry")]

osm <- st_transform(osm, 27700)
points <- st_transform(points, 27700)
polys <- st_transform(polys, 27700)

osm <- osm[,c("osm_id","name","ref","highway",
              "junction","maxspeed","geometry")]

### only intrested in roads not paths
osm <- osm[osm$highway %in% c("living_street","primary","primary_link",
                              "residential","secondary", "secondary_link",
                              "tertiary",  "tertiary_link", "unclassified"),]

osm <- rbind(osm, polys)
rm(osm_raw, polys)

### Find Junctions, OSM Points are both nodes that make up lines/polygons, and objects e.g. shops
### remove points that are not nodes on the line
### node points have no tags

col.names <- names(points)[!names(points) %in% c("osm_id","highway", "crossing", "crossing_ref","geometry")] # Get column names other than osm_id, and highway which is just for junction types, and crossing info which can be junction between cycle way and road
points.sub <- points
points <- points[,c("osm_id","highway")]
points.sub <- as.data.frame(points.sub)
points.sub$geometry <- NULL
points.sub <- points.sub[,col.names]
rowsum <- as.integer(rowSums(!is.na(points.sub)))

rm(points.sub, col.names)

points <- points[rowsum == 0,] #Remove points with any tags

### now check highway tag to remove things like traffic lights
points <- points[is.na(points$highway) | points$highway %in% c("mini_roundabout","motorway_junction"), ]
points <- points[,c("osm_id","geometry")]

### Look for points that intersect lines
inter <- st_intersects(points,osm)
len <- lengths(inter)
points <- points[len >= 2,] #Only keep points that intersec at least 2 lines i.e. a junction

### Remove any duplicated points
points <- points[!duplicated(points$geometry),]
rm(len, rowsum, inter)

# ################################################################# #
#### GET AADT COUNTS                                             ####
# ################################################################# #

### Bounds and traffic
# WHERE TO GET?
bounds <- readRDS("data/bounds.Rds")
traffic <- readRDS("data/traffic.Rds")
traffic <- traffic[,c("road","aadt","ncycles")]
traffic <- traffic[bounds,] #SUBSET??

### Subset major and minor roads
osm <- osm[bounds,]
osm_major <- osm[osm$highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),]
osm_minor <- osm[!osm$highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),]


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


# Assign Traffic Counts to the roads --------------------------------------

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

  ### Remove Unneeded Data
  osm.sub <- as.data.frame(osm.sub)
  osm.sub <- osm.sub[,c("osm_id","aadt","ncycles")]

  return(osm.sub)
}


### Separate Classified and Unclassified Roads
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





# Save these for later ----------------------------------------------------

st_write(osm, "data/iow_osm_all.gpkg", delete_dsn = TRUE)
st_write(points, "data/iow_points.gpkg", delete_dsn = TRUE)
st_write(traffic, "data/iow_traffic_points.gpkg", delete_dsn = TRUE)
