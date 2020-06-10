
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
#library(deldir)
#library(dodgr)
#library(geodist)
#library(dismo)
#library(RANN)
#library(pbapply)
#library(stplanr)

tmap_mode("view")

# ################################################################# #
#### GET ROAD OSM DATA                                           ####
# ################################################################# #

### Get Roads
q = opq(getbb("isle of wight, UK")) %>%
  add_osm_feature(key = "highway")

osm_raw = osmdata_sf(q = q)

saveRDS(osm_raw, "data/osm_raw.Rds")
osm_raw = readRDS("data/osm_raw.Rds")


# ################################################################# #
#### GET ROAD SHAPEFILES DATA                                    ####
# ################################################################# #

# Import Strategi shp

strategi <- st_read("data/strategi/urban_region.shp")

# Global Rural-Urban Mapping Project (GRUMP) from NASA, contains polygons from every urban settlement in the world

# grump <- st_read("data/grump/global_urban_extent_polygons_v1.01.shp") # For some reason it does not work
# qtm(grump)

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


qtm(osm, lines.col = "highway", lines.lwd = 3)

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

### Primary Roads sometimes are missing refs, lets fix that
#qtm(osm_major, lines.col = "ref", lines.lwd = 3)
#qtm(osm_minor, lines.col = "ref", lines.lwd = 3)


