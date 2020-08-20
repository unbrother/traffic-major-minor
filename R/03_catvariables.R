# Aim: Assign categorical variables
# This script assumes the data has already been cleaned and the AADT for major roads assigned


# Load Packages -----------------------------------------------------------

library(sf)
library(dodgr)
library(tmap)
tmap_mode("view")


# Load Data ---------------------------------------------------------------



# Assign Road Type Variable (major and minor) -----------------------------

# Based on the Morley SQL algorithm

# CLassify road type
osm_roadtype <- osm %>%
  mutate(roadtype = ifelse(highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),
                           "major", "minor"))
osm_roadtype <- st_transform(osm_roadtype, 4326)

qtm(osm_roadtype, lines.lwd = 3, lines.col = "roadtype")

# Assign area type (urban and rural) ---------------------------------------

# Import Strategi shp

strategi <- st_read("data/strategi/urban_region.shp") #Find if this can be obtained directly
strategi <- st_transform(strategi, 4326)

qtm(strategi)

tm_shape(strategi) +
  tm_polygons("LEGEND") +
  tm_add_legend()

# Classify as urban or rural

osm_areatype <- st_join(osm_roadtype, strategi, by = c("geom" = "geometry"))
osm_areatype <- osm_areatype %>%
  mutate(areatype = ifelse(LEGEND %in% c("Large Urban Area polygon"), "urban", "rural"))

qtm(osm_areatype, lines.lwd = 3, lines.col = "areatype")

graphs_areatype <- st_join(osm_areatype, graphs, by = c("osm_id" = "way_id"))

graphs_data <- st_drop_geometry(graphs_areatype[,c("roadtype", "areatype","centrality","major_aadt")])

plot(graphs_data)

mydata.cor = cor(graphs_data)
