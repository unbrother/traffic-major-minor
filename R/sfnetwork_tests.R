# sf network tests

library(sf)
library(dodgr)
library(tmap)
library(dplyr)
library(sfnetworks)
library(tidygraph)
tmap_mode("view")


# Load Data ---------------------------------------------------------------

osm = st_read("data/iow_osm_all.gpkg")
osm_minor <- osm[!osm$highway %in% c("motorway","motowray_link","primary","primary_link","trunk","trunk_link"),]
# doesn't work with roundabouts
net <- as_sfnetwork(osm_minor )


net <- net %>%
  activate("edges") %>%
  mutate(centrality = centrality_edge_betweenness())

gsf <- st_as_sf(net)
head(gsf)
qtm(gsf, lines.col = "centrality", lines.lwd = 3)
