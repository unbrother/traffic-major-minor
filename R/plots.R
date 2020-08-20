## Some graphs

traffic %>% ggplot(aes(x = road, y = aadt))+
  geom_boxplot()+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  xlab("Road reference code")+
  ylab("AADT")

traffic %>% ggplot(aes(x = road, y = aadt))+
  geom_col()+
  xlab("Road reference code")+
  ylab("AADT")


traffic %>% ggplot(aes(x = aadt))+
  geom_histogram()


graphs_areatype %>% ggplot(aes(x = major_aadt, y = aadt)) +
  geom_point(aes(colour = roadtype)) +
  geom_smooth(method="lm", se=F, fullrange=FALSE, level=0.95)


drop <- c("junction")
graphs_areatype = graphs_areatype[,!(names(graphs_areatype) %in% drop)]

graphs_clean <- (na.omit(graphs_areatype))


graphs_clean %>% ggplot(aes(x = major_aadt, y = centrality)) +
  geom_point(aes(colour = roadtype)) +
  geom_smooth(method="lm", se=F, fullrange=FALSE, level=0.95)


cor(graphs_clean$aadt, graphs_clean$centrality)

cor(graphs_clean$time_weighted, graphs_clean$centrality)

osm_roadtype %>% ggplot(aes(x = aadt))+
  geom_boxplot(aes(y = roadtype))

traffic_ref %>% filter(aadt != "NA") %>% ggplot(aes(x = roadtype))+
  geom_histogram(stat = "count")


osm_roadtype %>% filter(aadt != "NA") %>% summary()

group_ref <- traffic_ref %>%
  filter(aadt != "NA") %>%
  group_by(roadtype) %>%
  summarize(n())

osm_roadtype %>%
  group_by(roadtype) %>%
  summarize(n())

traffic_ref <- traffic %>%
  mutate(roadtype = ifelse(road %in% c("C","U"),
                           "minor", "major"))



osm_areatype %>% ggplot(aes(x = areatype))+
  geom_histogram(stat = "count")


osm_areatype %>% ggplot(aes(x = highway, fill = roadtype))+
  geom_histogram(stat = "count")


osm_areatype %>% ggplot(aes(x = areatype, y = aadt))+
  geom_point()


osm_areatype <- st_join(traffic_minor, st_transform(strategi, 27700))
osm_areatype <- osm_areatype %>%
  mutate(areatype = ifelse(LEGEND %in% c("Large Urban Area polygon"), "urban", "rural"))



osm_areatype %>%
  ggplot(aes(x = highway, y = aadt, fill = areatype))+
  geom_boxplot()


