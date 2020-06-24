library(dodgr)
set.seed(123)
graph <- weight_streetnet (hampi)

from <- graph$from_id[1:10]
to <- graph$to_id[30:34]
flows <- matrix(rep(1, length(from) * length(to)),
                 nrow = length(from))

graph1 <- dodgr_flows_aggregate(graph, from = from, to = to, flows = flows, heap = "FHeap")
graph2 <- dodgr_flows_aggregate(graph, from = from, to = to, flows = flows, heap = "BHeap")
graph3 <- dodgr_flows_aggregate(graph, from = from, to = to, flows = flows, heap = "TriHeap")
#crashes graph4 <- dodgr_flows_aggregate(graph, from = from, to = to, flows = flows, heap = "TriHeapExt")
graph5 <- dodgr_flows_aggregate(graph, from = from, to = to, flows = flows, heap = "Heap23")

identical(graph1, graph2) #TRUE, FALSE
identical(graph1, graph3) #TRUE
identical(graph1, graph5) #TRUE

summary(graph1$flow[graph1$flow > 0])
dodgr_flowmap(graph1)

# Make a simple Tree in igraph
library(igraph)
graph_i <- make_tree(19,2,"undirected")
plot(graph_i)

# Convert to dodgr and make undirected
graph <- as_data_frame(graph_i)
graph2 <- graph[,2:1]
names(graph2) <- c("from","to")
graph <- rbind(graph, graph2)
graph$d <- 1

# Flow of 1 from all to all (same as edge betweenness)
flows <- matrix(rep(1, 19 * 19), ncol = 19)
graph <- dodgr_flows_aggregate(graph, from = 1:19, to = 1:19, flows = flows)
graph$flow

# Do edge betweeness in igraph
E(graph_i)$between <- edge.betweenness(graph_i)
E(graph_i)$between


