library(data.table)
library(igraph)

bacon = fread(file = 'Edgelist_with_ConnectionType.csv', header = TRUE)

# do the analysis every 100 years
max(bacon$end_year) - min(bacon$start_year)
year1 <- min(bacon$start_year) + 100
year2 <- year1 + 100
year3 <- year2 + 100

# 1 -----------------------------------------------------------
edges_1500 = bacon[start_year <= year1 & end_year > year1, .(from_ID, to_ID)]

mygraph <- graph.data.frame(edges_1500, directed = FALSE)

dc <- degree(mygraph)
cc <- closeness(mygraph)
bc <- betweenness(mygraph)
ec <- page_rank(mygraph, directed = FALSE)

centralities_1500 = data.table(ID = as.integer(names(V(mygraph))), degree = dc, closeness = cc,
                               betweenness = bc, eigen.cent = ec$vector)
centralities_1500[, idx := c(1:1293)]

# barplot
#centralities = centralities[order(Job.classification), ]
#bar_degree <- barplot(centralities$degree, col = rainbow(9)[as.factor(centralities$Job.classification)])

# multidimensional scaling
dist = distances(mygraph)
dist[dist == Inf] = nrow(dist)
mdscale1 = cmdscale(dist, k = 2)

# 2 -----------------------------------------------------------
edges_1600 = bacon[start_year <= year2 & end_year > year2, .(from_ID, to_ID)]

mygraph2 <- graph.data.frame(edges_1600, directed = FALSE)

dc <- degree(mygraph2)
cc <- closeness(mygraph2)
bc <- betweenness(mygraph2)
ec <- page_rank(mygraph2, directed = FALSE)

centralities_1600 = data.table(ID = as.integer(names(V(mygraph2))), degree = dc, closeness = cc,
                               betweenness = bc, eigen.cent = ec$vector)
centralities_1600[, idx := c(1:3848)]

# multidimensional scaling
dist = distances(mygraph2)
dist[dist == Inf] = nrow(dist)
mdscale2 = cmdscale(dist, k = 2)

# 3 -----------------------------------------------------------
edges_1700 = bacon[start_year <= year3 & end_year > year3, .(from_ID, to_ID)]

mygraph3 <- graph.data.frame(edges_1700, directed = FALSE)

dc <- degree(mygraph3)
cc <- closeness(mygraph3)
bc <- betweenness(mygraph3)
ec <- page_rank(mygraph3, directed = FALSE)

centralities_1700 = data.table(ID = as.integer(names(V(mygraph3))), degree = dc, closeness = cc,
                               betweenness = bc, eigen.cent = ec$vector)
centralities_1700[, idx := c(1:3618)]

# multidimensional scaling
dist = distances(mygraph3)
dist[dist == Inf] = nrow(dist)
mdscale3 = cmdscale(dist, k = 2)

write.csv(mdscale1, file = "mdscale_1500.csv", row.names = FALSE)
write.csv(mdscale2, file = "mdscale_1600.csv", row.names = FALSE)
write.csv(mdscale3, file = "mdscale_1700.csv", row.names = FALSE)



# correlation table ---------------------------------------------------------
cor(centralities_1500[, .(degree,closeness,betweenness,eigen.cent)])
cor(centralities_1600[, .(degree,closeness,betweenness,eigen.cent)])
cor(centralities_1700[, .(degree,closeness,betweenness,eigen.cent)])



# core-periphery structure -------------------------------------
# method 1: plotting the graphs
plot(mygraph, vertex.label = NA, vertex.size = 8)
plot(mygraph2, vertex.label = NA, vertex.size = 8)
plot(mygraph3, vertex.label = NA, vertex.size = 8)

# method 2 & 3
a <- table(coreness(mygraph))
b <- table(coreness(mygraph2))
c <- table(coreness(mygraph3))
plot(names(a), unname(a), main="1500", xlab="coreness", ylab="number of nodes")
plot(names(b), unname(b), main="1600", xlab="coreness", ylab="number of nodes")
plot(names(c), unname(c), main="1700", xlab="coreness", ylab="number of nodes")

# method 4: multidimensional scaling
plot(mdscale1[,1], mdscale1[,2], main = "multidimensional scaling 1500")
plot(mdscale2[,1], mdscale2[,2], main = "multidimensional scaling 1600")
plot(mdscale3[,1], mdscale3[,2], main = "multidimensional scaling 1700")

# method 5
components = clusters(mygraph)
max(components$csize)/vcount(mygraph)
components = clusters(mygraph2)
max(components$csize)/vcount(mygraph2)
components = clusters(mygraph3)
max(components$csize)/vcount(mygraph3)



#epidemics <- c(1603, 1636, 1665, 1666, 1775, 1776, 1832, 1848, 1849, 1854, 1889, 1890)
