library(data.table)
library(igraph)

bacon = fread(file = 'Edgelist_with_ConnectionType.csv', header = TRUE)

# do the analysis every 100 years
max(bacon$end_year) - min(bacon$start_year)
year1 <- min(bacon$start_year) + 100
year2 <- year1 + 100
year3 <- year2 + 100

# 
edges = bacon[, .(from_ID, to_ID)]

mygraph <- graph.data.frame()

epidemics <- c(1603, 1636, 1665, 1666, 1775, 1776, 1832, 1848, 1849, 1854, 1889, 1890)