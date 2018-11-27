library(data.table)
library(ggplot2)

groups <- fread("SDFB_groups.csv")
View(groups)
people <- fread("SDFB_people.csv")
View(people)
edgelist <- fread("Edgelist_with_ConnectionType.csv")
View(edgelist)
connections <- fread("SDFB_Connection.csv")
View(connections)

length(unique(edgelist$relation_id)) # 118825 unique relations
unique(edgelist$relationship_type_name) # 53 relationship types
unique(edgelist$relationship_category) # Good 8 relationship categories
# Categories of relationship types: "Affective", "Kinship", "Labor/Professional", "Religious", 
# "Legal/Commercial", "Educational/Intellectual", "Political", "Spatial"  
unique(edgelist$relationship_type_name[edgelist$relationship_category ==  "Legal/Commercial"])
# Spatial looks ok, includes living together, neighbours, attended
# Political looks ok, includes clients and partrons of
# "Educational/Intellectual" - schoolmates and correspondents
# "Legal/Commercial" - spouses and in-laws; employment
unique(people$`Historical Significance`)  # Too many (~1400) different ones

# Calculating for every person the number of enemy relationships they have
length(edgelist$from_ID[edgelist$relationship_type_name %in% c("Rival of", "Enemy of")]) 
setkeyv(edgelist, c("relation_id", "from_ID", "to_ID"))
# Rbinding with the from - to swapped in order to have the symetrical relationships
edgelistT <- edgelist[,c(1,3,2,4,5,6,7,8,9,10)]
edgelist <- rbind(edgelist, edgelistT)
#dim(edgelist)
edgelist[relationship_type_name %in% c("Rival of", "Enemy of"), enemies := .N,from_ID]
enemies <- edgelist[relationship_type_name %in% c("Rival of", "Enemy of"),.N,from_ID]
setkeyv(people, c("SDFB Person ID"))
setkeyv(enemies, "from_ID")
people2 <- merge(people, enemies, by.x = "SDFB Person ID", by.y = "from_ID", all.x = TRUE)

# Homophily ---------------------------------------------------------------
# (https://en.wikipedia.org/wiki/Network_homophily)
# Homophily = based on node attributes, similar nodes may be more likely to attach to each other than dissimilar ones
# Homophily in social relations may lead to a commensurate distance in networks leading to the creation of clusters that have been observed in social networking services 
# Homophily is a key topic in network science as it can determine the speed of the diffusion of information and ideas.

# Is number of cross-gender degrees <=> randomly expected cross-gender degrees

# Calculating the number of cross-gender degrees in a certain year



# If no homophily, then cross gender edges should be 2*(male/all nodes)*(female/all nodes)










# Kinship nodes
Kin <- edgelist[edgelist$relationship_category == "Kinship",c(2,3)]
Kin = unique(Kin)
# Kinship nodes + their 1st degrees
KinExt <- edgelist[edgelist$from_ID %in% Kin$from_ID,c(2,3)]
KinExt = unique(KinExt)
# split into chunks in a list for each ego so the chunks can vary in size
# only care about chunks with at least two rows, which represents two strong ties leaving a node
KinExt[, min_two := .N > 1, by = "from_ID"]
KinExt = KinExt[min_two==TRUE]
KinExt = split(KinExt, by = "from_ID")

# get potential friends of friends that would be implied by triadic closure
# this is potential pairs of people that ego is connected to, when ego has more than one strong tie
friends_of_friends = lapply(seq_along(KinExt), function(i) t(combn(KinExt[[i]]$to_ID, 2)))
friends_of_friends = unique(do.call(rbind, friends_of_friends))
# friends_of_friends contains all possible ties that should exist in the network

# to satisfy strong triadic closure, all of these ties should be realized in the actual edgelist

# using the symertical edgelist 
KinExt <- unique(do.call(rbind, KinExt))
  
# do the scan of potential triads closed to ones that actually are closed in the real data
fof = paste(friends_of_friends[,1],friends_of_friends[,2],sep=",")
real = paste(KinExt$from_ID, KinExt$to_ID, sep=",")

# shows which triples with two strong ties aren't closed
fof[!(fof %in% real)]
# and what proportion of strong triadic closure is realized
mean(fof %in% real)


# Religious nodes
#save <- edgelist
years <- sort(unique(save$start_year))
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Religious",c(2,3)]
  Kin = unique(Kin)
# Kinship nodes + their 1st degrees
  KinExt <- edgelist[edgelist$from_ID %in% Kin$from_ID,c(2,3)]
  KinExt = unique(KinExt)
# split into chunks in a list for each ego so the chunks can vary in size
# only care about chunks with at least two rows, which represents two strong ties leaving a node
  KinExt[, min_two := .N > 1, by = "from_ID"]
  KinExt = KinExt[min_two==TRUE]
  KinExt = split(KinExt, by = "from_ID")

# get potential friends of friends that would be implied by triadic closure
# this is potential pairs of people that ego is connected to, when ego has more than one strong tie
  friends_of_friends = lapply(seq_along(KinExt), function(i) t(combn(KinExt[[i]]$to_ID, 2)))
  friends_of_friends = unique(do.call(rbind, friends_of_friends))
# friends_of_friends contains all possible triadic ties that should exist in the network

# to satisfy strong triadic closure, all of these ties should be realized in the actual edgelist

# do the scan of potential triads closed to ones that actually are closed in the real data
  fof = paste(friends_of_friends[,1],friends_of_friends[,2],sep=",")
  real = paste(edgelist$from_ID, edgelist$to_ID, sep=",")

# shows which triples with two strong ties aren't closed
# fof[!(fof %in% real)]
# and what proportion of strong triadic closure is realized
  obs <- mean(fof %in% real)
  closure[year == y,mean_closure := obs] 
}

closure[is.na(mean_closure),2] = 0
(cl <- ggplot(closure, aes(x=year, y =mean_closure)) +
  geom_line())



#Animating network development over time:
list_of_years <- sort(unique(vertices$Deal.Year))
vertices_columns <- vertices[,3:29]
pairs = list()

for (y in list_of_years){
  combinations = list()
  #  pairs = list()   Removed the clearing of pairs, in order to account for the cummulitative network state in each year
  investors = list() 
  vertices_subset <- filter(vertices_columns, Deal.Year==y)
  vertices_subset <- vertices_subset[,1:26]
  # creating the edge list for that year
  if (nrow(vertices_subset) != 0) {
    for (r in (1:nrow(vertices_subset))) {
      investors = list()
      for (c in 1:26) {
        if(!is.na(as.vector(vertices_subset[r,c]))) {
          investors <- c(investors, as.vector(vertices_subset[r,c]))}}
      if (length(investors) > 1) {
        combinations <- as.character(combn(investors, 2, FUN = NULL, simplify = TRUE, collapse = ","))
        # adding to the pairs list of previous year, because we want a cummulative network
        pairs <- c(pairs, combinations) } } }
  # creating and simplifying a network_graph for that year
  network_graph <- graph(as.character(pairs), directed = FALSE)
  network_graph <- simplify(network_graph, remove.multiple = TRUE, remove.loops = TRUE)
  # Determining the communities in that year
  communities <- cluster_fast_greedy(network_graph, merges = FALSE, modularity = FALSE,
                                     membership = TRUE, weights = NULL)
  # Printing out the graph for that year
  plot(communities, network_graph, vertex.label= NA, vertex.size = 1)
}