library(data.table)
library(ggplot2)
library(igraph)
library(ergm)
library(intergraph)

setwd("C:/Users/MJ/Documents/GitHub/MSBA-Team6-BaconProject")

groups <- fread("SDFB_groups.csv")
#View(groups)
people <- fread("SDFB_people.csv")
#View(people)
edgelist <- fread("Edgelist_with_ConnectionType.csv")
#View(edgelist)
connections <- fread("SDFB_Connection.csv")
#View(connections)

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
colnames(edgelistT)[2] <- "from_ID"
colnames(edgelistT)[3] <- "to_ID"
edgelist <- rbind(edgelist, edgelistT)
dim(edgelist)   # connections repeating both ways because we are dealing with an undirected network
#edgelist[relationship_type_name %in% c("Rival of", "Enemy of"), enemies := .N,from_ID]
#enemies <- edgelist[relationship_type_name %in% c("Rival of", "Enemy of"),.N,from_ID]
setkeyv(people, c("SDFB Person ID"))
setkeyv(enemies, "from_ID")
people2 <- merge(people, enemies, by.x = "SDFB Person ID", by.y = "from_ID", all.x = TRUE)
save <- edgelist

# Homophily on genger and social class (titled/religious/etc.)------------------------------------------------------
# (https://en.wikipedia.org/wiki/Network_homophily)
# Homophily = based on node attributes, similar nodes may be more likely to attach to each other than dissimilar ones
# Homophily in social relations may lead to a commensurate distance in networks leading to the creation of clusters that have been observed in social networking services 
# Homophily is a key topic in network science as it can determine the speed of the diffusion of information and ideas.
# Is number of cross-gender degrees <=> randomly expected cross-gender degrees
# Calculating the number of cross-gender degrees in a certain year
# If no homophily, then cross gender edges should be 2*(male/all nodes)*(female/all nodes)

# Evaluating the network of king Henry VIII (10012119) and his father king Henry VII (10012118)

# Getting the court of king Henry VII (1st and 2nd degree connections)
HVII_1degree <- edgelist[from_ID == 10012118, c("from_ID", "to_ID")]
HVII_2degree <- unique(as.data.frame(HVII_1degree[,to_ID]))
colnames(HVII_2degree) <- "from_ID"
to_ID <- unique(as.data.frame(edgelist[,c("from_ID","to_ID")]))
HVII_2degree <- merge(HVII_2degree, to_ID, by = "from_ID") # all.x = FALSE, allowing this to exclude ppl who do not have a further connection  
HVII_court <- unique(rbind(HVII_1degree, HVII_2degree)) # note that this does not include the reciprocated connection

# and the court of his son king Henry VIII (1st and 2nd degree connections)
HVII_1degree <- edgelist[from_ID == 10012119, c("from_ID", "to_ID")]
HVII_2degree <- unique(as.data.frame(HVII_1degree[,to_ID]))
colnames(HVII_2degree) <- "from_ID"
to_ID <- unique(as.data.frame(edgelist[,c("from_ID","to_ID")]))
HVII_2degree <- merge(HVII_2degree, to_ID, by = "from_ID") # all.x = FALSE, allowing this to exclude ppl who do not have a further connection  
HVIII_court <- unique(rbind(HVII_1degree, HVII_2degree)) # note that this does not include the reciprocated connection

# Should I be subsetting this to a certain year?

graph_HVII <- graph_from_data_frame(HVII_court, directed = FALSE)
graph_HVIII <- graph_from_data_frame(HVIII_court, directed = FALSE)
graph_HVII <- simplify(graph_HVII, remove.multiple = TRUE, remove.loops = TRUE)
graph_HVIII <- simplify(graph_HVIII, remove.multiple = TRUE, remove.loops = TRUE)
matrix_HVII <- as_adjacency_matrix(graph_HVII, sparse = FALSE)
matrix_HVIII <- as_adjacency_matrix(graph_HVIII, sparse = FALSE)
nodes_in_matrix_HVII <- as.data.frame(colnames(matrix_HVII, do.NULL = TRUE, prefix = "col"))
nodes_in_matrix_HVIII <- as.data.frame(colnames(matrix_HVIII, do.NULL = TRUE, prefix = "col"))
colnames(nodes_in_matrix_HVII) <- "SDFB Person ID"
colnames(nodes_in_matrix_HVIII) <- "SDFB Person ID"
gender_HVII <- merge(nodes_in_matrix_HVII, people[,c("SDFB Person ID", "Gender")]) 
gender_HVIII <- merge(nodes_in_matrix_HVIII, people[,c("SDFB Person ID", "Gender")]) 
gender_HVII$Gender[gender_HVII$Gender == "male"] <- 1
gender_HVII$Gender[gender_HVII$Gender == "female"] <- 0
gender_HVII$Gender <- as.numeric(gender_HVII$Gender)
sum(gender_HVII$Gender == 0)
# Henry VII had 12 women in his court, out of a court of 127
gender_HVIII$Gender[gender_HVIII$Gender == "male"] <- 1
gender_HVIII$Gender[gender_HVIII$Gender == "female"] <- 0
gender_HVIII$Gender <- as.numeric(gender_HVIII$Gender)
sum(gender_HVIII$Gender == 0)
# Henry VIII had 14 women, out of a court of 156
# checking for grouping based on gender
moran_HVII <- nacf(matrix_HVII, gender_HVII[,2], type = "moran")
# sum(moran_HVII)/127 = [1] 0.004940715
moran_HVIII <- nacf(matrix_HVIII, gender_HVIII[,2], type = "moran")
# > sum(moran_HVIII)/155 = [1] 0.005382852

# ERGM for their networks
network_HVII <- as.network.matrix(matrix_HVII, directed = F, loops = F)
network_HVII <- set.vertex.attribute(network_HVII, 'Gender', gender_HVII$Gender, v=seq_len(network.size(network_HVII))) # Were they introducts in the same order? 
model1 <- ergm(network_HVII ~ edges + triangles) #triangles creates a problem because of the sparcity of triangles
model1 <- ergm(network50y ~ edges)








# Looking at the entire network for a certain years (1550)
edgelist50y <- edgelist[start_year <= 1550 & end_year > 1550,]
graph50y <- graph_from_data_frame(edgelist50y[,2:3], directed = FALSE)
graph50y <- simplify(graph50y, remove.multiple = TRUE, remove.loops = TRUE)
matrix50y <- as_adjacency_matrix(graph50y, sparse = FALSE)
sum(matrix50y)
nodes_in_matrix <- as.data.frame(colnames(matrix50y, do.NULL = TRUE, prefix = "col"))
# nodes_in_matrix2 <- as.data.frame(V(graph50y)) - cannot coerse to data frame
colnames(nodes_in_matrix) <- "SDFB Person ID"
gender50y <- merge(nodes_in_matrix, people[,c("SDFB Person ID", "Gender")])
gender50y$Gender[gender50y$Gender == "male"] <- 1  
gender50y$Gender[gender50y$Gender == "female"] <- 0  
gender50y$Gender <- as.numeric(gender50y$Gender)
# Checking if grouping is present through Moran's I 
#- only descriptive, does not tell us whether that is due to homophily or another reason
nacf(matrix50y, gender50y[,2], type = "moran")

# End goal - evaluate the whole network for a window of the ruling of each king and the
# Compare agains the king's court (1st+2nd degree connections) for the same time window

# Get the list of monarch and perform the same calculation for only their court. Time window or both is the ruling(or life) of that monarch




# ERGM analysis of the same time window- again only descriptive
# add social class once Alex has completed that code
network50y <- as.network.matrix(matrix50y, directed = F, loops = F)
network50y <- set.vertex.attribute(network50y, 'Gender', gender50y$Gender, v=seq_len(network.size(network50y))) # Were they introducts in the same order? 
model1 <- ergm(network50y ~ edges + triangles) #triangles creates a problem because of the sparcity of triangles
model1 <- ergm(network50y ~ edges)
# Number of edges in a simulated network exceeds that in the observed by a factor of more than 20. This is a strong indicator of model degeneracy or a very poor starting parameter configuration. 
# Degeneracy in a linear programming problem is said to occur when a basic feasible solution contains a smaller number of non-zero variables than the number of independent constraints when values of some basic variables are zero and the Replacement ratio is same.
model2 <- ergm(network50y ~ edges + nodematch("Gender", diff = FALSE))
summary(model2)
summary(model1)

# Converting log odds to conditional probability of a tie forming when they are of the same gender
coef(model2)
plogis(coef(model2)[['edges']] + coef(model2)[['nodematch.Gender']])
0.005484331
# Probability of a tie forming when they are not of the same gender
plogis(coef(model2)[['edges']])
0.003873538



# Triadic closure of the entire network -----------------------------------
save <- edgelist
years <- sort(unique(save$start_year))
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  #Kin <- edgelist[,c(2,3)]
  #Kin = unique(Kin)
  KinExt <- edgelist[,c(2,3)]
  KinExt = unique(KinExt)
  # Kinship nodes + their 1st degrees
  #KinExt <- edgelist[edgelist$from_ID %in% Kin$from_ID,c(2,3)]
  #KinExt = unique(KinExt)
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

(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("Mean triadic closure") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Mean triadic closure of the global network over time") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )


# Triadic closure of categories excluding their first degree conne --------
save <- edgelist
categories <- unique(edgelist$relationship_category) 
years <- sort(unique(save$start_year))

#Kinship
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  KinExt <- edgelist[relationship_category == "Affective",c(2,3)]
  KinExt = unique(KinExt)
  # Kinship nodes + their 1st degrees
  #KinExt <- edgelist[edgelist$from_ID %in% Kin$from_ID,c(2,3)]
  #KinExt = unique(KinExt)
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
#closure[is.na(mean_closure),2] = 0
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Kinship category, only category members") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )




# Triadic closure of sub-networks -----------------------------------------
save <- edgelist
categories <- unique(edgelist$relationship_category) 
years <- sort(unique(save$start_year))

#Kinship
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Kinship",c(2,3)]
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
#closure[is.na(mean_closure),2] = 0
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Kinship category") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )
?geom_point
# Plotting the kinship Igraph

  edgelist <- save[start_year <= 1700 & end_year > 1700,]
  Kin <- edgelist[relationship_category == "Religious",c(2,3)]
  Kin = unique(Kin)
  # Kinship nodes + their 1st degrees
  KinExt <- edgelist[edgelist$from_ID %in% Kin$from_ID,c(2,3)]
  KinExt = unique(KinExt)
  gr <- graph.data.frame(KinExt,directed=FALSE)
  plot(gr, vertex.label = NA, vertex.size = 2, layout=layout_with_graphopt)

# "Affective"
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Affective",c(2,3)]
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
#closure[is.na(mean_closure),2] = 0
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Affective category") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )

# "Labor/Professional"
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Labor/Professional",c(2,3)]
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
#closure[is.na(mean_closure),2] = 0
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Labor & Professional category") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )

# "Religious" 
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Religious" ,c(2,3)]
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
#closure[is.na(mean_closure),2] = 0
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Religious category") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )

# Legal/Commercial
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Legal/Commercial" ,c(2,3)]
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
#closure[is.na(mean_closure),2] = 0
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Legal & Commercial category") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )

# Educational/Intellectual
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Educational/Intellectual" ,c(2,3)]
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
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Educational & Intellectual category") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )

# Political
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Political" ,c(2,3)]
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
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    ylim(0,1) +  
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Political category") +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) )

# Spatial
closure <- data.table(year = years, mean_closure = 0)
for (y in years) {
  edgelist <- save[start_year <= y & end_year > y,]
  Kin <- edgelist[relationship_category == "Spatial" ,c(2,3)]
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
(ggplot(closure, aes(x=year, y =mean_closure)) +
    geom_line() +
    theme_bw() +
    ylab("") +
    xlab("") +
    theme(plot.title = element_text(hjust = 0.5))+
    ggtitle("Spatial category") +
    theme(axis.line = element_line(colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()) )


