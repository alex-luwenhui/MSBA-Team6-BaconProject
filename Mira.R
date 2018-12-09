library(data.table)
library(ggplot2)
library(igraph)
library(ergm)
library(intergraph)
library(sna)

setwd("C:/Users/MJ/Documents/GitHub/MSBA-Team6-BaconProject")

groups <- fread("SDFB_groups.csv")
#View(groups)
people <- fread("SDFB_people.csv")
#View(people)
edgelist <- fread("Edgelist_with_ConnectionType.csv")
original_edgelist <- edgelist
#View(edgelist)
connections <- fread("SDFB_Connection.csv")
#View(connections)

# Calculating for every person the number of enemy relationships they have
length(edgelist$from_ID[edgelist$relationship_type_name %in% c("Rival of", "Enemy of")]) 
setkeyv(edgelist, c("relation_id", "from_ID", "to_ID"))
# Rbinding with the from - to swapped in order to have the symetrical relationships
edgelistT <- edgelist[,c(1,3,2,4,5,6,7,8,9,10)] # should I be swapping other columns here?
colnames(edgelistT)[2] <- "from_ID"
colnames(edgelistT)[3] <- "to_ID"
edgelist <- rbind(edgelist, edgelistT)
dim(edgelist)   # connections repeating both ways because we are dealing with an undirected network
#edgelist[relationship_type_name %in% c("Rival of", "Enemy of"), enemies := .N,from_ID]
#enemies <- edgelist[relationship_type_name %in% c("Rival of", "Enemy of"),.N,from_ID]
setkeyv(people, c("SDFB Person ID"))
#setkeyv(enemies, "from_ID")
#people2 <- merge(people, enemies, by.x = "SDFB Person ID", by.y = "from_ID", all.x = TRUE)
save <- edgelist

# Homophily on genger and social class (titled/religious/etc.)------------------------------------------------------
# (https://en.wikipedia.org/wiki/Network_homophily)
# Homophily = based on node attributes, similar nodes may be more likely to attach to each other than dissimilar ones
# Homophily in social relations may lead to a commensurate distance in networks leading to the creation of clusters that have been observed in social networking services 
# Homophily is a key topic in network science as it can determine the speed of the diffusion of information and ideas.
# Is number of cross-gender degrees <=> randomly expected cross-gender degrees
# Calculating the number of cross-gender degrees in a certain year
# If no homophily, then cross gender edges should be 2*(male/all nodes)*(female/all nodes)

# Evaluating the network of king Henry VIII (10012119) and his father king Henry VII (10012118) --------
# Getting the court of king Henry VII (1st and 2nd degree connections) --------
#limited to only connections which existed while the king was alive
birth_HVII <- as.numeric(people[people$`SDFB Person ID` == 10012118, "Extant Birth Year"][[1]])  #1457
death_HVII <- as.numeric(people[people$`SDFB Person ID` == 10012118, "Extant Death Year"][[1]]) #1509
df <- as.data.frame(edgelist)
edgelist_HVII_alive <- as.data.table(df[(df$start_year <= birth_HVII & df$end_year > birth_HVII & df$end_year <death_HVII) |
                    (df$start_year > birth_HVII & df$start_year < death_HVII & df$end_year <= death_HVII & df$end_year > birth_HVII) |
                    (df$start_year >= birth_HVII & df$start_year <= death_HVII & df$end_year > death_HVII),])
HVII_1degree <- edgelist_HVII_alive[from_ID == 10012118, c("from_ID", "to_ID")] 
HVII_2degree <- unique(as.data.frame(HVII_1degree[,to_ID])) #207
colnames(HVII_2degree) <- "from_ID"
to_ID <- unique(as.data.frame(edgelist_HVII_alive[,c("from_ID","to_ID")])) 
HVII_2degree <- merge(HVII_2degree, to_ID, by = "from_ID") # all.x = FALSE, allowing this to exclude ppl who do not have a further connection  
HVII_court <- unique(rbind(HVII_1degree, HVII_2degree)) # note that this does not include the reciprocated connection


# Getting the court of king Henry VIII (1st and 2nd degree connections --------
birth_HVIII <- as.numeric(people[people$`SDFB Person ID` == 10012119, "Extant Birth Year"][[1]])  #1491
death_HVIII <- as.numeric(people[people$`SDFB Person ID` == 10012119, "Extant Death Year"][[1]]) #1547
edgelist_HVIII_alive <- as.data.table(df[(df$start_year <= birth_HVIII & df$end_year > birth_HVIII & df$end_year <death_HVIII) |
                                          (df$start_year > birth_HVIII & df$start_year < death_HVIII & df$end_year <= death_HVIII & df$end_year > birth_HVIII) |
                                          (df$start_year >= birth_HVIII & df$start_year <= death_HVIII & df$end_year > death_HVIII),])
HVIII_1degree <- edgelist_HVIII_alive[from_ID == 10012119, c("from_ID", "to_ID")] 
HVIII_2degree <- unique(as.data.frame(HVIII_1degree[,to_ID])) 
colnames(HVIII_2degree) <- "from_ID"
to_ID <- unique(as.data.frame(edgelist_HVIII_alive[,c("from_ID","to_ID")])) 
HVIII_2degree <- merge(HVIII_2degree, to_ID, by = "from_ID") # all.x = FALSE, allowing this to exclude ppl who do not have a further connection  
HVIII_court <- unique(rbind(HVIII_1degree, HVIII_2degree)) # note that this does not include the reciprocated connection

# Mapping their networks
graph_HVII <- graph.data.frame(HVII_court, directed = FALSE)
graph_HVIII <- graph.data.frame(HVIII_court, directed = FALSE)
graph_HVII <- simplify(graph_HVII, remove.multiple = TRUE, remove.loops = TRUE)
graph_HVIII <- simplify(graph_HVIII, remove.multiple = TRUE, remove.loops = TRUE)
matrix_HVII <- as_adjacency_matrix(graph_HVII, sparse = FALSE) #1062
matrix_HVII <- matrix_HVII[, order(as.integer(colnames(matrix_HVII)))]  #1062 but now in adsc. order
matrix_HVIII <- as_adjacency_matrix(graph_HVIII, sparse = FALSE) #2470
matrix_HVIII <- matrix_HVIII[, order(as.integer(colnames(matrix_HVIII)))] #2470 but now in adsc. order
nodes_in_matrix_HVII <- as.data.frame(colnames(matrix_HVII, do.NULL = TRUE, prefix = "col")) # 1062, correctly adsc.
nodes_in_matrix_HVIII <- as.data.frame(colnames(matrix_HVIII, do.NULL = TRUE, prefix = "col")) # 2470, correctly adsc.
colnames(nodes_in_matrix_HVII) <- "SDFB Person ID"
colnames(nodes_in_matrix_HVIII) <- "SDFB Person ID"
gender_HVII <- merge(nodes_in_matrix_HVII, people[,c("SDFB Person ID", "Gender")]) 
gender_HVIII <- merge(nodes_in_matrix_HVIII, people[,c("SDFB Person ID", "Gender")]) 
gender_HVII$Gender[gender_HVII$Gender == "male"] <- 1
gender_HVII$Gender[gender_HVII$Gender == "female"] <- 0
gender_HVII$Gender <- as.numeric(gender_HVII$Gender)
sum(gender_HVII$Gender == 0)
# Henry VII had 64 women in his court, out of a court of 1062 (6.03%)
gender_HVIII$Gender[gender_HVIII$Gender == "male"] <- 1
gender_HVIII$Gender[gender_HVIII$Gender == "female"] <- 0
gender_HVIII$Gender <- as.numeric(gender_HVIII$Gender)
sum(gender_HVIII$Gender == 0)
# Henry VIII had 160 women, out of a court of 2470 (6.48%)
# checking for grouping based on gender
moran_HVII <- nacf(matrix_HVII, gender_HVII[,2], type = "moran")
# sum(moran_HVII)/   ???
moran_HVIII <- nacf(matrix_HVIII, gender_HVIII[,2], type = "moran")
# > sum(moran_HVIII)/ ???

# ERGM for the court of Henry VII -----------------------------------------------
network_HVII <- as.network.matrix(matrix_HVII, directed = F, loops = F)
network_HVII <- set.vertex.attribute(network_HVII, 'Gender', gender_HVII$Gender, v=seq_len(network.size(network_HVII)))  
#model1 <- ergm(network_HVII ~ edges + triangles) #triangles creates a problem because of the sparcity of triangles
#HVII_model1.1 <- ergm(network_HVII ~ edges)
#summary(HVII_model1.1)
#trying the more robust version of modeling triangles: the geometrically-weighed edgewise shared partner term (GWESP)
#HVII_model1.2 <- ergm(network_HVII ~ edges + gwesp(0.25,fixed=T))
#mcmc.diagnostics(HVII_model1.2)
#summary(HVII_model1.2)
# checking the goodness of fit
#gof_HVII_model1.2 <- gof(HVII_model1.2, GOF = ~model)
# p-values = 0, thus model1.2 models the data well and we can use it
# Converting log odds to conditional probability of a tie forming when it is closing a triangle
#coef(HVII_model1.2)
#plogis(coef(HVII_model1.2)[['edges']] + coef(HVII_model1.2)[['gwesp.fixed.0.25']])
# 0.01258359
#plogis(coef(HVII_model1.2)[['edges']])
# 0.002377116

#model2 - adds gender attribute
HVII_model2 <- ergm(network_HVII ~ edges + gwesp(0.25,fixed=T) + nodematch("Gender", diff = TRUE))
summary(HVII_model2)
# Converting log odds to conditional probability of a tie forming when they are both male
# probability of any random tie
plogis(coef(HVII_model2)[['edges']])
# 0.002521758
# Probability of a tie between males
plogis(coef(HVII_model2)[['edges']] + coef(HVII_model2)[['nodematch.Gender.1']])
# 0.002476469 - But Gender is not significant
# Probability of a tie if it closes a triangle
plogis(coef(HVII_model2)[['edges']] + coef(HVII_model2)[['gwesp.fixed.0.25']])
# 0.01286464
# Probability of a tie when it connects males and closes a triangle
plogis(coef(HVII_model2)[['edges']] + coef(HVII_model2)[['gwesp.fixed.0.25']] + coef(HVII_model2)[['nodematch.Gender.1']])
# 0.01263595

# ERGM for the court of Henry VIII ----------------------------------------
network_HVIII <- as.network.matrix(matrix_HVIII, directed = F, loops = F)
network_HVIII <- set.vertex.attribute(network_HVIII, 'Gender', gender_HVIII$Gender, v=seq_len(network.size(network_HVIII)))  
#model2 - adds gender attribute
HVIII_model2 <- ergm(network_HVIII ~ edges + gwesp(0.25,fixed=T) + nodematch("Gender", diff = TRUE))
summary(HVIII_model2)
# Converting log odds to conditional probability of a tie forming when they are both male
# probability of any random tie
plogis(coef(HVIII_model2)[['edges']])
# 0.005179749
# Probability of a tie between males
plogis(coef(HVIII_model2)[['edges']] + coef(HVIII_model2)[['nodematch.Gender.1']])
# 0.004039333
# Probability of a tie if it closes a triangle
plogis(coef(HVIII_model2)[['edges']] + coef(HVIII_model2)[['gwesp.fixed.0.25']])
# 0.01225444
# Probability of a tie when it connects males and closes a triangle
plogis(coef(HVIII_model2)[['edges']] + coef(HVIII_model2)[['gwesp.fixed.0.25']] + coef(HVIII_model2)[['nodematch.Gender.1']])
# 0.009571386




# Getting the the entire network cummulative for the years of life of Henry VII --------
birth_HVII <- as.numeric(people[people$`SDFB Person ID` == 10012118, "Extant Birth Year"][[1]])  #1457
#  people[people$`SDFB Person ID` == 10012118, "Extant Birth Year"] #1457
death_HVII <- as.numeric(people[people$`SDFB Person ID` == 10012118, "Extant Death Year"][[1]]) #1509
df <- as.data.frame(edgelist)
edgelist50y <- df[(df$start_year <= birth_HVII & df$end_year > birth_HVII & df$end_year <death_HVII) |
                    (df$start_year > birth_HVII & df$start_year < death_HVII & df$end_year <= death_HVII & df$end_year > birth_HVII) |
                    (df$start_year >= birth_HVII & df$start_year <= death_HVII & df$end_year > death_HVII),]
graph50y <- graph_from_data_frame(edgelist50y[,2:3], directed = FALSE)
graph50y <- simplify(graph50y, remove.multiple = TRUE, remove.loops = TRUE)
matrix50y <- as_adjacency_matrix(graph50y, sparse = FALSE)
sum(matrix50y) #16590 edges
length(unique(c(edgelist50y$from_ID, edgelist50y$to_ID))) #1551 nodes  Henry VII's court = 1062

nodes_in_matrix <- as.data.frame(colnames(matrix50y, do.NULL = TRUE, prefix = "col"))
colnames(nodes_in_matrix) <- "SDFB Person ID"
gender50y <- merge(nodes_in_matrix, people[,c("SDFB Person ID", "Gender")])
gender50y$Gender[gender50y$Gender == "male"] <- 1  
gender50y$Gender[gender50y$Gender == "female"] <- 0  
gender50y$Gender <- as.numeric(gender50y$Gender)
# Checking if grouping is present through Moran's I 
#- only descriptive, does not tell us whether that is due to homophily or another reason
nacf(matrix50y, gender50y[,2], type = "moran")

# ERGM for the entire community during the life of Henry VII -------------
network_HVII_50y <- as.network.matrix(matrix50y, directed = F, loops = F)
network_HVII_50y <- set.vertex.attribute(network_HVII_50y, 'Gender', gender50y$Gender, v=seq_len(network.size(network_HVII_50y)))  
HVII_50y_model2 <- ergm(network_HVII_50y ~ edges + gwesp(0.25,fixed=T) + nodematch("Gender", diff = TRUE))
summary(HVII_50y_model2)
# probability of any random tie
plogis(coef(HVII_50y_model2)[['edges']])
# 0.0002887832
# Probability of a tie between males
plogis(coef(HVII_50y_model2)[['edges']] + coef(HVII_model2)[['nodematch.Gender.1']])
# 0.0002835854 - But Gender is not significant
# Probability of a tie if it closes a triangle
plogis(coef(HVII_50y_model2)[['edges']] + coef(HVII_50y_model2)[['gwesp.fixed.0.25']])
# 0.005101579
# Probability of a tie when it connects males and closes a triangle
plogis(coef(HVII_50y_model2)[['edges']] + coef(HVII_50y_model2)[['gwesp.fixed.0.25']] + coef(HVII_50y_model2)[['nodematch.Gender.1']])
# 0.005270788

# Getting the the entire network cummulative for the years of life of Henry VIII --------
birth_HVIII <- as.numeric(people[people$`SDFB Person ID` == 10012119, "Extant Birth Year"][[1]])  #1457
#  people[people$`SDFB Person ID` == 10012118, "Extant Birth Year"] #1457
death_HVIII <- as.numeric(people[people$`SDFB Person ID` == 10012119, "Extant Death Year"][[1]]) #1509
df <- as.data.frame(edgelist)
edgelist50y <- df[(df$start_year <= birth_HVIII & df$end_year > birth_HVIII & df$end_year <death_HVIII) |
                    (df$start_year > birth_HVIII & df$start_year < death_HVIII & df$end_year <= death_HVIII & df$end_year > birth_HVIII) |
                    (df$start_year >= birth_HVIII & df$start_year <= death_HVIII & df$end_year > death_HVIII),]
graph50y <- graph_from_data_frame(edgelist50y[,2:3], directed = FALSE)
graph50y <- simplify(graph50y, remove.multiple = TRUE, remove.loops = TRUE)
matrix50y <- as_adjacency_matrix(graph50y, sparse = FALSE)
sum(matrix50y) #16590 edges
length(unique(c(edgelist50y$from_ID, edgelist50y$to_ID))) #1551 nodes  Henry VII's court = 1062

nodes_in_matrix <- as.data.frame(colnames(matrix50y, do.NULL = TRUE, prefix = "col"))
colnames(nodes_in_matrix) <- "SDFB Person ID"
gender50y <- merge(nodes_in_matrix, people[,c("SDFB Person ID", "Gender")])
gender50y$Gender[gender50y$Gender == "male"] <- 1  
gender50y$Gender[gender50y$Gender == "female"] <- 0  
gender50y$Gender <- as.numeric(gender50y$Gender)
# Checking if grouping is present through Moran's I 
#- only descriptive, does not tell us whether that is due to homophily or another reason
# nacf(matrix50y, gender50y[,2], type = "moran")  - too big to calculate

# ERGM for the entire community during the life of Henry VIII -------------
network_HVIII_50y <- as.network.matrix(matrix50y, directed = F, loops = F)
network_HVIII_50y <- set.vertex.attribute(network_HVIII_50y, 'Gender', gender50y$Gender, v=seq_len(network.size(network_HVIII_50y)))  
HVIII_50y_model2 <- ergm(network_HVIII_50y ~ edges + gwesp(0.25,fixed=T) + nodematch("Gender", diff = TRUE))
summary(HVIII_50y_model2)
# probability of any random tie
plogis(coef(HVIII_50y_model2)[['edges']])
# 0.0002735385
# Probability of a tie between males
plogis(coef(HVIII_50y_model2)[['edges']] + coef(HVIII_model2)[['nodematch.Gender.1']])
# 0.0002130826 - But Gender is not significant
# Probability of a tie if it closes a triangle
plogis(coef(HVIII_50y_model2)[['edges']] + coef(HVIII_50y_model2)[['gwesp.fixed.0.25']])
# 0.004947381
# Probability of a tie when it connects males and closes a triangle
plogis(coef(HVIII_50y_model2)[['edges']] + coef(HVIII_50y_model2)[['gwesp.fixed.0.25']] + coef(HVIII_50y_model2)[['nodematch.Gender.1']])
# 0.005138015
# End goal - evaluate the whole network for a window of the ruling of each king and the
# Compare agains the king's court (1st+2nd degree connections) for the same time window


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




save.image(file='ERGMs.RData')
