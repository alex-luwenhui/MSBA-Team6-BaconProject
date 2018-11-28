library(data.table)
library(ggplot2)

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
edgelist <- rbind(edgelist, edgelistT)
dim(edgelist)
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


