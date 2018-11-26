library(data.table)

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
unique(edgelist$relationship_type_name[edgelist$relationship_category ==  "Religious"])
# Spatial looks ok, includes living together, neighbours, attended
# Political looks ok, includes clients and partrons of
# "Educational/Intellectual" - schoolmates and correspondents
# "Legal/Commercial" - spouses and in-laws; employment
unique(people$`Historical Significance`)  # Too many (~1400) different ones

# Calculating for every person the number of enemy relationships they have
length(edgelist$from_ID[edgelist$relationship_type_name %in% c("Rival of", "Enemy of")]) 
 







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