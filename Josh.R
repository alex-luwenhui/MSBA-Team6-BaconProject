install.packages('psych')
library(igraph)
library(data.table)
library(psych)

setwd('C:/Users/joshw/OneDrive - Emory University/Academic Work/Network Analytics/Group Project/Relationships')
files <- list.files(pattern="*.csv")
dt_relation <- rbindlist(lapply(files, fread))

setwd('C:/Users/joshw/OneDrive - Emory University/Academic Work/Network Analytics/Group Project')

group <- read.csv('SDFB_groups.csv', stringsAsFactors = FALSE)
people <- read.csv('SDFB_people.csv', stringsAsFactors = FALSE)
type <- read.csv('SDFB_RelationshipTypes.csv', stringsAsFactors = FALSE)
connection <- read.csv('SDFB_Connection.csv', stringsAsFactors = FALSE)

head(connection)
connection <- connection[c(2,3,4,5,6,8,10)]

dt_relation <- dt_relation[,c(1,2,3,5,9,13)]
colnames(dt_relation) <- c('relation_id','from_ID',"to_ID",'weight',
                           'start_year','end_year')

# Remove relationships that are unlikely to exist
dt_relation <- dt_relation[weight > 20,]

# Remove data that are problematic
dt_relation <- dt_relation[!(start_year == 9),]

# Reverse the relationship years that appear to be wrong
dt_inverseyr <- dt_relation[(end_year  < start_year),]
dt_inverseyr <- setcolorder(dt_inverseyr, c(1,2,3,4,6,5))
colnames(dt_inverseyr)[5:6] <- c('start_year','end_year')
dt_relation[(end_year  < start_year),] <- dt_inverseyr

# Merge relatino type data with edge list
colnames(connection)
nrow(dt_fullinfo)
dt_fullinfo <- merge(dt_relation, connection, by.x='relation_id', by.y='relationship_id')

write.csv(dt_fullinfo, 'Edgelist_with_ConnectionType.csv')

# Check some relation have several types
dt_relation[2,]
connection[connection$relationship_id == 100016003,]
nrow(connection)
length(unique(connection[,1]))
nrow(unique(dt_relation[,1]))

head(dt_relation)

# Create network for the entire graph (not recommended)
network_total <- graph_from_data_frame(dt_relation[,c('from_ID','to_ID','weight')], directed = FALSE)
network_total <- simplify(network_total, remove.multiple = TRUE, remove.loops = TRUE)

firstyr <- min(dt_relation[,'start_year'])
lastyr <- max(dt_relation[,'end_year'])

summary(dt_relation)
plot(network_total, vertex.label=NA, vertex.size=5)

# dplyr::filter(people, grepl("king",people$Historical.Significance))
king_ids <- people[which(grepl('king',people$Historical.Significance,ignore.case=TRUE)),]
king_ids <- king_ids[,c('SDFB.Person.ID','Display.Name','Historical.Significance','Extant.Death.Year')]
king_ids$Extant.Death.Year <- as.numeric(king_ids$Extant.Death.Year)
# Remove row 4, 14, 15, 16, 18, 19, 20, 21, 23, 24
king_ids <- king_ids[-(c(4, 14, 15, 16, 18, 19, 20, 21, 23, 24)),]

queen_ids <- people[which(grepl('queen',people$Historical.Significance,ignore.case=TRUE)),]
queen_ids <- queen_ids[,c('SDFB.Person.ID','Display.Name','Historical.Significance','Extant.Death.Year')]
queen_ids$Extant.Death.Year <- as.numeric(queen_ids$Extant.Death.Year)
# Remove queen 11, 13, 14, 15, 17, 20, 24, 27, 30, 31, 32, 36, 37, 38
queen_ids <- queen_ids[-c(11, 13, 14, 15, 17, 20, 24, 27, 30, 31, 32, 36, 37, 38),]


# Death of Royal Family Affect Relationships based on neighbors -------------------------------

# Create the network when King was alive
# Identify a king
this_king <- as.character(king_ids[2,1])
this_year <- king_ids[2,4]
year_range <- seq(this_year-5,this_year+5)

for(i in 1:length(year_range)) {
  year <- year_range[i]
  this_king <- 
  # Subset data by relationships existed when the king was alive
  relation <- dt_relation[which(end_year >= year & start_year < year),]
  
  # Create the network
  network_this <- graph_from_data_frame(relation[,c('from_ID','to_ID')], directed = FALSE)
  network_this <- simplify(network_this, remove.multiple = TRUE, remove.loops = TRUE)
  
  # Find the neighbors of the king
  adjacent <- neighbors(network_this, this_king)
  
  # Calculate the closeness centrality of all those neighbors
  close_neighbor <- closeness(network_this, vids=adjacent)
  avg_close <- mean(close_old)
  avg_close
  
}