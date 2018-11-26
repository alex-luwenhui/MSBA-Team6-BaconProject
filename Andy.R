# -------------------------------------------------------------
setwd('D:/MSBA/Social Network/GP/six-degrees-of-francis-bacon')
library(data.table)
library(igraph)
library(nnet)
groups <- fread('SDFB_groups.csv')
people <- fread('SDFB_people.csv')
relationtype <- fread('SDFB_RelationshipTypes.csv')
relationship <- fread('SDFB_relationships_100000000_100020000.csv')
filename <- c('SDFB_relationships_100020001_100040000.csv','SDFB_relationships_100040001_100060000.csv',
              'SDFB_relationships_100060001_100080000.csv','SDFB_relationships_100080001_100100000.csv',
              'SDFB_relationships_100100001_100120000.csv','SDFB_relationships_100120001_100140000.csv',
              'SDFB_relationships_100140001_100160000.csv','SDFB_relationships_100160001_100180000.csv',
              'SDFB_relationships_greater_than_100180000.csv')
connections <- fread('SDFB_Connection.csv')
edgelist <- fread("Edgelist_with_ConnectionType.csv")
for (i in filename){
  temp <- fread(i)
  relationship <- rbind(relationship,temp)
  rm(temp)
}
# -------------------------------------------------------------
people[Title=='',Title:=NA]
people[Prefix=='',Prefix:=NA]
# Fund king ID and assign title
king_ids <- people[grepl('king',people[,`Historical Significance`],ignore.case=T),]
king_ids <- king_ids[,c('SDFB Person ID')]
# Remove row 4, 14, 15, 16, 18, 19, 20, 21, 23, 24
king_ids <- king_ids[-(c(4, 14, 15, 16, 18, 19, 20, 21, 23, 24)),]
people[`SDFB Person ID` %in% king_ids,Title:='King']
people[`SDFB Person ID` %in% people[grepl('bishop',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`],
       Title:='church']
people[`SDFB Person ID` %in% people[grepl('church',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`],
       Title:='church']
#titlelist <- c('Duke','Duchess','Ducchess','Marquess','Marchioness','Earl','Count','Countess',
#               'Viscount','Viscountess','baronet','Baronet','Baroness','Baron','King','Queen','Prince','Princess')
people[is.na(Prefix)==0&is.na(Title)==1,Title:=Prefix]
people[,level:='Normal']
people[is.na(Title)==1,level:='No Title']
for (i in c('church','bishop')){
  people[grepl(i,people[,Title],ignore.case=T),level:='Religion']
}
for (i in c('Duke','Duchess','Ducchess','Marquess','Marchioness','Earl','Count','Countess','Viscount','Viscountess','Baron')){
  people[grepl(i,people[,Title],ignore.case=T),level:='Titled']
}
for (i in c('King','Queen','Prince','Princess')){
  people[grepl(i,people[,Title],ignore.case=T),level:='Royal']
}
people[,`SDFB Person ID`:=as.character(`SDFB Person ID`)]
people[is.na(level)==0,.N,level]
# -------------------------------------------------------------
# abnormal data
people[!(Gender %in% c('male','female')),Gender:=NA]
relationship <- relationship[`Start Year`!=9] 
people[grepl('/',people$`Extant Birth Year`)|grepl('/',people$`Extant Death Year`),
       .(`SDFB Person ID`,`Extant Birth Year`,`Extant Death Year`)]
people[`SDFB Person ID`=='10054709',`Extant Death Year`:='1710']
people[`SDFB Person ID`=='10054747',`Extant Death Year`:='1738']
people[`SDFB Person ID`=='10054779',c('Extant Birth Year','Extant Death Year'):=list('1715','1785')]
people[`SDFB Person ID`=='10054783',c('Extant Birth Year','Extant Death Year'):=list('1693','1750')]

people[complete.cases(people[,.(`Extant Birth Year`,`Extant Death Year`)]),
       lifespan:= as.integer(`Extant Death Year`) - as.integer(`Extant Birth Year`)+1]

relationship[,length:=`End Year`-`Start Year`+1]
relationlength <- rbind(relationship[,.(`Person 1 ID`,length)],relationship[,.('Person 1 ID'=`Person 2 ID`,length)])
relationlength[,`Person 1 ID`:=as.character(`Person 1 ID`)]
people <- merge(people,relationlength[,mean(length),`Person 1 ID`],by.x = 'SDFB Person ID',by.y = 'Person 1 ID',all.x = T)
setnames(people,'V1','rlength')
people[,ratio:=rlength/lifespan]
# -------------------------------------------------------------
g <- graph_from_data_frame(relationship[,.(`Person 1 ID`,`Person 2 ID`)], directed = F)
people <- merge(people,data.table('SDFB Person ID'=names(degree(g)),deg=unlist(degree(g))),all.x = T)
people <- merge(people,data.table('SDFB Person ID'=names(betweenness(g)),bet=unlist(betweenness(g))),all.x = T)
people <- merge(people,data.table('SDFB Person ID'=names(closeness(g)),clo=unlist(closeness(g))),all.x = T)
people <- merge(people,data.table('SDFB Person ID'=names(eigen_centrality(g)$`vector`),ec=unlist(eigen_centrality(g)$`vector`)),all.x = T)
people <- merge(people,data.table('SDFB Person ID'=names(page_rank(g)$`vector`),pr=unlist(page_rank(g)$`vector`)),all.x = T)

enemyn <- rbind(edgelist[,.('SDFB Person ID'=from_ID,relationship_type_name)],edgelist[,.('SDFB Person ID'=to_ID,relationship_type_name)])
enemyn[relationship_type_name %in% c("Rival of", "Enemy of"), .N,`SDFB Person ID`]
enemyn[,`SDFB Person ID`:=as.character(`SDFB Person ID`)]
people <- merge(people,enemyn[relationship_type_name %in% c("Rival of", "Enemy of"), .N,`SDFB Person ID`],all.x = T)
setnames(people,'N','enemies')
people[is.na(enemies)==1,enemies:=0]


model1 <- lm(lifespan ~  bet + level + Gender + ratio + enemies, data = people)
summary(model1)
# -------------------------------------------------------------

outedges <- relationship[`Original Confidence`>=60,.(`Person 1 ID`,`Person 2 ID`,`Original Confidence`,`Start Year`,`End Year`)]
setnames(outedges,
         c('Person 1 ID','Person 2 ID'),
         c('Source','Target'))
outedges <- outedges[Source!=10050190]
write.csv(outedges,'edgelist.csv',row.names = F)
outnodes <- data.table(ID=unique(c(outedges$Source,outedges$Target)))
outnodes <- merge(outnodes,people[,.(as.integer(`SDFB Person ID`),`Display Name`,Gender,level,lifespan,`Extant Birth Year`,`Extant Death Year`)],
                      by.x = 'ID',
                      by.y = 'V1',
                      all.x = T)
write.csv(outnodes,'nodelist.csv',row.names = F)
str(outedges)
str(outnodes)

people[grepl('king',people$`Historical Significance`),.(`SDFB Person ID`,`Historical Significance`)]