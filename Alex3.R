library(data.table)

bacon = fread(file = 'Edgelist_with_ConnectionType.csv', header = TRUE)
people = fread(file = 'SDFB_people.csv', header = TRUE)

# change empty strings to NA
people[Title=='',Title:=NA]
people[Prefix=='',Prefix:=NA]

# Find king ID and assign title
king_ids <- people[grepl('king',people[,`Historical Significance`],ignore.case=T),]
king_ids <- king_ids[,c('SDFB Person ID')]
king_ids <- king_ids[-(c(4, 14, 15, 16, 18, 19, 20, 21, 23, 24)),] # Remove kings we don't care about
people[`SDFB Person ID` %in% king_ids, Title:='King']

# Find church affiliates and assign title
people[`SDFB Person ID` %in% people[grepl('bishop',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='church']
people[`SDFB Person ID` %in% people[grepl('church',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='church']
people[`SDFB Person ID` %in% people[grepl('minister',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='church']
people[`SDFB Person ID` %in% people[grepl('prime minister',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:=NA]
people[`SDFB Person ID` %in% people[grepl('clergyman',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='church']
people[`SDFB Person ID` %in% people[grepl('priest',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='church']
people[`SDFB Person ID` %in% people[grepl('preacher',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='church']
people[`SDFB Person ID` %in% people[grepl('Jesuit',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='church']

# try to fill title with Prefix if title doesnt exist
people[is.na(Prefix)==0 & is.na(Title)==1, Title:=Prefix]

# assign "no title" to people with no title or prefix
people[, level:='Significant'] # initiate level column
people[is.na(Title)==1, level:='No Title']

for (i in c('church','bishop', 'Br.', 'Cardinal', 'Rev')){
  people[grepl(i,people[,Title],ignore.case=T),level:='Religion']
}
for (i in c('Duke','Duchess','Ducchess','Marquess','Marchioness','Earl','Count','Countess','Viscount','Viscountess','Baron','Baroness','baronet','Lord','Lady','Elector','Electress')){
  people[grepl(i,people[,Title],ignore.case=T),level:='Noble']
}
for (i in c('Nobleman','Noblewoman')){
  people[grepl(i,people[,`Historical Significance`],ignore.case=T),level:='Noble']
}
people[`SDFB Person ID` %in% people[grepl('clan',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], Title:='Nobel']
for (i in c('King','Queen','Prince','Princess')){
  people[grepl(i,people[,Title],ignore.case=T),level:='Royal']
}
people[`SDFB Person ID` %in% people[grepl('courtier',people[,`Historical Significance`],ignore.case=T),`SDFB Person ID`], level:='Significant']

people[,`SDFB Person ID`:=as.character(`SDFB Person ID`)]
people[is.na(level)==0,.N,level]


for (i in c('Officer','Commander','Soldier')){
  people[grepl(i,people[,`Historical Significance`],ignore.case=T), military:=1]
}
for (i in c('Art','Sing','Paint','Actor','Actress','violin','Poet','music','danc','play','writ','author','compos','architect','sculptor','novel','designer','organ','theatre')){
  people[grepl(i,people[,`Historical Significance`],ignore.case=T), art:=1]
}
for (i in c('law','judge','legal','magistrate')){
  people[grepl(i,people[,`Historical Significance`],ignore.case=T), law:=1]
}
for (i in c('politic','diplom','govern','prime','admin','parliam','House of Commons')){
  people[grepl(i,people[,`Historical Significance`],ignore.case=T), politics:=1]
}

people[is.na(military), military := 0]
people[is.na(art), art := 0]
people[is.na(law), law := 0]
people[is.na(politics), politics := 0]

write.csv(people, file = "people_with_professions.csv", row.names = FALSE)
