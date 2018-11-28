library(data.table)

# centralities over the years

mymatrix <- matrix(0, nrow = 386, ncol = 7)
i <- 1
largest_compo_perc <- c()
num_nodes_by_year <- c()
for (year in c(1400:1786)){
    edges = bacon[start_year <= year & end_year > year, .(from_ID, to_ID)]
    mygraph <- graph.data.frame(edges, directed = FALSE)
    
    cc <- centr_clo(mygraph, normalized = FALSE)
    bc <- centr_betw(mygraph, directed = FALSE, normalized = FALSE)
    ec <- centr_eigen(mygraph, directed = FALSE, normalized = FALSE)
    centralities = data.table(person = names(V(mygraph)), clo = cc$res, betw = bc$res, eige = ec$vector)

    mymatrix[i, ] <- c(year, cc$centralization, bc$centralization, ec$centralization,
                       toString(centralities[clo == max(cc$res), ]$person),
                       toString(centralities[betw == max(bc$res), ]$person),
                       toString(centralities[eige == max(ec$vector), ]$person))

    i <- i + 1
    
    #components = clusters(mygraph)
    #largest_compo_perc <- c(largest_compo_perc, max(components$csize)/vcount(mygraph))
    #num_nodes_by_year <- c(num_nodes_by_year, nrow(bacon[start_year <= year & end_year > year, ]))
}

write.csv(mymatrix, file = "centralities_by_year.csv", row.names = FALSE)

plot(c(1400:1786), largest_compo_perc, xlab = "Year")
plot(c(1400:1786), num_nodes_by_year, xlab = "Year")

# count how many times people appeared as the highest centrality man

highest_clo <- mymatrix[, 5]
unique_years <- c(1400:1786)
people <- c()
years <- c()
for (i in c(1:length(highest_clo))){
    this_year <- unique_years[i]
    str_people_this_year <- highest_clo[i]
    if (nchar(str_people_this_year) == 8){
        people <- c(people, str_people_this_year)
        years <- c(years, this_year)
    }
    else{
        people_this_year <- unlist(strsplit(str_people_this_year, ", "))
        people <- c(people, unlist(people_this_year))
        years <- c(years, rep(this_year, length(people_this_year)))
    }
}
highest_clo_dt = data.table(year = years, ID = people)
highest_clo_dt <- highest_clo_dt[, .N, by = ID]
highest_clo_dt <- highest_clo_dt[order(-N), ]
highest_clo_dt

highest_betw <- mymatrix[, 6]
people <- c()
years <- c()
for (i in c(1:length(highest_betw))){
  this_year <- unique_years[i]
  str_people_this_year <- highest_betw[i]
  if (nchar(str_people_this_year) == 8){
    people <- c(people, str_people_this_year)
    years <- c(years, this_year)
  }
  else{
    people_this_year <- unlist(strsplit(str_people_this_year, ", "))
    people <- c(people, unlist(people_this_year))
    years <- c(years, rep(this_year, length(people_this_year)))
  }
}
highest_betw_dt = data.table(year = years, ID = people)
highest_betw_dt <- highest_betw_dt[, .N, by = ID]
highest_betw_dt <- highest_betw_dt[order(-N), ]
highest_betw_dt

highest_eige <- mymatrix[, 7]
people <- c()
years <- c()
for (i in c(1:length(highest_eige))){
  this_year <- unique_years[i]
  str_people_this_year <- highest_eige[i]
  if (nchar(str_people_this_year) == 8){
    people <- c(people, str_people_this_year)
    years <- c(years, this_year)
  }
  else{
    people_this_year <- unlist(strsplit(str_people_this_year, ", "))
    people <- c(people, unlist(people_this_year))
    years <- c(years, rep(this_year, length(people_this_year)))
  }
}
highest_eige_dt = data.table(year = years, ID = people)
highest_eige_dt <- highest_eige_dt[, .N, by = ID]
highest_eige_dt <- highest_eige_dt[order(-N), ]
highest_eige_dt

plot()

temp = fread(file = "SDFB_people.csv")
temp[`SDFB Person ID` == "10011681", ]
