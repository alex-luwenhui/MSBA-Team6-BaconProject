library(data.table)

# centralities over the years

c(min(bacon$start_year), max(bacon$end_year))
mymatrix <- matrix(0, nrow = 386, ncol = 7)
i <- 1
for (year in c(1701:1786)){
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
}

write.csv(mymatrix, file = "centralities_by_year.csv", row.names = FALSE)
