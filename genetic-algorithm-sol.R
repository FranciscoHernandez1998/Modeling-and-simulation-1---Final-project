library(dplyr)

generate_chromosome <-function(weights){
  weight   <- 0
  pool     <- weights
  pool_idx <- 1:length(pool)
  selected <- c()
  for(i in 1:length(pool)){
    value_idx <- sample(pool_idx,1)
    pool_idx  <- setdiff(pool_idx,value_idx)
    weight    <- weight+pool[value_idx]
    if(weight > max_weight) break
    selected  <- c(selected,value_idx)
  }
  out <- rep(0,length(pool) )
  out[selected] <- 1
  return(out)
}

generate_population <- function (N, weights_bids) {
  population <- t(sapply(1:N,function(x,w){generate_chromosome(w)},w=weights_bids[,1] ))
  return(population)
}

# crossover <- function(parents, weights_bids) {
#   p1 <- as.numeric(parents[1,])
#   p2 <- as.numeric(parents[2,])
#   (rcut <- which(cumsum(p1*weights_bids[,1])>max_weight/2)[1])
#   if(!is.na(rcut)){
#     (lcut <- rcut-1)
#     proto_child <- c(p1[1:lcut],p2[rcut:20])
#   } else {
#     (rcut <- which(cumsum(p2*weights_bids[,1])>max_weight/2)[1])
#     (lcut <- rcut-1)
#     print(rcut)
#     print(lcut)
#     proto_child <- c(p2[1:lcut],p1[rcut:20])
#   }
#   remove_weight <- which(cumsum(proto_child*weights_bids[,1])>max_weight)
#   proto_child[remove_weight] <- 0
#   child <- proto_child
#     # print(p1)
#     # print(p2)
#     # print(child)
#   return(child)
# }

crossover <- function(parents,n, weights_bids){
  parent_1 <- as.numeric(parents[1,])
  parent_2 <- as.numeric(parents[2,])
  child <- rep(0,length(parent_1))
  genes_parent_1 <- sample(1:length(parent_1),n,replace=FALSE)
  child[genes_parent_1] <- parent_1[genes_parent_1]
  child[-genes_parent_1] <- setdiff(parent_2,parent_1[genes_parent_1])
  remove_weight <- which(cumsum(child*weights_bids[,1])>max_weight)
  child[remove_weight] <- 0
  return(child)
}

make_mutation <- function(child, weights_bids) {
  child <- child[1:(length(child))]
  index<-sample(1:length(child),2)
  i <- child[index[1]]
  child[index[1]] <- child[index[2]]
  child[index[2]] <- i
  child <- c(child)
  remove_weight <- which(cumsum(child*weights_bids[,1])>max_weight)
  child[remove_weight] <- 0
  return(child)
}

fitness <- function(population, N, weights_bids){
  population <- cbind(population, population %*% weights_bids[,2])
  return(population)
}

selection <- function(population,K){
  players_index <- sample(1:nrow(population),size = K,replace = FALSE)
  players <- population[players_index,'fit']
  winner_index <- players_index[which.max(players)[1]]
  player_1 <- as.numeric(population[winner_index,-ncol(population)])
  indx <- setdiff(1:nrow(population),players_index[winner_index])
  players_index <- sample(indx,size = K,replace = FALSE)
  players <- population[players_index,'fit']
  winner_index <- players_index[which.max(players)[1]]
  player_2 <- as.numeric(population[winner_index,-ncol(population)])
  return(rbind(player_1,player_2))
}

airline_company_genetic_algorithm <- function(max_weight, package_list, N, n) {
  weights_bids <- matrix(package_list, ncol = 2, byrow = TRUE) 
  colnames(weights_bids) <- c('weight','bids')
  head(weights_bids)
  
  population <- generate_population(N, weights_bids)
  for(generation in 1:n){
    population <- fitness(population, N, weights_bids)
    population <- data.frame(population)
    names(population)[names(population) == 'X21'] <- 'fit'
    # print(generation)
    # print(max(population$fit))
    if(generation == n) {
      selected_packages <- population[2,]
      packages_to_send <- c()
      selected_chromosome <- c()
      for(i in 1:length(selected_packages)) {
        if((selected_packages[[i]]) == 1) {
          packages_to_send <- c(packages_to_send, i)
          selected_chromosome <- c(selected_chromosome, 1)
        } else if ((selected_packages[[i]]) == 0) {
          selected_chromosome <- c(selected_chromosome, 0)
        }
      }
      # print(selected_chromosome)
      print(paste('To earn $.', max(population$fit)))
      print('The packages to send are: ')
      print(packages_to_send)
      print('With a total weight in kg of ')
      print(sum(selected_chromosome*weights_bids[,1]))
    }
    new_population <- list()
    for (i in 1:n) {
      parents <- selection(population,5)
      child<-crossover(parents, 20, weights_bids)
      if(runif(1) <= 0.01){
        child <- make_mutation(child, weights_bids)
      }
      new_population <- append(new_population,list(child))
    }
    population <- matrix(unlist(new_population),ncol = 20,byrow = TRUE )
  }
}
