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

initial_packages <- function (weights_bids) {
  packages <- t(sapply(1:1,function(x,w){generate_chromosome(w)},w=weights_bids[,1] ))
  return(packages)
}

rnd_neighbord <- function(child, weights_bids) {
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

calculate_payment <- function(packages, weights_bids) {
  return(packages %*% weights_bids[,2])
}


airline_company_simulated_annealing <- function (max_weight, package_list, N, n, temp, alpha) {
  weights_bids <- matrix(package_list, ncol = 2, byrow = TRUE) 
  colnames(weights_bids) <- c('weight','bids')
  head(weights_bids)
  packages <- initial_packages(weights_bids)
  payment <- calculate_payment(packages, weights_bids)
  temp_min = 0.001
  while(temp > temp_min){
    i <- 1
    # print(temp)
    while(i <= 100){
      new_packages <- rnd_neighbord(packages, weights_bids)
      new_payment <- calculate_payment(new_packages, weights_bids)
      acceptance_probability <- exp((payment-new_payment)/temp)
      if(acceptance_probability < runif(1) ){
        packages <- new_packages
        payment <- new_payment
      } else if(new_payment > payment) {
        payment <- new_payment
        packages <- new_packages
      }
      i <- i+1
    }
    temp <- temp*alpha
  }
  packages_to_send <- c()
  selected_chromosome <- c()
  for(i in 1:length(packages)) {
    if((packages[[i]]) == 1) {
      packages_to_send <- c(packages_to_send, i)
      selected_chromosome <- c(selected_chromosome, 1)
    } else if ((packages[[i]]) == 0) {
      selected_chromosome <- c(selected_chromosome, 0)
    }
  }
  # print(selected_chromosome)
  print(paste('To earn $.', payment))
  print('The packages to send are: ')
  print(packages_to_send)
  print('With a total weight in kg of ')
  print(sum(selected_chromosome*weights_bids[,1]))
}
