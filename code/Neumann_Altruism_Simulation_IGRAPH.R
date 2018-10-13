## To-do list:

# Make sure that simulation actually has 100 generations

######

library('igraph') #create random networks
library('arm') #invlogit()

#library('profvis') #only needed for performance profiling
#profvis({ #profile code

##Define Functions

#function to find all the neighbors of players
findneighbor <- function(graph, player){
  as.integer(ego(graph, order = 1, nodes = player, mindist = 1)[[1]])
}



#same as above, but randomly sample one neighbor
find1neighbor <- function(graph, player){
  sample(as.integer(ego(graph, order = 1, nodes = player, mindist = 1)[[1]]), 1)
}

#given a vector (of neighbors), retain each in the vector with a probability of 'prob'
#vector a, probability prob
retain <- function(a, prob){
  a[as.logical(rbinom(length(a), T, prob))]
}

#spread the image score player 1 has earned through his actions
#to player2
#to any neighbors of player 2 with a 50% chance for each
#and to any neighbors of those neighbors with a 25% chance for each
spread <- function(graph, player1, player2){
  
  #1. Player 2 definitely receives the information
  #2. The neighbors of player 2 receive the information with 50% probability
  #find all the neighbors of player 2 except player 1
  player2_selected_neighbors <- findneighbor(graph, player2)
  player2_selected_neighbors <- player2_selected_neighbors[player2_selected_neighbors!=player1]
  player2_selected_neighbors <- unlist(lapply(player2_selected_neighbors, retain, prob = 0.5))
  #3. The neighbors of the neighbors who received the information
  #   receive the information with 25% probability
  #   If a player has 2 connections to direct neighbors of player 1, they have 2 chances to receive the information
  #find all the neighbors of the selected neighbors
  player2_neighbors_of_neighbors <- lapply(player2_selected_neighbors, findneighbor, graph = g)
  #retain each with a 25% probability
  player2_selected_neighbors_of_neighbors <- lapply(player2_neighbors_of_neighbors, retain, prob = 0.25)
  player2_selected_neighbors_of_neighbors <- unique(unlist(player2_selected_neighbors_of_neighbors))
  #remove player one
  player2_selected_neighbors_of_neighbors <- player2_selected_neighbors_of_neighbors[player2_selected_neighbors_of_neighbors!=player1]
  
  #all the players who receive the information
  selected_players <- unique(c(player2,player2_selected_neighbors,player2_selected_neighbors_of_neighbors))
  
  return(selected_players)
}


###### Code for a simulation begins here

set.seed(1)

##Parameters
#nsim <- 1 #number of simulations #default is 100

#N <- 300 #default is 300
#the exists allows this value to be set in another script
#and uses this one if that has not been done
if(exists("generations")==F){
  generations <- 100 #default is 100
}
if(exists("roundsPerGen")==F){
roundsPerGen <- 10 #default is 10
}

donation.img.gain <- 1 #default is 1
donation.img.loss <- -1 #default is -1
donation.fit.gain <- 2 #default is 2
donation.fit.loss <- -1 #default is -1

#network_type <- "WS" #set to BA or WS
#BA.power <- 2  #default is -2
#BA.m <- 5  #default is 5
#WS.nei <- 5  #default is 5
#WS.p <- 0.1  #default is 0.1


###Setting up

results_sims <- data.frame(matrix(0, nrow = generations*nsim, ncol = 12))
names(results_sims) <- c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6")

for(k in 1:nsim){
  
  #vary the following as desired
  
  #N <- seq(100,1000,length.out=nsim)[k]
  #BA.power <- seq(-2,2,length.out=nsim)[k]
  #BA.m <- seq(1, 10, length.out = nsim)[k]
  #WS.nei <- seq(1,10,length.out=nsim)[k]
  #WS.p <- seq(0.05,0.5,length.out=nsim)[k]
  
  #Depending on which parameter (if any) is set to be varied across multiple simulations
  #set the parameter for this particular sim
  
  if(vary=="p"){
    WS.p <- WS.p.seq[k]
  }
  if(vary=="nei"){
    WS.nei <- WS.nei.seq[k]
  }
  if(vary=="power"){
    BA.power <- BA.power.seq[k]
  }
  if(vary=="m"){
    BA.m <- BA.m.seq[k]
  }
  
  # creating a Barabasi-Albert network using the igraph package
  if (network_type=="BA") {
    g <- ba.game(N, power = BA.power, m = BA.m, directed = F)
  }
  
  # creating a Watts-Strogatz network using the igraph package
  if (network_type=="WS") {
    g <- watts.strogatz.game(dim = 1, size = N, nei = WS.nei, p = WS.p)
    while(any(degree(g)==0)){g <- watts.strogatz.game(dim = 1, size = N, nei = WS.nei, p = WS.p)}
  }
  
  # creating a results data frame which indicates
  # how well each strategy has done in each generation
  results <- data.frame(matrix(0, nrow = generations, ncol = 12))
  names(results) <- c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6")
  
  # now every individual chooses a strategy at random
  V(g)$strategy <- sample(c(-5:6), size=length(V(g)), replace=T) #randomly assign strategies, i.e. k, between -5 (always cooperate) and 6 (never cooperate)
  # everyone starts with fitness 0
  V(g)$fitness <- 0
  
  # store the strategy and fitness of every player in a data frame
  attributes <- data.frame(V(g)$strategy,V(g)$fitness)
  names(attributes) <- c("strategy","fitness")
  
  #get the adjacency matrix of the random network
  #and turn it into an actual matrix object
  #N*N sized adjacency matrix
  #m <- as.matrix(as_adjacency_matrix(g))
  #m_e <- as_edgelist(g)
  # keep m as the adjacency matrix
  # and create a new matrix img of the same dimensions as m
  # to hold the image scores
  #N*N matrix
  #img <- m
  #img[,] <- 0
  img <- matrix(0, length(V(g)), length(V(g)))
  
  
  ###The actual simulation begins here
  
  #Every simulation consists of a number (by default 100) of generations
  for(j in 1:generations){
    
    #Each generation plays the donation game a number (by default 10) of times
    for(i in 1:roundsPerGen){
      
      #create an edgelist
      #this data frame stores all pertinent information
      edgelist <- data.frame(c(1:N),rep(0,N),attributes$strategy,rep(0,N),rep(0,N),attributes$fitness,rep(0,N))
      names(edgelist) <- c("Player1","Player2","StrategyP1","ImageP1ofP2","ImageP2ofP1","FitnessP1","FitnessP2")
      #for every player1, randomly find and assign a neighbor as player 2
      edgelist$Player2 <- unlist(lapply(edgelist$Player1, find1neighbor, graph = g))
      
      #get the fitness of P2 from attributes data frame
      #abd put it in the edgelist data frame
      edgelist$FitnessP2 <- attributes$fitness[edgelist$Player2]
      
      #get the opinion P1 has of P2 from the image matrix and put it in the edgelist data frame
      #the cbind command allows me to get specific row/column pairs from the matrix
      edgelist$ImageP1ofP2 <- img[cbind(edgelist$Player1,edgelist$Player2)]
      #get the opinion P2 has of P1 from the image matrix and put it in the edgelist data frame
      edgelist$ImageP2ofP1 <- img[cbind(edgelist$Player2,edgelist$Player1)]
      
      #this is where the actual donation game takes place
      outcome <- matrix(0, nrow=nrow(edgelist), ncol=3) #matrix to store outcomes in
      #P1 defects if the strategy of P1 > image of P2 with P1:
      outcome[edgelist$StrategyP1>edgelist$ImageP1ofP2,1] <- donation.img.loss #P1 incurs image loss with P2
      outcome[edgelist$StrategyP1>edgelist$ImageP1ofP2,2] <- 0 #no fitness change to P1
      outcome[edgelist$StrategyP1>edgelist$ImageP1ofP2,3] <- 0 #no fitness change to P2
      #P1 helps if the strategy of P1 <= image of P2 with P1:
      outcome[edgelist$StrategyP1<=edgelist$ImageP1ofP2,1] <- donation.img.gain #P1 incurs image gain with P2
      outcome[edgelist$StrategyP1<=edgelist$ImageP1ofP2,2] <- donation.fit.loss #lower fitness of P1
      outcome[edgelist$StrategyP1<=edgelist$ImageP1ofP2,3] <- donation.fit.gain #raise fitness of P2
      
      
      #append outcome to edgelist
      outcome <- data.frame(outcome)
      names(outcome) <- c("ChangeImageP2ofP1","ChangeFitnessP1","ChangeFitnessP2")
      edgelist <- cbind(edgelist,outcome)
      
      
      ##spread information
      #for every P2, change their impression of their P1 in the image matrix
      for(l in 1:nrow(edgelist)){
        #determine all the players a who learn about the actions of P1
        a <- spread(g, edgelist$Player1[l], edgelist$Player2[l])
        #change the opinions players a have of P1 (i is the index of p1 since edgelist is sorted by P1)
        img[a,l] <- img[a,l]+edgelist$ChangeImageP2ofP1[l]
      }
      
      #the maximum image score is 5, the minimum image score is -5. Adjust, if necessary
      img[img>5] <- 5
      img[img<(-5)] <- -5
      
      #change fitness
      #calculate sum of fitness change for every player2
      fit2 <- aggregate(ChangeFitnessP2 ~ Player2 , data = edgelist , FUN = sum)
      #merge into edgelist so that every player 1 now has all the fitness changes he receives when he is player2
      edgelist <- merge(edgelist,fit2,by.x="Player1",by.y="Player2",all.x=T)
      edgelist$ChangeFitnessP2.y[is.na(edgelist$ChangeFitnessP2.y)==T] <- 0 #change NAs to 0
      edgelist$ChangeFitnessP1Final <- edgelist$ChangeFitnessP1+edgelist$ChangeFitnessP2.y #add change to fitness as P1 to changes as P2 for total fitness of P1
      #add the change to the attributes list
      attributes$fitness <- attributes$fitness+edgelist$ChangeFitnessP1Final
      
      #Print progress
      print(paste0("Round ", i, " finished!"))
      
    } #end of i rounds per generation loop
    
    #count the number of times each strategy occurs and write it to the results data frame
    #results[j,] <- table(attributes$strategy)
    for(s in 1:ncol(results)){
      results[j,s] <- length(which(attributes$strategy==seq(-5,6)[s]))
    }
    
    ##Reproduction:
    
    #Calculate the mean fitness for each strategy
    survival <- aggregate(fitness ~ strategy , data = attributes , FUN = mean)
    #Inverse logit function to get a fitness value between 0 and 1
    survival$fitness2 <- invlogit(survival$fitness)
    #survival$fitness2 <- survival$fitness
    #Calculate the relative fitness of each strategy compared to all strategies
    survival$rate <- survival$fitness2/sum(survival$fitness2)
    #calculate the rate at which each strategy will replicate
    survival$freq <- round(survival$rate*N)
    
    #create a vector with the new strategies
    #distributed according the fitness rates calculated above
    newstrat <- c(rep(survival$strategy,survival$freq))
    #shuffle the strategies (but retain the rates determined above)
    newstrat <- newstrat[sample(1:length(newstrat))]
    
    #if, due to rounding error, the vector is too long or too short
    #add or remove players with randomly selected strategies to it
    #until it has the right length
    #only add players with strategies that haven't already died out
    while(length(newstrat)<N){newstrat <- append(newstrat,sample(unique(newstrat),1))}
    while(length(newstrat)>N){newstrat <- newstrat[-sample(1:length(newstrat),1)]}
    #while(length(newstrat)>N){newstrat <- newstrat[-1]}
    
    # recreating a Barabasi-Albert network
    if (network_type=="BA") {
      g <- ba.game(N, power = BA.power, m = BA.m, directed = F)
    }
    
    # recreating a Watts-Strogatz network
    if (network_type=="WS") {
      g <- watts.strogatz.game(dim = 1, size = N, nei = WS.nei, p = WS.p)
      while(any(degree(g)==0)){g <- watts.strogatz.game(dim = 1, size = N, nei = WS.nei, p = WS.p)}
    }
    
    #assign the newly created strategies to the network object
    V(g)$strategy <- newstrat
    #in the new generation, everyone starts out with fitness = 0
    V(g)$fitness <- 0
    
    #create a new attributes data frame
    attributes <- data.frame(V(g)$strategy,V(g)$fitness)
    names(attributes) <- c("strategy","fitness")
    
    #create a new adjacency matrix
    #m <- as.matrix(as_adjacency_matrix(g))
    # keep m as the adjacency matrix
    # and create a new matrix img of the same dimensions as m
    # to hold the image scores
    #img <- m
    #img[,] <- 0
    img <- matrix(0, length(V(g)), length(V(g)))
    
    #update results
    #results[j,] <- table(attributes$strategy)
    for(s in 1:ncol(results)){
      results[j,s] <- length(which(attributes$strategy==seq(-5,6)[s]))
    }
    
    #Print progress
    print(paste0("Generation ", j, " finished!"))
    
  } #end of j generations loop
  
  #count the number of times each strategy appears, for each generation, and for each simulation
  #identical to the results data frame if the number of simulations k = 1
  #if k < 1, the results of simulation 1 appear at the top, then the results of simulation 2, and so on
  #so if the number of generations is 100, then the first 100 values will belong to simulation 1, etc.
  results_sims[(((k-1)*generations)+1):(k*generations),] <- results
  
} #end of k simulations loop

#}) # end of profvis
