# Created July 2020
# Paper:  Indirect Reciprocity with Contagious Reputation in Large-Scale Small-World Networks
#
# Author: Markus Neumann
# Code:   The purpose of this script is to run the simulation with the parameters set in the analysis files
#         This script should not be executed directly, it is intended to be sourced


library('igraph') # create random networks
library('arm') # for the invlogit() function

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
spread <- function(neighbors_list, player1, player2, p_first_spread = 0.5, p_second_spread = 0.25){
  
  #1. Player 2 definitely receives the information
  #2. The neighbors of player 2 receive the information with 50% probability
  #find all the neighbors of player 2 except player 1
  player2_selected_neighbors <- neighbors_list[[player2]]
  player2_selected_neighbors <- player2_selected_neighbors[player2_selected_neighbors!=player1]
  player2_selected_neighbors <- unlist(lapply(player2_selected_neighbors, retain, prob = p_first_spread))
  #3. The neighbors of the neighbors who received the information
  #   receive the information with 25% probability
  #   If a player has 2 connections to direct neighbors of player 1, they have 2 chances to receive the information
  #find all the neighbors of the selected neighbors
  player2_neighbors_of_neighbors <- lapply(player2_selected_neighbors, function(x){neighbors_list[[x]]})
  #retain each with a 25% probability
  player2_selected_neighbors_of_neighbors <- lapply(player2_neighbors_of_neighbors, retain, prob = p_second_spread)
  player2_selected_neighbors_of_neighbors <- unique(unlist(player2_selected_neighbors_of_neighbors))
  #remove player one
  player2_selected_neighbors_of_neighbors <- player2_selected_neighbors_of_neighbors[player2_selected_neighbors_of_neighbors!=player1]
  
  #all the players who receive the information
  selected_players <- unique(c(player2,player2_selected_neighbors,player2_selected_neighbors_of_neighbors))
  
  return(selected_players)
}

#greater probability of spreading to people with similar opinions of the target
spread_similar_opinion <- function(neighbors_list, img_mat, player1, player2, p_floor = 0.2, p_ceiling = 0.8){
  
  #1. Player 2 definitely receives the information
  #2. The neighbors of player 2 receive the information with 50% probability
  #find all the neighbors of player 2 except player 1
  player2_selected_neighbors <- neighbors_list[[player2]]
  player2_selected_neighbors <- player2_selected_neighbors[player2_selected_neighbors!=player1]
  
  #What is the current opinions the neighbors of player 2 have of player 1?
  p2neighbors_p1img <- img_mat[player2_selected_neighbors,player1]
  #What does p 2 think of p 1?
  p2_p1img <- img_mat[player2,player1]
  #How big is the difference?
  img_diff <- abs(p2_p1img-p2neighbors_p1img)
  #since the maximum score is 5, and the minimum score is -5, the difference can be, at the most, 10
  #and at the least 0
  #the probability of spreading is 1-img_diff/10
  #i.e. always spread if p2 and the receiver have the same image of p1
  #and never spread if they disagree completely
  p_spread <- 1-img_diff/10
  p_spread[p_spread<p_floor] <- p_floor
  p_spread[p_spread>p_ceiling] <- p_ceiling
  
  first_spread_selection <- as.logical(rbinom(length(player2_selected_neighbors), T, p_spread))
  #the neighbors of p2 who receive the message
  player2_selected_neighbors <- player2_selected_neighbors[first_spread_selection]
  #their opinion of p1 (so far)
  p2_selected_neighbors_p1img <- p2neighbors_p1img[first_spread_selection]
  
  #3. The neighbors of the neighbors who received the information
  #   receive the information with 25% probability
  #   If a player has 2 connections to direct neighbors of player 1, they have 2 chances to receive the information
  #find all the neighbors of the selected neighbors
  player2_neighbors_of_neighbors <- lapply(player2_selected_neighbors, function(x){neighbors_list[[x]]})
  
  #what do all of those neighbors think of p1
  p2neighbors_of_neighbors_p1img <- lapply(player2_neighbors_of_neighbors, function(x){img_mat[x, player1]})
  #How big is the difference?
  #--- only do this if in the first spread, some players actually received the information
  if(length(p2_selected_neighbors_p1img)!=0){
    img_diff <- mapply(function(x, y){abs(x-p2_selected_neighbors_p1img[y])},
                       p2neighbors_of_neighbors_p1img,
                       1:length(p2_selected_neighbors_p1img))
    
    p_spread <- lapply(img_diff, function(x){1-x/10})
    p_spread <- unlist(p_spread)
    p_spread[p_spread<.2] <- 0.2
    p_spread[p_spread>.8] <- 0.8
    
    second_spread_selection <- as.logical(rbinom(length(unlist(player2_neighbors_of_neighbors)), T, p_spread))
    #the neighbors of neighbors of p2 who receive the message
    player2_selected_neighbors_of_neighbors <- unlist(player2_neighbors_of_neighbors)[second_spread_selection]
    #unique
    player2_selected_neighbors_of_neighbors <- unique(player2_selected_neighbors_of_neighbors)
    #remove player one
    player2_selected_neighbors_of_neighbors <- player2_selected_neighbors_of_neighbors[player2_selected_neighbors_of_neighbors!=player1]
  }
  else{
    player2_selected_neighbors_of_neighbors <- as.integer()
  }
  #all the players who receive the information
  selected_players <- unique(c(player2,player2_selected_neighbors,player2_selected_neighbors_of_neighbors))
  
  return(selected_players)
}

###### Code for a simulation begins here

set.seed(1)

##Parameters
#nsim <- 1 #number of simulations #default is 100

#N <- 300 #default is 300
#the exists statement allows this value to be set in another script
#and uses this one if that has not been done
if(exists("generations")==F){
  generations <- 100 #default is 100
}
if(exists("roundsPerGen")==F){
  roundsPerGen <- 10 #default is 10
}
if(exists("donation.img.gain")==F){
  donation.img.gain <- 1 #default is 1
}
if(exists("donation.img.loss")==F){
  donation.img.loss <- -1 #default is -1
}
if(exists("donation.fit.gain")==F){
  donation.fit.gain <- 2 #default is 2
}
if(exists("donation.fit.loss")==F){
  donation.fit.loss <- -1 #default is -1
}

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
  if(vary=="N"){
    N <- N.seq[k]
  }
  if(vary=="cb"){
    donation.fit.loss <- donation.fit.loss.seq[k]
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
  
  # create a matrix to hold the image scores
  img <- matrix(0, length(V(g)), length(V(g)))
  
  
  ###The actual simulation begins here
  
  #Every simulation consists of a number (by default 100) of generations
  for(j in 1:generations){
    
    # The following needs to be done only the first time if replication == "retain"
    # since the network doesn't change
    if(replication == "new" | j == 1){
      
      # get the edgelist
      # this contains every edge only once
      el <- get.edgelist(g)
      # create an edgelist that contains all the neighbors of every node in one column
      # to do this, simply rbind the edgelist to a copy of itself with the columns reversed
      el <- rbind(el, el[,c(2,1)])
      el <- el[order(el[,1], el[,2]),]
      # split the edgelist into an actual list, where each element describes the elements
      # and the index in the list describes the node
      out <- split(el[,2], f = el[,1])
      
    }
    
    #Each generation plays the donation game a number (by default 10) of times
    for(i in 1:roundsPerGen){
      
      #create a list of all the pairings (the players selected to play the game with each other)
      #this data frame stores all pertinent information
      pairinglist <- data.frame(c(1:N),rep(0,N),attributes$strategy,rep(0,N),rep(0,N),attributes$fitness,rep(0,N))
      names(pairinglist) <- c("Player1","Player2","StrategyP1","ImageP1ofP2","ImageP2ofP1","FitnessP1","FitnessP2")
      #for every player1, randomly find and assign a neighbor as player 2
      
      # sample the recipients from the edgelist
      pairinglist$Player2 <- unlist(lapply(out, sample, size = 1))
      #get the fitness of P2 from attributes data frame
      #and put it in the pairinglist data frame
      pairinglist$FitnessP2 <- attributes$fitness[pairinglist$Player2]
      
      #get the opinion P1 has of P2 from the image matrix and put it in the pairinglist data frame
      #the cbind command allows me to get specific row/column pairs from the matrix
      pairinglist$ImageP1ofP2 <- img[cbind(pairinglist$Player1,pairinglist$Player2)]
      #get the opinion P2 has of P1 from the image matrix and put it in the pairinglist data frame
      pairinglist$ImageP2ofP1 <- img[cbind(pairinglist$Player2,pairinglist$Player1)]
      
      #this is where the actual donation game takes place
      outcome <- matrix(0, nrow=nrow(pairinglist), ncol=3) #matrix to store outcomes in
      #P1 defects if the strategy of P1 > image of P2 with P1:
      outcome[pairinglist$StrategyP1>pairinglist$ImageP1ofP2,1] <- donation.img.loss #P1 incurs image loss with P2
      outcome[pairinglist$StrategyP1>pairinglist$ImageP1ofP2,2] <- 0 #no fitness change to P1
      outcome[pairinglist$StrategyP1>pairinglist$ImageP1ofP2,3] <- 0 #no fitness change to P2
      #P1 helps if the strategy of P1 <= image of P2 with P1:
      outcome[pairinglist$StrategyP1<=pairinglist$ImageP1ofP2,1] <- donation.img.gain #P1 incurs image gain with P2
      outcome[pairinglist$StrategyP1<=pairinglist$ImageP1ofP2,2] <- donation.fit.loss #lower fitness of P1
      outcome[pairinglist$StrategyP1<=pairinglist$ImageP1ofP2,3] <- donation.fit.gain #raise fitness of P2
      
      
      #append outcome to pairinglist
      outcome <- data.frame(outcome)
      names(outcome) <- c("ChangeImageP2ofP1","ChangeFitnessP1","ChangeFitnessP2")
      pairinglist <- cbind(pairinglist,outcome)
      
      
      ##spread information
      #information is spread truthfully
      if(mechanism=="truthful"){
        #for every P2, change their impression of their P1 in the image matrix
        for(l in 1:nrow(pairinglist)){
          #determine all the players a who learn about the actions of P1
          a <- spread(neighbors_list = out, pairinglist$Player1[l], pairinglist$Player2[l])
          #change the opinions players a have of P1 (i is the index of p1 since pairinglist is sorted by P1)
          img[a,l] <- img[a,l]+pairinglist$ChangeImageP2ofP1[l]
        }
      }
      #negative actions are more likely to be spread
      if(mechanism=="negative"){
        
        pairinglist_negative <- pairinglist[pairinglist$ChangeImageP2ofP1==-1,]
        pairinglist_positive <- pairinglist[pairinglist$ChangeImageP2ofP1==1,]
        
        # negative actions
        #only if there is at least one player who refuses to help
        if(nrow(pairinglist_negative)>0){
          #for every P2, change their impression of their P1 in the image matrix
          for(l in 1:nrow(pairinglist_negative)){
            #determine all the players a who learn about the actions of P1
            a <- spread(neighbors_list = out, pairinglist_negative$Player1[l], pairinglist_negative$Player2[l], p_first_spread = 0.75, p_second_spread = 0.375)
            #change the opinions players a have of P1 (i is the index of p1 since pairinglist is sorted by P1)
            img[a,pairinglist_negative$Player1[l]] <- img[a,pairinglist_negative$Player1[l]]+pairinglist_negative$ChangeImageP2ofP1[l]
          }
        }
        
        # positive actions
        #only if there is at least one player who helped
        if(nrow(pairinglist_positive)>0){
          #for every P2, change their impression of their P1 in the image matrix
          for(l in 1:nrow(pairinglist_positive)){
            #determine all the players a who learn about the actions of P1
            a <- spread(neighbors_list = out, pairinglist_positive$Player1[l], pairinglist_positive$Player2[l], p_first_spread = 0.5, p_second_spread = 0.25)
            #change the opinions players a have of P1 (i is the index of p1 since pairinglist is sorted by P1)
            img[a,pairinglist_positive$Player1[l]] <- img[a,pairinglist_positive$Player1[l]]+pairinglist_positive$ChangeImageP2ofP1[l]
          }
        }
        
      }
      
      # Greater probability of spreading to people with similar opinions of the target
      if(mechanism=="similar"){
        
        #for every P2, change their impression of their P1 in the image matrix
        for(l in 1:nrow(pairinglist)){
          #determine all the players a who learn about the actions of P1
          a <- spread_similar_opinion(neighbors_list = out, img_mat = img, player1 = pairinglist$Player1[l], player2 = pairinglist$Player2[l])
          #change the opinions players a have of P1 (i is the index of p1 since pairinglist is sorted by P1)
          img[a,l] <- img[a,l]+pairinglist$ChangeImageP2ofP1[l]
        }
        
        
      }
      
      #the maximum image score is 5, the minimum image score is -5. Adjust, if necessary
      img[img>5] <- 5
      img[img<(-5)] <- -5
      
      #change fitness
      #calculate sum of fitness change for every player2
      fit2 <- aggregate(ChangeFitnessP2 ~ Player2 , data = pairinglist , FUN = sum)
      #merge into pairinglist so that every player 1 now has all the fitness changes he receives when he is player2
      pairinglist <- merge(pairinglist,fit2,by.x="Player1",by.y="Player2",all.x=T)
      pairinglist$ChangeFitnessP2.y[is.na(pairinglist$ChangeFitnessP2.y)==T] <- 0 #change NAs to 0
      pairinglist$ChangeFitnessP1Final <- pairinglist$ChangeFitnessP1+pairinglist$ChangeFitnessP2.y #add change to fitness as P1 to changes as P2 for total fitness of P1
      #add the change to the attributes list
      attributes$fitness <- attributes$fitness+pairinglist$ChangeFitnessP1Final
      
      #Print progress
      print(paste0("Round ", i, " finished!"))
      
    } #end of i rounds per generation loop
    
    #count the number of times each strategy occurs and write it to the results data frame
    for(s in 1:ncol(results)){
      results[j,s] <- length(which(attributes$strategy==seq(-5,6)[s]))
    }
    
    if(j!=generations){
    
    ##Reproduction:
    
    #Calculate the mean fitness for each strategy
    survival <- aggregate(fitness ~ strategy , data = attributes , FUN = mean)
    #Inverse logit function to get a fitness value between 0 and 1
    survival$fitness2 <- invlogit(survival$fitness)
    #Calculate the relative fitness of each strategy compared to all strategies
    survival$rate <- survival$fitness2/sum(survival$fitness2)
    #calculate the rate at which each strategy will replicate
    survival$freq <- round(survival$rate*N)
    
    # survival mechanism where strategies survive in aggregate and a new network is generated
    if(replication == "new"){
      
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
      
    }
    
    # survival mechanism where strategies survive individually and the network is retained
    if(replication == "retain"){
      
      # alternative survival mechanism
      attributes2 <- attributes
      # rather than having strategies survive in aggregate, survival happens at the level
      # of the individual players
      # each player's fitness is ranked compared to everyone else's
      attributes2$fitness2 <- invlogit(attributes2$fitness)
      # then a Bernoulli trial is conducted using the probability derived from the inverse logit above
      # determining whether the player passes on their "genes" (i.e. strategy)
      attributes2$survives <- as.logical(rbinom(nrow(attributes2), T, attributes2$fitness2))
      #how many didn't pass on their strategy?
      len_refill <- length(which(attributes2$survives==F))
      #for those, randomly sample a new strategy according to the same survival rate in the original mechanism
      newstrat <- sample(survival$strategy, len_refill, prob = survival$rate, replace = T)
      # players whose strategy survived
      attributes2$new_strategy <- attributes$strategy
      # players whose strategy didn't survive
      attributes2$new_strategy[attributes2$survives==F] <- newstrat
      
      #assign the newly created strategies to the network object
      V(g)$strategy <- attributes2$new_strategy
      #in the new generation, everyone starts out with fitness = 0
      V(g)$fitness <- 0
      
    }
    
    #create a new attributes data frame
    attributes <- data.frame(V(g)$strategy,V(g)$fitness)
    names(attributes) <- c("strategy","fitness")
    
    # new image matrix
    img <- matrix(0, length(V(g)), length(V(g)))
    
    }
    
    #update results
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

#}, prof_output = "profvis_out.Rprof") # end of profvis
