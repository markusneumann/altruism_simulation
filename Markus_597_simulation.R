######FUNCTIONS

findneighbor <- function(player){
  neighbors <- NULL
  for(i in 1:length(player)){
    neighbors <- append(neighbors,which(m[player[i],]==1))}
  return(neighbors)
}

find1neighbor <- function(player){
  neighbors <- NULL
  for(i in 1:length(player)){
    neighbors <- append(neighbors,which(m[player[i],]==1))}
  neighbors <- sample(neighbors,1)
  return(neighbors)
}

retain <- function(a,prob){
  if(runif(1,0,1)<prob)
    return(a)
}

spread <- function(player2){
  a <- player2
  b <- unlist(lapply(findneighbor(a), retain, prob = 0.50))
  c <- unlist(lapply(findneighbor(b), retain, prob = 0.25))
  return(c(a,b,c))
}
######

#setwd("C:/Users/Markus/OneDrive/2_Hatemi/paper/code")
library(igraph)
library(reshape) 
library(ggplot2)
library(arm)


###### WHEN RUNNING A NEW SIMULATION, SELECT CODE STARTING HERE

##Parameters
nsim <- 100 #number of simulations

N <- 300 #default is 300
generations <- 100 #default is 100
roundsPerGen <- 10 #default is 11

donation.img.gain <- 1 #default is 1
donation.img.loss <- -1 #default is -1
donation.fit.gain <- 2 #default is 2
donation.fit.loss <- -1 #default is -1

network_type <- "BA" #set to BA or WS
BA.power <- -2  #default is -2
BA.m <- 5  #default is 5
WS.nei <- 5  #default is 5
WS.p <- 0.1  #default is 0.1

###Setting up

results_sims <- data.frame(matrix(0, nrow = generations*nsim, ncol = 12))
names(results_sims) <- c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6")

for(k in 1:nsim){
  
  #vary the following as desired
  
  #N <- seq(100,1000,length.out=nsim)[k]
  #BA.power <- seq(-2,2,length.out=nsim)[k]
  BA.m <- seq(1,10,length.out=nsim)[k]
  #WS.nei <- seq(1,10,length.out=nsim)[k]
  #WS.p <- seq(0.05,0.5,length.out=nsim)[k]  
  
  # creating a Barabasi-Albert network using the igraph package
  if (network_type=="BA") {
    g <- ba.game(N, power=BA.power, m=BA.m, directed = F)
  }
  
  # creating a Watts-Strogatz network using the igraph package
  if (network_type=="WS") {
    g <- watts.strogatz.game(dim=1, size=N, nei=WS.nei, p=WS.p)
  }
  
  results <- data.frame(matrix(0, nrow = generations, ncol = 12))
  names(results) <- c("-5","-4","-3","-2","-1","0","1","2","3","4","5","6")
  
  # now every individual chooses a strategy at random
  V(g)$strategy <- sample(c(-5:6), size=length(V(g)), replace=T) #randomly assign strategies, i.e. k, between -5 (always cooperate) and 6 (never cooperate)
  # everyone starts with fitness 0
  V(g)$fitness <- 0
  
  attributes <- data.frame(V(g)$strategy,V(g)$fitness)
  names(attributes) <- c("strategy","fitness")
  
  m <- as.matrix(as_adjacency_matrix(g))
  img <- m
  img[,] <- 0
  #colnames(img) <- rownames(img)
  
  
  
  ###The actual simulation begins here
  
  for(j in 1:generations){
    
    
    for(i in 1:roundsPerGen){
      
      #create an edgelist
      edgelist <- data.frame(c(1:nrow(m)),rep(0,nrow(m)),attributes$strategy,rep(0,nrow(m)),rep(0,nrow(m)),attributes$fitness,rep(0,nrow(m)))
      names(edgelist) <- c("Player1","Player2","StrategyP1","ImageP1ofP2","ImageP2ofP1","FitnessP1","FitnessP2")
      edgelist$Player2 <- unlist(lapply(edgelist$Player1,find1neighbor))
      
      for(i in 1:nrow(edgelist)){ #set P2 fitness
        edgelist$FitnessP2[i] <- attributes$fitness[edgelist$Player2[i]]
      }
      
      for(i in 1:nrow(edgelist)){ #set images
        edgelist$ImageP1ofP2[i] <- img[edgelist$Player1[i],edgelist$Player2[i]] #ImageP1ofP2
        edgelist$ImageP2ofP1[i] <- img[edgelist$Player2[i],edgelist$Player1[i]] #ImageP2ofP1
      }
      
      #THIS is where the actual donation game takes place
      outcome <- matrix(0, nrow=nrow(edgelist), ncol=3)
      outcome[edgelist$StrategyP1>edgelist$ImageP1ofP2,1] <- donation.img.loss
      outcome[edgelist$StrategyP1>edgelist$ImageP1ofP2,2] <- 0
      outcome[edgelist$StrategyP1>edgelist$ImageP1ofP2,3] <- 0
      outcome[edgelist$StrategyP1<=edgelist$ImageP1ofP2,1] <- donation.img.gain
      outcome[edgelist$StrategyP1<=edgelist$ImageP1ofP2,2] <- donation.fit.loss
      outcome[edgelist$StrategyP1<=edgelist$ImageP1ofP2,3] <- donation.fit.gain
      
      
      #append outcome to edgelist
      outcome <- data.frame(outcome)
      names(outcome) <- c("ChangeImageP2ofP1","ChangeFitnessP1","ChangeFitnessP2")
      edgelist <- cbind(edgelist,outcome)
      
      
      ###
      ##spread information
      
      for(i in 1:nrow(edgelist)){
        a <- spread(edgelist$Player2[i])
        img[a,i] <- img[a,i]+edgelist$ChangeImageP2ofP1[i]
      }
      
      
      img[img>5] <- 5 #turn
      img[img<(-5)] <- -5 #turn
      
      #change fitness
      #calculate sum of fitness change for every player2
      fit2 <- aggregate(ChangeFitnessP2 ~ Player2 , data = edgelist , FUN = sum )
      #merge into edgelist so that every player 1 now has all the fitness changes he receives when he is player2
      edgelist <- merge(edgelist,fit2,by.x="Player1",by.y="Player2",all.x=T)
      edgelist$ChangeFitnessP2.y[is.na(edgelist$ChangeFitnessP2.y)==T] <- 0 #change NAs to 0
      edgelist$ChangeFitnessP1Final <- edgelist$ChangeFitnessP1+edgelist$ChangeFitnessP2.y #add change to fitness as P1 to changes as P2 for total fitness of P1
      
      attributes$fitness <- attributes$fitness+edgelist$ChangeFitnessP1Final
      
    }
    
    results[j,1] <- length(which(attributes$strategy==-5))
    results[j,2] <- length(which(attributes$strategy==-4))
    results[j,3] <- length(which(attributes$strategy==-3))
    results[j,4] <- length(which(attributes$strategy==-2))
    results[j,5] <- length(which(attributes$strategy==-1))
    results[j,6] <- length(which(attributes$strategy==0))
    results[j,7] <- length(which(attributes$strategy==1))
    results[j,8] <- length(which(attributes$strategy==2))
    results[j,9] <- length(which(attributes$strategy==3))
    results[j,10] <- length(which(attributes$strategy==4))
    results[j,11] <- length(which(attributes$strategy==5))
    results[j,12] <- length(which(attributes$strategy==6))
    
    #reproduction
    survival <- aggregate(fitness ~ strategy , data = attributes , FUN = mean )
    survival$fitness2 <- invlogit(survival$fitness)
    survival$rate <- survival$fitness2/sum(survival$fitness2)
    survival$freq <- round(survival$rate*N)
    sum(survival$freq)
    
    newstrat <- c(rep(survival$strategy,survival$freq))
    newstrat <- newstrat[sample(1:length(newstrat))]
    
    if(length(newstrat)==N-1){newstrat <- append(newstrat,sample(-5:6,1))}
    if(length(newstrat)==N-2){newstrat <- append(newstrat,sample(-5:6,2))}
    if(length(newstrat)==N-3){newstrat <- append(newstrat,sample(-5:6,3))}
    if(length(newstrat)==N-4){newstrat <- append(newstrat,sample(-5:6,4))}
    if(length(newstrat)==N-5){newstrat <- append(newstrat,sample(-5:6,5))}
    
    if(length(newstrat)==N+1){newstrat <- newstrat[-c(1)]}
    if(length(newstrat)==N+2){newstrat <- newstrat[-c(1,2)]}
    if(length(newstrat)==N+3){newstrat <- newstrat[-c(1,2,3)]}
    if(length(newstrat)==N+4){newstrat <- newstrat[-c(1,2,3,4)]}
    if(length(newstrat)==N+5){newstrat <- newstrat[-c(1,2,3,4,5)]}
    
    # recreating a Watts-Strogatz network
    if (network_type=="BA") {
      g <- ba.game(N, power=BA.power, m=BA.m, directed = F)
    }
    
    # recreating a Watts-Strogatz network
    if (network_type=="WS") {
      g <- watts.strogatz.game(dim=1, size=N, nei=WS.nei, p=WS.p)
    }
    
    V(g)$strategy <- newstrat
    V(g)$fitness <- 0
    
    attributes <- data.frame(V(g)$strategy,V(g)$fitness)
    names(attributes) <- c("strategy","fitness")
    
    m <- as.matrix(as_adjacency_matrix(g))
    img <- m
    img[,] <- 0
    
  }
  
  results_sims[(((k-1)*generations)+1):(k*generations),] <- results
  
}
#saving the results, to be used in the other rscript
save(results_sims, file = "results_BA_300_m_100sims.rdata")
