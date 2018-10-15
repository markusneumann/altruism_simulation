library(reshape2)
library(ggplot2)

makeGenerationPlot <- function(resultsSims, nNodes, nGenerations, bw = F){
  resultsSims <- resultsSims/nNodes*100 #convert to percent
  results2 <- melt(resultsSims)
  results2$generation <- rep(1:nGenerations,12)
  names(results2) <- c("strat","Percentage","Generation")
  results2$Strategy <- NA
  results2$Strategy[as.numeric(as.character(results2$strat))<0] <- "Altruistic"
  results2$Strategy[as.numeric(as.character(results2$strat))>0] <- "Egoistic"
  results2$Strategy[as.numeric(as.character(results2$strat))==0] <- "Neutral"
  #in black and white
  if(bw == T){
    g1 <- ggplot(results2, aes(Generation, Percentage, fill = strat)) +
      xlim(0,100) + geom_line(aes(linetype = Strategy)) +
      scale_fill_grey() + theme_bw() +
      scale_linetype_manual(values = c("solid", "dashed", "dotted"))
    return(g1)
  }
  #in color
  if(bw == F){
    g1 <- ggplot(results2, aes(Generation, Percentage, colour = strat)) +
      xlim(0,100) + geom_line() + 
      scale_color_discrete(name = "Strategy") +
      theme_bw()
    g1
  }
}

makeSimulationPlot <- function(resultsSims, nNodes, nGenerations, nSims, netwPropSeq, netwPropName, bw = F){
  resultsSims <- resultsSims[seq(nGenerations, nGenerations*nSims, length.out = nSims),]
  resultsSims <- resultsSims/nNodes*100 #convert to percent
  results2 <- melt(resultsSims)
  results2$netwPropSeq <- rep(netwPropSeq,12)
  names(results2) <- c("strat","Percentage","networkProperty")
  results2$Strategy <- NA
  results2$Strategy[as.numeric(as.character(results2$strat))<0] <- "Altruistic"
  results2$Strategy[as.numeric(as.character(results2$strat))>0] <- "Egoistic"
  results2$Strategy[as.numeric(as.character(results2$strat))==0] <- "Neutral"
  #in black and white
  if(bw == T){
    g1 <- ggplot(results2, aes(networkProperty, Percentage, fill = strat)) +
      geom_line(aes(linetype = Strategy)) +
      scale_fill_grey() + theme_bw() + labs(x = netwPropName) +
      scale_linetype_manual(values = c("solid", "dashed", "dotted"))
    return(g1)
  }
  #in color
  if(bw == F){
    g1 <- ggplot(results2, aes(networkProperty, Percentage, colour = strat)) +
      geom_line() + 
      scale_color_discrete(name = "Strategy") +
      theme_bw() + labs(x = netwPropName)
    g1
  }
}


#makeGenerationPlot(results_sims, N, generations, bw = F)
#makeGenerationPlot(results_sims, N, generations, bw = T)

#makeSimulationPlot(results_sims, N, generations, nsim, WS.p.seq, "P (Rewiring probability)", bw = T)
#makeSimulationPlot(results_sims, N, generations, nsim, WS.p.seq, "P (Rewiring probability)", bw = F)