#In order to execute all of this code and produce the plots in pdf format...
#... create a folder called "Figures" in the working directory first

setwd("C:/Users/Markus/OneDrive/2_Hatemi/paper/code")

library(igraph)
library(reshape) 
library(ggplot2)
library(arm)


#BA Default
load("results_BA_300_default.rdata")
results_sims <- results_sims/300*100 #convert to percent
results2 <- melt(results_sims)
results2$generation <- rep(1:100,12)
names(results2) <- c("Strategy","Percentage","Generation")
g1 <- ggplot(results2, aes(Generation, Percentage, colour = Strategy)) +
  xlim(0,100) + geom_line() + ggtitle("Strategies over time")
g1
ggsave(filename="../Figures/results_BA_300_default.pdf", plot=g1)

#WS Default
load("results_WS_300_default.rdata")
results_sims <- results_sims/300*100 #convert to percent
results2 <- melt(results_sims)
results2$generation <- rep(1:100,12)
names(results2) <- c("Strategy","Percentage","Generation")
g1 <- ggplot(results2, aes(Generation, Percentage, colour = Strategy)) +
  xlim(0,100) + geom_line() + ggtitle("Strategies over time")
g1
ggsave(filename="../Figures/results_WS_300_default.pdf", plot=g1)

#BA 10000 population
load("results_BA_10000_default.rdata")
results_sims <- results_sims/10000*100 #convert to percent
results2 <- melt(results_sims)
results2$generation <- rep(1:100,12)
names(results2) <- c("Strategy","Percentage","Generation")
g1 <- ggplot(results2, aes(Generation, Percentage, colour = Strategy)) +
  xlim(0,100) + geom_line() + ggtitle("Strategies over time with N=10000")
g1
ggsave(filename="../Figures/results_BA_10000_default.pdf", plot=g1)

#WS 10000 population
load("results_WS_10000_default.rdata")
results_sims <- results_sims/10000*100 #convert to percent
results2 <- melt(results_sims)
results2$generation <- rep(1:100,12)
names(results2) <- c("Strategy","Percentage","Generation")
g1 <- ggplot(results2, aes(Generation, Percentage, colour = Strategy)) +
  xlim(0,100) + geom_line() + ggtitle("Strategies over time with N=10000")
g1
ggsave(filename="../Figures/results_WS_10000_default.pdf", plot=g1)

#Watts-Strogaz, varying p
load("results_WS_300_p_100sims.rdata")

results_final <- results_sims[seq(100,10000,length.out = 100),]
results_final <- results_final[1:95,]
results_final <- results_final/300*100 #convert to percent
results_final <- melt(results_final)
names(results_final) <- c("Strategy","Percentage")
results_final$Connectivity <- rep(seq(0.05,0.47727273,length.out=95))

g1 <- ggplot(results_final, aes(Connectivity, Percentage, colour = Strategy)) + geom_line() + ggtitle("Viable strategies, varying connectivity")
g1
ggsave(filename="../Figures/results_WS_300_p_100sims.pdf", plot=g1)

#Watts-Strogaz, varying nei
load("results_WS_300_nei_100sims.rdata")

results_final <- results_sims[seq(100,10000,length.out = 100),]
results_final <- results_final/300*100 #convert to percent
results_final <- melt(results_final)
names(results_final) <- c("Strategy","Percentage")
results_final$Neighborhood <- rep(seq(5,10,length.out=100))

g1 <- ggplot(results_final, aes(Neighborhood, Percentage, colour = Strategy)) + geom_line() + ggtitle("Viable strategies, varying neighborhood")
g1
ggsave(filename="../Figures/results_WS_300_nei_100sims.pdf", plot=g1)

#Barabasi-Albert, varying power
load("results_BA_300_power_100sims.rdata")

results_final <- results_sims[seq(100,10000,length.out = 100),]
results_final <- results_final/300*100 #convert to percent
results_final <- melt(results_final)
names(results_final) <- c("Strategy","Percentage")
results_final$Power <- rep(seq(-2,2,length.out=100))

g1 <- ggplot(results_final, aes(Power, Percentage, colour = Strategy)) + geom_line() + ggtitle("Viable strategies, varying power")
g1
ggsave(filename="../Figures/results_BA_300_power_100sims.pdf", plot=g1)

#Barabasi-Albert, varying m
load("results_BA_300_m_100sims.rdata")

results_final <- results_sims[seq(100,10000,length.out = 100),]
results_final <- results_final/300*100 #convert to percent
results_final <- melt(results_final)
names(results_final) <- c("Strategy","Percentage")
results_final$m <- rep(seq(1,10,length.out=100))

g1 <- ggplot(results_final, aes(m, Percentage, colour = Strategy)) + geom_line() + ggtitle("Viable strategies, varying m")
g1
ggsave(filename="../Figures/results_BA_300_m_100sims.pdf", plot=g1)


# create the network and then plot it with the Fruchterman-Reingold algorithm
# compare BA and WS
g <- watts.strogatz.game(dim=1, size=50, nei=5, p=.1)
par(mfrow=c(1,2))
plot(g, 
     layout=layout_with_fr(g, dim = 2, niter = 100, weights = E(g)$weight), 
     main="Watts-Strogaz",
     vertex.label = NA,
     vertex.size = 4)

g <- barabasi.game(n=50, power=-2, m=5, directed = F)
plot(g, 
     layout=layout_with_fr(g, dim = 2, niter = 100, weights = E(g)$weight),
     main="Barabasi-Albert",
     vertex.label = NA,
     vertex.size = 4)

# WS, high and low connectivity
g <- watts.strogatz.game(dim=1, size=50, nei=5, p=.1)
par(mfrow=c(1,2))
plot(g, 
     layout=layout_with_fr(g, dim = 2, niter = 100, weights = E(g)$weight), 
     main="Watts-Strogaz - Low Connectivity",
     vertex.label = NA,
     vertex.size = 4)

g <- watts.strogatz.game(dim=1, size=50, nei=5, p=.5)
plot(g, 
     layout=layout_with_fr(g, dim = 2, niter = 100, weights = E(g)$weight),
     main="Watts-Strogaz - High Connectivity",
     vertex.label = NA,
     vertex.size = 4)
