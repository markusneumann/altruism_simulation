library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 1
network_type <- "WS"
WS.nei <- 5  #default is 5
WS.p <- 0.05 #default is 0.1
vary = "none"
generations <- 100

#Run the simulation
source("Neumann_Altruism_Simulation_IGRAPH_dont_reset_img.R")

nodeOfInterest <- 200
colfunc <- colorRampPalette(c("green", "red"))
colNodes <- colfunc(12)[img[,nodeOfInterest]+5]
colNodes[img[,nodeOfInterest]==0] <- "#808080"
colNodes[nodeOfInterest] <- "#0000ff"
pdf(file = "../figures/illustration_Contagion.pdf", width = 12, height = 12)
png(file = "../figures/illustration_Contagion.pdf", width = 800, height = 800)
set.seed(1)
plot(g, vertex.size=6, vertex.label=NA, vertex.color=colNodes) #alternatively, vertex.size=(degree(g))*0.6
dev.off()
