rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)
library(igraph)

N <- 300 #default is 300
nsim <- 1
network_type <- "WS"
WS.nei <- 5  #default is 5
WS.p <- 0.1 #default is 0.1
vary = "none"
generations <- 100
mechanisms <- c("truthful", "negative", "similar")
replication_methods <- c("new", "retain")

mechanism <- mechanisms[1]
replication <- replication_methods[1]

#Run the simulation
source("Neumann_Altruism_Simulation_dont_reset_img.R")

#Illustrate
nodeOfInterest <- 4
cols <- c("6" = "#b30000", "5" = "#cc4735", "4" = "#d86a4f", "3" = "#e48d69", "2" = "#f1b184", "1" = "#fdd49e",
          "0" = "#bdbdbd",
          "-1" = "#9ecae1", "-2" = "#6baed6", "-3" = "#4292c6", "-4" = "#2171b5", "-5" = "#084594")
cols <- rev(cols)
colNodes <- cols[img[,nodeOfInterest]+6]
colNodes[img[,nodeOfInterest]==0] <- "#808080"
colNodes[nodeOfInterest] <- "#000000"
#pdf(file = "../figures/illustration_Contagion.pdf", width = 12, height = 12)
png(file = "../figures/illustration_Contagion.png", width = 800, height = 800)
set.seed(1)
plot(g, vertex.size=6, vertex.label=NA, vertex.color=colNodes) #alternatively, vertex.size=(degree(g))*0.6
dev.off()

save.image("../results/img_WS_N300_default_contagion_illustration.rdata")
