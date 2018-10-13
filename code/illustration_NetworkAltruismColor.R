library(igraph)
set.seed(1)

#set a color gradient from blue to red for the different degrees of altruism --> egoism
colfunc <- colorRampPalette(c("blue", "red"))

# #set the network parameters
# N <- 300 #default is 300
# nsim <- 1
# network_type <- "WS"
# WS.nei <- 5  #default is 5
# vary = "none"
# generations = 20

#-------
# p = 0.05

#Load the data from the simulation
load("savedImages/img_analysis_WS_300_p005.rdata")
pdf(file = "./figures/illustrationWS_p005.pdf", width = 12, height = 12)
plot(g, vertex.size=(degree(g))*0.3, vertex.label=NA, vertex.color=colfunc(12)[attributes$strategy+6])
dev.off()

#-------
# p = 0.25

# WS.p <- 0.25
# 
# #Run the simulation
# source("Neumann_Altruism_Simulation_IGRAPH.R")
# pdf(file = "./figures/illustrationWS_p025.pdf", width = 600, height = 600)
# plot(g, vertex.size=(degree(g))*0.3, vertex.label=NA, vertex.color=colfunc(12)[attributes$strategy+6])
# dev.off()

#-------
# p = 0.3

#Load the data from the simulation
load("savedImages/img_analysis_WS_300_p03.rdata")
pdf(file = "./figures/illustrationWS_p03.pdf", width = 12, height = 12)
plot(g, vertex.size=(degree(g))*0.3, vertex.label=NA, vertex.color=colfunc(12)[attributes$strategy+6])
dev.off()
