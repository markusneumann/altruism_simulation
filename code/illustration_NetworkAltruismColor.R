library(igraph)
set.seed(1)

#set a color gradient from blue to red for the different degrees of altruism --> egoism
colfunc <- colorRampPalette(c("blue", "red"))

#-------
# p = 0.05

#Load the data from the simulation
load("../results/img_WS_N300_p005.rdata")
#pdf(file = "../figures/illustrationWS_p005.pdf", width = 12, height = 12)
png(file = "../figures/illustrationWS_p005.png", width = 800, height = 800)
plot(g, vertex.size=(degree(g))*0.3, vertex.label=NA, vertex.color=colfunc(12)[attributes$strategy+6])
dev.off()

#-------
# p = 0.3

#Load the data from the simulation
load("../results/img_WS_N300_p03.rdata")
#pdf(file = "../figures/illustrationWS_p03.pdf", width = 12, height = 12)
png(file = "../figures/illustrationWS_p03.png", width = 800, height = 800)
plot(g, vertex.size=(degree(g))*0.3, vertex.label=NA, vertex.color=colfunc(12)[attributes$strategy+6])
dev.off()
