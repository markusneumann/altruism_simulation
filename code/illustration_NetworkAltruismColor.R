rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(igraph)
set.seed(1)

#set a color gradient from blue to red for the different degrees of altruism --> egoism
cols <- c("6" = "#b30000", "5" = "#cc4735", "4" = "#d86a4f", "3" = "#e48d69", "2" = "#f1b184", "1" = "#fdd49e",
          "0" = "#bdbdbd",
          "-1" = "#9ecae1", "-2" = "#6baed6", "-3" = "#4292c6", "-4" = "#2171b5", "-5" = "#084594")
cols <- rev(cols)

#-------
# p = 0.05

#Load the data from the simulation
load("../results/img_WS_N300_p005.rdata")
#pdf(file = "../figures/illustrationWS_p005.pdf", width = 12, height = 12)
png(file = "../figures/illustrationWS_p005.png", width = 800, height = 800)
plot(g, vertex.size=(degree(g))*0.3, vertex.label=NA, vertex.color=cols[attributes$strategy+6])
dev.off()

#-------
# p = 0.3

#Load the data from the simulation
load("../results/img_WS_N300_p03.rdata")
#pdf(file = "../figures/illustrationWS_p03.pdf", width = 12, height = 12)
png(file = "../figures/illustrationWS_p03.png", width = 800, height = 800)
plot(g, vertex.size=(degree(g))*0.3, vertex.label=NA, vertex.color=cols[attributes$strategy+6])
dev.off()
