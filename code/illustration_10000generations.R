rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 1
network_type <- "WS"
WS.nei <- 5  #default is 5
WS.p <- 0.1 #default is 0.1
vary = "none"
generations <- 10000
mechanisms <- c("truthful", "negative", "similar")
replication_methods <- c("new", "retain")

mechanism <- mechanisms[1]
replication <- replication_methods[1]

#Run the simulation
source("Neumann_Altruism_Simulation.R")
#Save the results
save.image("../results/img_WS_N300_default_10000_generations.rdata")

source("analysisFunctions.R")
makeGenerationPlot(results_sims, N, generations, bw = F) + ylim(0, 25) + ylab("Percentage of surviving strategies")
ggsave(filename="../figures/results_default_10000_generations.png", width = 8, height = 6, dpi = 100, units = "in")
