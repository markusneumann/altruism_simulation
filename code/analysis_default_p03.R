#This script itself does not produce a figure that is in the paper
#but it produces the network that is used to make figure 3b

rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 1
network_type <- "WS"
WS.nei <- 5  #default is 5
WS.p <- 0.3 #default is 0.1
vary = "none"
generations <- 100
mechanisms <- c("truthful", "negative", "similar")
replication_methods <- c("new", "retain")

mechanism <- mechanisms[1]
replication <- replication_methods[1]

#Run the simulation
source("Neumann_Altruism_Simulation.R")
#Save the results
save.image("../results/img_WS_N300_p03.rdata")
# save(results_sims, file = "../results/result_WS_N300_p03.rdata")

# Read in the functions to generate plots
# source("analysisFunctions.R")
# make the plot in color
# makeGenerationPlot(results_sims, N, generations, bw = F) + ylim(0, 25) + ylab("Percentage of surviving strategies")
# ggsave(filename="../figures/results_WS_N300_p03.pdf", width = 8, height = 6, dpi = 100, units = "in")
# ggsave(filename="../figures/results_WS_N300_p03.png", width = 8, height = 6, dpi = 100, units = "in")
# #make the plot in black and white
# makeGenerationPlot(results_sims, N, generations, bw = T) + ylim(0, 25) + ylab("Percentage of surviving strategies")
# ggsave(filename="../figures/results_WS_N300_p03_bw.pdf", width = 8, height = 6, dpi = 100, units = "in")
# ggsave(filename="../figures/results_WS_N300_p03_bw.png", width = 8, height = 6, dpi = 100, units = "in")
