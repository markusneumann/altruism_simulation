rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 100 #default is 100
network_type <- "WS"
vary <- "p"
WS.nei <- 5  #default is 5
WS.p.seq <- seq(0.05, 0.50, length.out = nsim)  #default is 0.05 to 0.50
generations <- 100
mechanisms <- c("truthful", "negative", "similar")
replication_methods <- c("new", "retain")
mechanism <- mechanisms[1]
replication <- replication_methods[1]

#Run the simulation and save the results
source("Neumann_Altruism_Simulation.R")
results_name <- paste0(network_type, "_N", N, "_", mechanism, "_", replication, "_vary_", vary)
save.image(paste0("../results/img_", results_name, ".rdata"))
#save(results_sims, file = paste0("../results/result_", results_name, ".rdata"))

#Read in the functions to generate plots
source("analysisFunctions.R")
#make the plot in color
makeSimulationPlot(results_sims, N, generations, nsim, WS.p.seq, "p (Rewiring probability)", bw = F) + ylab("Percentage of surviving strategies")
#ggsave(filename = paste0("../figures/results_", results_name, ".pdf"), width = 8, height = 6, dpi = 100, units = "in")
ggsave(filename = paste0("../figures/results_", results_name, ".png"), width = 8, height = 6, dpi = 100, units = "in")
#make the plot in black and white
# makeSimulationPlot(results_sims, N, generations, nsim, WS.p.seq, "p (Rewiring probability)", bw = T) + ylab("Percentage of surviving strategies")
# ggsave(filename = paste0("../figures/results_", results_name, "_bw.pdf"), width = 8, height = 6, dpi = 100, units = "in")
# ggsave(filename = paste0("../figures/results_", results_name, "_bw.png"), width = 8, height = 6, dpi = 100, units = "in")
