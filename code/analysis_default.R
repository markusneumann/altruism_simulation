rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)

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
source("Neumann_Altruism_Simulation.R")
#Save the results
results_name <- paste0(network_type, "_N", N, "_", mechanism, "_", replication, "_default")
save.image(paste0("../results/img_", results_name, ".rdata"))
# save(results_sims, file = paste0("../results/result_", results_name, ".rdata"))

#Read in the functions to generate plots
source("analysisFunctions.R")
#make the plot in color
makeGenerationPlot(results_sims, N, generations, bw = F) + ylim(0, 25) + ylab("Percentage of surviving strategies")
# ggsave(filename = paste0("../figures/results_", results_name, ".pdf"), width = 8, height = 6, dpi = 100, units = "in")
ggsave(filename = paste0("../figures/results_", results_name, ".png"), width = 8, height = 6, dpi = 100, units = "in")
# #make the plot in black and white
# makeGenerationPlot(results_sims, N, generations, bw = T) + ylim(0, 25) + ylab("Percentage of surviving strategies")
# ggsave(filename = paste0("../figures/results_", results_name, "_bw.pdf"), width = 8, height = 6, dpi = 100, units = "in")
# ggsave(filename = paste0("../figures/results_", results_name, "_bw.png"), width = 8, height = 6, dpi = 100, units = "in")
