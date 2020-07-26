rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 100 #default is 100
network_type <- "WS"
vary <- "cb"
WS.nei <- 5  #default is 5
WS.p <- 0.2  #default is 0.1; 0.2 corresponds to Figure 4
#Note: the reason this is 0.2 and not the default 0.1 here is because if it was 0.1,
# the plot wouldn't show any variation based on the cost-benefit ratio;
# in that case, the left-hand side of the plot would simply correspond to Figure 5.
#Instead, the conditions need to be sufficiently difficult for altruism,
# for the benefits of a higher cost-benefit ratio to become evident
generations <- 100
mechanisms <- c("truthful", "negative", "similar")
replication_methods <- c("new", "retain")
mechanism <- mechanisms[1]
replication <- replication_methods[1]

donation.fit.loss.seq <- seq(-0.5, -0.1, length.out = nsim)
donation.fit.gain <- 1

#Run the simulation and save the results
source("Neumann_Altruism_Simulation.R")
results_name <- paste0(network_type, "_N", N, "_", mechanism, "_", replication, "_vary_", vary)
save.image(paste0("../results/img_", results_name, ".rdata"))
# save(results_sims, file = paste0("../results/result_", results_name, ".rdata"))

#Read in the functions to generate plots
source("analysisFunctions.R")
#make the plot in color
makeSimulationPlot(results_sims, N, generations, nsim, donation.fit.loss.seq, "Cost-benefit ratio", bw = F) + ylab("Percentage of surviving strategies")
# ggsave(filename = paste0("../figures/results_", results_name, ".pdf"), width = 8, height = 6, dpi = 100, units = "in")
ggsave(filename = paste0("../figures/results_", results_name, ".png"), width = 8, height = 6, dpi = 100, units = "in")
# #make the plot in black and white
# makeSimulationPlot(results_sims, N, generations, nsim, donation.fit.loss.seq, "p (Rewiring probability)", bw = T) + ylab("Percentage of surviving strategies")
# ggsave(filename = paste0("../figures/results_", results_name, "_bw.pdf"), width = 8, height = 6, dpi = 100, units = "in")
# ggsave(filename = paste0("../figures/results_", results_name, "_bw.png"), width = 8, height = 6, dpi = 100, units = "in")
