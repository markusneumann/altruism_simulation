library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 100 #default is 100
network_type <- "WS"
vary <- "nei"
WS.nei.seq <- seq(3, 10, length.out = nsim)
WS.p <- 0.1  #default is 0.1
generations <- 100

#Run the simulation
source("Neumann_Altruism_Simulation_IGRAPH.R")
save.image("../results/img_analysis_WS_300_nei.rdata")
save(results_sims, file = "../results/result_analysis_WS_300_nei.rdata")
#Read in the functions to generate plots
source("analysisFunctions.R")
#make the plot in color
makeSimulationPlot(results_sims, N, generations, nsim, WS.nei.seq, "l (Neighborhood coefficient)", bw = F) + ylab("Percentage of surviving strategies") + ylim(0,25)
ggsave(filename="../figures/results_WS_300_nei_100sims.pdf", width = 8, height = 6, dpi = 100, units = "in")
ggsave(filename="../figures/results_WS_300_nei_100sims.png", width = 8, height = 6, dpi = 100, units = "in")
#make the plot in black and white
makeSimulationPlot(results_sims, N, generations, nsim, WS.nei.seq, "l (Neighborhood coefficient)", bw = T) + ylab("Percentage of surviving strategies") + ylim(0,25)
ggsave(filename="../figures/results_WS_300_nei_100sims_bw.pdf", width = 8, height = 6, dpi = 100, units = "in")
ggsave(filename="../figures/results_WS_300_nei_100sims_bw.png", width = 8, height = 6, dpi = 100, units = "in")
