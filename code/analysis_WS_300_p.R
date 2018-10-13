library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 100 #default is 100
network_type <- "WS"
vary <- "p"
WS.nei <- 5  #default is 5
WS.p.seq <- seq(0.05, 0.50, length.out = nsim)  #default is 0.05 to 0.50
generations <- 100

#Run the simulation
source("Neumann_Altruism_Simulation_IGRAPH.R")
save.image("savedImages/img_analysis_WS_300_p.rdata")
save(results_sims, file = "savedResults/result_analysis_WS_300_p.rdata")
#Read in the functions to generate plots
source("analysisFunctions.R")
#make the plot in color
makeSimulationPlot(results_sims, N, generations, nsim, WS.p.seq, "p (Rewiring probability)", bw = F) + ylab("Percentage of surviving strategies")
ggsave(filename="./figures/results_WS_300_p_100sims.pdf", width = 8, height = 6, dpi = 100, units = "in")
#make the plot in black and white
makeSimulationPlot(results_sims, N, generations, nsim, WS.p.seq, "p (Rewiring probability)", bw = T) + ylab("Percentage of surviving strategies")
ggsave(filename="./figures/results_WS_300_p_100sims_bw.pdf", width = 8, height = 6, dpi = 100, units = "in")
