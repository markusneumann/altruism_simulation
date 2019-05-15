library(reshape2)
library(ggplot2)

N <- 300 #default is 300
nsim <- 1
network_type <- "WS"
WS.nei <- 5
WS.p <- 0.2
vary = "none"
generations <- 100

#Run the simulation
source("Neumann_Altruism_Simulation_IGRAPH.R")
save.image("../results/img_analysis_WS_300_p02.rdata")
save(results_sims, file = "../results/result_analysis_WS_300_p02.rdata")
#Read in the functions to generate plots
source("analysisFunctions.R")
#make the plot in color
makeGenerationPlot(results_sims, N, generations, bw = F) + ylim(0, 25) + ylab("Percentage of surviving strategies")
ggsave(filename="../figures/results_WS_300_p02.pdf", width = 8, height = 6, dpi = 100, units = "in")
ggsave(filename="../figures/results_WS_300_p02.png", width = 8, height = 6, dpi = 100, units = "in")
#make the plot in black and white
makeGenerationPlot(results_sims, N, generations, bw = T) + ylim(0, 25) + ylab("Percentage of surviving strategies")
ggsave(filename="../figures/results_WS_300_p02_bw.pdf", width = 8, height = 6, dpi = 100, units = "in")
ggsave(filename="../figures/results_WS_300_p02_bw.png", width = 8, height = 6, dpi = 100, units = "in")
