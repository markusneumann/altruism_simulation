rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)

nsim <- 20
N.seq <- seq(1000, 20000, length.out = nsim) #default is 300
network_type <- "WS"
WS.nei <- 5  #default is 5
WS.p <- 0.05 #default is 0.1
vary = "N"
generations <- 100
mechanisms <- c("truthful", "negative", "similar")
replication_methods <- c("new", "retain")
mechanism <- mechanisms[1]
replication <- replication_methods[1]

source("Neumann_Altruism_Simulation.R")
#Save the results
results_name <- paste0(network_type, "_N", N, "_", mechanism, "_", replication, "_population")
save.image(paste0("../results/img_", results_name, ".rdata"))
#save(results_sims, file = paste0("../results/result_", results_name, ".rdata"))


library(tidyr)

load("../results/img_WS_N20000_truthful_new_population.rdata")

results_sims$N <- sort(rep(seq(1000, 20000, length.out = nsim), 100))
results_sims$generation <- rep(1:generations, nsim)
results2 <- gather(results_sims, strat, fitness, -N, -generation)
results2$fitness <- results2$fitness/results2$N*100
results2$strat <- as.factor(as.numeric(results2$strat))

#Plot including N1000-20000
ggplot(results2, aes(generation, fitness, colour = strat)) +
  xlim(0,generations) + geom_line() +
  theme_bw() +
  facet_wrap(~N) + 
  scale_color_discrete(name = "Strategy") +
  xlab("Generation") +
  ylab("Percentage of surviving strategies")
ggsave(filename = "../figures/results_1000_20000_truthful_new.png", width = 8, height = 6, dpi = 100, units = "in")

#Plot including only N20000
ggplot(results2[results2$N==20000,], aes(generation, fitness, colour = strat)) +
  xlim(0,generations) + ylim(7.5,9) + geom_line() +
  theme_bw() +
  scale_color_discrete(name = "Strategy") +
  xlab("Generation") +
  ylab("Percentage of surviving strategies")
ggsave(filename = "../figures/results_N20000_truthful_new.png", width = 8, height = 6, dpi = 100, units = "in")
