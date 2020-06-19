#For N=300
load("../results/img_WS_N300_default_contagion_illustration.rdata")
mean(apply(img, 1, function(x){length(which(x!=0))}))
#64.81667 -- for N=300; p=0.1
#47.27667 -- for N=300; p=0.05

#For N=20000
rm(list = ls()) #ensure that no hidden variables are loaded through .Rhistory or .Rsession

library(reshape2)
library(ggplot2)

N <- 20000 #default is 300
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
source("Neumann_Altruism_Simulation_dont_reset_img.R")
save.image("../results/img_WS_N20000_dunbars_number.rdata")

mean(apply(img, 1, function(x){length(which(x!=0))}))
#50.143 -- for N=1000