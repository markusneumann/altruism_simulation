library(reshape2)
library(ggplot2)
library(stringr)

mechanisms <- c("truthful", "negative", "similar")
replication_methods <- c("new", "retain")

makeSimulationPlot <- function(resultsSims, nNodes, nGenerations, nSims, netwPropSeq, netwPropName){
  resultsSims <- resultsSims[seq(nGenerations, nGenerations*nSims, length.out = nSims),]
  resultsSims <- resultsSims/nNodes*100 #convert to percent
  results2 <- melt(resultsSims)
  results2$netwPropSeq <- rep(netwPropSeq,12)
  names(results2) <- c("strat","Percentage","networkProperty")
  results2$Strategy <- NA
  results2$Strategy[as.numeric(as.character(results2$strat))<0] <- "Altruistic"
  results2$Strategy[as.numeric(as.character(results2$strat))>0] <- "Egoistic"
  results2$Strategy[as.numeric(as.character(results2$strat))==0] <- "Neutral"
  return(results2)
}

dir.create("tmp")
for(mech in 1:length(mechanisms)){
  for(repl_m in 1:length(replication_methods)){
    results_name <- paste0("../results/img_WS_N300_", mechanisms[mech], "_", replication_methods[repl_m], "_vary_p.rdata")
    load(results_name)
    a <- makeSimulationPlot(results_sims, N, generations, nsim, WS.p.seq, "p (Rewiring probability)")
    save(a, file = paste0("tmp/tmp_", mechanisms[mech], "_", replication_methods[repl_m], ".rdata"))
  }
}

f <- list.files("tmp", full.names = T)
for(i in 1:length(f)){
  load(f[i])
  if(i==1){
    df <- a
    mechanism <- str_remove_all(str_extract(f[i], "_(.*?)_"), "_")
    df$mechanism <- mechanism
    df$replication_method <- str_remove_all(str_extract(str_remove(f[i], mechanism), "_(.*?)\\."), "[_|.]")
  }
  else{
    df2 <- a
    mechanism <- str_remove_all(str_extract(f[i], "_(.*?)_"), "_")
    df2$mechanism <- mechanism
    df2$replication_method <- str_remove_all(str_extract(str_remove(f[i], mechanism), "_(.*?)\\."), "[_|.]")
    df <- rbind(df, df2)
  }
}

df$mechanism[df$mechanism=="truthful"] <- "Mechanism: Truthful (default)"
df$mechanism[df$mechanism=="negative"] <- "Mechanism: Negative"
df$mechanism[df$mechanism=="similar"] <- "Mechanism: Similar"
df$replication_method[df$replication_method=="new"] <- "Replication: New network (default)"
df$replication_method[df$replication_method=="retain"] <- "Replication: Retain network"

# g1 <- ggplot(df, aes(networkProperty, Percentage, fill = strat)) +
#   geom_line(aes(linetype = Strategy)) +
#   scale_fill_grey() + theme_bw() + labs(x = "P (Rewiring probability)") +
#   scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
#   facet_grid(replication_method~mechanism)
# g1

g1 <- ggplot(df, aes(networkProperty, Percentage, colour = strat)) +
  geom_line() + 
  scale_color_discrete(name = "Strategy") +
  theme_bw() + labs(x = "P (Rewiring probability)") +
  facet_grid(replication_method~mechanism)
g1

ggsave(filename = "../figures/results_vary_mechanisms.png", width = 8, height = 6, dpi = 100, units = "in")
