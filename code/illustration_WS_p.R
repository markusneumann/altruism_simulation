library(igraph)
library(ggplot2)

#Simulate 1000 networks with varying values of p
#Then calculate measures of centrality
nWsSims <- 1000
degreeCentrality <- list()
closenessCentrality <- list()
eigenvectorCentrality <- list()
betweennessCentrality <- list()
closenessCentralityVar <- list()
eigenvectorCentralityVar <- list()
betweennessCentralityVar <- list()
Modularity <- list()
p.Val <- seq(0.05, 1, length.out = nWsSims)
for(i in 1:nWsSims){
  ws <- watts.strogatz.game(dim = 1, size = 300, nei = 5, p = p.Val[i])
  while(any(degree(ws)==0)){ws <- watts.strogatz.game(dim = 1, size = 300, nei = 5, p = p.Val[i])}
  degreeCentrality[[i]] <- centr_degree(ws)$centralization
  closenessCentrality[[i]] <- centr_clo(ws, mode="all")$centralization
  eigenvectorCentrality[[i]] <- centr_eigen(ws, directed=FALSE)$centralization
  betweennessCentrality[[i]] <- centr_betw(ws)$centralization
  ######### variance in centrality measures
  closenessCentralityVar[[i]] <- var(centr_clo(ws, mode="all")$res)
  eigenvectorCentralityVar[[i]] <- var(centr_eigen(ws, directed=FALSE)$vector)
  betweennessCentralityVar[[i]] <- var(centr_betw(ws)$res)
  ######### modularity (i.e. degree of clustering into communities)
  Modularity[[i]] <- modularity(ws, 1:300)
}

df_cent <- data.frame(degreeCentrality = unlist(degreeCentrality),
                      closenessCentrality = unlist(closenessCentrality),
                      eigenvectorCentrality = unlist(eigenvectorCentrality),
                      betweennessCentrality = unlist(betweennessCentrality),
                      closenessCentralityVar = unlist(closenessCentralityVar),
                      eigenvectorCentralityVar = unlist(eigenvectorCentralityVar),
                      betweennessCentralityVar = unlist(betweennessCentralityVar),
                      Modularity = unlist(Modularity),
                      p = p.Val)

#Measures of centrality
ggplot(df_cent, aes(x = p, y = degreeCentrality)) + geom_point()
ggplot(df_cent, aes(x = p, y = closenessCentrality)) + geom_point() #most appropriate for p in Watts-Strogatz
ggplot(df_cent, aes(x = p, y = eigenvectorCentrality)) + geom_point()
ggplot(df_cent, aes(x = p, y = betweennessCentrality)) + geom_point()
#Variance
ggplot(df_cent, aes(x = p, y = closenessCentralityVar)) + geom_point()
ggplot(df_cent, aes(x = p, y = eigenvectorCentralityVar)) + geom_point()
ggplot(df_cent, aes(x = p, y = betweennessCentralityVar)) + geom_point()
#Modularity
ggplot(df_cent, aes(x = p, y = Modularity)) + geom_point() + xlab("p (Rewiring probability)") + theme_bw()
ggsave(filename="./figures/illustrateWS_p_modularity.pdf", width = 8, height = 6, dpi = 100, units = "in")
