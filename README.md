# Replication Materials
This repository contains the replication materials for my JASSS submission: 'Indirect Reciprocity with Contagious Reputation in Large-Scale Small-World Networks'.

The figures in the paper are produced with the following scripts located in the code folder:

Figure 1  -- illustration_Contagion.R  
Figure 2  -- illustration_WS_p.R  
Figure 3  -- illustration_NetworkAltruismColor.R  

Figure 4  -- analysis_default_p02.R  
Figure 5  -- analysis_default.R  
Figure 6  -- analysis_default_vary_p.R  
Figure 7  -- analysis_N20000_truthful_new.R  
Figure 8  -- 6 scripts: analysis_truthful_new_vary_p.R, etc., then the plot is made in illustration_vary_mechanisms.R  

Figure 9  -- analysis_default_vary_nei.R  
Figure 10 -- analysis_default_vary_cb.R  
Figure 11 -- analysis_cb1_10_vary_p.R  
Figure 12 -- illustration_10000generations.R  
Figure 13 -- analysis_N20000_truthful_new.R  

The code for the simulation itself resides in Neumann_Altruism_Simulation.R. The scripts listed above set the simulation parameters and then source this file. The results are saved in the results folder. Since many of the simulations require a large amount of memory and can take several days to run, the contents of the results folder are included in this repository. The code to make the figures is sourced from analysisFunctions.R. Figures are, by default, saved in the figures folder.

[CoMSES version](https://www.comses.net/codebases/41bc083e-b8d3-4133-83ed-150bb63a4b39/releases/1.0.0/) of this repository (identical code, but without the figures and the paper).
