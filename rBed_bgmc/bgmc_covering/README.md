# Description

> Minimum set covering problems arise in a number of do- mains. In logistics, the context includes market analysis, crew scheduling, emergency services, etc. Electronic de- sign automation deals with logic minimization, technology mapping, and FSM optimization. In bioinformatics, com- bining Chromatin ImmunoPrecipitation (ChIP) with DNA sequencing to identify the binding sites of DNA-associated proteins leads to formulation of the motif selection prob- lem, mapped to a variant of the set cover problem.

The major point of our paper is to use greedy algorithm to solve the set cover problem, and use a stochastic heuritics and isomorph concepts to improve performance. bgmc_covering contains all necessary functions to support experiments in our paper.

## Instruction
Once we install **__init_rBed_01.R** in our home directory, we can proceed the following commands in R shell or RStudio. These functions support all figures and tables in our experiments shown in section **Maximum Matching in Bipartite Graphs** in our paper.

```
  source("~/__init_rBed_01.R")
  rBedPath = glob[["rBedPath"]]  
  glob[["workDir"]] = paste0(rBedPath, "/rBed_bgmc", "/bgmc_covering", "/workDir") 
  setwd(glob[["workDir"]]) 
  instancePath = "../../../_data/bigraph/unate/steiner3"
  instanceDef = file.path(instancePath, "steiner3_009_12.cnfU")
  
  unate_init_greedy(instanceDef, greedyId="chvatal_S")
  unate_greedy_chvatal()
  
  unate_greedy_chvatal_experiment(instanceDef, replicateSize = 100, greedyId="chvatal_S")
  unate_greedy_chvatal_experiment_distr(instanceDef, replicateSize = 10, greedyId="chvatal_S")
  unate_greedy_chvatal_experiment_F_iso(instanceDef, replicateSize = 10)
  unate_greedy_chvatal_UB(instanceDef)
```

# Chvatal Greedy Algorithm
In our program, we use Chvatal Greedy Algorithm to solve the set cover problem. The main idea is to treat an instance as a matrix with n columns and m rows. Each column is assigned with a weight, w. The problem is to find minimum weights of columns such that these columns cover all rows. For more detailed information, please take a look at our paper!

# Heuristics
The reason why Chvatal Greedy Algorithm needs heuritics to improve the performance is because when there are multiple same value of price per element (refering to the pseudocode in paper), if we always pick the first one, the performance can be limited. In our paper, we developed two heuristics, in terms of stochastic version and isomorph version. In short words, the stochastic heuristic uses random number to select the column, and isomorph heuristic reorders the matrix in order and pick the first column if mutiple same values are found. In our paper, there is a more detailed version of heuristic comparision, and you are very welcome to take a look or share any thoughts!

# Limitation
For the current version of movieLib in R, we need to download the entire rBed_01 and place it on our Desktop. Mac users can easily install, while other users may need to make some modifications to the code, specifically, correcting the path in **__init_rBed_01.R** from home directory and **__lib_R_resources.R** from _lib_R in rBed_01, so that the program will execute correctly.
