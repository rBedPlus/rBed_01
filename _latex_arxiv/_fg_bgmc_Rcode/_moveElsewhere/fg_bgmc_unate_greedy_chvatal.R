all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  rm(list=ls())
  workDir = "~/DeskTop/OPUS2_2021_bgmc_Li/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
  fg_bgmc_unate_greedy_chvatal_abcd()
  
  rm(list=ls())
  workDir = "~/DeskTop/OPUS2_2021_bgmc_Li/github/_latex/_Figures/fg_bgmc/workDir" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data/bigraph/unate/steiner3/"
  instanceDef = file.path(data_dir, "steiner3_009_12.cnfU")
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
  unate_init_greedy(instanceDef, greedyId="chvatal_S")
  unate_greedy_chvatal()
  
  rm(list=ls())
  workDir = "~/DeskTop/OPUS2_2021_bgmc_Li/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data/bigraph/unate/steiner3/"
  instanceDef = file.path(data_dir, "steiner3_009_12.cnfU")
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
  unate_greedy_chvatal_experiment(instanceDef, replicateSize = 10, greedyId="chvatal_S")
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data/bigraph/unate/steiner3/"
  instanceDef = file.path(data_dir, "steiner3_009_12.cnfU")
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
  unate_greedy_chvatal_experiment_distr(instanceDef, replicateSize = 10, greedyId="chvatal_S")
  
} # all_tests = function()


#-----plot-----
fg_bgmc_unate_greedy_chvatal_abcd = function() {
  
  # school_9_16.cnfW
  dt1 = data.table(
    x = c(1,    1, 1.05,  1.05, 1.15, 1.15, 1.40),
    y = c(0, 296,     0,   486,    0,  219, 486)/1000
  )
  start_dt1 = unique(dt1, by = "x", fromLast = F)
  end_dt1 = unique(dt1, by = "x", fromLast = T)
  merge_dt1 = merge(start_dt1, end_dt1, by = "x")
  
  sp1_tb = data.table(instance = "school_9_16.cnfW", solver = "chvatal_stochastic", 
                      replicateSize = 1000, numColumns=9)
  
  sp1 = ggplot(data=merge_dt1, aes(x=x))  +
    geom_segment(aes(x = x, y = y.x, xend = x, yend = y.y), col="red", lwd=1) + 
    geom_point(y=merge_dt1$y.x) + 
    geom_point(y=merge_dt1$y.y) +
    labs(x="ratio = valueGreedy/bestKnownValue", y="empirical distribution",
         title = "(a)")+
    theme(plot.title = element_text(size=10, hjust = 0.5), legend.title = element_blank()) +
    annotation_custom(tableGrob(t(sp1_tb)), xmin = 1.237, ymin=0.322,)
  
  # school_19_20.cnfW
  dt2 = data.table(
    x = c(1,    1, 1.0435,  1.0435, 1.0870, 1.0870, 1.1304, 1.1304, 1.1739, 1.1739, 1.2174, 1.2174, 1.2609, 1.2609, 1.3043, 1.3043, 1.3478, 1.3478,    1.40),
    y = c(0,   66,      0,      12,     0,     190,      0,    182,      0,    185,      0,    148,      0,    172,      0,     41,      0,      5,     190)/1000
  )
  start_dt2 = unique(dt2, by = "x", fromLast = F)
  end_dt2 = unique(dt2, by = "x", fromLast = T)
  merge_dt2 = merge(start_dt2, end_dt2, by = "x")
  
  sp2_tb = data.table(instance = "school_19_20.cnfW", solver = "chvatal_stochastic", 
                      replicateSize = 1000, numColumns = 19)
  
  sp2 = ggplot(data=merge_dt2, aes(x=x))  +
    geom_segment(aes(x = x, y = y.x, xend = x, yend = y.y), col="red", lwd=1) + 
    geom_point(y=merge_dt2$y.x) + 
    geom_point(y=merge_dt2$y.y) +
    labs(x="ratio = valueGreedy/bestKnownValue", y="empirical distribution",
         title = "(b)")+
    theme(plot.title = element_text(size=10, hjust = 0.5), legend.title = element_blank()) +
    annotation_custom(tableGrob(t(sp2_tb)), xmin = 1.23, ymin=0.123)
  
  space=paste(rep(" ", 170), collapse = "")
  p1 = grid.arrange(sp1, sp2, nrow=1,
                    top = textGrob(paste("fg_bgmc_unate_greedy_chvatal_a",space ,
                                         "fg_bgmc_unate_greedy_chvatal_b"),
                                   x = 0.04, y = -1, hjust = 0,
                                   gp=gpar(fontsize=8,font=1, col="gray47")))
  
  ggsave("fg_bgmc_unate_greedy_chvatal_ab.pdf", p1, width = 14, height = 3.5)
  
  # scpb1.cnfW
  dt1 = data.table(
    x = c(1.0435,1.0435,1.0580,1.0580,1.0725,1.0725,1.0870,1.0870,1.1014,1.1014,
          1.1159,1.1159,1.1304,1.1304,1.1449,1.1449,1.1594,1.1594,1.1739,1.1739,
          1.1884,1.1884,1.2029,1.2029,1.2174,1.2174,1.40),
    y = c(0,31,0,97,0,213,0,131,0,185,0,154,0,103,0,36,0,25,0,13,0,8,0,2,0,3,213)/1000
  )
  start_dt1 = unique(dt1, by = "x", fromLast = F)
  end_dt1 = unique(dt1, by = "x", fromLast = T)
  merge_dt1 = merge(start_dt1, end_dt1, by = "x")
  
  sp1_tb = data.table(instance = "scpb1.cnfW", solver = "chvatal_stochastic", 
                      replicateSize = 1000, numColumns=3000)
  
  sp1 = ggplot(data=merge_dt1, aes(x=x))  +
    geom_segment(aes(x = x, y = y.x, xend = x, yend = y.y), col="red", lwd=1) + 
    geom_point(y=merge_dt1$y.x) + 
    geom_point(y=merge_dt1$y.y) +
    labs(x="ratio = valueGreedy/bestKnownValue", y="empirical distribution",
         title = "(c)")+
    theme(plot.title = element_text(size=10, hjust = 0.5), legend.title = element_blank()) +
    annotation_custom(tableGrob(t(sp1_tb)), xmin = 1.237, ymin=0.138)+
    xlim(1, 1.4)
  
  # scpd1.cnfW
  dt2 = data.table(
    x = c(1.1000,1.1000,1.1167,1.1167,1.1333,1.1333,1.1500,1.1500,1.1667,1.1667,
          1.1833,1.1833,1.2000,1.2000,1.2167,1.2167,1.2333,1.2333,1.2500,1.2500,
          1.2667,1.2667,1.4),
    y = c(0,2,0,4,0,63,0,158,0,196,0,227,0,183,0,114,0,42,0,10,0,2,227)/1000
  )
  start_dt2 = unique(dt2, by = "x", fromLast = F)
  end_dt2 = unique(dt2, by = "x", fromLast = T)
  merge_dt2 = merge(start_dt2, end_dt2, by = "x")
  
  sp2_tb = data.table(instance = "scpd1.cnfW", solver = "chvatal_stochastic", 
                      replicateSize = 1000, numColumns = 4000)
  
  sp2 = ggplot(data=merge_dt2, aes(x=x))  +
    geom_segment(aes(x = x, y = y.x, xend = x, yend = y.y), col="red", lwd=1) + 
    geom_point(y=merge_dt2$y.x) + 
    geom_point(y=merge_dt2$y.y) +
    labs(x="ratio = valueGreedy/bestKnownValue", y="empirical distribution",
         title = "(d)")+
    theme(plot.title = element_text(size=10, hjust = 0.5), legend.title = element_blank()) +
    annotation_custom(tableGrob(t(sp2_tb)), xmin = 1.237, ymin=0.145) +
    xlim(1, 1.4)
  
  space=paste(rep(" ", 170), collapse = "")
  p1 = grid.arrange(sp1, sp2, nrow=1,
                    top = textGrob(paste("fg_bgmc_unate_greedy_chvatal_c",space ,
                                         "fg_bgmc_unate_greedy_chvatal_d"),
                                   x = 0.04, y = -1, hjust = 0,
                                   gp=gpar(fontsize=8,font=1, col="gray47")))
  
  ggsave("fg_bgmc_unate_greedy_chvatal_cd.pdf", p1, width = 14, height = 3.5)
  
}


#----functions-----
unate_init_greedy = function(instanceDef, greedyId="", valueTarget="",
                             replicateSize="", seedInit="") {
  
  # initialization for chvatal greedy algo
  thisFunction = "unate_init_greedy"
  
  # read instance and continue to initalize glob
  readId = ""
  if (greedyId == "chvatal_S") {
    readId = "matrixS"
  } else if (greedyId == "chvatal_F") {
    readId = "matrixF"
  } else {
    errorMsg = paste("\n .. ERROR from ", thisFunction, 
                     "\n.. greedyId does not match: 1. chvatal_S ; 2. chvatal_F\n")
    stop(errorMsg)
  }
  glob[["greedyId"]] = greedyId  
  glob[["matrixId"]] = readId
  
  start_time = Sys.time()
  read_bgu(instanceDef, readId)  # READ FILE
  end_time = Sys.time() 
  runtime  = round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  glob[["runtime_read"]] = runtime
  
  instanceName  = basename(instanceDef)
  glob[["instanceDef"]]  = instanceDef
  glob[["instanceName"]] = basename(instanceDef)
  
  nCols           = glob[["nCols"]] 
  nBits           = nCols
  glob[["nBits"]] = nBits
  coordSizeLmt           =  2^nBits      ;#   => population size!!
  glob[["coordSizeLmt"]] =  coordSizeLmt ;#  print(glob)
  
  # if (greedyId == "") {
  #   greedyId = "random"
  # } 
  
  if (valueTarget == "") {
    valueTarget = as.numeric(bg_find_target_unate(instanceDef)$BKV[1])
  }
  glob[["valueTarget"]] =  valueTarget  
  
  if (replicateSize == "") {
    replicateSize = 0
  }
  glob[["replicateSize"]]     = replicateSize  
  sampleFractionLmt           = prettyNum(replicateSize/coordSizeLmt)
  glob[["sampleFractionLmt"]] = sampleFractionLmt
  
  if (seedInit == "") {
    seedInit = 0
  }
  glob[["seedInit"]]      = seedInit
  glob[["seedInitFirst"]] = seedInit
  glob[["replicaId"]] = seedInit
  set.seed(seedInit)
  
} # unate_init_greedy

unate_greedy_chvatal = function() {
  
  # main Chvatal algorithm that retrieves parameters from glob
  thisFunction = "unate_greedy_chvatal" 
  
  # the best full matrix version of chvatal 
  valueTarget    = glob[["valueTarget"]]
  replicateSize  = glob[["replicateSize"]]
  greedyId       = glob[["greedyId"]]
  n              = glob[["nCols"]]
  m              = glob[["mRows"]]
  coord          = rep(0, n)
  M              = glob[[ glob[["matrixId"]] ]]
  colWeights     = glob[["colWeights"]]
  replicaId      = glob[["replicaId"]]
  
  
  glob[["nOps"]] = 0
  
  start_time = Sys.time() 
  
  while(TRUE) {
    percentages = colWeights / colSums(M) 
    if (all(percentages==Inf)) { break }
    if (replicaId == 0) {
      jdx     = which.min(percentages)
    } else {
      jdx_vec = which(percentages == min(percentages))
      jdx_cnt = sample(1:length(jdx_vec))[1]
      jdx     = jdx_vec[jdx_cnt]         
    }
    rem_vec        = which(M[,jdx] %in% 1)
    M[rem_vec,]    = 0
    coord[jdx]     = 1
    glob[["nOps"]] = glob[["nOps"]] + 1
  }
  
  end_time = Sys.time() 
  runtime = round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)
  valueGreedy = as.numeric(t(coord) %*% colWeights)
  coord = paste(coord, collapse = "")
  diff = valueGreedy - valueTarget  
  
  glob[["valueGreedy"]] = valueGreedy
  
  dtPairs = data.table(
    seedInit           = glob[["seedInit"]],
    nOps               = glob[["nOps"]],
    runtime            = runtime, 
    nOpsRate           = glob[["nOps"]]/runtime,
    valueTarget        = valueTarget,
    valueGreedy        = valueGreedy,
    diff               = diff,
    coordGreedy        = coord)
  
  return(list(dt = dtPairs))
} #unate_greedy_chvatal

unate_greedy_chvatal_experiment = function(
  instanceDef, greedyId="chvatal_F", valueTarget="", replicateSize="", 
  isSeedConsecutive=T) 
{
  # given as single instanceDef, find all necessary info for chvatal algo with replication
  
  thisFunction = "unate_greedy_chvatal_experiment"
  
  cat("\n.. ENTERING", thisFunction,  
      "\n        greedy unate cover solutions for", greedyId,
      "\n                  instance     =", basename(instanceDef),
      "\n             replicateSize     =", replicateSize,
      "\n             isSeedConsecutive =", isSeedConsecutive,
      "\n                      date     =",  date(), "\n\n")
  
  clear(glob)
  
  
  start_time_main = Sys.time() 
  unate_init_greedy(
    instanceDef, greedyId=greedyId, valueTarget=valueTarget, 
    replicateSize=replicateSize) 
  
  
  valueTarget    = glob[["valueTarget"]]
  runtime_read = glob[["runtime_read"]]
  replicateSize = glob[["replicateSize"]]
  
  if (is.na(valueTarget) || is.na(runtime_read) || is.na(replicateSize)) {
    errorMsg = paste("\n .. ERROR from ", thisFunction, 
                     "\n.. glob initialization has errors, please check unate_init_greedy function\n")
    stop(errorMsg)
  }
  
  dt = data.table()
  
  for (replicaId in 0:replicateSize) {
    
    glob[["replicaId"]] = replicaId
    if (isSeedConsecutive) {
      seedInit = replicaId 
    } else {
      seedInit = trunc(1e6*runif(1)) 
    }
    if (replicaId != 0) {
      set.seed(seedInit)
    } else {
      seedInit = NA
    }
    glob[["seedInit"]] = seedInit 
    
    answ = unate_greedy_chvatal() 
    
    runtime     = answ$dt$runtime
    valueGreedy = answ$dt$valueGreedy
    coordGreedy = answ$dt$coordGreedy
    diff        = answ$dt$diff
    matrixId = glob[["matrixId"]]
    
    dt = rbind(dt, list(
      instanceName = basename(instanceDef),
      matrixId = matrixId,
      valueTarget = valueTarget,
      replicaId = replicaId,
      seedInit  = seedInit,
      nOps = glob[["nOps"]],
      runtime_read = runtime_read,
      runtime = runtime,
      nOpsRate = round(glob[["nOps"]]/runtime, 3),
      valueGreedy = valueGreedy,
      diff = diff,
      coordGreedy = coordGreedy)
    )
    
  } # for (replicaId in 0:replicateSize)
  
  cat("\n.. COMPLETING", thisFunction, "\n")
  
  valueGreedy_min = min(dt$valueGreedy)
  ratioBest       = round(valueGreedy_min/valueTarget, 3)
  valueGreedy_max = max(dt$valueGreedy)
  ratioWorst      = round(valueGreedy_max/valueTarget, 3)
  
  coordUniqSize  = nrow(unique(dt, by = "coordGreedy"))
  coordDuplSize  = replicateSize + 1 - coordUniqSize
  
  coordBest = dt[valueGreedy == valueGreedy_min]
  coordBestUniq = unique(coordBest, by = "coordGreedy")
  
  coordBestSize      = nrow(coordBest)
  coordBestSizeUniq  = nrow(coordBestUniq)
  coordBestSizeDupl  = coordBestSize - coordBestSizeUniq
  
  coordBest_first = coordBest[1,]$coordGreedy
  
  replicaId_best = dt[valueGreedy == valueGreedy_min]$replicaId
  replicaId_best_size = length(replicaId_best)
  fraction_best = round(replicaId_best_size/replicateSize, 4)
  
  replicaId_worst = dt[valueGreedy == valueGreedy_max]$replicaId
  replicaId_worst_size = length(replicaId_worst)
  fraction_worst = round(replicaId_worst_size/replicateSize, 4)
  
  coord_best_uniq = coordBestUniq$coordGreedy
  coord_best_uniq_size = length(coord_best_uniq)
  
  valueGreedy_min    = round(valueGreedy_min, 2)
  valueGreedy_median = round(median(dt$valueGreedy), 2)
  valueGreedy_mean   = round(mean(dt$valueGreedy), 3)
  valueGreedy_stDev  = round(sd(dt$valueGreedy), 3)
  valueGreedy_max    = round(valueGreedy_max, 2)
  
  valueGreedy_stats  = paste(c(
    valueGreedy_min, valueGreedy_median, valueGreedy_mean,
    valueGreedy_stDev, valueGreedy_max), collapse=",") 
  
  end_time_main = Sys.time() 
  runtime_main = round(as.numeric(difftime(end_time_main, start_time_main, units = "secs")), 3)
  
  
  dtPairs = data.table(
    thisFunction = thisFunction,
    instanceDef  = basename(instanceDef),
    nCols        = glob[["nCols"]],
    mRows        = glob[["mRows"]],
    colWeights_min = min(glob[["colWeights"]]),
    colWeights_max = max(glob[["colWeights"]]),
    colWeights_sum = sum(glob[["colWeights"]]),
    matrixDens     = glob[["matrixDens"]],
    matrixSize     = glob[["matrixSize"]],
    matrixId       = matrixId,
    greedyId       = greedyId,
    "----" = "",
    userId    = Sys.info()[["user"]], 
    cpuName   = Sys.info()[["nodename"]], 
    sysName   = Sys.info()[["sysname"]],
    dateStamp = Sys.Date(),
    timeStamp = format(Sys.time(), "%X"),
    "----" = "",
    isSeedConsecutive  = isSeedConsecutive,
    replicateSize      = replicateSize,
    coordDuplSize      = coordDuplSize,
    coordUniqSize      = coordUniqSize,
    valueTarget        = valueTarget,
    ratioBest          = ratioBest,
    valueGreedy_min    = valueGreedy_min,
    valueGreedy_median = valueGreedy_median,
    valueGreedy_mean   = valueGreedy_mean,
    valueGreedy_stDev  = valueGreedy_stDev,
    valueGreedy_max    = valueGreedy_max,
    ratioWorst         = ratioWorst,
    valueGreedy_stats  = valueGreedy_stats,
    "----" = "",
    runtime_main   = round(runtime_main, 3),
    runtime_read   = round(runtime_read, 3),
    runtime_min    = round(min(dt$runtime), 3),
    runtime_median = round(median(dt$runtime), 3),
    runtime_mean   = round(mean(dt$runtime), 3),
    runtimeS_stDev = round(sd(dt$runtime), 3),
    runtime_max    = round(max(dt$runtime), 3),
    "----" = "",
    coordBestSize     = coordBestSize,
    coordBestSizeDupl = coordBestSizeDupl,
    coordBestSizeUniq = coordBestSizeUniq,
    valueBest         = valueGreedy_min)
    # coordBest_first   = coordBest_first)
  
  print(t(dtPairs))
  # 
  # 
  # cat("\nvalueGreedy_best =",valueGreedy_min, 
  #     "; replicaId_best_size =", replicaId_best_size, 
  #     "; fraction_best =", fraction_best, 
  #     "\nreplicaId_best =",
  #     "\n")
  # print(replicaId_best)
  # 
  # 
  # cat("\nvalueGreedy_worst =",valueGreedy_max, 
  #     "; fraction_worst =", fraction_worst,
  #     "\nreplicaId_worst =",
  #     "\n")
  # print(replicaId_worst)
  # 
  # 
  # cat("\ncoord_best_uniq_size =", coord_best_uniq_size, 
  #     "\ncoord_best_uniq=", 
  #     "\n") 
  # print(coord_best_uniq)
  
  # return(dtPairs)
  
} # unate_greedy_chvatal_experiment

unate_greedy_chvatal_experiment_distr = function(
  instanceDef, greedyId="chvatal_F", valueTarget="", replicateSize="",
  isSeedConsecutive=T) 
{
  # given as single instanceDef, find distribution for chvatal algo with replication
  
  thisFunction = "unate_greedy_chvatal_experiment_distr"
  
  cat("\n.. ENTERING", thisFunction,  
      "\n        greedy unate cover solutions for", greedyId,
      "\n                  instance     =", basename(instanceDef),
      "\n             replicateSize     =", replicateSize,
      "\n             isSeedConsecutive =", isSeedConsecutive,
      "\n                      date     =",  date(), "\n\n")
  #
  clear(glob)
  unate_init_greedy(
    instanceDef, greedyId=greedyId, valueTarget=valueTarget, 
    replicateSize=replicateSize) 
  
  valueTarget    = glob[["valueTarget"]]
  runtime_read = glob[["runtime_read"]]
  replicateSize = glob[["replicateSize"]]
  
  if (is.na(valueTarget) || is.na(runtime_read) || is.na(replicateSize)) {
    errorMsg = paste("\n .. ERROR from ", thisFunction, 
                     "\n.. glob initialization has errors, please check unate_init_greedy function\n")
    stop(errorMsg)
  }
  
  dt = data.table()
  
  for (replicaId in 0:replicateSize) {
    
    glob[["replicaId"]] = replicaId
    if (isSeedConsecutive) {
      seedInit = replicaId 
    } else {
      seedInit = trunc(1e6*runif(1)) 
    }
    if (replicaId != 0) {
      set.seed(seedInit)
    } else {
      seedInit = NA
    }
    glob[["seedInit"]] = seedInit 
    
    answ = unate_greedy_chvatal() ;# print(answF) 
    
    runtime     = answ$dt$runtime
    valueGreedy = answ$dt$valueGreedy
    coordGreedy = answ$dt$coordGreedy
    diff        = answ$dt$diff
    ratio       = round(valueGreedy/valueTarget, 4)
    
    dt = rbind(dt, list(
      instanceName = basename(instanceDef),
      valueTarget = valueTarget,
      replicaId = replicaId,
      seedInit  = seedInit,
      nOps = glob[["nOps"]],
      runtime_read = runtime_read,
      runtime = runtime,
      nOpsRate = round(glob[["nOps"]]/runtime, 3),
      valueGreedy = valueGreedy,
      diff = diff,
      ratio = ratio,
      coordGreedy = coordGreedy)
    )
    
  } # for (replicaId in 0:replicateSize)
  
  cat("\n.. COMPLETING", thisFunction, "\n")
  
  valueGreedy_distr = sort(unique(dt$ratio))
  print(valueGreedy_distr)   
  
  hist = data.table()
  for (ratio in valueGreedy_distr) {
    ratio_cnt = length(dt$ratio[dt$ratio == ratio])
    hist = rbind(hist, list(
      ratio = ratio,
      ratio_cnt = ratio_cnt
    ))
  }
  print(hist)
  return(thisFunction)
  
} # unate_greedy_chvatal_experiment_distr

unate_greedy_chvatal_experiment_F_iso = function(
  instanceDef,  OFname="OFb_unateF", greedyId="chvatal_F",  
  valueTarget="", replicateSize="", isSeedConsecutive=T) 
{
  #
  #
  thisFunction = "unate_greedy_chvatal_experiment_F_iso"
  #
  cat("\n.. ENTERING", thisFunction,  
      "\n        greedy unate cover solutions for", greedyId,
      "\n                  instance     =", basename(instanceDef),
      "\n             replicateSize     =", replicateSize,
      "\n             isSeedConsecutive =", isSeedConsecutive,
      "\n                      date     =",  date(), "\n\n")
  #
  clear(glob)
  unate_init_greedy(
    instanceDef, OFname=OFname, greedyId=greedyId, valueTarget=valueTarget, 
    replicateSize=replicateSize) 
  
  # print(glob); return()
  
  valueTarget    = glob[["valueTarget"]]
  runtime_read = glob[["runtime_read"]]
  
  glob[["replicateSize"]] = replicateSize
  dt = data.table()
  
  n              = glob[["nCols"]]
  M_ref          = glob[["matrixF"]]
  colWeights_ref = glob[["colWeights"]]
  replicateSize  = glob[["replicateSize"]]
  
  glob[["replicateSize"]] = replicateSize
  dt = data.table()
  coordHash = hash()  ; clear(coordHash)
  
  # print(glob) ; return()
  
  for (replicaId in 0:replicateSize) {
    
    glob[["replicaId"]] = replicaId
    if (isSeedConsecutive) {
      seedInit = replicaId 
      # this choice is as good as random choice of seed below
    } else {
      seedInit = trunc(1e6*runif(1))  
    }
    if (replicaId == 0) {seedInit = 0}
    glob[["seedInit"]] = seedInit 
    
    if (replicaId == 0) {
      
      glob[["seedInit"]] = NA
      coordPV_ref = 1:n # reference permutation (natural order)
      coordP_ref  = paste(coordPV_ref, collapse=",")
      answF       = unate_greedy_chvatal() ;# print(answF)
      
    } else {
      
      glob[["seedInit"]] = seedInit
      set.seed(seedInit)
      coordPV_iso = sample(coordPV_ref) ;# permute coordPV_ref
      coordP_iso  = paste(coordPV_iso, collapse=",")
      
      M = M_ref
      colWeights = colWeights_ref
      
      # create an isomorph of M_ref
      # THANKS!! https://www.r-bloggers.com/2009/03/r-tips-swapping-columns-in-a-matrix/
      for (idx in seq_len(n)) {
        i = idx ; j = coordPV_iso[idx] 
        M[ , c(i,j)]  <- M[ , c(j,i)]
        colWeights[c(i,j)] = colWeights[c(j,i)]
      }
      # print(M)
      # re-initialize  as an isomorph of M_ref
      glob[["matrixF"]] = M  
      glob[["colWeights"]] = colWeights
      answF = unate_greedy_chvatal()
    }
    
    runtime     = answF$dt$runtime
    valueGreedy = answF$dt$valueGreedy
    diff        = answF$dt$diff
    coordGreedy = answF$dt$coordGreedy
    coordHash[[coordGreedy]] = ""
    
    dt = rbind(dt, list(
      instanceName = basename(instanceDef),
      valueTarget = valueTarget,
      replicaId = replicaId,
      seedInit  = seedInit,
      nOps = glob[["nOps"]],
      runtime_read = runtime_read,
      runtime = runtime,
      nOpsRate = round(glob[["nOps"]]/runtime, 3),
      valueGreedy = valueGreedy,
      diff = diff,
      coordGreedy = coordGreedy)
    )
    
  } # for (replicaId in 0:replicateSize)
  # print(dt, nrow=101) ;# print(coordHash)
  
  cat("\n.. COMPLETING", thisFunction, "\n")
  # print(dt)
  
  valueGreedy_min = min(dt$valueGreedy)
  ratioBest       = round(valueGreedy_min/valueTarget, 3)
  valueGreedy_max = max(dt$valueGreedy)
  ratioWorst      = round(valueGreedy_max/valueTarget, 3)
  
  valueGreedy_min    = round(valueGreedy_min, 2)
  valueGreedy_median = round(median(dt$valueGreedy), 2)
  valueGreedy_mean   = round(mean(dt$valueGreedy), 3)
  valueGreedy_stDev  = round(sd(dt$valueGreedy), 3)
  valueGreedy_max    = round(valueGreedy_max, 2)
  
  valueGreedy_stats  = paste(c(
    valueGreedy_min, valueGreedy_median, valueGreedy_mean,
    valueGreedy_stDev, valueGreedy_max), collapse=",") 
  
  coordUniqSize  = length(coordHash)
  coordDuplSize  = replicateSize + 1 - coordUniqSize
  
  coordBest = dt[valueGreedy == valueGreedy_min]
  # print(coordBest$coordGreedy_F)
  coordBestHash = hash() ; clear(coordBestHash)
  # h = hash(keys=letters, values="")
  # > keys(h)[1]
  # [1] "a"
  
  for (coord in coordBest$coordGreedy) {
    coordBestHash[[coord]] = ""
  }
  # print(keys(coordBestHash))
  coordBestSize      = nrow(coordBest)
  coordBestSizeUniq  = length(coordBestHash)
  coordBestSizeDupl  = coordBestSize - coordBestSizeUniq
  
  dtPairs = data.table(
    thisFunction = thisFunction,
    instanceDef  = basename(instanceDef),
    nCols        = glob[["nCols"]],
    mRows        = glob[["mRows"]],
    colWeights_min = min(glob[["colWeights"]]),
    colWeights_max = max(glob[["colWeights"]]),
    colWeights_sum = sum(glob[["colWeights"]]),
    matrixDens     = glob[["matrixDens"]],
    greedyId       = greedyId,
    "----" = "",
    userId    = Sys.info()[["user"]], 
    cpuName   = Sys.info()[["nodename"]], 
    sysName   = Sys.info()[["sysname"]],
    dateStamp = Sys.Date(),
    timeStamp = format(Sys.time(), "%X"),
    "----" = "",
    isSeedConsecutive  = isSeedConsecutive,
    replicateSize      = replicateSize,
    coordDuplSize      = coordDuplSize,
    coordUniqSize      = coordUniqSize,
    valueTarget        = valueTarget,
    ratioBest          = ratioBest,
    valueGreedy_min    = valueGreedy_min,
    valueGreedy_median = valueGreedy_median,
    valueGreedy_mean   = valueGreedy_mean,
    valueGreedy_stDev  = valueGreedy_stDev,
    valueGreedy_max    = valueGreedy_max,
    ratioWorst         = ratioWorst,
    valueGreedy_stats  = valueGreedy_stats,
    "----" = "",
    runtime_read   = round(runtime_read, 3),
    runtime_min    = round(min(dt$runtime), 3),
    runtime_median = round(median(dt$runtime), 3),
    runtime_mean   = round(mean(dt$runtime), 3),
    runtimeS_stDev = round(sd(dt$runtime), 3),
    runtime_max    = round(max(dt$runtime), 3),
    "----" = "",
    coordBestSize     = coordBestSize,
    coordBestSizeDupl = coordBestSizeDupl,
    coordBestSizeUniq = coordBestSizeUniq,
    valueBest         = valueGreedy_min,
    coordBest_first   = keys(coordBestHash)[1])
  
  print(t(dtPairs))
  # print(coordBestHash)
  
  valueGreedy_min = min(dt$valueGreedy)
  valueGreedy_max = max(dt$valueGreedy)
  
  replicaId_best = dt[valueGreedy == valueGreedy_min]$replicaId
  replicaId_best_size = length(replicaId_best)
  fraction_best = round(replicaId_best_size/replicateSize, 4)
  cat("\n valueGreedy_best =",valueGreedy_min, 
      "; replicaId_best_size =", replicaId_best_size, 
      "; fraction_best =", fraction_best, 
      "\n")
  print(replicaId_best)
  
  replicaId_worst = dt[valueGreedy == valueGreedy_max]$replicaId
  replicaId_worst_size = length(replicaId_worst)
  fraction_worst = round(replicaId_worst_size/replicateSize, 4)
  cat("\n valueGreedy_worst =",valueGreedy_max, 
      "; fraction_worst =", fraction_worst, 
      "\n")
  print(replicaId_worst)
  return()
  
} # unate_greedy_chvatal_experiment_F_iso