all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  rm(list=ls())
  workDir = "~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/rBed_bgmc" ; setwd(workDir)
  lib_R_dir     = "../"
  source(file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/_lib_R_resources.R"))
  source(file.path(workDir, "bgmc_unate_greedy_chvatal.R" ))
  fg_bgmc_unate_greedy_chvatal_abcd()
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data/bigraph/unate/steiner3/"
  instanceDef = file.path(data_dir, "steiner3_009_12.cnfU")
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
  unate_init_greedy(instanceDef, greedyId="chvatal_S")
  unate_greedy_chvatal()
  
  rm(list=ls())
  workDir = "~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/rBed_bgmc" ; setwd(workDir)
  lib_R_dir     = "../"
  source(file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/_lib_R_resources.R"))
  instanceDef = file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_data_tiny/bigraph/unateAll/chvatal_6_5.cnfW")
  source(file.path(workDir, "bgmc_unate_greedy_chvatal.R" ))
  fg_bgmc_unate_greedy_chvatal_iso_distr_school_9_11()
  
  rm(list=ls())
  workDir = "~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/rBed_bgmc" ; setwd(workDir)
  lib_R_dir     = "../"
  source(file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/_lib_R_resources.R"))
  instanceDef = file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_data_tiny/bigraph/unateAll/chvatal_6_5.cnfW")
  source(file.path(workDir, "bgmc_unate_greedy_chvatal.R" ))
  unate_greedy_chvatal_experiment_F_iso(instanceDef, replicateSize = 10)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/rBed_bgmc" ; setwd(workDir)
  lib_R_dir     = "../"
  source(file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/_lib_R_resources.R"))
  instanceDef = file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_data/bigraph/orlib/scpb1.cnfW")
  source(file.path(workDir, "bgmc_unate_greedy_chvatal.R" ))
  unate_greedy_chvatal_experiment_distr(instanceDef, replicateSize = 10, greedyId="chvatal_S")
  
  rm(list=ls())
  workDir = "~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/rBed_bgmc" ; setwd(workDir)
  lib_R_dir     = "../"
  source(file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/_lib_R_resources.R"))
  instanceDef = file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_data/bigraph/unate/steiner3/steiner3_027_117.cnfU")
  source(file.path(workDir, "bgmc_unate_greedy_chvatal.R" ))
  unate_greedy_chvatal_experiment(instanceDef, replicateSize = 1000, greedyId="chvatal_S")
  
  
  rm(list=ls())
  workDir = "~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/rBed_bgmc" ; setwd(workDir)
  lib_R_dir     = "../"
  source(file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_lib_R/_lib_R_resources.R"))
  instanceDef = file.path("~/DeskTop/github/OPUS2_2021_bgmc_Li/_data/bigraph/orlib/scpb1.cnfW")
  source(file.path(workDir, "bgmc_unate_greedy_chvatal.R" ))
  unate_greedy_chvatal_UB(instanceDef)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/OPUS2_2021_bgmc_Li/_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
  fg_bgmc_upper_bound_steiner()
  
} # all_tests = function()

#----Functions-----
all_tests = function() {
  source("~/__init_rBed_01.R")
  rBedPath = glob[["rBedPath"]]  
  glob[["workDir"]] = paste0(rBedPath, "/rBed_bgmc", "/bgmc_covering", "/workDir") 
  setwd(glob[["workDir"]]) 
  instanceDirs = "../../../_data_tiny/bigraph/unate/" 
  
  
}
unate_greedy_chvatal_UB = function(instanceDef, valueTarget="") {
  # # Assume C stores all column degrees
  # Initialize d <- max(C)
  # harmonicNum <- H(d)
  # UB <- harmonicNum * BKV
  # Return UB
  
  thisFunction = "unate_greedy_chvatal_UB"
  
  unate_init_greedy(
    instanceDef, greedyId="chvatal_S", valueTarget=valueTarget, 
    replicateSize="")
  
  C = glob[['colDegs']]
  BKV = glob[['valueTarget']]
  d = max(C)
  harmonicNum = sum(1/seq(d))
  UB = harmonicNum * BKV
  return(UB)
  
  
} # unate_greedy_chvatal_UB


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
  
  # normalize colWeights
  normWeights = (colWeights-min(colWeights))/(max(colWeights)-min(colWeights))
  
  
  valueGreedy_norm = as.numeric(t(coord) %*% normWeights)
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
    valueGreedy_norm   = valueGreedy_norm,
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
    valueGreedy_norm = answ$dt$valueGreedy_norm
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
      valueGreedy_norm = valueGreedy_norm,
      diff = diff,
      coordGreedy = coordGreedy)
    )
    
  } # for (replicaId in 0:replicateSize)
  
  cat("\n.. COMPLETING", thisFunction, "\n")
  
  valueGreedy_min = min(dt$valueGreedy)
  ratioBest       = round(valueGreedy_min/valueTarget, 3)
  valueGreedy_max = max(dt$valueGreedy)
  ratioWorst      = round(valueGreedy_max/valueTarget, 3)
  
  valueGreedy_norm_min = min(dt$valueGreedy_norm)
  valueGreedy_norm_max = max(dt$valueGreedy_norm)
  
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
  
  valueGreedy_norm_min    = round(valueGreedy_norm_min, 2)
  valueGreedy_norm_median = round(median(dt$valueGreedy_norm), 2)
  valueGreedy_norm_mean   = round(mean(dt$valueGreedy_norm), 3)
  valueGreedy_norm_stDev  = round(sd(dt$valueGreedy_norm), 3)
  valueGreedy_norm_max    = round(valueGreedy_norm_max, 2)
  
  valueGreedy_norm_stats  = paste(c(
    valueGreedy_norm_min, valueGreedy_norm_median, valueGreedy_norm_mean,
    valueGreedy_norm_stDev, valueGreedy_norm_max), collapse=",") 
  
  h1 = sum(1/seq(glob[["mRows"]]))
  upperBound1 = valueTarget * h1
  
  colDegMax = max(glob[["colDegs"]])
  h2 = sum(1/seq(colDegMax))
  upperBound2 = valueTarget * h2
  
  h3 = valueGreedy_max
  upperBound3 = valueTarget * h3
  
  h4 = valueGreedy_norm_max
  upperBound4 = valueTarget * h4
  
  h5 = sum(1/seq(colDegMax))
  upperBound5 = valueTarget * h5
  
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
    valueGreedy_norm_stats = valueGreedy_norm_stats,
    upperBound1       = upperBound1,
    upperBound2       = upperBound2,
    upperBound3       = upperBound3,
    upperBound4       = upperBound4,
    upperBound5        = upperBound5,
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
    instanceDef, greedyId=greedyId, valueTarget=valueTarget, 
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
      
      
      print(coordP_ref)
      
      print(glob[["matrixF"]])
      
    } else {
      
      glob[["seedInit"]] = seedInit
      set.seed(seedInit)
      coordPV_iso = sample(coordPV_ref) ;# permute coordPV_ref
      coordP_iso  = paste(coordPV_iso, collapse=",")
      
      print(coordP_iso)
      
      M = M_ref
      colWeights = colWeights_ref
      
      # create an isomorph of M_ref
      # THANKS!! https://www.r-bloggers.com/2009/03/r-tips-swapping-columns-in-a-matrix/
      # for (idx in 1:n) {
      #   
      #   i = idx ; j = coordPV_iso[idx]
      #   print(j)
      #   M[ , c(i,j)]  <- M[ , c(j,i)]
      #   colWeights[c(i,j)] = colWeights[c(j,i)]
      # }
      M = M[,coordPV_iso]
      print(M)
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

# fg_bgmc_unate_school_9_16.R   # MUST be sourced from github/_rlibR/_lib_R_resources.R



#----Tables----
tb_unate_greedy_asym = function(instanceDir, 
                                outputFolder,
                                greedyId="chvatal_S", 
                                valueTarget="",
                                replicateSize="",
                                isSeedConsecutive=T 
) {
  # generate RDS file that contains all necessary info processed by chvatal algo
  thisFunction = "tb_unate_greedy_asym"
  
  data_name = str_split(instanceDir,"/")[[1]]
  set_name = tail(data_name[data_name!=""], n=1)
  files = c()
  for (dir in instanceDir) {
    files = c(files, list.files(instanceDir, pattern=".*.cnf"))
  }
  
  
  dt = data.table()
  for (file in files) {
    
    instanceDef = paste(instanceDir, file,sep = "")
    print(instanceDef)
    # 
    # unate_init_greedy(
    #   instanceDef, greedyId=greedyId, valueTarget="", 
    #   replicateSize=replicateSize) 
    # runtime_read = glob[["runtime_read"]]
    
    upperList = tb_unate_greedy_chvatal_experiment_bound(instanceDef, greedyId = greedyId,)
    runtime_read = glob[["runtime_read"]]
    
    print(upperList$upperBound)
    
    
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
      
      dt = rbind(dt, list(
        instanceDef = basename(instanceDef),
        nCols       = glob[['nCols']],
        mRows       = glob[['mRows']],
        colDeg_min  = min(glob[["colDegs"]]),
        colDeg_max  = max(glob[["colDegs"]]),
        rowDeg_min  = min(glob[["rowDegs"]]),
        rowDeg_max  = max(glob[["rowDegs"]]),
        colWeights_min = min(glob[["colWeights"]]),
        colWeights_max = max(glob[["colWeights"]]),
        colWeights_tot = sum(glob[["colWeights"]]),
        numEdges       = glob[['numEdges']],
        matrixDens     = glob[["matrixDens"]],
        replicaId = replicaId,
        replicateSize = replicateSize,
        seedInit  = seedInit,
        isSeedConsecutive = isSeedConsecutive,
        greedyId = glob[['greedyId']],
        runtime_read = runtime_read,
        runtime = runtime,
        nOps = glob[["nOps"]],
        userId    = Sys.info()[["user"]], 
        cpuName   = Sys.info()[["nodename"]], 
        sysName   = Sys.info()[["sysname"]],
        dateStamp = Sys.Date(),
        timeStamp = format(Sys.time(), "%X"),
        ratio = as.numeric(valueGreedy)/as.numeric(glob[['valueTarget']]),
        valueTarget = glob[['valueTarget']],
        valueGreedy = valueGreedy,
        upperBound = upperList$upperBound,
        coordGreedy = coordGreedy)
      )
    }
  }
  
  fileRDS = paste(sep="", outputFolder,"/tb_unate_greedy_asym_chvatal_",
                  set_name, ".RDS")
  
  saveRDS(dt, fileRDS)
  cat(sep="",
      "------------------------------------------------",
      "\n** saved a datatable generated by function ", thisFunction,
      "\n in the file = ", fileRDS, "\n",
      "\n Access the file with either as",
      "\n                 print(readRDS(\"", fileRDS, "\"))",
      "\n or as",
      "\n               dtExp = readRDS(\"", fileRDS, "\")",
      "\n")
  return(dt)
}


tb_unate_greedy_asymp_to_latex = function(instanceRDS, matchRDS=NA, isLatex=T) {
  
  # convert RDS file generated by tb_unate_greedy_asym into latex format table
  thisFunction = "tb_unate_greedy_asymp_to_latex"
  
  dtExp = readRDS(instanceRDS)
  
  dtFinal = dtExp[,c("instanceDef", "nCols", "mRows", "replicateSize",
                     "isSeedConsecutive", "greedyId", "runtime_read",
                     "runtime", "nOps", "cpuName", "dateStamp",
                     "ratio", "valueTarget", "valueGreedy", "upperBound",
                     "colDeg_max","matrixDens")]
  
  runtime_read_stat_vec = rep(NA, length(unique(dtFinal$instanceDef)))
  runtime_stat_vec = rep(NA, length(unique(dtFinal$instanceDef)))
  nOps_stat_vec = rep(NA, length(unique(dtFinal$instanceDef)))
  ratio_stat_vec = rep(NA, length(unique(dtFinal$instanceDef)))
  valueGreedy_stat_vec = rep(NA, length(unique(dtFinal$instanceDef)))
  
  cnt = 1
  for (instance in unique(dtFinal$instanceDef)) {
    
    dt = dtFinal[with(dtFinal, instanceDef==instance),]
    
    runtime_read_min = round(min(dt$runtime_read),2)
    runtime_read_mean = round(mean(dt$runtime_read),2)
    runtime_read_median = round(median(dt$runtime_read),2)
    runtime_read_stDev = round(sd(dt$runtime_read),2)
    runtime_read_max = round(max(dt$runtime_read),2)
    
    runtime_read_stat = paste(runtime_read_min, runtime_read_median, runtime_read_mean,
                              runtime_read_stDev, runtime_read_max, sep = ",")
    
    runtime_read_stat_vec[cnt] = runtime_read_stat
    
    runtime_min = round(min(dt$runtime),2)
    runtime_mean = round(mean(dt$runtime),2)
    runtime_median = round(median(dt$runtime),2)
    runtime_stDev = round(sd(dt$runtime),2)
    runtime_max = round(max(dt$runtime),2)
    
    runtime_stat = paste(runtime_min, runtime_median, runtime_mean,
                         runtime_stDev, runtime_max, sep = ",")
    
    runtime_stat_vec[cnt] = runtime_stat
    
    nOps_min = round(min(dt$nOps),2)
    nOps_mean = round(mean(dt$nOps),2)
    nOps_median = round(median(dt$nOps),2)
    nOps_stDev = round(sd(dt$nOps),2)
    nOps_max = round(max(dt$nOps),2)
    
    nOps_stat = paste(nOps_min, nOps_median, nOps_mean,
                      nOps_stDev, nOps_max, sep = ",")
    
    nOps_stat_vec[cnt] = nOps_stat
    
    ratio_min = format(round(min(dt$ratio),2), nsmall = 2)
    ratio_mean = format(round(mean(dt$ratio),2), nsmall = 2)
    ratio_median = format(round(median(dt$ratio),2), nsmall = 2)
    ratio_stDev = format(round(sd(dt$ratio),2), nsmall = 2)
    ratio_max = format(round(max(dt$ratio),2), nsmall = 2)
    
    ratio_stat = paste(ratio_min, ratio_median, ratio_mean,
                       ratio_stDev, ratio_max, sep = ",")
    
    ratio_stat_vec[cnt] = ratio_stat
    
    valueGreedy_min = round(min(dt$valueGreedy),2)
    valueGreedy_mean = round(mean(dt$valueGreedy),2)
    valueGreedy_median = round(median(dt$valueGreedy),2)
    valueGreedy_stDev = round(sd(dt$valueGreedy),2)
    valueGreedy_max = round(max(dt$valueGreedy),2)
    
    valueGreedy_stat = paste(valueGreedy_min, valueGreedy_median, valueGreedy_mean,
                             valueGreedy_stDev, valueGreedy_max, sep = ",")
    
    valueGreedy_stat_vec[cnt] = valueGreedy_stat
    
    cnt = cnt + 1
  }
  
  dtFinal = unique(dtFinal[,c("instanceDef", "nCols", "mRows", "replicateSize",
                              "isSeedConsecutive", "greedyId", "cpuName", "dateStamp",
                              "valueTarget", "upperBound", "colDeg_max","matrixDens")], by = "instanceDef")
  
  runtime_read_stats = runtime_read_stat_vec
  runtime_stats = runtime_stat_vec
  nOps_stats = nOps_stat_vec
  ratio_stats = ratio_stat_vec
  valueGreedy_stats = valueGreedy_stat_vec
  
  dtInfo = cbind(dtFinal, runtime_read_stats, runtime_stats, nOps_stats,
                 ratio_stats, valueGreedy_stats)
  
  if (!is.na(matchRDS)) {
    m1Tb = readRDS(matchRDS)
    dtM = m1Tb[,instanceDef, matching_size]
    print(dtM)
    print(dtInfo)
    
    dtInfo <- merge(dtInfo, dtM, by.x ="instanceDef", by.y="instanceDef")
    
    if (isLatex) {
      cat("\n...Latex format start...\n\n")
      # for (i in 1:nrow(dtInfo)) {
      line = paste(dtInfo$instanceDef, dtInfo$nCols, dtInfo$mRows,
                   format(round(dtInfo$matrixDens,4), msize=4), 
                   format(round(dtInfo$matching_size/dtInfo$nCols,2), msize=2), 
                   dtInfo$colDeg_max,
                   dtInfo$valueTarget, round(dtInfo$upperBound,2), dtInfo$valueGreedy_stats,
                   dtInfo$ratio_stats, sep = " & ")
      line = paste(line, "\\\\ \n", sep = " ")
      cat(line)
      
      cat("\n...Latex format ends...\n\n")
      
    }
    
    return(dtInfo)
  }
  
  
  
  
  if (isLatex) {
    cat("\n...Latex format start...\n\n")
    # for (i in 1:nrow(dtInfo)) {
    line = paste(dtInfo$instanceDef, dtInfo$nCols, dtInfo$mRows,
                 format(round(dtInfo$matrixDens,4), msize=4), dtInfo$colDeg_max,
                 dtInfo$valueTarget, round(dtInfo$upperBound,2), dtInfo$valueGreedy_stats,
                 dtInfo$ratio_stats, sep = " & ")
    line = paste(line, "\\\\ \n", sep = " ")
    cat(line)
    
    cat("\n...Latex format ends...\n\n")
    
  }
  
  return(dtInfo)
}

tb_unate_greedy_chvatal_experiment_bound = function(
  instanceDef, greedyId="chvatal_F", valueTarget="",
  isSeedConsecutive=T) 
{
  # given as single instanceDef, find the upper bound for chvatal algo
  
  thisFunction = "tb_unate_greedy_chvatal_experiment_bound"
  
  cat("\n.. ENTERING", thisFunction,  
      "\n        greedy unate cover solutions for", greedyId,
      "\n                      date     =",  date(), "\n\n")
  
  
  clear(glob)
  unate_init_greedy(
    instanceDef, greedyId=greedyId, valueTarget="", 
    replicateSize="") 
  
  
  valueTarget    = glob[["valueTarget"]]
  runtime_read = glob[["runtime_read"]]
  replicateSize = glob[["replicateSize"]]
  colDegs = glob[["colDegs"]]
  
  if (is.na(valueTarget) || is.na(runtime_read) || is.na(replicateSize) ||
      is.na(colDegs)) {
    errorMsg = paste("\n .. ERROR from ", thisFunction, 
                     "\n.. glob initialization has errors, please check unate_init_greedy function\n")
    stop(errorMsg)
  }
  
  colDegMax = max(glob[["colDegs"]])
  harmonicNum = sum(1/seq(colDegMax))
  upperBound = valueTarget * harmonicNum
  
  L = list(
    colDegMax = colDegMax,
    harmonicNum = harmonicNum,
    upperBound = upperBound
  )
  
  cat("\n.. COMPLETING", thisFunction, "\n")
  return(L)
  
} # tb_unate_greedy_chvatal_experiment_bound



#-----Figures-----

fg_bgmc_unate_greedy_chvatal_iso_distr_school_9_11 = function() {
  
  # 1
  dt1 = data.table(x = c("R", "A", "B", "C", "D", "E", "F", "G"),
                   y.x = rep(0,8),
                   y.y = c(0.3311,  0.3417,  0.3370,  0.3311,  0.3259,  0.3327,  0.3450,  0.3346))
  
  dt1$x <- factor(dt1$x,levels = c("R", "A", "B", "C", "D", "E", "F", "G"))
  
  p1 = ggplot(data=dt1, aes(x=x))  +
    geom_segment(aes(x = x, y = y.x, xend = x, yend = y.y), col="red", lwd=1) + 
    geom_point(y=dt1$y.x) + 
    geom_point(y=dt1$y.y) +
    scale_y_continuous(breaks=seq(0.0, 0.6, 0.1), limits=c(0, 0.52)) +
    labs(x="1.00", y="empirical distribution") +
    theme(plot.title = element_text(size=10, hjust = 0.5), legend.title = element_blank())
  
  # 1.25
  dt1 = data.table(x = c("R", "A", "B", "C", "D", "E", "F", "G"),
                   y.x = rep(0,8),
                   y.y = c(0.4998,  0.4939,  0.4967,  0.4998,  0.5100,  0.5023,  0.4863,  0.4976))
  
  dt1$x <- factor(dt1$x,levels = c("R", "A", "B", "C", "D", "E", "F", "G"))
  
  # sp1_tb = data.table(instance = "school_9_16.cnfW", solver = "chvatal_stochastic", 
  #                     replicateSize = 1000, numColumns=9)
  
  p2 = ggplot(data=dt1, aes(x=x))  +
    geom_segment(aes(x = x, y = y.x, xend = x, yend = y.y), col="red", lwd=1) + 
    geom_point(y=dt1$y.x) + 
    geom_point(y=dt1$y.y) +
    scale_y_continuous(breaks=seq(0.0, 0.6, 0.1), limits=c(0, 0.52)) +
    labs(x="1.25", y="empirical distribution")+
    theme(plot.title = element_text(size=10, hjust = 0.5), legend.title = element_blank())
  
  # 1.50
  dt1 = data.table(x = c("R", "A", "B", "C", "D", "E", "F", "G"),
                   y.x = rep(0,8),
                   y.y = c(0.1691,   0.1644,  0.1663,  0.1691,  0.1641,  0.1650,  0.1687,  0.1678))
  
  dt1$x <- factor(dt1$x,levels = c("R", "A", "B", "C", "D", "E", "F", "G"))
  
  # sp1_tb = data.table(instance = "school_9_16.cnfW", solver = "chvatal_stochastic", 
  #                     replicateSize = 1000, numColumns=9)
  
  p3 = ggplot(data=dt1, aes(x=x))  +
    geom_segment(aes(x = x, y = y.x, xend = x, yend = y.y), col="red", lwd=1) + 
    geom_point(y=dt1$y.x) + 
    geom_point(y=dt1$y.y) +
    # ylim(0, 0.55) +
    scale_y_continuous(breaks=seq(0.0, 0.6, 0.1), limits=c(0, 0.52)) +
    labs(x="1.50", y="empirical distribution")+
    theme(plot.title = element_text(size=10, hjust = 0.5), legend.title = element_blank())
  
  
  sp = grid.arrange(p1, p2, p3, nrow=1,
                    top = textGrob(paste("fg_bgmc_unate_greedy_chvatal_iso_distr_school_9_11"),
                                   x = 0.04, y = 0, hjust = 0,
                                   gp=gpar(fontsize=8,font=1, col="gray47")),
                    bottom = textGrob(paste("ratio = valueGreedy/bestKnownValue"),
                                      #x = 0.04, y = 0, hjust = 0,
                                      y = 0.8,
                                      gp=gpar(fontsize=12,font=1, col="black")))
  
  ggsave("fg_bgmc_unate_greedy_chvatal_iso_distr_school_9_11.pdf", sp, width = 14, height = 3.5)
  # annotation_custom(tableGrob(t(sp1_tb)), xmin = 1.237, ymin=0.322,)
}

fg_bgmc_upper_bound_steiner = function() {
  dt = data.table()
  
  dt$nCols = c(9, 15, 27, 45, 81, 135, 243, 405, 729)
  dt$BKV = c(5, 9, 18, 30, 61, 103, 198, 335, 617)
  dt$valueMin = c(5, 9, 19, 31, 65, 107, 211, 349, 665)
  dt$valueMax = c(5, 9, 19, 33, 65, 111, 211, 357, 665)
  dt$upperBound_H2 = c(10.42, 23.34, 57.24, 110.72, 260.99, 493.3, 1064.67, 1972.47, 3995.53)
  
  Molten <- melt(dt, id.vars = "nCols")
  p1 = ggplot(Molten, aes(x = nCols, y = value, colour = variable)) + geom_line() +
    scale_x_continuous("size of steiner3", labels = as.character(dt$nCols), breaks = dt$nCols) +
    labs(title = "Experiments for upper bounds in steiner3 instances") +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          plot.title = element_text(size=10, hjust = 0.5), 
          legend.title = element_blank())
  
  ggsave("fg_bgmc_upper_bound_steiner.pdf", p1, width = 14, height = 3.5)
}

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
  # .. ENTERING unate_greedy_chvatal_experiment_distr 
  # greedy unate cover solutions for chvatal_S 
  # instance     = scpb1.cnfW 
  # replicateSize     = 10000 
  # isSeedConsecutive = TRUE 
  # date     = Fri Nov 12 14:58:20 2021 
  # 
  # .. initialized instance as a sparse matrix 'matrixS' under global array glob
  # 
  # .. COMPLETING unate_greedy_chvatal_experiment_distr 
  # [1] 1.0435 1.0580 1.0725 1.0870 1.1014 1.1159 1.1304 1.1449 1.1594
  # [10] 1.1739 1.1884 1.2029 1.2174 1.2319
  # ratio ratio_cnt
  # 1: 1.0435       315
  # 2: 1.0580       986
  # 3: 1.0725      2001
  # 4: 1.0870      1278
  # 5: 1.1014      1960
  # 6: 1.1159      1661
  # 7: 1.1304      1046
  # 8: 1.1449       352
  # 9: 1.1594       161
  # 10: 1.1739       128
  # 11: 1.1884        66
  # 12: 1.2029        27
  # 13: 1.2174        16
  # 14: 1.2319         4
  # [1] "unate_greedy_chvatal_experiment_distr"
  # 
  dt1 = data.table(
    x = c(1.0435,1.0435,1.0580,1.0580,1.0725,1.0725,1.0870,1.0870,1.1014,1.1014,
          1.1159,1.1159,1.1304,1.1304,1.1449,1.1449,1.1594,1.1594,1.1739,1.1739,
          1.1884,1.1884,1.2029,1.2029,1.2174,1.2174,1.2319,1.2319),
    y = c(0,315,0,986,0,2001,0,1278,0,1960,0,1661,0,1046,0,352,0,161,0,128,0,66,0,27,0,16,0,4)/10000
  )
  start_dt1 = unique(dt1, by = "x", fromLast = F)
  end_dt1 = unique(dt1, by = "x", fromLast = T)
  merge_dt1 = merge(start_dt1, end_dt1, by = "x")
  
  sp1_tb = data.table(instance = "scpb1.cnfW", solver = "chvatal_stochastic", 
                      replicateSize = 10000, numColumns=3000)
  
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
  # .. ENTERING unate_greedy_chvatal_experiment_distr 
  # greedy unate cover solutions for chvatal_S 
  # instance     = scpd1.cnfW 
  # replicateSize     = 10000 
  # isSeedConsecutive = TRUE 
  # date     = Fri Nov 12 15:18:04 2021 
  # 
  # .. initialized instance as a sparse matrix 'matrixS' under global array glob
  # 
  # .. COMPLETING unate_greedy_chvatal_experiment_distr 
  # [1] 1.1000 1.1167 1.1333 1.1500 1.1667 1.1833 1.2000 1.2167 1.2333
  # [10] 1.2500 1.2667 1.2833 1.3000
  # ratio ratio_cnt
  # 1: 1.1000         9
  # 2: 1.1167        91
  # 3: 1.1333       558
  # 4: 1.1500      1460
  # 5: 1.1667      2075
  # 6: 1.1833      2157
  # 7: 1.2000      1956
  # 8: 1.2167      1190
  # 9: 1.2333       340
  # 10: 1.2500       112
  # 11: 1.2667        44
  # 12: 1.2833         8
  # 13: 1.3000         1
  # [1] "unate_greedy_chvatal_experiment_distr"
  dt2 = data.table(
    x = c(1.1000,1.1000,1.1167,1.1167,1.1333,1.1333,1.1500,1.1500,1.1667,1.1667,
          1.1833,1.1833,1.2000,1.2000,1.2167,1.2167,1.2333,1.2333,1.2500,1.2500,
          1.2667,1.2667,1.2833,1.2833,1.3000,1.3000),
    y = c(0,9,0,91,0,558,0,1460,0,2075,0,2157,0,1956,0,1190,0,340,0,112,0,44,0,8,0,1)/10000
  )
  start_dt2 = unique(dt2, by = "x", fromLast = F)
  end_dt2 = unique(dt2, by = "x", fromLast = T)
  merge_dt2 = merge(start_dt2, end_dt2, by = "x")
  
  sp2_tb = data.table(instance = "scpd1.cnfW", solver = "chvatal_stochastic", 
                      replicateSize = 10000, numColumns = 4000)
  
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



#----school_9_16 example----

all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_rBed"
  fileRDS_dtPairs = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dtPairs.RDS")
  fileRDS_dt      = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dt.RDS")
  source(file.path(workDir, "fg_bgmc_unate_school_9_16.R" ))
  fg_bgmc_unate_school_9_16_index(fileRDS_dtPairs, fileRDS_dt)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_rBed"
  fileRDS_dtPairs = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dtPairs.RDS")
  fileRDS_dt      = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dt.RDS")
  source(file.path(workDir, "fg_bgmc_unate_school_9_16.R" ))
  fg_bgmc_unate_school_9_16_rank(fileRDS_dtPairs, fileRDS_dt)
  
} # all_tests = function()

fg_bgmc_unate_school_9_16_index_gg = function(fileRDS_dtPairs, fileRDS_dt) {
  thisFunction = "fg_bgmc_unate_school_9_16_index_gg"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  print(dtPairs)
  dtE    = readRDS(fileRDS_dt)
  nRows = nrow(dtE)
  
  # plot value-vs-index
  x = dtE$index ; y = dtE$value
  idx_min    = which.min(y)  
  value_best = y[idx_min]
  value_max  = max(dtE$value[dtE$isCover == T])
  index_best = dtE$index[idx_min] 
  
  dt = data.table(x=x, y=y)
  
  highlight_dt = data.table(xW=index_best, yW=value_best)
  
  p1 = ggplot(data=dt, aes(x=x,y=y)) + 
    geom_point(shape=1) +
    labs(x=paste("Index of binary coordinates  
                  (at index = ", index_best, ", solution_best = [", index_best, ";", value_best, "])", sep=""),
         y=paste("OF value", sep=""),
         title="(b)") +
    geom_hline(yintercept=value_best, linetype="dashed", color = "red") +
    geom_hline(yintercept=value_max, linetype="dashed", color = "red") +
    geom_vline(xintercept=index_best, linetype="dashed", color = "red") +
    geom_point(data=highlight_dt, 
               aes(x=xW,y=yW), 
               color='red',
               size=3) +
    theme(plot.title = element_text(hjust = 0.5, size=16))
  
  p = grid.arrange(p1,  
                   top = textGrob("fg_bgmc_unate_school_9_16_b", x = 0.07, y = -1,hjust = 0,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_unate_school_9_16_b.pdf", p, width = 7, height = 5)
  
  
}

fg_bgmc_unate_school_9_16_rank_gg = function(fileRDS_dtPairs, fileRDS_dt) {
  
  thisFunction = "fg_bgmc_unate_school_9_16_rank_gg"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  print(dtPairs)
  dtE    = readRDS(fileRDS_dt)
  nRows = nrow(dtE)
  
  # plot value-vs-rank
  x = dtE$rank ; y = dtE$value
  idx_min    = which.min(y)  
  value_best = y[idx_min]
  value_max  = max(dtE$value[dtE$isCover == T])
  coord_best = dtE$coord[idx_min]   ;# print(coord_best)
  rank_best  = bina_coord_rankC(coord_best)
  
  dt = data.table(x=x, y=y)
  
  p1 = ggplot(data=dt, aes(x=x,y=y)) + 
    geom_point(shape=1) +
    labs(x=paste("rank of binary coordinates   
                  (at rank = ", rank_best, ", solution_best = [", coord_best, ";", value_best, "])", sep=""),
         y=paste("OF value", sep=""), title="(a)") +
    geom_hline(yintercept=value_best, linetype="dashed", color = "red") +
    geom_hline(yintercept=value_max, linetype="dashed", color = "red") +
    geom_vline(xintercept=rank_best, linetype="dashed", color = "red") +
    geom_vline(xintercept=rank_best+2, linetype="dashed", color = "blue") +
    theme(plot.title = element_text(hjust = 0.5, size=16))
  
  
  
  coord_walk = c("110101111", "111100111", "011100111", "011100110")
  step = -1
  walk_length = length(coord_walk)
  for (i in 1:walk_length) {
    step       = step + 1
    coord      = coord_walk[i]
    rank       = bina_coord_rankC(coord_walk[i])
    coord_rank = dtE$coord[dtE$rank   == rank]
    value_rank = dtE$value[dtE$rank   == rank]
    isCov_rank = dtE$isCover[dtE$rank == rank]
    idx        = which(coord_rank == coord)
    value      = value_rank[idx]
    isCov      = isCov_rank[idx]
    # cat(step, rank, coord, value, isCov, "\n")
    if (step == 0) {
      p1 = p1 + geom_point(data=data.table(rank=rank, value=value), 
                           aes(x=rank,y=value), 
                           color='blue',
                           size=3)
      # points(rank, value, pch=19, col="blue", cex=2)
    } else {
      p1 = p1 + geom_point(data=data.table(rank=rank, value=value), 
                           aes(x=rank,y=value), 
                           color='red',
                           size=3)
      # points(rank, value, pch=19, col="red", cex=2)     
    }
    if (step == 0) {
      dtW = data.table(
        step = step,
        rank = rank,
        coord = coord,
        value = value,
        isCov = isCov
      )
    } else {
      dtW = rbind(dtW, list(
        step = step,
        rank = rank,
        coord = coord,
        value = value,
        isCov = isCov
      ))
    }
  }
  print(dtW)
  
  
  p = grid.arrange(p1,  
                   top = textGrob("fg_bgmc_unate_school_9_16_a", x = 0.07, y = -1,hjust = 0,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_unate_school_9_16_a.pdf", p, width = 7, height = 5)
  
  
  
  # watermark = "fg_bgmc_unate_school_9_16_a"
  # mtext(watermark, col="gray47", cex= 0.9, side = 3, at=0.50, line=0.5)  
  # 
  # filePdf = paste(watermark, ".pdf", sep="")
  # cat("\n... save this plot as", filePdf, "\n\n")
  
}

fg_bgmc_unate_school_9_16_index = function(fileRDS_dtPairs, fileRDS_dt)
{
  # ...
  thisFunction = "fg_bgmc_unate_school_9_16_index"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  print(dtPairs)
  dtE    = readRDS(fileRDS_dt)
  nRows = nrow(dtE)
  
  # plot value-vs-index
  x = dtE$index ; y = dtE$value
  idx_min    = which.min(y)  
  value_best = y[idx_min]
  value_max  = max(dtE$value[dtE$isCover == T])
  index_best = dtE$index[idx_min]   
  
  # https://stackoverflow.com/questions/4241798/how-to-increase-font-size-in-a-plot-in-r
  plot(x, y, log="y", pch=1, col="black", axes=T, cex.lab=1.1,
       xlab=paste("index coordinate  
                  (at index = ", index_best, ", solution_best = [", index_best, ";", value_best, "])", sep=""),
       ylab=paste("OF value", sep="")
  )
  abline(h=value_best,   lwd=1, lty="dashed", col="red") 
  abline(h=value_max,    lwd=1, lty="dashed", col="red") 
  abline(v=index_best,    lwd=1, lty="dashed", col="red") 
  
  points(index_best, value_best, pch=19, col="red", cex=2) 
  
  watermark = "fg_bgmc_unate_school_9_16_b"
  mtext(watermark, col="gray47", cex= 0.9, side = 3, at=17, line=0.5)  
  
  filePdf = paste(watermark, ".pdf", sep="")
  cat("\n... save this plot as", filePdf, "\n\n")
  
} # fg_bgmc_unate_school_9_16_index

fg_bgmc_unate_school_9_16_rank = function(fileRDS_dtPairs, fileRDS_dt)
{
  # ...
  thisFunction = "fg_bgmc_unate_school_9_16_rank"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  print(dtPairs)
  dtE    = readRDS(fileRDS_dt)
  nRows = nrow(dtE)
  
  # plot value-vs-rank
  x = dtE$rank ; y = dtE$value
  idx_min    = which.min(y)  
  value_best = y[idx_min]
  value_max  = max(dtE$value[dtE$isCover == T])
  coord_best = dtE$coord[idx_min]   ;# print(coord_best)
  rank_best  = bina_coord_rankC(coord_best)
  
  # https://stackoverflow.com/questions/4241798/how-to-increase-font-size-in-a-plot-in-r
  plot(x, y, log="y", pch=1, col="black", axes=T, cex.lab=1.1,
       xlab=paste("binary coordinate rank  
                  (at rank = ", rank_best, ", solution_best = [", coord_best, ";", value_best, "])", sep=""),
       ylab=paste("OF value", sep=""))
  
  # axis(side, at=at, labels=, pos=, lty=, col=, las=, tck=, ...)
  # https://www.statmethods.net/advgraphs/axes.html
  side = 1 # bottom
  axis(side, at=dtE$rank, cex.axis=1.1)
  side = 2 # left
  axis(side, at=c(10, 15, 18, 20, 25, 30, 35), cex.axis=1.1)
  
  abline(h=value_best,   lwd=1, lty="dashed", col="red") 
  abline(h=value_max,    lwd=1, lty="dashed", col="red") 
  abline(v=rank_best,    lwd=1, lty="dashed", col="red") 
  abline(v=rank_best+2,  lwd=1, lty="dashed", col="blue")
  
  coord_walk = c("110101111", "111100111", "011100111", "011100110")
  step = -1
  walk_length = length(coord_walk)
  for (i in 1:walk_length) {
    step       = step + 1
    coord      = coord_walk[i]
    rank       = bina_coord_rankC(coord_walk[i])
    coord_rank = dtE$coord[dtE$rank   == rank]
    value_rank = dtE$value[dtE$rank   == rank]
    isCov_rank = dtE$isCover[dtE$rank == rank]
    idx        = which(coord_rank == coord)
    value      = value_rank[idx]
    isCov      = isCov_rank[idx]
    # cat(step, rank, coord, value, isCov, "\n")
    if (step == 0) {
      points(rank, value, pch=19, col="blue", cex=2)
    } else {
      points(rank, value, pch=19, col="red", cex=2)     
    }
    if (step == 0) {
      dtW = data.table(
        step = step,
        rank = rank,
        coord = coord,
        value = value,
        isCov = isCov
      )
    } else {
      dtW = rbind(dtW, list(
        step = step,
        rank = rank,
        coord = coord,
        value = value,
        isCov = isCov
      ))
    }
  }
  print(dtW)
  
  watermark = "fg_bgmc_unate_school_9_16_a"
  mtext(watermark, col="gray47", cex= 0.9, side = 3, at=0.50, line=0.5)  
  
  filePdf = paste(watermark, ".pdf", sep="")
  cat("\n... save this plot as", filePdf, "\n\n")
  
} # fg_bgmc_unate_school_9_16_rank

# fg_bgmc_unate_school_9_16.R   # MUST be sourced from github/_rlibR/_lib_R_resources.R



#----school_9_11 example----
all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_rBed"
  # fileRDS_dtP = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfU_dtPairs.RDS")
  # fileRDS_dtE     = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfU_dt.RDS")
  fileRDS_dtP     = file.path(workDir, "OFb_unateF_exh_school_9_11.cnfU_dtPairs.RDS")
  fileRDS_dtE     = file.path(workDir, "OFb_unateF_exh_school_9_11.cnfU_dt.RDS")
  source(file.path(workDir, "fg_bgmc_unate_school_9_11.R" ))
  fg_bgmc_unate_school_9_11_index_gray(fileRDS_dtP, fileRDS_dtE)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  fileRDS_dtP     = file.path(workDir, "OFb_unateF_exh_school_9_11.cnfU_dtPairs.RDS")
  fileRDS_dtE     = file.path(workDir, "OFb_unateF_exh_school_9_11.cnfU_dt.RDS")
  source(file.path(workDir, "fg_bgmc_unate_school_9_11.R" ))
  fg_bgmc_unate_school_9_11_rank_gray(fileRDS_dtP, fileRDS_dtE)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_unate_school_9_11.R" ))
  fg_bgmc_school_9_11_walk_gray()
  
} # all_tests = function()

fg_bgmc_unate_school_9_11_index_gray_gg = function(fileRDS_dtPairs, fileRDS_dt) {
  thisFunction = "fg_bgmc_unate_school_9_11_index_gray_gg"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  print(dtPairs)
  dtE    = readRDS(fileRDS_dt)
  nRows = nrow(dtE)
  
  # plot value-vs-index
  x = dtE$index ; y = dtE$value
  idx_min    = which.min(y)  
  value_best = y[idx_min]
  value_max  = max(dtE$value[dtE$isCover == T])
  index_best = dtE$index[idx_min] 
  
  dt = data.table(x=x, y=y)
  
  highlight_dt = data.table(xW=index_best, yW=value_best)
  
  p1 = ggplot(data=dt, aes(x=x,y=y)) + 
    geom_point(shape=1) +
    labs(x=paste("Index of binary coordinates  
                  (at index = ", index_best, ", solution_best = [", index_best, ";", value_best, "])", sep=""),
         y=paste("OF value", sep=""),
         title="(b)") +
    geom_hline(yintercept=value_best, linetype="dashed", color = "red") +
    geom_hline(yintercept=value_max, linetype="dashed", color = "red") +
    geom_vline(xintercept=index_best, linetype="dashed", color = "red") +
    geom_point(data=highlight_dt, 
               aes(x=xW,y=yW), 
               color='red',
               size=3) +
    theme(plot.title = element_text(hjust = 0.5, size=16))
  
  p = grid.arrange(p1,  
                   top = textGrob("fg_bgmc_unate_school_9_16_b", x = 0.07, y = -1,hjust = 0,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_unate_school_9_11_b.pdf", p, width = 7, height = 5)
  print("saved!")
  
  
} # fg_bgmc_unate_school_9_11_index_gray_gg

fg_bgmc_unate_school_9_11_rank_gray_gg = function(fileRDS_dtPairs, fileRDS_dt) {
  
  thisFunction = "fg_bgmc_unate_school_9_11_rank_gray_gg"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  print(dtPairs)
  dtE    = readRDS(fileRDS_dt)
  nRows = nrow(dtE)
  
  # plot value-vs-rank
  x = dtE$rank ; y = dtE$value
  idx_min    = which.min(y)  
  value_best = y[idx_min]
  value_max  = max(dtE$value[dtE$isCover == T])
  coord_best = dtE$coord[idx_min]   ;# print(coord_best)
  rank_best  = bina_coord_rankC(coord_best)
  
  dt = data.table(x=x, y=y)
  
  p1 = ggplot(data=dt, aes(x=x,y=y)) + 
    geom_point(shape=1) +
    labs(x=paste("rank of binary coordinates   
                  (at rank = ", rank_best, ", solution_best = [", coord_best, ";", value_best, "])", sep=""),
         y=paste("OF value", sep=""), title="(a)") +
    geom_hline(yintercept=value_best, linetype="dashed", color = "red") +
    geom_hline(yintercept=value_max, linetype="dashed", color = "red") +
    geom_vline(xintercept=rank_best, linetype="dashed", color = "red") +
    geom_vline(xintercept=rank_best+2, linetype="dashed", color = "blue") +
    theme(plot.title = element_text(hjust = 0.5, size=16))
  
  
  
  coord_walk = c("110101111", "111100111", "011100111", "011100110")
  step = -1
  walk_length = length(coord_walk)
  for (i in 1:walk_length) {
    step       = step + 1
    coord      = coord_walk[i]
    rank       = bina_coord_rankC(coord_walk[i])
    coord_rank = dtE$coord[dtE$rank   == rank]
    value_rank = dtE$value[dtE$rank   == rank]
    isCov_rank = dtE$isCover[dtE$rank == rank]
    idx        = which(coord_rank == coord)
    value      = value_rank[idx]
    isCov      = isCov_rank[idx]
    # cat(step, rank, coord, value, isCov, "\n")
    if (step == 0) {
      p1 = p1 + geom_point(data=data.table(rank=rank, value=value), 
                           aes(x=rank,y=value), 
                           color='blue',
                           size=3)
      # points(rank, value, pch=19, col="blue", cex=2)
    } else {
      p1 = p1 + geom_point(data=data.table(rank=rank, value=value), 
                           aes(x=rank,y=value), 
                           color='red',
                           size=3)
      # points(rank, value, pch=19, col="red", cex=2)     
    }
    if (step == 0) {
      dtW = data.table(
        step = step,
        rank = rank,
        coord = coord,
        value = value,
        isCov = isCov
      )
    } else {
      dtW = rbind(dtW, list(
        step = step,
        rank = rank,
        coord = coord,
        value = value,
        isCov = isCov
      ))
    }
  }
  print(dtW)
  
  
  p = grid.arrange(p1,  
                   top = textGrob("fg_bgmc_unate_school_9_16_a", x = 0.07, y = -1,hjust = 0,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_unate_school_9_11_a.pdf", p, width = 7, height = 5)
  
  
  
  # watermark = "fg_bgmc_unate_school_9_16_a"
  # mtext(watermark, col="gray47", cex= 0.9, side = 3, at=0.50, line=0.5)  
  # 
  # filePdf = paste(watermark, ".pdf", sep="")
  # cat("\n... save this plot as", filePdf, "\n\n")
  
} # fg_bgmc_unate_school_9_11_rank_gray_gg

fg_bgmc_unate_school_9_11_index_gray = function(fileRDS_dtPairs, fileRDS_dt)
{
  # ...
  thisFunction = "fg_bgmc_unate_school_9_11_index_gray"
  
  dtP   = readRDS(fileRDS_dtPairs)
  dtE   = readRDS(fileRDS_dt)
  nCols = dtP$nCols[1]
  mRows = dtP$mRows[1]
  U     = dtP$nOps[1]
  print(t(dtP)) ; # return()
  
  # plot value-vs-index
  x = dtE$index_gray ; y = dtE$value
  # idx_min    = which.min(y)  
  # when there is more than 1 minimum
  idx_min    = which(y == min(y))
  value_best = y[idx_min]
  value_max  = max(dtE$value[dtE$isCover == T])
  index_best = dtE$index[idx_min]   
  
  # https://stackoverflow.com/questions/4241798/how-to-increase-font-size-in-a-plot-in-r
  plot(x, y, log="y", pch=1, col="black", axes=T, cex.lab=1.1,
       xlab=paste("gray index of binary coordinates
       (at indices = ", index_best[1], ",", index_best[2], ": value_best = ", value_best[1], ")",sep=""),
       ylab=paste("OF value", sep="")
  )
  abline(h=value_best,   lwd=1, lty="dashed", col="red") 
  abline(h=value_max,    lwd=1, lty="dashed", col="red") 
  abline(v=index_best,    lwd=1, lty="dashed", col="red") 
  
  points(index_best, value_best, pch=19, col="red", cex=2) 
  
  watermark = "fg_bgmc_unate_school_9_11_b"
  mtext(watermark, col="gray47", cex= 0.9, side = 3, at=17, line=0.5)  
  
  filePdf = paste(watermark, ".pdf", sep="")
  cat("\n... save this plot as", filePdf, "\n\n")
  
} # fg_bgmc_unate_school_9_11_index_gray

fg_bgmc_unate_school_9_11_rank_gray = function(fileRDS_dtPairs, fileRDS_dt)
{
  # ...
  thisFunction = "fg_bgmc_unate_school_9_11_rank"
  
  dtP   = readRDS(fileRDS_dtPairs)
  dtE   = readRDS(fileRDS_dt)
  nCols = dtP$nCols[1]
  mRows = dtP$mRows[1]
  U     = dtP$nOps[1]
  print(t(dtP)) ; # return()
  
  # x_cover  = dtE$rank[dtE$isCover == T]
  # x_cover  = dtE$rank[dtE$isCover == T]
  # y_cover  = dtE$value[dtE$isCover == T]
  # # dt = data.table(x_cover, y_cover)
  # # print(dt)
  
  # essential value to plot value-vs-rank
  x = dtE$rank ; y = dtE$value
  # idx_min    = which.min(y)  
  # MUST use "which" when expecting more than 1 minimum
  idx_min    = which(y == min(y))   ;# print(idx_min) ;# return()
  numMinima  = length(idx_min)      ;# print(numMinima)
  value_best = y[idx_min]           ;# print(value_best)
  value_max  = max(dtE$value[dtE$isCover == T]) ;# print(value_max)
  value_max_covNot  = max(dtE$value)            ;# print(value_max_covNot)
  coord_best = dtE$coord[idx_min]               ;# print(coord_best)
  rank_best  = bina_coord_rankC(coord_best[1])  ;# print(rank_best)
  
  coord_cov   = hash()  ; clear(coord_cov)
  coord_covNot = hash() ; clear(coord_covNot)  
  size_cov   = hash()   ; clear(size_cov)
  size_covNot = hash()  ; clear(size_covNot)
  for (i in 1:U) {
    rank = paste(dtE$rank[i])
    coord = dtE$coord[i]
    if (dtE$isCover[i] == T) {
      coord_cov[[rank]] = c(coord_cov[[rank]], coord)
      size_cov[[rank]]  = length(coord_cov[[rank]])
      
    } else {
      if (rank < rank_best) {
        size_cov[[rank]] = 0
      }
      coord_covNot[[rank]] = c(coord_covNot[[rank]], coord)
      size_covNot[[rank]]  = length(coord_covNot[[rank]])
    }
  }
  rank_max = max(dtE$rank)
  size_covNot[[paste(rank_max)]]  = 0
  # print(coord_covNot) ; print(coord_cov)
  # print(size_covNot) ; print(size_cov)
  # <hash> containing 10 key-value pair(s).
  # 0 : 1
  # 1 : 9
  # 2 : 36
  # 3 : 84
  # 4 : 124
  # 5 : 115
  # 6 : 63
  # 7 : 18
  # 8 : 2
  # 9 : 0
  # <hash> containing 10 key-value pair(s).
  # 0 : 0
  # 1 : 0
  # 2 : 0
  # 3 : 0
  # 4 : 2
  # 5 : 11
  # 6 : 21
  # 7 : 18
  # 8 : 7
  # 9 : 1
  # > 
  # return()
  xlim=c(0, rank_max)          ;# print(ylim)
  y_min = value_best[1]
  y_max = value_max_covNot + 3 
  ylim=c(y_min, y_max)         ;# print(ylim)
  # https://stackoverflow.com/questions/4241798/how-to-increase-font-size-in-a-plot-in-r
  plot(x, y,  xlim=xlim, ylim=ylim,
       log="y", pch=1, col="black", axes=T, cex.lab=1.1,
       xlab=paste("rank of binary coordinates
  (at rank = ", rank_best[1], ": value_best = ", value_best[1], ")",sep=""),
       ylab=paste("OF value", sep=""))
  
  # axis(side, at=at, labels=, pos=, lty=, col=, las=, tck=, ...)
  # https://www.statmethods.net/advgraphs/axes.html
  side = 1 # bottom
  axis(side, at=dtE$rank, cex.axis=1.1)
  side = 2 # left
  # axis(side, at=dtE$value, cex.axis=1.1)
  axis(side, at=c(4,5,6,7,8,9,10,15, y_max), cex.axis=1.1)
  
  abline(h=value_best,       lwd=1, lty="dashed", col="red") 
  abline(h=value_max,        lwd=1, lty="dashed", col="red") 
  abline(h=value_max_covNot, lwd=1, lty="dashed", col="red") 
  abline(v=rank_best,        lwd=1, lty="dashed", col="red") 
  abline(v=rank_best+2,      lwd=1, lty="dashed", col="blue")
  
  # from fg_bgmc_school_9_11_walk_gray = function()
  # step     coord value rank isCover
  # 1:    0 011111100    16    6   FALSE
  # 2:    1 110111100     6    6    TRUE
  # 3:    2 110111101     7    7    TRUE
  # 4:    3 110111111     8    8    TRUE
  # ...
  # 32:   31 110101101     6    6    TRUE
  # 33:   32 110101100     5    5    TRUE
  # 34:   33 110001100     4    4    TRUE
  coord_walk = c("011111100", "110111100", "110101101", "110101100", 110001100)
  step = -1
  walk_length = length(coord_walk)
  for (i in 1:walk_length) {
    step       = step + 1
    coord      = coord_walk[i]
    rank       = bina_coord_rankC(coord_walk[i])
    coord_rank = dtE$coord[dtE$rank   == rank]
    value_rank = dtE$value[dtE$rank   == rank]
    isCov_rank = dtE$isCover[dtE$rank == rank]
    idx        = which(coord_rank == coord)
    value      = value_rank[idx]
    isCov      = isCov_rank[idx]
    # cat(step, rank, coord, value, isCov, "\n")
    if (step == 0) {
      points(rank, value, pch=19, col="blue", cex=2)
    } else {
      points(rank, value, pch=19, col="red", cex=2)     
    }
    
    # at each rank: labels for num_of_values_not_covered 
    for (i in 1:(rank_max + 1)) {
      rank = i - 1
      x     = rank 
      y     = value_max_covNot + 2
      label = size_covNot[[paste(rank)]]
      text(x, y, label, cex=1.05)
    }
    
    # at each rank: labels for num_of_values_covered 
    for (i in 1:(rank_max + 1)) {
      rank = i - 1
      x     = rank 
      y     = value_max + 0.75
      label = size_cov[[paste(rank)]]
      text(x, y, label, cex=1.05)
    }
    
    
    # if (step == 0) {
    #   dtW = data.table(
    #     step = step,
    #     rank = rank,
    #     coord = coord,
    #     value = value,
    #     isCov = isCov
    #   )
    # } else {
    #   dtW = rbind(dtW, list(
    #     step = step,
    #     rank = rank,
    #     coord = coord,
    #     value = value,
    #     isCov = isCov
    #   ))
    # }
  }
  # print(dtW)
  
  watermark = "fg_bgmc_unate_school_9_11_a"
  mtext(watermark, col="gray47", cex= 0.9, side = 3, at=0.50, line=0.5)  
  
  filePdf = paste(watermark, ".pdf", sep="")
  cat("\n... save this plot as", filePdf, "\n\n")
  
} # fg_bgmc_unate_school_9_11_rank


fg_bgmc_school_9_11_walk_gray = function()
{
  # a talking point for fg_bgmc_school_9_11
  thisFunction = "fg_bgmc_school_9_11_walk_gray"
  
  OFname  = "OFb_unateF"
  OFb     = match.fun(OFname)
  
  # read the instance (note the extra level in path of instanceDef)
  instanceDef = "../../../_data_tiny/bigraph/unate/school_9_11.cnfU" 
  read_bgu(instanceDef, id="matrixF") ;# print(glob) ; return()
  instanceName = basename(instanceDef)
  nCols        = glob[["nCols"]]
  mRows        = glob[["mRows"]]
  colWeights   = glob[["colWeights"]]
  
  coordInit = "011111100" ;# randomly chosen coord at rank = 6
  coordV = as.integer(unlist(strsplit(coordInit, "")))
  answ   = OFb(coordV)
  value  = answ$valueOF
  dt = data.table(
    step = 0,
    coord = coordInit,
    value = value,
    rank  = 6,
    isCover = F)
  
  # # find the FIRST feasible coordinate via bina_coord_neighb_B
  # rm(list=ls())
  # workDir = "~/DeskTop/github/rBed_bgmc/workDir"
  # setwd(workDir) ; source("../../_lib_R/_lib_R_resources.R")
  # instanceDef = "../../_data_tiny/bigraph/unate/school_9_11.cnfU" 
  # coordPiv    = "011111100"
  # bina_coord_neighb_B_OFb_unateF(instanceDef, coordPiv)
  # 
  # .. data table of the **same rank ij-neighbors**  and OF-values 
  # ij     coord value isCov rank dist
  # 1: 0,0 011111100    16 FALSE    6    0
  # 2: 2,9 001111101    17 FALSE    6    2
  # 3: 2,8 001111110    17 FALSE    6    2
  # ...
  # 14: 1,2 101111100    16 FALSE    6    2
  # 15: 1,3 110111100     6  TRUE    6    2
  # 16: 1,4 111011100     6  TRUE    6    2
  # 17: 1,5 111101100     6  TRUE    6    2
  # 18: 1,6 111110100    16 FALSE    6    2
  # 19: 1,7 111111000    16 FALSE    6    2
  # > 
  ## i.e. the FIRST coordinate for graphB_gray_compressed
  # coordFirst = 111011100
  # 
  # workDir = "~/DeskTop/github/rBed_mclass/workDir"
  # setwd(workDir) ; source("../../_lib_R/_lib_R_resources.R")
  # graphB_gray_compressed(coordFirst="110111100")
  
  coords_gray = c(
    "110111100","110111101","110111111","110111110","110111010",
    "110111011","110111001","110111000","110110000","110110001",
    "110110011","110110010","110110110","110110111","110110101",
    "110110100","110100100","110100101","110100111","110100110",
    "110100010","110100011","110100001","110100000","110101000",
    "110101001","110101011","110101010","110101110","110101111",
    "110101101","110101100","110001100"
  )
  # now continue the walk from coordFirst
  step = 0
  for (coord in coords_gray) {
    step = step + 1
    coordV = as.integer(unlist(strsplit(coord, "")))
    rank   = bina_coord_rank(coordV)
    answ   = OFb(coordV)
    value  = answ$valueOF
    isCover = answ$isCover
    dt     = rbind(dt, list(
      step=step, 
      coord=coord, 
      value=value, 
      rank=rank, 
      isCover=isCover))
  }
  print(dt)
  
  # rm(list=ls())
  # workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  # lib_R_dir     = "../../../_lib_R"
  # source(file.path(lib_R_dir, "_lib_R_resources.R"))
  # source(file.path(workDir, "fg_bgmc_unate_school_9_11.R" ))
  # fg_bgmc_school_9_11_walk_gray()
  #  
  # step     coord value rank isCover
  # 1:    0 011111100    16    6   FALSE
  # 2:    1 110111100     6    6    TRUE
  # 3:    2 110111101     7    7    TRUE
  # 4:    3 110111111     8    8    TRUE
  # 5:    4 110111110     7    7    TRUE
  # 6:    5 110111010    16    6   FALSE
  # 7:    6 110111011     7    7    TRUE
  # 8:    7 110111001     6    6    TRUE
  # 9:    8 110111000    15    5   FALSE
  # 10:    9 110110000    15    4   FALSE
  # 11:   10 110110001    15    5   FALSE
  # 12:   11 110110011     6    6    TRUE
  # 13:   12 110110010    15    5   FALSE
  # 14:   13 110110110     6    6    TRUE
  # 15:   14 110110111     7    7    TRUE
  # 16:   15 110110101    16    6   FALSE
  # 17:   16 110110100    15    5   FALSE
  # 18:   17 110100100    14    4   FALSE
  # 19:   18 110100101    15    5   FALSE
  # 20:   19 110100111     6    6    TRUE
  # 21:   20 110100110     5    5    TRUE
  # 22:   21 110100010    16    4   FALSE
  # 23:   22 110100011    16    5   FALSE
  # 24:   23 110100001    16    4   FALSE
  # 25:   24 110100000    16    3   FALSE
  # 26:   25 110101000    16    4   FALSE
  # 27:   26 110101001    16    5   FALSE
  # 28:   27 110101011    17    6   FALSE
  # 29:   28 110101010    17    5   FALSE
  # 30:   29 110101110     6    6    TRUE
  # 31:   30 110101111     7    7    TRUE
  # 32:   31 110101101     6    6    TRUE
  # 33:   32 110101100     5    5    TRUE
  # 34:   33 110001100     4    4    TRUE
  # step     coord value rank isCover
  # > 
  
} # fg_bgmc_school_9_11_walk_gray


