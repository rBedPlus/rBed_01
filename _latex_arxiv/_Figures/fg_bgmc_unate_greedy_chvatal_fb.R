all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal_fb.R" ))
  fg_bgmc_unate_greedy_chvatal_abcd()
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data/bigraph/unate/steiner3/"
  instanceDef = file.path(data_dir, "steiner3_009_12.cnfU")
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal_fb.R" ))
  unate_greedy_chvatal_experiment(instanceDef, replicateSize = 10, greedyId="chvatal_S")
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data_tiny/bigraph/unate/"
  instanceDef = file.path(data_dir, "school_9_16.cnfW")
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal_fg.R" ))
  unate_greedy_chvatal_experiment_F_iso(instanceDef, replicateSize = 10, greedyId="chvatal_F")
  
  
} # all_tests = function()

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
    theme(plot.title = element_text(size=10, hjust = 0.5)) +
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
    theme(plot.title = element_text(size=10, hjust = 0.5)) +
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
    theme(plot.title = element_text(size=10, hjust = 0.5)) +
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
    theme(plot.title = element_text(size=10, hjust = 0.5)) +
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
unate_init_greedy = function(
  instanceDef, OFname="", greedyId="", valueTarget="", 
  replicateSize="",  seedInit="", iterationLmt="")
{
  #
  thisFunction = "unate_init_greedy"
  
  # read instance and continue to initalize glob
  readId = ""
  if (str_starts(greedyId, "chvatal_S")) {
    readId = "matrixS"
  } else {
    readId = "matrixF"
  }
  
  
  glob[["matrixId"]] = readId
  
  start_time = Sys.time()
  
  read_bgu(instanceDef, readId) 
  
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
  
  if (OFname == "") {
    OFname = "OFb_unateM"
  }
  # exists_unate = str_detect(OFname, "unate")
  # stopifnot(exists_unate)
  # all_functions = glob[["ls()"]] ;# print(all_functions)
  # exists_OFname = !is.na(match(OFname, all_functions))
  # stopifnot(OFname)
  glob[["OFname"]] = OFname
  
  if (greedyId == "") {
    greedyId = "random"
  } 
  glob[["greedyId"]] = greedyId  
  
  if (valueTarget == "") {
    valueTarget = as.numeric(bg_find_target_unate(instanceDef)$BKV[1])
  }
  glob[["valueTarget"]] =  valueTarget  
  
  if (replicateSize == "") {
    replicateSize = 0
    # stop("Please enter replicateSize...\n")
  }
  glob[["replicateSize"]]     = replicateSize  
  sampleFractionLmt           = prettyNum(replicateSize/coordSizeLmt)
  glob[["sampleFractionLmt"]] = sampleFractionLmt
  
  if (seedInit == "") {
    seedInit = 0
    # set.seed(seedInit)
  }
  glob[["seedInit"]]      = seedInit
  glob[["seedInitFirst"]] = seedInit
  
  if (iterationLmt == "") {
    iterationLmt = 10 # for now
  }
  glob[["iterationLmt"]] = iterationLmt
  
  # print(glob)
  
} # unate_init_greedy

unate_greedy_chvatal = function() {
  
  thisFunction = "unate_greedy_chvatal" # ; a full-matrix implementation
  
  # the best full matrix version of chvatal 
  
  valueTarget   = glob[["valueTarget"]]
  replicateSize = glob[["replicateSize"]]
  greedyId      = glob[["greedyId"]]
  
  cat("\n** entering", thisFunction, "; greedyId=", greedyId, "\n")
  
  n          = glob[["nCols"]]
  m          = glob[["mRows"]]
  coord      = rep(0, n)
  
  M          = glob[[ glob[["matrixId"]] ]]
  colWeights = glob[["colWeights"]]
  
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
  
  
  # cat("\n** returning 'sampleDt' as dt from", thisFunction,  
  #     "\n   ", date(), "\n\n")
  
  return(list(dt = dtPairs))
  
}

unate_greedy_chvatal_experiment = function(
  instanceDef,  OFname="OFb_unateF", greedyId="chvatal_F",
  valueTarget="", replicateSize="", isSeedConsecutive=T,  seedInit="", iterationLmt="") 
{
  # given as single instanceDef, find runtime_read, runtime, and
  # solutions for solver chvatalS 
  
  thisFunction = "unate_greedy_chvatal_experiment_F"
  
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
  
  
  valueTarget    = glob[["valueTarget"]]
  runtime_read = glob[["runtime_read"]]
  
  glob[["replicateSize"]] = replicateSize 
  dt = data.table()
  print(glob) ; return()
  
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
  
  valueGreedy_min    = round(valueGreedy_min, 2)
  valueGreedy_median = round(median(dt$valueGreedy), 2)
  valueGreedy_mean   = round(mean(dt$valueGreedy), 3)
  valueGreedy_stDev  = round(sd(dt$valueGreedy), 3)
  valueGreedy_max    = round(valueGreedy_max, 2)
  
  valueGreedy_stats  = paste(c(
    valueGreedy_min, valueGreedy_median, valueGreedy_mean,
    valueGreedy_stDev, valueGreedy_max), collapse=",") 
  
  
  coordUniqSize  = nrow(unique(dt, by = "coordGreedy"))
  coordDuplSize  = replicateSize + 1 - coordUniqSize
  
  coordBest = dt[valueGreedy == valueGreedy_min]
  coordBestUniq = unique(coordBest, by = "coordGreedy")
  
  coordBestSize      = nrow(coordBest)
  coordBestSizeUniq  = nrow(coordBestUniq)
  coordBestSizeDupl  = coordBestSize - coordBestSizeUniq
  
  coordBest_first = coordBest[1,]$coordGreedy
  
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
    coordBest_first   = coordBest_first)
  
  print(t(dtPairs))
  
  replicaId_best = dt[valueGreedy == valueGreedy_min]$replicaId
  replicaId_best_size = length(replicaId_best)
  fraction_best = round(replicaId_best_size/replicateSize, 4)
  cat("\nvalueGreedy_best =",valueGreedy_min, 
      "; replicaId_best_size =", replicaId_best_size, 
      "; fraction_best =", fraction_best, 
      "\nreplicaId_best =",
      "\n")
  print(replicaId_best)
  
  replicaId_worst = dt[valueGreedy == valueGreedy_max]$replicaId
  replicaId_worst_size = length(replicaId_worst)
  fraction_worst = round(replicaId_worst_size/replicateSize, 4)
  cat("\nvalueGreedy_worst =",valueGreedy_max, 
      "; fraction_worst =", fraction_worst,
      "\nreplicaId_worst =",
      "\n")
  print(replicaId_worst)
  
  coord_best_uniq = coordBestUniq$coordGreedy
  coord_best_uniq_size = length(coord_best_uniq)
  cat("\ncoord_best_uniq_size =", coord_best_uniq_size, 
      "\ncoord_best_uniq=", 
      "\n") 
  print(coord_best_uniq)
  
  return(dtPairs)
  
} # unate_greedy_chvatal_experiment_F


#----delete soon----
unate_greedy_chvatal_F = function()
{
  thisFunction = "unate_greedy_chvatal_F" # ; a full-matrix implementation

  # the best full matrix version of chvatal

  valueTarget   = glob[["valueTarget"]]
  replicateSize = glob[["replicateSize"]]
  greedyId      = glob[["greedyId"]]

  greedyId = "chvatal_F"

  # cat("\n** entering", thisFunction, "; greedyId=", greedyId, "\n")

  n          = glob[["nCols"]]
  m          = glob[["mRows"]]
  coord      = rep(0, n)

  M          = glob[["matrixF"]]
  colWeights = glob[["colWeights"]]

  replicaId      = glob[["replicaId"]]
  glob[["nOps"]] = 0

  start_time = Sys.time()

  while(TRUE) {
    percentages = colWeights %*% diag(1/colSums(M))
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


  # cat("\n** returning 'sampleDt' as dt from", thisFunction,
  #     "\n   ", date(), "\n\n")

  return(list(dt = dtPairs))

} # unate_greedy_chvatal_F

unate_greedy_chvatal_S = function()
{
  thisFunction = "unate_greedy_chvatal_S" # ; a sparse-matrix implementation

  # the best sparse matrix version of chvatal

  valueTarget   = glob[["valueTarget"]]
  replicateSize = glob[["replicateSize"]]
  greedyId      = glob[["greedyId"]]

  greedyId = "chvatal_S"

  # cat("\n** entering", thisFunction, "; greedyId=", greedyId, "\n")

  n          = glob[["nCols"]]
  m          = glob[["mRows"]]
  coord      = rep(0, n)

  mS         = glob[["matrixS"]]
  colWeights = glob[["colWeights"]]

  replicaId      = glob[["replicaId"]]
  seedInit       = glob[["seedInit"]]
  glob[["nOps"]] = 0

  start_time = Sys.time()

  #
  # length(dl@x)

  while(TRUE) {

    percentages = colWeights %*% diag(1/colSums(mS))
    if (all(percentages==Inf)) { break }
    if (replicaId == 0) {
      jdx = which.min(percentages)
    } else {
      jdx_vec = which(percentages == min(percentages))
      jdx_cnt = sample(1:length(jdx_vec))[1]
      jdx     = jdx_vec[jdx_cnt]
    }
    rem_vec     = which(mS[,jdx] %in% 1)
    mS[rem_vec,] = 0
    coord[jdx]  = 1
    glob[["nOps"]] = glob[["nOps"]] + 1

  }
  end_time = Sys.time()
  runtime = round(as.numeric(difftime(end_time, start_time, units = "secs")), 3)

  valueGreedy = as.numeric(t(coord) %*% colWeights)
  coord = paste(coord, collapse = "")

  diff = valueGreedy -  valueTarget

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

  # cat("\n** returning 'sampleDt' as dt from", thisFunction,
  #     "\n   ", date(), "\n\n")

  return(list(dt = dtPairs))

} # unate_greedy_chvatal_S


unate_greedy_chvatal_experiment_F = function(
  instanceDef,  OFname="OFb_unateF", greedyId="chvatal_F",
  valueTarget="", replicateSize="", isSeedConsecutive=T,  seedInit="", iterationLmt="")
{
  # given as single instanceDef, find runtime_read, runtime, and
  # solutions for solver chvatalS

  thisFunction = "unate_greedy_chvatal_experiment_F"

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


  valueTarget    = glob[["valueTarget"]]
  runtime_read_F = glob[["runtime_read"]]

  glob[["replicateSize"]] = replicateSize
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

    answF = unate_greedy_chvatal_F() ;# print(answF)

    runtime     = answF$dt$runtime
    valueGreedy = answF$dt$valueGreedy
    coordGreedy = answF$dt$coordGreedy
    diff        = answF$dt$diff


    dt = rbind(dt, list(
      instanceName = basename(instanceDef),
      valueTarget = valueTarget,
      replicaId = replicaId,
      seedInit  = seedInit,
      nOps = glob[["nOps"]],
      runtime_read = runtime_read,
      runtime_F = runtime_F,
      nOpsRate_F = round(glob[["nOps"]]/runtime_F, 3),
      valueGreedy_F = valueGreedy_F,
      diff_F = diff_F,
      coordGreedy_F = coordGreedy_F)
    )

  } # for (replicaId in 0:replicateSize)

  #print(dt, nrow=1 + replicateSize) ;# print(coordHash)
  # print(dt)
  cat("\n.. COMPLETING", thisFunction, "\n")
  # return()
  # print(dt, nrow = 1 + replicateSize)

  valueGreedy_min = min(dt$valueGreedy_F)
  ratioBest       = round(valueGreedy_min/valueTarget, 3)
  valueGreedy_max = max(dt$valueGreedy_F)
  ratioWorst      = round(valueGreedy_max/valueTarget, 3)

  valueGreedy_min    = round(valueGreedy_min, 2)
  valueGreedy_median = round(median(dt$valueGreedy), 2)
  valueGreedy_mean   = round(mean(dt$valueGreedy), 3)
  valueGreedy_stDev  = round(sd(dt$valueGreedy), 3)
  valueGreedy_max    = round(valueGreedy_max, 2)

  valueGreedy_stats  = paste(c(
    valueGreedy_min, valueGreedy_median, valueGreedy_mean,
    valueGreedy_stDev, valueGreedy_max), collapse=",")


  coordUniqSize  = nrow(unique(dt, by = "coordGreedy"))
  coordDuplSize  = replicateSize + 1 - coordUniqSize

  # return()

  coordBest = dt[valueGreedy_F == valueGreedy_min]
  coordBestUniq = unique(coordBest, by = "coordGreedy_F")

  # print(coordBestUniq)
  # print(coordBest$coordGreedy_F)
  # coordBestHash = hash() ; clear(coordBestHash)
  # h = hash(keys=letters, values="")
  # > keys(h)[1]
  # [1] "a"

  # for (coord in coordBest$coordGreedy_F) {
  #   coordBestHash[[coord]] = ""
  # }
  # print(keys(coordBestHash))
  coordBestSize      = nrow(coordBest)
  coordBestSizeUniq  = nrow(coordBestUniq)
  coordBestSizeDupl  = coordBestSize - coordBestSizeUniq

  coordBest_first = coordBest[1,]$coordGreedy_F

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
    runtime_read_F   = round(runtime_read_F, 3),
    runtime_min_F    = round(min(dt$runtime_F), 3),
    runtime_median_F = round(median(dt$runtime_F), 3),
    runtime_mean_F   = round(mean(dt$runtime_F), 3),
    runtimeS_stDev_F = round(sd(dt$runtime_F), 3),
    runtime_max_F    = round(max(dt$runtime_F), 3),
    "----" = "",
    coordBestSize     = coordBestSize,
    coordBestSizeDupl = coordBestSizeDupl,
    coordBestSizeUniq = coordBestSizeUniq,
    valueBest         = valueGreedy_min,
    coordBest_first   = coordBest_first)

  print(t(dtPairs))
  # print(coordBestHash)


  replicaId_best = dt[valueGreedy_F == valueGreedy_min]$replicaId
  replicaId_best_size = length(replicaId_best)
  fraction_best = round(replicaId_best_size/replicateSize, 4)
  cat("\nvalueGreedy_best =",valueGreedy_min,
      "; replicaId_best_size =", replicaId_best_size,
      "; fraction_best =", fraction_best,
      "\nreplicaId_best =",
      "\n")
  print(replicaId_best)

  replicaId_worst = dt[valueGreedy_F == valueGreedy_max]$replicaId
  replicaId_worst_size = length(replicaId_worst)
  fraction_worst = round(replicaId_worst_size/replicateSize, 4)
  cat("\nvalueGreedy_worst =",valueGreedy_max,
      "; fraction_worst =", fraction_worst,
      "\nreplicaId_worst =",
      "\n")
  print(replicaId_worst)

  coord_best_uniq = coordBestUniq$coordGreedy_F
  coord_best_uniq_size = length(coord_best_uniq)
  cat("\ncoord_best_uniq_size =", coord_best_uniq_size,
      "\ncoord_best_uniq=",
      "\n")
  print(coord_best_uniq)

  return(dtPairs)

} # unate_greedy_chvatal_experiment_F


unate_greedy_chvatal_experiment_S = function(
  instanceDef, OFname="OFb_unateS", greedyId="chvatal_S",isSeedConsecutive=T,
  valueTarget="", replicateSize="", seedInit="", iterationLmt="")
{
  # given as single instanceDef, find runtime_read, runtime, and
  # solutions for solver chvatalS

  thisFunction = "unate_greedy_chvatal_experiment_S"

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



  valueTarget    = glob[["valueTarget"]]
  runtime_read_S = glob[["runtime_read"]]

  glob[["replicateSize"]] = replicateSize
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

    answS = unate_greedy_chvatal_S() ;# print(answF)

    runtime_S     = answS$dt$runtime
    valueGreedy_S = answS$dt$valueGreedy
    coordGreedy_S = answS$dt$coordGreedy
    diff_S        = answS$dt$diff


    dt = rbind(dt, list(
      instanceName = basename(instanceDef),
      valueTarget = valueTarget,
      replicaId = replicaId,
      seedInit  = seedInit,
      nOps = glob[["nOps"]],
      runtime_read_S = runtime_read_S,
      runtime_S = runtime_S,
      nOpsRate_S = round(glob[["nOps"]]/runtime_S, 3),
      valueGreedy_S = valueGreedy_S,
      diff_S = diff_S,
      coordGreedy_S = coordGreedy_S)
    )

  } # for (replicaId in 0:replicateSize)

  #print(dt, nrow=1 + replicateSize) ;# print(coordHash)
  # print(dt)
  cat("\n.. COMPLETING", thisFunction, "\n")
  # return()
  # print(dt, nrow = 1 + replicateSize)

  valueGreedy_min = min(dt$valueGreedy_S)
  ratioBest       = round(valueGreedy_min/valueTarget, 3)
  valueGreedy_max = max(dt$valueGreedy_S)
  ratioWorst      = round(valueGreedy_max/valueTarget, 3)

  valueGreedy_min    = round(valueGreedy_min, 2)
  valueGreedy_median = round(median(dt$valueGreedy_S), 2)
  valueGreedy_mean   = round(mean(dt$valueGreedy_S), 3)
  valueGreedy_stDev  = round(sd(dt$valueGreedy_S), 3)
  valueGreedy_max    = round(valueGreedy_max, 2)

  valueGreedy_stats  = paste(c(
    valueGreedy_min, valueGreedy_median, valueGreedy_mean,
    valueGreedy_stDev, valueGreedy_max), collapse=",")

  # print(dt)

  coordUniqSize  = nrow(unique(dt, by = "coordGreedy_S"))
  coordDuplSize  = replicateSize + 1 - coordUniqSize

  # return()

  coordBest = dt[valueGreedy_S == valueGreedy_min]
  coordBestUniq = unique(coordBest, by = "coordGreedy_S")
  # print(coordBest$coordGreedy_F)
  # coordBestHash = hash() ; clear(coordBestHash)
  # h = hash(keys=letters, values="")
  # > keys(h)[1]
  # [1] "a"

  # for (coord in coordBest$coordGreedy_F) {
  #   coordBestHash[[coord]] = ""
  # }
  # print(keys(coordBestHash))
  coordBestSize      = nrow(coordBest)
  coordBestSizeUniq  = nrow(coordBestUniq)
  coordBestSizeDupl  = coordBestSize - coordBestSizeUniq

  coordBest_first = coordBest[1,]$coordGreedy_S

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
    runtime_read_S   = round(runtime_read_S, 3),
    runtime_min_S    = round(min(dt$runtime_S), 3),
    runtime_median_S = round(median(dt$runtime_S), 3),
    runtime_mean_S   = round(mean(dt$runtime_S), 3),
    runtimeS_stDev_S = round(sd(dt$runtime_S), 3),
    runtime_max_S    = round(max(dt$runtime_S), 3),
    "----" = "",
    coordBestSize     = coordBestSize,
    coordBestSizeDupl = coordBestSizeDupl,
    coordBestSizeUniq = coordBestSizeUniq,
    valueBest         = valueGreedy_min,
    coordBest_first   = coordBest_first)

  print(t(dtPairs))
  # print(coordBestHash)


  replicaId_best = dt[valueGreedy_S == valueGreedy_min]$replicaId
  replicaId_best_size = length(replicaId_best)
  fraction_best = round(replicaId_best_size/replicateSize, 4)
  cat("\nvalueGreedy_best =",valueGreedy_min,
      "; replicaId_best_size =", replicaId_best_size,
      "; fraction_best =", fraction_best,
      "\nreplicaId_best =",
      "\n")
  print(replicaId_best)

  replicaId_worst = dt[valueGreedy_S == valueGreedy_max]$replicaId
  replicaId_worst_size = length(replicaId_worst)
  fraction_worst = round(replicaId_worst_size/replicateSize, 4)
  cat("\nvalueGreedy_worst =",valueGreedy_max,
      "; fraction_worst =", fraction_worst,
      "\nreplicaId_worst =",
      "\n")
  print(replicaId_worst)

  coord_best_uniq = coordBestUniq$coordGreedy_S
  coord_best_uniq_size = length(coord_best_uniq)
  cat("\ncoord_best_uniq_size =", coord_best_uniq_size,
      "\ncoord_best_uniq=",
      "\n")
  print(coord_best_uniq)

  return(dtPairs)
} # unate_greedy_chvatal_experiment_S
# 
# 
# 
# 
