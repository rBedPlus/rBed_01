all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
  fg_bgmc_unate_greedy_chvatal_abcd()
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  instanceDir   = "../../../_data/bigraph/unate/random/"
  data_rBed_dir   = "../../../_data_rBed"
  source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
  tb_unate_greedy_asym(instanceDir, replicateSize = 1000, outputFolder = data_rBed_dir)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_RDS"
  fileRDS = file.path(data_rBed_dir, "tb_unate_greedy_asym_chvatal_steiner3.RDS")
  source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
  tb_unate_greedy_asymp_to_latex(fileRDS)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_rBed"
  fileRDS = file.path(data_rBed_dir, "tb_unate_greedy_asym_chvatal_orlib.RDS")
  # fileRDS_dt      = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dt.RDS")
  source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
  tb_unate_greedy_asymp_to_latex(fileRDS)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_rBed"
  fileRDS = file.path(data_rBed_dir, "tb_unate_greedy_asym_chvatal_random.RDS")
  # fileRDS_dt      = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dt.RDS")
  source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
  tb_unate_greedy_asymp_to_latex(fileRDS)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data_tiny/bigraph/unate"
  instanceDef = file.path(data_dir, "chvatal_6_5.cnfW") ; print(file.exists(instanceDef))
  source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
  tb_unate_greedy_chvatal_experiment_bound(instanceDef,replicateSize = 10, greedyId="chvatal_S")
  
} # all_tests = function()



#----tables----
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
  
  files = list.files(instanceDir, pattern=".*.cnf")
  
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


tb_unate_greedy_asymp_to_latex = function(instanceRDS, isLatex=T) {
  
  # convert RDS file generated by tb_unate_greedy_asym into latex format table
  thisFunction = "tb_unate_greedy_asymp_to_latex"
  
  dtExp = readRDS(instanceRDS)
  
  dtFinal = dtExp[,c("instanceDef", "nCols", "mRows", "replicateSize",
                     "isSeedConsecutive", "greedyId", "runtime_read",
                     "runtime", "nOps", "cpuName", "dateStamp",
                     "ratio", "valueTarget", "valueGreedy", "upperBound",
                     "colDeg_max")]
  
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
                              "valueTarget", "upperBound", "colDeg_max")], by = "instanceDef")
  
  runtime_read_stats = runtime_read_stat_vec
  runtime_stats = runtime_stat_vec
  nOps_stats = nOps_stat_vec
  ratio_stats = ratio_stat_vec
  valueGreedy_stats = valueGreedy_stat_vec
  
  dtInfo = cbind(dtFinal, runtime_read_stats, runtime_stats, nOps_stats,
                 ratio_stats, valueGreedy_stats)
  
  if (isLatex) {
    cat("\n...Latex format start...\n\n")
    # for (i in 1:nrow(dtInfo)) {
    line = paste(dtInfo$instanceDef, dtInfo$nCols, dtInfo$mRows, dtInfo$colDeg_max,
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
  
  
  cat("\nvalueGreedy_best =",valueGreedy_min, 
      "; replicaId_best_size =", replicaId_best_size, 
      "; fraction_best =", fraction_best, 
      "\nreplicaId_best =",
      "\n")
  print(replicaId_best)
  
  
  cat("\nvalueGreedy_worst =",valueGreedy_max, 
      "; fraction_worst =", fraction_worst,
      "\nreplicaId_worst =",
      "\n")
  print(replicaId_worst)
  
  
  cat("\ncoord_best_uniq_size =", coord_best_uniq_size, 
      "\ncoord_best_uniq=", 
      "\n") 
  print(coord_best_uniq)
  
  return(dtPairs)
  
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


