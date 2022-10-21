# all_tests = function()
# { 
#   # cut and paste into the R-shell any group of these commands
#   
#   rm(list=ls())
#   workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
#   lib_R_dir     = "../../../_lib_R"
#   source(file.path(lib_R_dir, "_lib_R_resources.R"))
#   source(file.path(workDir, "fg_bgmc_unate_greedy_chvatal.R" ))
#   fg_bgmc_unate_greedy_chvatal_abcd()
#   
#   rm(list=ls())
#   workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
#   lib_R_dir     = "../../../_lib_R"
#   source(file.path(lib_R_dir, "_lib_R_resources.R"))
#   data_dir   = "../../../_data/bigraph/unate/random/"
#   data_rBed_dir   = "../../../_data_rBed"
#   source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
#   tb_unate_greedy_asym(data_dir, replicateSize = 1000, outputFolder = data_rBed_dir)
#   
#   rm(list=ls())
#   workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
#   lib_R_dir     = "../../../_lib_R"
#   source(file.path(lib_R_dir, "_lib_R_resources.R"))
#   data_rBed_dir   = "../../../_data_rBed"
#   fileRDS = file.path(data_rBed_dir, "tb_unate_greedy_asym_chvatal_steiner3.RDS")
#   source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
#   tb_unate_greedy_asymp_to_latex(fileRDS)
#   
#   rm(list=ls())
#   workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
#   lib_R_dir     = "../../../_lib_R"
#   source(file.path(lib_R_dir, "_lib_R_resources.R"))
#   data_rBed_dir   = "../../../_data_rBed"
#   fileRDS = file.path(data_rBed_dir, "tb_unate_greedy_asym_chvatal_orlib.RDS")
#   # fileRDS_dt      = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dt.RDS")
#   source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
#   tb_unate_greedy_asymp_to_latex(fileRDS)
#   
#   rm(list=ls())
#   workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
#   lib_R_dir     = "../../../_lib_R"
#   source(file.path(lib_R_dir, "_lib_R_resources.R"))
#   data_rBed_dir   = "../../../_data_rBed"
#   fileRDS = file.path(data_rBed_dir, "tb_unate_greedy_asym_chvatal_random.RDS")
#   # fileRDS_dt      = file.path(data_rBed_dir, "OFb_unateF_exh_school_9_16.cnfW_dt.RDS")
#   source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
#   tb_unate_greedy_asymp_to_latex(fileRDS)
#   
#   rm(list=ls())
#   workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
#   lib_R_dir     = "../../../_lib_R"
#   source(file.path(lib_R_dir, "_lib_R_resources.R"))
#   data_dir   = "../../../_data_tiny/bigraph/unate"
#   instanceDef = file.path(data_dir, "chvatal_6_5.cnfW") ; print(file.exists(instanceDef))
#   source(file.path(workDir, "tb_bgmc_unate_greedy_chvatal.R" ))
#   tb_unate_greedy_chvatal_experiment_bound(instanceDef,replicateSize = 10, greedyId="chvatal_S")
#   
# } # all_tests = function()
# 
# 
# 
# tb_unate_greedy_chvatal_experiment_bound = function(
#   instanceDef, greedyId="chvatal_F", valueTarget="",
#   isSeedConsecutive=T) 
# {
#   # given as single instanceDef, find the upper bound for chvatal algo
#   
#   thisFunction = "tb_unate_greedy_chvatal_experiment_bound"
#   
#   cat("\n.. ENTERING", thisFunction,  
#       "\n        greedy unate cover solutions for", greedyId,
#       "\n                      date     =",  date(), "\n\n")
#   
#   
#   clear(glob)
#   unate_init_greedy(
#     instanceDef, greedyId=greedyId, valueTarget="", 
#     replicateSize="") 
#   
#   
#   valueTarget    = glob[["valueTarget"]]
#   runtime_read = glob[["runtime_read"]]
#   replicateSize = glob[["replicateSize"]]
#   colDegs = glob[["colDegs"]]
#   
#   if (is.na(valueTarget) || is.na(runtime_read) || is.na(replicateSize) ||
#       is.na(colDegs)) {
#     errorMsg = paste("\n .. ERROR from ", thisFunction, 
#                      "\n.. glob initialization has errors, please check unate_init_greedy function\n")
#     stop(errorMsg)
#   }
#   
#   colDegMax = max(glob[["colDegs"]])
#   harmonicNum = sum(1/seq(colDegMax))
#   upperBound = valueTarget * harmonicNum
#   
#   L = list(
#     colDegMax = colDegMax,
#     harmonicNum = harmonicNum,
#     upperBound = upperBound
#   )
#   
#   cat("\n.. COMPLETING", thisFunction, "\n")
#   return(L)
#   
# } # tb_unate_greedy_chvatal_experiment_bound

rm(list=ls())
workDir = "~/DeskTop/github/__rBed_latex/_Tables/tb_bgmc" ; setwd(workDir)
lib_R_dir     = "../../../_lib_R"
source(file.path(lib_R_dir, "_lib_R_resources.R"))

instanceDir   = "instance/"
outputFolder   = "~/Desktop/github/__rBed_latex/_Tables/tb_bgmc"
greedyId="chvatal_S" 
valueTarget=""
replicateSize=1
isSeedConsecutive=T 

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
  
  
  clear(glob)
  # unate_init_greedy(
  #   instanceDef, greedyId=greedyId, valueTarget="", 
    # replicateSize="") 
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
  
  
  
  
  seedInit = 0
  
  glob[["seedInit"]]      = seedInit
  glob[["seedInitFirst"]] = seedInit
  glob[["replicaId"]] = seedInit
  set.seed(seedInit)
  
  
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
  
  upperList = list(
    colDegMax = colDegMax,
    harmonicNum = harmonicNum,
    upperBound = upperBound
  )
  
  # upperList = tb_unate_greedy_chvatal_experiment_bound(instanceDef, greedyId = greedyId,)
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
    
    answ = list()
    
    answ$dt = data.table(
      seedInit           = glob[["seedInit"]],
      nOps               = glob[["nOps"]],
      runtime            = runtime, 
      nOpsRate           = glob[["nOps"]]/runtime,
      valueTarget        = valueTarget,
      valueGreedy        = valueGreedy,
      diff               = diff,
      coordGreedy        = coord)
    
    # answ = unate_greedy_chvatal() ;# print(answF) 
    
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

fileRDS = paste(sep="","tb_unate_greedy_asym_chvatal_",
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

