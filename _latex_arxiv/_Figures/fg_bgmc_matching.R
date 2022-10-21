# fg_bgmc_movieLib_data.R  

all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_matching.R" ))
  javaRDS = "fg_bgmc_matching_experiment_java_56_dt.RDS"
  rRDS = "fg_bgmc_matching_experiment_R_56_dt.RDS"
  fg_bgmc_matching_experiments_plot(javaRDS, rRDS)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_matching.R" ))
  instanceDef = "../../../_data_tiny/bigraph/unate/affil_9_12.cnfU"
  # fg_bgmc_matching_plot(instanceDef, id="matrixF")
  fg_bgmc_matching_plot(instanceDef)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_matching.R" ))
  instanceDirs = c("../../../_data/bigraph/unate/total/")
  fg_bgmc_matching_experiment_R(instanceDirs, id="matrixF")
  
  
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_matching.R" ))
  dt = fread("../../../_data/bigraph/unate/total/all.txt")
  dt$runtime_read = round(dt$runtime_read / 1000, 3)
  dt$runtime = round(dt$runtime / 1000, 3)
  fileRDS         = "fg_bgmc_matching_experiment_java_59_dt.RDS"
  saveRDS(dt     , fileRDS)
  cat(sep="",
      "------------------------------------------------",
      "\n   print(readRDS(\"", fileRDS, "\"))",
      "\n\n")
  
} # all_tests = function()


# fg_bgmc_matching_runtime_read & fg_bgmc_matching_runtime_match
fg_bgmc_matching_experiments_plot = function(javaRDS, rRDS) {
  
  dtJava = readRDS(javaRDS)
  colnames(dtJava) = c("instanceDef", "runtime_read", "runtime_match",
                       "matching_size")
  dtJava$matrix_id = "matrixF"
  dtJava$type = "Java"
  dtJava = dtJava[order(instanceDef)]
  dtJava$instanceIndex = seq(nrow(dtJava))
  
  dtR    = readRDS(rRDS)
  colnames(dtR) = c("instanceDef", "matrix_id", "matching_size",
                       "runtime_read", "runtime_match")
  dtR$type = "R"
  dtR = dtR[order(instanceDef)]
  dtR$instanceIndex = seq(nrow(dtR))
  
  dtExp = rbind(dtJava,dtR)
  
  print(dtExp, nrow=112)
  
  
  # dtIndex_read = unique(dtExp[runtime_read > 0.1]$instanceIndex)
  # 
  # dt = dtExp[instanceIndex %in% dtIndex_read,]
  
  
  dtIndex_read = unique(dtExp[runtime_read > 0.4]$instanceIndex)
  dtIndex_orlib = dtExp[instanceDef %in% c("scpb1.cnfU","scpc1.cnfU",
                                           "scpd1.cnfU", "steiner3_243_9801.cnfU",
                                           "steiner3_135_3015.cnfU")]$instanceIndex
  
  
  dtIndex_read = c(dtIndex_read, dtIndex_orlib)
  dt = dtExp[instanceIndex %in% dtIndex_read,]
  # 
  # dt = dtExp
  dt$instanceIndex = rep(seq(nrow(dt)/2),2)
  
  # dt$instanceDef = sub("W$", "U", dt$instanceDef)
  
  sp_tb = dt[,c("instanceIndex", "instanceDef")]
  sp_tb = unique(sp_tb)
  # sp2_tb = tail(dtExp[,c("instanceIndex", "instance")], 17)
  
  p1 = ggplot(dt, aes(instanceIndex, runtime_read, fill=type)) +
    geom_bar(stat = "identity", position = 'dodge') +
    scale_x_continuous(labels = as.character(seq(nrow(dt)/2)), breaks = seq(nrow(dt)/2)) +
    annotation_custom(tableGrob(sp_tb,rows = NULL,theme = ttheme_default(base_size = 8))) +
    theme(legend.position = c(0.1, 0.4), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          legend.title = element_blank())
  
  p2 = ggplot(dt, aes(instanceIndex, runtime_match, fill=type)) +
    geom_bar(stat = "identity", position = 'dodge') +
    scale_x_continuous(labels = as.character(seq(nrow(dt)/2)), breaks = seq(nrow(dt)/2)) +
    annotation_custom(tableGrob(sp_tb,rows = NULL,theme = ttheme_default(base_size = 8))) +
    theme(legend.position = c(0.1, 0.4), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          legend.title = element_blank())
  
  space=paste(rep(" ", 180), collapse = "")
  p = grid.arrange(p1, p2, nrow=1,
                    top = textGrob(paste("fg_bgmc_matching_read",space ,
                                         "fg_bgmc_matching_match"),
                                   x = 0.04, y = 0, hjust = 0,
                                   gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_matching_experiment.pdf", p, width = 14, height = 5)
  
  
}




fg_bgmc_matching_plot = function(instanceDef)
{
  # ...
  thisFunction = "fg_bgmc_matching_plot"
  
  # read the instance
  read_bgu(instanceDef, id="matrixF") ;# print(glob) ; return()
  
  instanceName = basename(instanceDef)
  nCols        = glob[["nCols"]]
  mRows        = glob[["mRows"]]
  M            = glob[["matrixF"]]
  
  # find degree of each column vertex 
  
  # find numEdges
  colDegs = c()
  for (j in 1:nCols) { colDegs[j] = sum(M[,j]) }
  edgesFromCols = sum(colDegs)
  colDegsMin      = min(colDegs)
  colDegsMedian   = median(colDegs) 
  colDegsMean     = round(mean(colDegs), 4)
  colDegsStdev    = round(sd(colDegs), 4)
  colDegsMax      = max(colDegs)
  colDegs        = paste(sort(colDegs), collapse=",")
  
  # find degree of each row vertex 
  rowDegs = c()
  for (i in 1:mRows)  { rowDegs[i] = sum(M[i,]) }
  edgesFromRows = sum(rowDegs)
  rowDegsMin      = min(rowDegs) 
  rowDegsMedian   = median(rowDegs) 
  rowDegsMean     = round(mean(rowDegs), 4)
  rowDegsStdev    = round(sd(rowDegs), 4)
  rowDegsMax      = max(rowDegs)  
  rowDegs        = paste(sort(rowDegs), collapse=",")
  
  tempM = t(M)
  rownames(tempM) = paste("p", seq(nrow(tempM)), sep = "")
  colnames(tempM) = paste("t", seq(ncol(tempM)), sep = "")
  
  
  G    = graph_from_incidence_matrix( tempM )  ;# need a transpose here  
  
  answ = max_bipartite_match(G) ;# print(answ)
  matching_size = answ$matching_size
  matching      = answ$matching 
  
  
  
  cat("\n.. edgeList \n")    
  edges <- ends(G, es=E(G), names=F) ; print(edges)
  
  V(G)$name = c(rownames(tempM), colnames(tempM))
  V(G)$color = 'cadetblue1'
  V(G)[type]$color = 'antiquewhite'
  V(G)$label.cex <- .5
  V(G)$size <- 12
  E(G)$color = "gray"
  
  match_name = names(answ$matching)
  match_value = unname(answ$matching)
  
  for (i in 1:length(match_name)) {
    if (is.na(match_value[i])) {
      next
    }
    path = c(match_name[i], match_value[i])
    # E(G, path=path)$color <- "red"
  }
  plot(G, layout = layout_as_bipartite)
  
  dtPairs = data.table(
    thisFunction = thisFunction,
    date       = date(),
    instanceDef = basename(instanceDef),
    nCols = nCols,
    mRows    = mRows,
    numEdges   = answ$matching_size,
    matrixDens = glob[["matrixDens"]], 
    " " = "",
    colDegsMin    = colDegsMin,
    colDegsMedian = colDegsMedian,
    colDegsMean   = colDegsMean,
    colDegsStdev  = colDegsStdev,
    colDegsMax    = colDegsMax,
    " " = "",
    rowDegsMin    = rowDegsMin,
    rowDegsMedian = rowDegsMedian,
    rowDegsMean   = rowDegsMean,
    rowDegsStdev  = rowDegsStdev,
    rowDegsMax    = rowDegsMax,
    colDegs = colDegs,
    rowDegs = rowDegs,
    matchingMax = matching_size,
    matchingEdges = matching
  )
  print(t(dtPairs)) 
  
  cat(".. plotted a bipartite graph with package 'igraph'\n")
  return(paste(".. returning from function", thisFunction))
  
} # fg_bgmc_matching_plot

fg_bgmc_matching_experiment_R = function(instanceDirs, id="matrixF")
{  
  
  thisFunction = "fg_bgmc_matching_experiment_R"
  cat("\n.. entering", thisFunction, " on", date(), "\n")
  
  
  # find required files form for specified instanceDir
  filePaths = c()
  for (instanceDir in instanceDirs) {
    files = list.files(instanceDir)
    for (file in files) {
      fileExts = c("cnf", "cnfU", "cnfW")
      if (file_ext(file) %in% fileExts) {
        filePaths = c(filePaths, file.path(instanceDir, file))
      }
    }
  }
  
  
  matrix_id = id
 
  dt = data.table()
  for (instanceDef in filePaths) {
    
    # read the instance
    start_time = Sys.time() 
    read_bgu(instanceDef, matrix_id)  
    instanceName = basename(instanceDef)
    nCols        = glob[["nCols"]]
    mRows        = glob[["mRows"]]
    M            = glob[["matrixF"]]
    end_time = Sys.time() 
    runtime_read  = round(as.numeric(end_time - start_time), 3)
    
    # need a transpose of M to generate the graph 
    start_time = Sys.time() 
    G = graph_from_incidence_matrix(t(M) ) 
    answ = max_bipartite_match(G)
    matching_size = answ$matching_size
    end_time = Sys.time()
    runtime_match  = round(as.numeric(end_time - start_time), 3)
    
    dt = rbind(dt, data.table(
      instanceDef  = basename(instanceDef),
      matrix_id = matrix_id,
      matching_size = matching_size,
      runtime_read = runtime_read,
      runtime_match = runtime_match)     
    )
    cat(".. solved", basename(instanceDef), 
        "runtime_read =", runtime_read,
        "runtime_match =", runtime_match,
        "\n")
  }
  
  num_of_instances = length(filePaths)
  dtPairs = data.table(
    thisFunction       = thisFunction,
    userId             = Sys.info()[["user"]], 
    cpuName            = Sys.info()[["nodename"]], 
    sysName            = Sys.info()[["sysname"]],
    dateStamp          = Sys.Date(),
    timeStamp          = format(Sys.time(), "%X"),
    "---- " = "",
    num_of_instances = num_of_instances,
    matrix_id  = matrix_id)
  
  fileRDS_dt      = paste(thisFunction,  "_", num_of_instances, "_dt.RDS", sep="")
  fileRDS_dtPairs = paste(thisFunction,  "_", num_of_instances, "_dtPairs.RDS", sep="")
  saveRDS(dt     , fileRDS_dt)
  saveRDS(dtPairs, fileRDS_dtPairs)  
  cat(sep="",
      "------------------------------------------------",
      "\n** saved datatables generated by function ", thisFunction, " in files",
      "\n   ", fileRDS_dtPairs,
      "\n   ", fileRDS_dt, 
      "\n Access any file either as",
      "\n   print(readRDS(\"", fileRDS_dtPairs, "\"))",
      "\n   print(readRDS(\"", fileRDS_dt, "\"))",
      "\n or as",
      "\n   dtP = readRDS(\"", fileRDS_dtPairs, "\")",
      "\n   dtE = readRDS(\"", fileRDS_dt, "\")",
      "\n\n")
  
  print(t(dtPairs))
  cat("\n.. completed", thisFunction, " on", date(), "\n")
  
} # fg_bgmc_matching_experiment_R

#------OLD-----
all_tests_old = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  setwd("/Users/brglez/Desktop/OPUS2-rBed-bgmc/_lib_Rtop_tests/basic_misc")
  rm(list=ls()) ; source("../../_lib_Rtop/_lib_Rtop_resources.R")
  instanceDef = "../../_benchm/unate/misc/stack_5_6.cnfU"
  igraph_cnfU(instanceDef)  
  
  setwd("/Users/brglez/Desktop/OPUS2-rBed-bgmc/_lib_Rtop_tests/basic_misc")
  rm(list=ls()) ; source("../../_lib_Rtop/_lib_Rtop_resources.R")
  instanceDef = "../../_benchm/unate/misc/skiena_7_11.cnfU"
  igraph_cnfU_mincover(instanceDef, solutions=c("1000101", "1010001"))  
  
} # all_tests_old  

igraph_bg_plot_trace1 = function(instanceDef) {
  
  thisFunction = "igraph_bg_plot_trace1"
  read_cnf(instanceDef, instanceDS="matrixI") ;# print(glob)
  M     = glob[["matrixI"]] 
  nCols = glob[["nVars"]]  
  mRows = glob[["nClauses"]]  
  print(M)
  
  # find degree of each column vertex 
  colDegs = c()
  for (j in 1:nCols) { colDegs[j] = sum(M[,j]) }
  edgesFromCols = sum(colDegs)
  colDegsMin      = min(colDegs)
  colDegsMedian   = median(colDegs) 
  colDegsMean     = round(mean(colDegs), 4)
  colDegsStdev    = round(sd(colDegs), 4)
  colDegsMax      = max(colDegs)
  colDegs        = paste(sort(colDegs), collapse=",")
  
  # find degree of each row vertex 
  rowDegs = c()
  for (i in 1:mRows)  { rowDegs[i] = sum(M[i,]) }
  edgesFromRows = sum(rowDegs)
  rowDegsMin      = min(rowDegs) 
  rowDegsMedian   = median(rowDegs) 
  rowDegsMean     = round(mean(rowDegs), 4)
  rowDegsStdev    = round(sd(rowDegs), 4)
  rowDegsMax      = max(rowDegs)  
  rowDegs        = paste(sort(rowDegs), collapse=",")
  
  stopifnot(edgesFromCols == edgesFromRows)
  numEdges   = edgesFromCols 
  matrixDens = round(numEdges/(nCols*mRows), 4)
  
  tempM = t(M)
  rownames(tempM) = c(1:nrow(tempM))
  colnames(tempM) = LETTERS[seq(from = 1, to = ncol(tempM))]
  
  
  G    = graph_from_incidence_matrix( tempM )  ;# need a transpose here  
  
  V(G)$name = c(rownames(tempM), colnames(tempM))
  
  V(G)$color = 'cadetblue1'
  V(G)[type]$color = 'antiquewhite'
  
  V(G)$label.cex <- .5
  V(G)$size <- 12
  
  edges <- ends(G, es=E(G), names=F)
  
  # https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
  library(RColorBrewer)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  
  for (i in 1:nrow(edges)) {
    path = edges[i,]
    E(G, path=path)$color <- col_vector[edges[i,1]+4]
  }
  
  plot(G, layout = layout_as_bipartite)
  
  answ = max_bipartite_match(G) ;# print(answ)
  matching_size = answ$matching_size
  matching      = paste(answ$matching, collapse=",")
  
  
  dtPairs = data.table(
    thisFunction = thisFunction,
    date       = date(),
    instanceDef = basename(instanceDef),
    nCols = nCols,
    mRows    = mRows,
    numEdges   = numEdges,
    matrixDens = matrixDens, 
    " " = "",
    colDegsMin    = colDegsMin,
    colDegsMedian = colDegsMedian,
    colDegsMean   = colDegsMean,
    colDegsStdev  = colDegsStdev,
    colDegsMax    = colDegsMax,
    " " = "",
    rowDegsMin    = rowDegsMin,
    rowDegsMedian = rowDegsMedian,
    rowDegsMean   = rowDegsMean,
    rowDegsStdev  = rowDegsStdev,
    rowDegsMax    = rowDegsMax,
    colDegs = colDegs,
    rowDegs = rowDegs,
    matchingMax = matching_size,
    matchingEdges = matching
  )
  print(t(dtPairs))
  
} # igraph_bg_plot_trace1





igraph_cnfU = function(instanceDef, seedInit="", isRandom=F)
{
  thisFunction = "igraph_cnfU"
  if (isRandom == T) {
    # create and plot a random bipartite graph
    if (seedInit == "") {seedInit = trunc(1e4*runif(1))}
    set.seed(seedInit) 
    inc <- matrix(sample(0:1, 32, replace = TRUE, prob=c(1.5,1)), 4, 8)
    incT = t(inc)
    cat("seedInit= ", seedInit, "\n") ; print(inc) ; print(incT)
    g <- graph_from_incidence_matrix(inc) ; print(V(g)$type+1)
    plot(g, layout = layout_as_bipartite,
         vertex.color=c("green","cyan")[V(g)$type+1]) 
  }
  read_cnf(instanceDef, instanceDS="matrixI") ;# print(glob)
  iM = t(glob[["matrixI"]]) ;# need a transpose here  
  g  = graph_from_incidence_matrix(iM)
  plot(g, layout = layout_as_bipartite)
  cat(".. plotted a bipartite graph with package 'igraph'\n")
  return(paste(".. returning from function", thisFunction))
} # igraph_cnfU

igraph_cnfU_mincover = function(instanceDef, solutions)
{
  thisFunction = "igraph_cnfU_mincover"
  
  read_cnf(instanceDef, instanceDS="matrixI") ;# print(glob)
  iM = t(glob[["matrixI"]]) ;# need a transpose here  
  g  = graph_from_incidence_matrix(iM)
  plot(g, layout = layout_as_bipartite)
  cat(".. plotted a bipartite graph with package 'igraph'\n")
  
  nSolutions = length(solutions)
  cat("\n number of mincover solutions = ", nSolutions, 
      "\n solutions to decode for marking vertices at level 1:",
      "\n")
  print(solutions)
  return(paste(".. returning from function", thisFunction))
} # igraph_cnfU