# fg_bgmc_unate_school_9_16.R   # MUST be sourced from github/_rlibR/_lib_R_resources.R

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
