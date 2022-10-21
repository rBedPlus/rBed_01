# fg_bgmc_unate_school_9_16.R   # MUST be sourced from github/_rlibR/_lib_R_resources.R

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
