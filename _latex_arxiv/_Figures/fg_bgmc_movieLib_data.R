# fg_bgmc_movieLib_data.R  

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
  source(file.path(workDir, "fg_bgmc_movieLib_data.R" ))
  urnSizes   = c(2^(10:14))
  trialSizes = c(2^(10:14))
  fg_urn_sample_withRepl(urnSizes, trialSizes, replicateSize=10)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_rBed"
  fileRDS_dtPairs = file.path(data_rBed_dir, "fg_urn_sample_withRepl_20_20_100_dtPairs.RDS")
  fileRDS_dt      = file.path(data_rBed_dir, "fg_urn_sample_withRepl_20_20_100_dt.RDS")
  source(file.path(workDir, "fg_bgmc_movieLib_data.R" ))
  fg_bgmc_movieLib_data_unwatched(fileRDS_dtPairs, fileRDS_dt)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_rBed_dir   = "../../../_data_rBed"
  fileRDS_dtPairs = file.path(data_rBed_dir, "fg_urn_sample_withRepl_20_20_100_dtPairs.RDS")
  fileRDS_dt      = file.path(data_rBed_dir, "fg_urn_sample_withRepl_20_20_100_dt.RDS")
  source(file.path(workDir, "fg_bgmc_movieLib_data.R" ))
  fg_bgmc_movieLib_data_unwatched_gg(fileRDS_dtPairs, fileRDS_dt)
  

  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_movieLib_data.R" ))
  fg_bgmc_movieLib_a12()
  
} # all_tests = function()


fg_bgmc_movieLib_data_unwatched_gg = function(fileRDS_dtPairs, fileRDS_dt){
  # this is a ggplot version of "fg_bgmc_movieLib_data_unwatched"
  thisFunction = "fg_bgmc_movieLib_data_unwatched_gg"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  dtE     = readRDS(fileRDS_dt)
  nRows   = nrow(dtE)
  
  # plot cover-vs-size
  x = as.integer(log2(dtE$urnSize)) ; y = dtE$couponsCover
  x_min = min(x)
  x_max = max(x)
  y_min = min(y)  ; cat("\n.. y_min =", y_min, "\n")
  y_max = max(y)  ; cat(".. y_max =", y_max, "\n\n") 
  
  dt = data.table(x=x, y=y)
  
  yW = watchedCover = 
    1 -  c(371/2^10, 1525/2^12, 6005/2^14, 24111/2^16, 96348/2^18, 386105/2^20)
  xW = c(10, 12, 14, 16, 18, 20) 
  
 
  highlight_dt= data.table(xW=xW, yW=yW)
  p1 = ggplot(data=dt, aes(x=x,y=y)) + 
    geom_point(shape=1) + 
    labs(x=paste("capacity of urns = 2^(10:20)", sep=""),
         y=paste("fraction of unique movieIDs drawn with replacement after 2^(10:20) trials",
                 sep=""),
         title = "(c)") +
    geom_hline(yintercept=1 - exp(-1), linetype="dashed", color = "red") +
    geom_point(data=highlight_dt, 
               aes(x=xW,y=yW,colour="values observed from movieLib"), 
               # color='red',
               size=3) +
    theme(legend.position = c(0.6, 0.2), legend.key.size = unit(1, 'cm'),
          legend.text = element_text(size=12),
          plot.title = element_text(hjust = 0.5, size=16))
  
  p = grid.arrange(p1,  
                   top = textGrob("fg_bgmc_movieLib_data_c", x = 0.19, y = -1,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_data_c.pdf", p, width = 7, height = 7)
    
} # fg_bgmc_movieLib_data_unwatched_gg
  
fg_bgmc_movieLib_data_unwatched = function(fileRDS_dtPairs, fileRDS_dt) 
{ 
  # fg_bgmc_movieLib_data_unwatched(fileRDS_dtPairs, fileRDS_dt)
  # urnSize replicaId couponsUniq couponsCover
  # 1:    1024         0         648      0.63281
  # 2:    1024         1         647      0.63184
  # 3:    1024         2         645      0.62988
  # 4:    1024         3         646      0.63086
  # 5:    1024         4         640      0.62500
  thisFunction = "fg_bgmc_movieLib_data_unwatched"
  
  dtPairs = readRDS(fileRDS_dtPairs)
  dtE     = readRDS(fileRDS_dt)
  nRows   = nrow(dtE)
  
  # plot cover-vs-size
  x = as.integer(log2(dtE$urnSize)) ; y = dtE$couponsCover
  x_min = min(x)
  x_max = max(x)
  y_min = min(y)  ; cat("\n.. y_min =", y_min, "\n")
  y_max = max(y)  ; cat(".. y_max =", y_max, "\n\n") 

  
  # https://stackoverflow.com/questions/4241798/how-to-increase-font-size-in-a-plot-in-r
  plot(x, y, pch=1, col="black", axes=T, cex.lab=1.1,
       xlab=paste("capacity of urns = 2^(10:20)", sep=""), 
       ylab=paste("fraction of unique movieIDs drawn with replacement after 2^(10:20) trials", sep="")
  )
  # axis(side, at=at, labels=, pos=, lty=, col=, las=, tck=, ...)
  # https://www.statmethods.net/advgraphs/axes.html
  side = 1 # bottom
  axis(side, at=x_min:x_max, cex.axis=1.1)
  side = 2 # left
  axis(side, at=seq(0.6,0.66, 0.01), cex.axis=1.1)
  
  # the population mean as the populationSize -> Inf
  # a nice illustration for the "law of large numbers"
  abline(h= 1 - exp(-1),  lwd=1, lty="dashed", col="red") 
  
  # from Eason
  # > (n = c(10, 12, 14, 16, 18,20) ); (movies = 2^n)
  # [1] 10 12 14 16 18 20
  # [1]    1024    4096   16384   65536  262144 1048576
  # unwatched = c(371, 1525, 6005, 24111, 96348, 386105)
  yW = watchedCover = 
    1 -  c(371/2^10, 1525/2^12, 6005/2^14, 24111/2^16, 96348/2^18, 386105/2^20)
  xW = c(10, 12, 14, 16, 18, 20) 
  
  for (i in 1:6) {
    points(xW[i], yW[i], pch=19, col="red", cex=1.5) 
  }

  legend_line = "values observed from movieLib"
  legend(12.0, 0.62, legend_line,  pch=19, col="red", cex=1.5)
  
  fg_label = "(c)"
  mtext(fg_label, col="black", cex= 1.3, side = 3, at=16.00, line=1.0) 
  
  watermark = "fg_bgmc_movieLib_data_c"
  mtext(watermark, col="gray47", cex= 0.9, side = 3, at=11.00, line=0.5) 

  filePdf = paste(watermark, ".pdf", sep="")
  cat("\n... save this plot as", filePdf, "\n\n")
  
} # fg_bgmc_movieLib_data_unwatched


fg_urn_sample_withRepl = function(urnSizes, trialSizes, replicateSize) 
{
  #
  thisFunction = "fg_urn_sample_withRepl"
  
  # print(urnSizes) ; print(trialSizes) 
  
  for (urnId in 1:length(urnSizes)) {
    
    urnSize   = urnSizes[urnId]
    trialSize = trialSizes[urnId]

    for (experimentCnt in 1:(replicateSize + 1)) {
      
      replicaId   = experimentCnt - 1
      couponsHash = hash() ; clear(couponsHash)
      coupons     = 1:urnSize
      seedInit    = replicaId ; set.seed(seedInit)
      
      for (trial in 1:trialSize) {
        
        coupon   = trunc(urnSize*runif(1))
        couponsHash[[paste(coupon)]] = ""
        
      } # for (trial in 1:trialSize)
      
      couponsUniq  = length(couponsHash)
      couponsCover = round(couponsUniq/urnSize, 5)

      if (replicaId == 0 && urnId == 1) {
        dt = data.table(
          urnSize = urnSize,
          replicaId = replicaId,
          couponsUniq = couponsUniq,
          couponsCover = couponsCover
        )
      } else {
        dt = rbind(dt, list(
          urnSize = urnSize,
          replicaId = replicaId,
          couponsUniq = couponsUniq,
          couponsCover = couponsCover
        ))
      }
    } # for (replicaId in 0:replicateSize
    
  } # for (urnId in 1: length(urnSizes))
  print(dt)

  dtPairs = data.table(
    thisFunction       = thisFunction,
    userId             = Sys.info()[["user"]], 
    cpuName            = Sys.info()[["nodename"]], 
    sysName            = Sys.info()[["sysname"]],
    dateStamp          = Sys.Date(),
    timeStamp          = format(Sys.time(), "%X"),
    "---- " = "",
    urnSizes       = paste(urnSizes, collapse=","),
    trialSizes     = paste(trialSizes, collapse=","),
    replicateSize  = replicateSize)
  
  dtPairs = t(dtPairs)
  print(dtPairs)
  
  urnSizeMax   = max(log2(urnSizes))
  trialSizeMax = max(log2(trialSizes))
  fileRDS_dt      = paste(thisFunction,  "_", urnSizeMax,   "_", trialSizeMax, "_", replicateSize, "_dt.RDS", sep="")
  fileRDS_dtPairs = paste(thisFunction,  "_", urnSizeMax,   "_", trialSizeMax, "_", replicateSize, "_dtPairs.RDS", sep="")
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
      "\n   dtPairs = readRDS(\"", fileRDS_dtPairs, "\")",
      "\n   dt      = readRDS(\"", fileRDS_dt, "\")",
      "\n\n")
  
  
  
} # fg_urn_sample_withRepl




fg_bgmc_movieLib_a12 = function() {
  
  thisFunction="fg_bgmc_movieLib_a12"
  
  movieTb = data.table(MOVIE_ID = c("tt74", "tt92", "tt01", "tt30"),
                       TITLE = c("The Story of the Kelly Gang",
                                 "Den sorte drAm",
                                 "Cleopatra",
                                 "LInferno"),
                       YEAR = c(1906, 1911, 1912, 1911),
                       RUNTIME_MINUTES = c(70, 53, 100, 68))
  
  watchTb = data.table(WATCH_ID = c("w1","w2","w3","w4","w5"),
                       MOVIE_ID = c("tt30","tt74","tt30","tt74","tt74"),
                       WATCH_DATE = c("11/21/2015","02/28/2017","04/28/2015",
                                      "09/10/2016","09/10/2017"),
                       MINUTES_WATCHED=c(64,24,16,25,15))
  
  watermark_top_a1 = "fg_bgmc_movieLib_data\n"
  watermark_top_a2 = "fg_bgmc_movieLib_data\n"
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1.5)),
    colhead = list(fg_params=list(cex = 1.0)),
    rowhead = list(fg_params=list(cex = 1.0)))
  
  dd1 <- ggplot() +
    annotation_custom(tableGrob(movieTb, rows = NULL, theme = mytheme)) +
    labs(title = '(a1) MovieRecord') +
    theme(plot.margin = margin(10,5,10,5, "pt"),
          plot.title = element_text(hjust = 0.5, size=16),
          plot.background = element_rect(fill="gray92", color = NA))
  
  
  title <- grobTree( rectGrob(gp=gpar(fill="gray92", col="gray92")), 
                     textGrob(watermark_top_a1, x = 0.04, y = -7, hjust = 0,
                              gp=gpar(fontsize=12,font=1, col="gray47")))
  
  p1 = grid.arrange(dd1, top = title)
  
  ggsave("fg_bgmc_movieLib_data_a1.pdf", p1, width = 7, height = 3)
  
  dd2 <- ggplot() +
    annotation_custom(tableGrob(watchTb, rows = NULL, theme = mytheme)) +
    labs(title = '(a2) WatchRecord') +
    theme(plot.margin = margin(10,5,10,5, "pt"),
          plot.title = element_text(hjust = 0.5, size=16),
          plot.background = element_rect(fill="gray92", color = NA))
  
  
  title <- grobTree( rectGrob(gp=gpar(fill="gray92", col="gray92")), 
                     textGrob(watermark_top_a2, x = 0.14, y = -6, hjust = 0,
                              gp=gpar(fontsize=12,font=1, col="gray47")))
  
  p2 = grid.arrange(dd2, top = title)
  
  ggsave("fg_bgmc_movieLib_data_a2.pdf", p2, width = 7, height = 3)
  
}




