# fg_bgmc_movieLib_data.R  

all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  instance_dir = "../../../_data/movieLib"
  instanceDef = file.path(instance_dir, "watchRecords_10.csv")
  source(file.path(workDir, "fg_bgmc_movieLib_best10.R" ))
  fg_bgmc_movieLib_best10(instanceDef)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  instance_dir = "../../../_data/movieLib"
  instanceDef = file.path(instance_dir, "watchRecords_10.csv")
  source(file.path(workDir, "fg_bgmc_movieLib_best10.R" ))
  fg_bgmc_movieLib_best10_occurrence(instanceDef)
  
} # all_tests = function()

fg_bgmc_movieLib_best10 = function(instanceDef) {
  
  dt = fread(instanceDef)
  dt_best10 = dt[, COUNT := 1:.N, by="MOVIE_ID"]
  dt_best10 = unique(dt_best10, by="MOVIE_ID", fromLast = T)
  dt_best10 = head(dt_best10[order(-COUNT)], 10)
  dt_best10 = dt_best10[,c("MOVIE_ID", "COUNT")]
  
  cat("\n...Top 10 movies\n")
  
  # print(dt_best10)
  
  dt_best10$START = 0
  dt_best10$idx = seq(nrow(dt_best10))
  
  # print(dt_best10)
  # ...Top 10 movies
  #     MOVIE_ID COUNT START idx
  # 1: rivdpbpzb     9     0   1
  # 2: hfagnutxq     8     0   2
  # 3: fqdqgwjbe     8     0   3
  # 4: xivrbrexd     8     0   4
  # 5: aaabfidgn     8     0   5
  # 6: nbarpuura     8     0   6
  # 7: ekdunuezc     8     0   7
  # 8: qmjdhvpym     8     0   8
  # 9: kyosyinqz     8     0   9
  # 10: tt0125678    7     0  10
  
  
  sp2 = ggplot(data=dt_best10, aes(x=idx))  +
    geom_segment(aes(x = idx, y = START, xend = idx, yend = COUNT), col="red", lwd=1) + 
    geom_point(y=dt_best10$COUNT) + 
    geom_point(y=dt_best10$START) +
    scale_x_continuous(labels = as.character(dt_best10$idx), breaks = dt_best10$idx) +
    labs(x="Index of top 10 movies", y="Number of occurrence for movieLib") +
    labs(title = "(a)") +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          plot.title = element_text(size=10, hjust = 0.5))
  
  p = grid.arrange(sp2,  
                   top = textGrob("fg_bgmc_movieLib_best10", x = 0.18, y = -1,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_best10.pdf", p, width = 7, height = 3.5)
}

fg_bgmc_movieLib_best10_occurrence = function(file)
{
  thisFunction = "movie_watched"
  cat("\n...this function generates a plot where it counts how many 
      movies have been watched a certain number of times..\n")
  
  
  fileBase   = my_file_root(file)  
  fileExt    = my_file_ext(file)
  stopifnot(fileExt == "csv")
  
  df = fread(file) ;# print(df) ;# create a dataframe 
  
  dt = df[,count := 1:.N, by="MOVIE_ID"]
  dt = unique(dt, by="MOVIE_ID", fromLast = T)
  count_max = max(dt$count)
  
  occ = rep(NA, count_max)
  for (i in 1:count_max) {
    val = nrow(dt[count==i,])
    occ[i] = val
  }
  dt = data.table(N=occ, watchedFreqV=seq(length(occ)), start=1)
  
  # print(dt)
  # N watchedFreqV start
  # 1: 385128            1     1
  # 2: 193048            2     1
  # 3:  64321            3     1
  # 4:  16171            4     1
  # 5:   3207            5     1
  # 6:    512            6     1
  # 7:     75            7     1
  # 8:      8            8     1
  # 9:      1            9     1
  
  
  
  sp2 = ggplot(data=dt, aes(x=watchedFreqV))  +
    
    scale_y_log10() +
    geom_point(aes(y=N)) + 
    
    geom_segment(aes(x = watchedFreqV, y = start, xend = watchedFreqV, yend = N),
                 col="red", lwd=1) + 
    
    geom_point(aes(y=start)) +
     
    scale_x_continuous(labels = as.character(dt$watchedFreqV), breaks = dt$watchedFreqV) +
    labs(x="Index for movie occurrences", y="Number of watched movies with specific times") +
    labs(title = "(b)") +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          plot.title = element_text(size=10, hjust = 0.5)) 
  
  p = grid.arrange(sp2,  
                   top = textGrob("fg_bgmc_movieLib_best10_occurrence", x = 0.25, y = -1,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_best10_occurrence.pdf", p, width = 7, height = 3.5)
  
  
  
  # margin text watermark for this plot
  
  return(paste(".. exiting", thisFunction, "plotted watchedFreq", date()))


} # movie_watched

