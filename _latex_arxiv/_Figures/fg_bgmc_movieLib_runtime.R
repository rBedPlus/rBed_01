# fg_bgmc_movieLib_data.R  

all_tests = function()
{ 
  # cut and paste into the R-shell any group of these commands
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_movieLib_runtime.R" ))
  fg_bgmc_movieLib_runtime_search()
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  source(file.path(workDir, "fg_bgmc_movieLib_runtime.R" ))
  fg_bgmc_movieLib_runtime_init()
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data/movieLib/"
  source(file.path(workDir, "fg_bgmc_movieLib_runtime.R" ))
  unate_316_table_experiments(data_dir, numberMovies=10)
  
  rm(list=ls())
  workDir = "~/DeskTop/github/__rBed_latex/_Figures/fg_bgmc" ; setwd(workDir)
  lib_R_dir     = "../../../_lib_R"
  source(file.path(lib_R_dir, "_lib_R_resources.R"))
  data_dir   = "../../../_data/movieLib/"
  source(file.path(workDir, "fg_bgmc_movieLib_runtime.R" ))
  unate_316_frame_experiments(data_dir, numberMovies=10)
  
  
} # all_tests = function()


#------plot------
fg_bgmc_movieLib_runtime_init = function() {
  
  data_rBed_dir   = "../../../_data_rBed"
  RDS_folder = c(file.path(data_rBed_dir, "Java_chainHash.RDS"),
                 file.path(data_rBed_dir, "R_dataTable.RDS"))
  
  
  dtExp = data.table()
  
  for (file in RDS_folder) {
    # print(str_split(file,"\\.")[[1]][1])
    dt = readRDS(file)
    colnames(dt) = c("size", "runtime_read", "runtime_search")
    legend = str_split(basename(file),"\\.")[[1]][1]
    dt$legend = legend
    dtExp = rbind(dtExp, dt)
  }
  
  dtExp$runtime_total = dtExp$runtime_read + dtExp$runtime_search
  print(dtExp)
  
  size = unique(dtExp$size)
  
  p1<-ggplot(dtExp, aes(x=size, y=runtime_total, color=legend)) +
    geom_line()  +
    scale_x_continuous("size of movieLib=2^(10:20)", labels = as.character(size), breaks = size) +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          legend.title = element_blank())
  
  p = grid.arrange(p1, ncol=1, nrow=1,  
                   top = textGrob("fg_bgmc_movieLib_runtime_init", x = 0.07, hjust = 0,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_runtime_init.pdf",
         p, width = 7, height = 4)
  # ggsave("proj_316/__figures/fg_bgmc_movieLib_runtime_316_java_vs_R_search.png", p)
}


fg_bgmc_movieLib_runtime_search = function() {
  
  data_rBed_dir   = "../../../_data_rBed"
  RDS_folder = c(file.path(data_rBed_dir, "Java_chainHash.RDS"),
                 file.path(data_rBed_dir, "R_dataTable.RDS"),
                 file.path(data_rBed_dir, "R_dataFrame.RDS"))
  
  dtExp = data.table()
  
  for (file in RDS_folder) {
    # print(str_split(file,"\\.")[[1]][1])
    dt = readRDS(file)
    colnames(dt) = c("size", "runtime_read", "runtime_search")
    dt$legend = str_split(basename(file),"\\.")[[1]][1]
    dtExp = rbind(dtExp, dt)
  }
  
  dtExp$runtime_total = dtExp$runtime_read + dtExp$runtime_search
  print(dtExp)
  
  size = unique(dtExp$size)
  
  p1<-ggplot(dtExp, aes(x=size, y=runtime_search, color=legend)) +
    geom_line()  +
    scale_x_continuous("size of movieLib=2^(10:20)", labels = as.character(size), breaks = size) +
    labs(title = "(d)") +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          plot.title = element_text(size=10, hjust = 0.5), 
          legend.title = element_blank())
  
  p = grid.arrange(p1, ncol=1, nrow=1,  
                   top = textGrob("fg_bgmc_movieLib_runtime_search", x = 0.07, y = -1,
                                  hjust = 0,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_runtime_search_d.pdf",
         p, width = 7, height = 4)
  # ggsave("proj_316/__figures/fg_bgmc_movieLib_runtime_316_java_vs_R_search.png", p)
}

fg_bgmc_movieLib_runtime_read = function() {
  
  data_rBed_dir   = "../../../_data_rBed"
  RDS_folder = c(file.path(data_rBed_dir, "Java_chainHash.RDS"),
                 file.path(data_rBed_dir, "R_dataTable.RDS"),
                 file.path(data_rBed_dir, "R_dataFrame.RDS"))
  
  dtExp = data.table()
  
  for (file in RDS_folder) {
    # print(str_split(file,"\\.")[[1]][1])
    dt = readRDS(file)
    colnames(dt) = c("size", "runtime_read", "runtime_search")
    dt$legend = str_split(basename(file),"\\.")[[1]][1]
    dtExp = rbind(dtExp, dt)
  }
  
  dtExp$runtime_total = dtExp$runtime_read + dtExp$runtime_search
  print(dtExp)
  
  size = unique(dtExp$size)
  
  p1<-ggplot(dtExp, aes(x=size, y=runtime_read, color=legend)) +
    geom_line()  +
    scale_x_continuous("size of movieLib=2^(10:20)", labels = as.character(size), breaks = size) +
    labs(title = "(c)") +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          plot.title = element_text(size=10, hjust = 0.5), 
          legend.title = element_blank())
  
  p = grid.arrange(p1, ncol=1, nrow=1,  
                   top = textGrob("fg_bgmc_movieLib_runtime_read", x = 0.19, y = -1,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_runtime_read_c.pdf",
         p, width = 7, height = 4)
  # ggsave("proj_316/__figures/fg_bgmc_movieLib_runtime_316_java_vs_R_search.png", p)
} #-->> pdf plot of specified size)

fg_bgmc_movieLib_runtime_total_ab = function() {
  
  
  data_rBed_dir   = "../../../_data_rBed"
  RDS_folder = c(file.path(data_rBed_dir, "Java_chainHash.RDS"),
                 file.path(data_rBed_dir, "Java_hashMultiMap.RDS"),
                 file.path(data_rBed_dir, "Java_linearProbing.RDS"),
                 file.path(data_rBed_dir, "Java_linkedHashMultimap.RDS"))
  
  dtExp = data.table()
  
  for (file in RDS_folder) {
    # print(str_split(file,"\\.")[[1]][1])
    dt = readRDS(file)
    colnames(dt) = c("size", "runtime_read", "runtime_search")
    dt$legend = str_split(basename(file),"\\.")[[1]][1]
    dtExp = rbind(dtExp, dt)
  }
  
  dtExp$runtime_total = dtExp$runtime_read + dtExp$runtime_search
  print(dtExp)
  
  size = unique(dtExp$size)
  
  p1<-ggplot(dtExp, aes(x=size, y=runtime_total, color=legend)) +
    geom_line()  +
    scale_x_continuous("size of movieLib=2^(10:20)", labels = as.character(size), breaks = size) +
    labs(title = "(a)") +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
          legend.text = element_text(size=15),
          plot.title = element_text(size=10, hjust = 0.5), 
          legend.title = element_blank())
  
  p = grid.arrange(p1, ncol=1, nrow=1,  
                   top = textGrob("fg_bgmc_movieLib_runtime_total", x = 0.19, y = -1,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_runtime_total_a.pdf", p, width = 7, height = 4)
  
  
  
  
  data_rBed_dir   = "../../../_data_rBed"
  RDS_folder = c(file.path(data_rBed_dir, "Java_chainHash.RDS"),
                 file.path(data_rBed_dir, "R_dataTable.RDS"),
                 file.path(data_rBed_dir, "R_dataFrame.RDS"))
  
  dtExp = data.table()
  
  for (file in RDS_folder) {
    # print(str_split(file,"\\.")[[1]][1])
    dt = readRDS(file)
    colnames(dt) = c("size", "runtime_read", "runtime_search")
    dt$legend = str_split(basename(file),"\\.")[[1]][1]
    dtExp = rbind(dtExp, dt)
  }
  
  dtExp$runtime_total = dtExp$runtime_read + dtExp$runtime_search
  print(dtExp)
  
  size = unique(dtExp$size)
  
  p1<-ggplot(dtExp, aes(x=size, y=runtime_total, color=legend)) +
    geom_line()  +
    scale_x_continuous("size of movieLib=2^(10:20)", labels = as.character(size), breaks = size) +
    labs(title = "(b)") +
    theme(legend.position = c(0.3, 0.6), legend.key.size = unit(.3, 'inch'),
        legend.text = element_text(size=15),
        plot.title = element_text(size=10, hjust = 0.5), 
        legend.title = element_blank())
  
  p = grid.arrange(p1, ncol=1, nrow=1,  
                   top = textGrob("fg_bgmc_movieLib_runtime_total", x = 0.19, y = -1,
                                  gp=gpar(fontsize=8,font=1, col="gray47")))
  
  
  ggsave("fg_bgmc_movieLib_runtime_total_b.pdf", p, width = 7, height = 4)
  # ggsave("proj_316/__figures/fg_bgmc_movieLib_runtime_316_java_vs_R_search.png", p)
} #-->> pdf plot of specified size)


#-----functions-----
unate_316_table_experiments = function(instanceDir, numberMovies, isRDS=TRUE) {
  thisFunction = "unate_316_S_experiments"
  
  dtExp = data.table()
  
  solverName = "Top_Movie_T"
  
  dtExp = rbind(dtExp, auto_run_R_table(numberMovies, instanceDir, 1, 10, 20))
  
  if (isRDS) {
    fileRDS = "R_matrixS.RDS"
    saveRDS(dtExp, fileRDS)
    cat(sep="", "\n** saved a datatable generated by function ", thisFunction,
        "\n in the file = ", fileRDS, "\n",
        "\n Access the file with either as",
        "\n                 print(readRDS(\"", fileRDS, "\"))",
        "\n or as",
        "\n               dtExp = readRDS(\"", fileRDS, "\")",
        "\n")    
  } else {
    cat("\n** a datatable stdout generated by function", thisFunction, "**\n\n")
    print(dtExp)   
  }
  cat("\n.. returning from", thisFunction, "on", date(), "\n\n")
} # bgcu_experiment

unate_316_frame_experiments = function(instanceDir, numberMovies, isRDS=TRUE) {
  thisFunction = "movieManager_M_experiment"
  
  dtExp = data.table()
  
  solverName = "Top_Movie_M"
  
  dtExp = rbind(dtExp, auto_run_R_frame(numberMovies, instanceDir, 1, 10, 20))
  
  if (isRDS) {
    fileRDS = "R_matrixF.RDS"
    saveRDS(dtExp, fileRDS)
    cat(sep="", "\n** saved a datatable generated by function ", thisFunction,
        "\n in the file = ", fileRDS, "\n",
        "\n Access the file with either as",
        "\n                 print(readRDS(\"", fileRDS, "\"))",
        "\n or as",
        "\n               dtExp = readRDS(\"", fileRDS, "\")",
        "\n")    
  } else {
    cat("\n** a datatable stdout generated by function", thisFunction, "**\n\n")
    print(dtExp)   
  }
  cat("\n.. returning from", thisFunction, "on", date(), "\n\n")
} # bgcu_experiment


# auto-run R implementation and generates a txt file in _resultsArchive_R
auto_run_R_table <- function(numberOfMovies, file_path, seedInit, lower_bound, upper_bound) {
  set.seed(seedInit)
  files <- list.files(file_path)
  read_time_vec <- c()
  search_time_vec <- c()
  size_vec <- c()
  movie_vec <- c()
  watch_vec <- c()
  for (file in files) {
    if (substr(file,1,1) == "w") {
      watch_vec <- c(watch_vec, file)
    } else if (substr(file,1,1) == "m") {
      movie_vec <- c(movie_vec, file)
    }
  }
  i <- lower_bound
  # measure <- "sysTime"
  while (i <= upper_bound) {
    # print(paste("_", i, ".csv", sep = ""))
    movie_name <- ""
    watch_name <- ""
    time <- ""
    for (movie in movie_vec) {
      if (endsWith(movie, paste("_", i, ".csv", sep = ""))) {
        movie_name <- movie
        break
      }
    }
    for (watch in watch_vec) {
      if (endsWith(watch, paste("_", i, ".csv", sep = ""))) {
        watch_name <- watch
        break
      }
    }
    answ = unate_316_table(numberOfMovies, paste(file_path, movie_name, sep = ""), paste(file_path,watch_name, sep = ""), seedInit)
    
    read_time_vec <- c(read_time_vec, answ$runtime_read)
    search_time_vec <- c(search_time_vec, answ$runtime_search)
    size_vec <- c(size_vec, i)
    i <- i + 2
  }
  
  r_table <- data.table(`Size(2^x)` = size_vec, R_read_T = read_time_vec, R_search_T = search_time_vec)
  
  return(r_table)
}


# auto-run R implementation and generates a txt file in _resultsArchive_R
auto_run_R_frame <- function(numberOfMovies, file_path, seedInit, lower_bound, upper_bound) {
  
  set.seed(seedInit)
  files <- list.files(file_path)
  read_time_vec <- c()
  search_time_vec <- c()
  size_vec <- c()
  movie_vec <- c()
  watch_vec <- c()
  for (file in files) {
    if (substr(file,1,1) == "w") {
      watch_vec <- c(watch_vec, file)
    } else if (substr(file,1,1) == "m") {
      movie_vec <- c(movie_vec, file)
    }
  }
  i <- lower_bound
  # measure <- "sysTime"
  while (i <= upper_bound) {
    # print(paste("_", i, ".csv", sep = ""))
    movie_name <- ""
    watch_name <- ""
    time <- ""
    for (movie in movie_vec) {
      if (endsWith(movie, paste("_", i, ".csv", sep = ""))) {
        movie_name <- movie
        break
      }
    }
    for (watch in watch_vec) {
      if (endsWith(watch, paste("_", i, ".csv", sep = ""))) {
        watch_name <- watch
        break
      }
    }
    
    answ = unate_316_frame(numberOfMovies, paste(file_path, movie_name, sep = ""), paste(file_path,watch_name, sep = ""), seedInit)
    
    read_time_vec <- c(read_time_vec, answ$runtime_read)
    search_time_vec <- c(search_time_vec, answ$runtime_search)
    size_vec <- c(size_vec, i)
    i <- i + 2
  }
  r_table <- data.table(`Size(2^x)` = size_vec, R_read_M = read_time_vec, R_search_M = search_time_vec)
  
  return(r_table)
}

unate_316_table <- function(numberOfMovies, movieDef, watchDef, seedInit) {
  
  runtime_read = system.time({
    movie_table <- fread(movieDef, fill = TRUE, sep = ",")
    watch_table <- fread(watchDef, fill = TRUE)
  })[["elapsed"]]
  
  runtime_search = system.time({
    if (numberOfMovies <= 0) {
      stop("Please enter a number > 0.\n")
    }
    
    if (!"VIEWERS"%in% colnames(watch_table)) {
      watch_table <- watch_table[, VIEWERS := 1:.N, by="MOVIE_ID"]
    }
    unique_watch_table <- unique(watch_table, by = "MOVIE_ID", fromLast = TRUE)
    sort_watch_table = unique_watch_table[order(-unique_watch_table$VIEWERS)]
    head_watch_table = head(sort_watch_table, numberOfMovies)
    
    dtExp = movie_table[with(movie_table, ID %in% head_watch_table$MOVIE_ID),]
    merge_table = merge(dtExp, head_watch_table, by.x ="ID", by.y = "MOVIE_ID")
    
    merge_table <- merge_table[order(-merge_table$VIEWERS, merge_table$TITLE, merge_table$ID)]
    merge_table <- merge_table[, c("ID", "TITLE", "YEAR", "VIEWERS")]
    
    if (nrow(merge_table) == 0) {
      return("No movies have been streamed.\n")
    }
    
  })[["elapsed"]]
  
  return(list(table = merge_table, runtime_read = runtime_read, runtime_search = runtime_search))
  
}

unate_316_frame <- function(numberOfMovies, movieDef, watchDef, seedInit) {
  
  runtime_read = system.time({
    movie_table <- read.table(movieDef, fill = TRUE, sep = ",", header=T)
    watch_table <- read.table(watchDef, fill = TRUE, sep = ",", header=T)
  })[["elapsed"]]
  
  runtime_search = system.time({
    if (numberOfMovies <= 0) {
      return("Please enter a number > 0.\n")
    }
    watch_table$VIEWERS = 0
    # https://statisticsglobe.com/count-number-of-cases-within-each-group-of-data-frame-in-r
    watch_table = aggregate(VIEWERS ~ `MOVIE_ID`, data = watch_table, FUN = length)
    
    unique_watch_table <- unique(watch_table, by = "MOVIE_ID", fromLast = TRUE)
    sort_watch_table = unique_watch_table[order(-unique_watch_table$VIEWERS),]
    head_watch_table = head(sort_watch_table, numberOfMovies)
    
    dtExp = movie_table[with(movie_table, ID %in% head_watch_table$MOVIE_ID),]
    merge_table = merge(dtExp, head_watch_table, by.x ="ID", by.y = "MOVIE_ID")
    merge_table <- merge_table[order(-merge_table$VIEWERS, merge_table$TITLE, merge_table$ID),]
    merge_table <- merge_table[, c("ID", "TITLE", "YEAR", "VIEWERS")]
    
    if (nrow(merge_table) == 0) {
      return("No movies have been streamed.\n")
    }
    
  })[["elapsed"]]
  
  return(list(table = merge_table, runtime_read = runtime_read, runtime_search = runtime_search))
}

