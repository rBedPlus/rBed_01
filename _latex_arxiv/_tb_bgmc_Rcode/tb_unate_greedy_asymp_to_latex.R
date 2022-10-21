thisFunction = "tb_unate_greedy_asymp_to_latex"

instanceRDS = "tb_unate_greedy_asym_chvatal_instance.RDS"

isLatex = T

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
  
  ratio_min = round(min(dt$ratio),2)
  ratio_mean = round(mean(dt$ratio),2)
  ratio_median = round(median(dt$ratio),2)
  ratio_stDev = round(sd(dt$ratio),2)
  ratio_max = round(max(dt$ratio),2)
  
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

print(dtInfo)