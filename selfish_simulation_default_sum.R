dir <- "/isi/olga/xin/GTA_project/output/20160422/"
dir1 <- "/isi/olga/xin/GTA_project/output/20160503/"
outputfilename <- "mu_default.txt"
outputfiledir <- paste(dir1, outputfilename, sep = '')
files <- list.files(path = dir, pattern = "sf_default_")
mu.takeover <- vector(mode = "numeric", length = length(files))
for(x in 1:length(files)){
  input <- paste(dir, files[x], sep = '')
  load(input)
  for(y in 1:length(output)){
    pop_time <- output[[y]]$data[, 1:3]
    colnames(pop_time) <- c("time", "X1","X2")
    pop <- pop_time[length(pop_time[, 1]), 2:3]
    if (pop[1] > pop[2]) {
      mu.takeover[x] <- mu[y]
      break
    }
  }
  rm(list = ls(pattern = ".RData")) 
}
mu.takeover2 <- cbind(mu.takeover, "default") 
write.table(mu.takeover2, file = outputfiledir, sep = "\t")