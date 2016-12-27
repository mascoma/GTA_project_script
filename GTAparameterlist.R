library(plot3D)

dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20160824/GTAmodelpars_"
c.array <- c(0.0001, 0.0002, 0.0005, 0.0008, 0.001, 
             seq(0.0015, 0.1, length.out = 36), 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
             0.8, 0.9, 1)
K.array <- c(1e5, 1e6, 1e7, 2e7, 5e7, 8e7, seq(1e8, 1e9, length.out = 44))
r.array <- c(seq(0.0001, 0.03, length.out = 26), 0.05, 0.06, 0.08, 0.09,
             seq(0.1, 1, length.out = 20))
N.array <- c(1e-10, 5e-10, seq(1e-9, 1.5e-9, length.out = 42), 
             2e-9, 5e-9, 8e-9, 1e-8, 1e-7, 1e-6)
M <- mesh(c.array, K.array, r.array)

for (i in 1:length(N.array)){
  pars.list <- cbind(M$z, M$y, M$x, N.array[i])
  pars.list <- as.data.frame(pars.list)
  names(pars.list) <- c("r", "K", "c", "N")
  output <- paste(dir, i, ".txt", sep = "")
  write.table(pars.list, file = output, quote = F, row.names = F, sep = "\t")
}

 