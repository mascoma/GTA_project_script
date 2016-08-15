library(deSolve)
library(plot3D)

outputdir <- "/isi/olga/xin/GTA_project/output/20160701/X_ratio_r_N.txt"

c.array <- seq(0.0001, 1, length.out = 500)
K.array <- seq(1e5, 1e9, length.out = 500)
r.array <- seq(0.0001, 1, length.out = 500)
N.array <- c(1e-11, 1e-10, 5e-10, seq(1e-9, 1e-8, length.out = 495), 1e-7, 1e-6) 

M <- mesh(r.array, N.array)
array <- matrix(NA, nrow = length(M$x), ncol = 5)
 
for (j in 1 : length(M$x)){
  pars <- c(r = M$x[j], # growth rate
            K = 1e9, # carry capacity
            c = 0.0015, # cost
            N = M$y[j]) # N = b*i*ro*mu
  solve.output <- solveGTAeq(pars)
  array[j,] <- c(solve.output, M$x[j], M$y[j])
  print(j)
}


write.table(array, file = outputdir, quote = F, row.names = F, sep = "\t")
 
