#!/usr/bin/env Rscript
library(animation)
library(deSolve)
library(plot3D)
source("/isi/olga/xin/GTA_project/bin/solveGTAeq.R")

args <- commandArgs(trailingOnly = TRUE)

# test if there are three arguments: if not, return an error
if (length(args) != 2) {
  stop("two arguments must be supplied arg[1] = j, arg[2] = k.\n", 
       call.=FALSE)
}  

j <- args[1] # range of j is 1 to 50
k <- args[2] # range of K is 1 to 10
j <- as.numeric(j)
k <- as.numeric(k)

outputdir <- paste("/isi/olga/xin/GTA_project/output/20160709/GTAmodels_", k, 
                   "_", j, ".txt", sep = "")

c.array <- c(0.0001, 0.0002, 0.0005, 0.0008, 0.001, 
             seq(0.0015, 0.1, length.out = 36), 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
             0.8, 0.9, 1)
K.array <- c(1e5, 1e6, 1e7, 2e7, 5e7, 8e7, seq(1e8, 1e9, length.out = 44))
r.array <- c(seq(0.0001, 0.03, length.out = 26), 0.05, 0.06, 0.08, 0.09,
             seq(0.1, 1, length.out = 20))
N.array <- c(1e-10, 5e-10, seq(1e-9, 1.5e-9, length.out = 42), 
             2e-9, 5e-9, 8e-9, 1e-8, 1e-7, 1e-6)
M <- mesh(c.array, K.array, r.array)
 
array <- matrix(NA, nrow = 2500, ncol = 9)

for (i in (2500*(k-1)+1): (2500*k)) {
    pars <- c(r = M$z[i], # growth rate
              K = M$y[i], # carry capacity
              c = M$x[i], # cost
              N = N.array[j]) # N = b*i*ro*mu
    solve.output <- solveGTAeq(pars)
    array[(i-(k-1)*2500),] <- c(solve.output, pars)
    print(i)
}
 
write.table(array, file = outputdir, quote = F, row.names = F, sep = "\t")
