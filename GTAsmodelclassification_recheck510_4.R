#!/usr/bin/env Rscript
library(animation)
library(deSolve)
library(plot3D)
source("/isi/olga/xin/GTA_project/bin/solveGTAeq.R")

inputdir <- "/isi/olga/xin/GTA_project/output/20160807/"
inputfile <- paste(inputdir, "modelrecheck_group.txt", sep="") 

outputdir <- paste("/isi/olga/xin/GTA_project/output/20160815/GTArecheckmodels_510_4", 
                   ".txt", sep = "")
recheck.dat <- read.delim2(inputfile, stringsAsFactors = F, sep = "\t", header = T)

recheck.pars <- cbind(as.numeric(recheck.dat$r), as.numeric(recheck.dat$K), 
                      as.numeric(recheck.dat$c), as.numeric(recheck.dat$N))

recheck.pars <- as.data.frame(recheck.pars)
names(recheck.pars) <- c("r", "K", "c", "N")
array <- matrix(NA, nrow = 200, ncol = 9)
for (i in 611401: 611600) {
  pars <- c(r = recheck.pars$r[i], # growth rate
            K = recheck.pars$K[i], # carry capacity
            c = recheck.pars$c[i], # cost
            N = recheck.pars$N[i]) # N = b*i*ro*mu
  solve.output <- solveGTAeq(pars)
  array[(i-611400),] <- c(solve.output, pars)
  #print(i)
}

write.table(array, file = outputdir, quote = F, row.names = F, sep = "\t")