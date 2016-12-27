#!/usr/bin/env Rscript
library(animation)
library(deSolve)
library(plot3D)
source("/isi/olga/xin/GTA_project/bin/solveGTAeq.R")

args <- commandArgs(trailingOnly = TRUE)

# test if there are three arguments: if not, return an error
if (length(args) != 1) {
  stop("one arguments must be supplied arg[1] = k.\n", 
       call.=FALSE)
}  


j <- args[1] # range of j is 1 to 130

j <- as.numeric(j)

k <- 447

inputdir <- "/isi/olga/xin/GTA_project/output/20160822/"
inputfile <- paste(inputdir, "GTAnotnapara.txt", sep="") 

outputdir <- paste(inputdir,"GTAnotnarecheck_", j, "_", k, ".txt", sep = "")

range.all <- range(1, 77313)
range.sub <- range(range.all[1]+130*(k-1),  ((range.all[1]+130*k)-1))
range.sub2 <- range(range.sub[1]+1*(j-1), ((range.sub[1]+1*j)-1))

notna.dat <- read.delim2(inputfile, stringsAsFactors = F, sep = "\t", header = T)

notna.pars <- cbind(as.numeric(notna.dat$r), as.numeric(notna.dat$K), 
                    as.numeric(notna.dat$c), as.numeric(notna.dat$N))

notna.pars <- as.data.frame(notna.pars)
names(notna.pars) <- c("r", "K", "c", "N")
array <- matrix(NA, nrow = 1, ncol = 9)
for (i in range.sub2[1]:range.sub2[2]) {
  pars <- c(r = notna.pars$r[i], # growth rate
            K = notna.pars$K[i], # carry capacity
            c = notna.pars$c[i], # cost
            N = notna.pars$N[i]) # N = b*i*ro*mu
  solve.output <- solveGTAeq(pars)
  array[(i-range.sub2[1]+1),] <- c(solve.output, pars)
  print(i)
}

write.table(array, file = outputdir, quote = F, row.names = F, sep = "\t")
