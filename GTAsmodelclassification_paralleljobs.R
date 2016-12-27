#!/usr/bin/env Rscript

### this script is to parallel iteration processes with the pbs job t mode
### given a for loop in the range (1:x), we sill split the processes to y parallel jobs with each job
### runs (x/y) iterations. 


library(deSolve)
source("/isi/olga/xin/GTA_project/bin/solveGTAeq.R")

args <- commandArgs(trailingOnly = TRUE)

# test if there are three arguments: if not, return an error
if (length(args) != 4) {
  stop("five arguments must be supplied input <- args[1] # file name with full dir
output\n file <- args[2] # file name with full dir \n
       l <- args[3] # number of iteration for each job \n
       j <- args[4] # assigned by t.\n", 
       call.=FALSE)
}  

input <- args[1] # file name with full dir
outputfile <- args[2] # file name with full dir
l <- args[3] # number of iteration for each job
j <- args[4] # assigned by t
l <- as.numeric(l)
j <- as.numeric(j)
 
### test
#input <- "/Users/Xin/Desktop/projects/GTA_project/output/20160824/GTAmodelpars_1.txt"
#outputfile <- "/Users/Xin/Desktop/projects/GTA_project/output/20160824/test"
#l <- 100
#j <- 1

output <- paste(outputfile, "_", j,".txt", sep = "")

dat <- read.delim2(input, stringsAsFactors = F, sep = "\t", header = T)
range.full <- range(1, length(dat[, 1]))
range.sub <- range(range.full[1]+l*(j-1),  ((range.full[1]+l*j)-1))
 
array <- matrix(NA, nrow = l, ncol = 9)
for (i in range.sub[1]:range.sub[2]) {
  pars <- c(r = as.numeric(dat$r[i]), # growth rate
            K = as.numeric(dat$K[i]), # carry capacity
            c = as.numeric(dat$c[i]), # cost
            N = as.numeric(dat$N[i])) # N = b*i*ro*mu
  solve.output <- solveGTAeq(pars)
  array[(i-range.sub[1]+1),] <- c(solve.output, pars)
  print(i)
}

write.table(array, file = output, quote = F, row.names = F, sep = "\t")