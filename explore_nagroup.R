library(animation)
library(deSolve)
library(plot3D)


args <- commandArgs(trailingOnly = TRUE)

# test if there are three arguments: if not, return an error
if (length(args) != 1) {
  stop("two arguments must be supplied arg[1] = j, arg[2] = k.\n", 
       call.=FALSE)
}  


p <- args[1] # range of p is 1-597
p <- as.numeric(p)
 
outputdir <- paste("/isi/olga/xin/GTA_project/output/20160821/GTAnarecheck_",p, 
                   "_.txt", sep = "")

dir1 <- "/isi/olga/xin/GTA_project/output/20160821/"

file1 <- "GTAmodel_na.txt"
input1 <- paste(dir1, file1, sep = "")
nonsolve <- read.delim2(input1, sep = "\t", stringsAsFactors = FALSE, 
                        na.strings = "")
nonsolve.length <- length(nonsolve$p)
range.sub <- range(1+1000*(p-1),  ((1+1000*p)-1))
nonsolve.recheck <- matrix(NA, 1000, 7)
for (i in range.sub[1]: range.sub[2]) {
  yini <- c(X1 = 10000, X2 = 10000)
  GTAeq <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dX1 <- r * (1-c) * (1-(X1+X2)/K) * X1 - c * X1 + c * N* X1 * X2 # GTA postive
      dX2 <- r * (1-(X1+X2)/K) * X2 - c * N * X1 * X2     # GTA negative
      return(list(c(dX1, dX2)))
    }) 
  }
  times <- seq(0, 10000, by = 1)
  Pars <- c(r=as.numeric(nonsolve$r[i]), 
            K=as.numeric(nonsolve$K[i]), 
            c=as.numeric(nonsolve$c[i]),
            N=as.numeric(nonsolve$N[i]))
  out <- ode(yini, times, GTAeq, Pars)
  nonsolve.recheck[(i-range.sub[1]+1),] <- c(out[length(out[, 1]),], Pars)
  print(i)
}

write.table(nonsolve.recheck, file = outputdir, sep = '\t', quote = F, row.names = F)