library(animation)
library(deSolve)
library(dplyr)
 
dir <- "/isi/olga/xin/GTA_project/output/20160822/"
file1 <- "group_NA.txt"
file2 <- "group_na_longertime.txt"
input1 <- paste(dir, file1, sep = "")
 
outputdir2 <- paste(dir, file2, sep = "")

na.group <- read.delim2(input1, sep = "\t", stringsAsFactors = FALSE, 
                        header = F, na.strings = "")
na.pars <- na.group[, 6:9]
names(na.pars) <- c("r", "K", "c", "N")
array <- matrix(NA, length(na.pars[, 1]), 7)
for (i in 1:length(na.pars[, 1])) {
  yini <- c(X1 = 10000, X2 = 10000)
  GTAeq <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dX1 <- r * (1-c) * (1-(X1+X2)/K) * X1 - c * X1 + c * N* X1 * X2 # GTA postive
      dX2 <- r * (1-(X1+X2)/K) * X2 - c * N * X1 * X2     # GTA negative
      return(list(c(dX1, dX2)))
    }) 
  }
  times <- seq(0, 100000, by = 1)
  Pars <- c(r=as.numeric(na.pars$r[i]), 
            K=as.numeric(na.pars$K[i]), 
            c=as.numeric(na.pars$c[i]),
            N=as.numeric(na.pars$N[i]))
  out <- ode(yini, times, GTAeq, Pars)
  print(i)
  array[i, ] <- c(out[length(out[, 1]),], Pars)
}

write.table(array, file = outputdir2, sep = '\t', quote = F, row.names = F)

 