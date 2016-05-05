#!/usr/bin/env Rscript

#### GTA selfish gene model simulation #### 04/01/2016
#### assume a system has two genotype
#### population X1 has GTA genome; population X2 doesn't have GTA genome.
#### reactions ####
##### logistic growth, lysis, transduction #####
### parameters: r-growth rate; K-carry capacity; c-proportion to express GTA;
### mu-frequency of enlarged head GTA particles containing GTA genome
### i-infection rate;
### ro-integration rate;
### b-number of GTA producing per cell
### n = i * ro
library(GillespieSSA)
args <- commandArgs(trailingOnly = TRUE)

# test if there are three arguments: if not, return an error
if (length(args) != 3) {
  stop("Three argument must be supplied arg[1] = c, arg[2] = b, arg[3] = ro.\n", 
       call.=FALSE)
}  

c <- args[1]
b <- args[2]
ro <- args[3]

c <- as.numeric(c)
b <- as.numeric(b)
ro <- as.numeric(ro)

# c <- 0.03
# b <- 50
# ro <- 10^-5

outputdir <- "/isi/olga/xin/GTA_project/output/20160401/selfishmodel_test.RData"

Gillespie.SSA <- function (mu, time, tau, verbose, method) {
  parms <- c(r = 0.1, K = 10^9, c = c, mu = mu, 
             n = 0.01*ro,  b = b)
  x0 <- c(X1 = 9*10^6, X2 = 8.91*10^8)
  a  <- c("r*(1-(X1+X2)/K)*X1*(1-c)", "c*X1", "b*c*mu*n*X1*X2", "r*(1-(X1+X2)/K)*X2")
  nu <- matrix(data = NA, nrow = 2, ncol = 4)
  nu[1,] <- c(1,-1,1,0)
  nu[2,] <- c(0,0,-1,1)
  
  tf <- time
  out <- ssa(x0 = x0, a, nu, parms, tf, method = method, verbose = verbose, 
             consoleInterval = 1, tau = tau)
  return(out)
}

#ptm <- proc.time()
#lpfreq1_out<-Gillespie.SSA(0.00004, 500000, 0.1, verbose=T, method="ETL")
#proc.time() - ptm
mu <- seq(0.000001, 0.001, 0.000002)

length(mu)

output <- vector("list", length(mu))
for (i in 1: length(mu)){
  
  temp <- Gillespie.SSA(mu[i], 30000, 0.1, verbose = T, method = "ETL")
  output[[i]]<-temp
  
}

save.image(outputdir)



