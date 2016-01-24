#### GTA selfish gene model #### 06/3/15
#### assume a system has two genotype
#### population X1 has GTA genome; population X2 doesn't have GTA genome.
#### reactions ####
##### logistic growth, lysis, transduction #####
### parameters: r-growth rate; K-carry capacity; c-proportion to express GTA;
### g-proportion of GTA particles containing GTA genome;
### i-infection rate;
### re-incoporation rate;
### b-number of GTA producing per cell
## n = i*re
library(GillespieSSA)

Gillespie.SSA <- function (lpfreq, time, tau,verbose, method) {
  parms <- c(r=0.1, K=10^9, c=0.001, lp=lpfreq,n = 10^-6,  b=100)
  x0 <- c(X1=9*10^6, X2=8.91*10^8)
  a  <- c("r*(1-(X1+X2)/K)*X1*(1-c)", "c*X1","b*c*lp*n*X1*X2","r*(1-(X1+X2)/K)*X2")
  nu <- matrix (data=NA, nrow=2, ncol=4)
  nu[1,] <- c(1,-1,1,0)
  nu[2,] <- c(0,0,-1,1)
  
  tf <- time
  out<-ssa(x0 = x0,a,nu,parms,tf, method = method,verbose=verbose,consoleInterval=1,tau=tau)
  return(out)
}

#ptm <- proc.time()
#lpfreq1_out<-Gillespie.SSA(0.00004, 500000, 0.1, verbose=T, method="ETL")
#proc.time() - ptm
lpfreqs<-seq(0.00001, 0.01, 0.00002)

length(lpfreqs)

output<-vector("list",length(lpfreqs))
for (i in 1: length(lpfreqs)){
  
  temp <- Gillespie.SSA(lpfreqs[i], 500000, 0.1, verbose=T, method="ETL")
  output[[i]]<-temp
  
}

save.image("neutralmodel_lprun1.RData")






#test1<-Gillespie.SSA(0.0007, 10000, 0.05, verbose=T)

#proc.time() - ptm
out<-Gillespie.SSA(1e-04, 500000, 0.1, verbose=T, method="ETL")
