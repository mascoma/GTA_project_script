library(deSolve)
library(plot3D)

outputdir <- "/isi/olga/xin/GTA_project/output/20160701/X_ratio_c_K.txt"

c.array <- seq(0.0001, 1, length.out = 500)
K.array <- seq(1e5, 1e9, length.out = 500)
r.array <- seq(0.0001, 1, length.out = 500)
N.array <- c(1e-11, 1e-10, 5e-10, seq(1e-9, 1e-8, length.out = 495), 1e-7, 1e-6) 

M <- mesh(c.array, K.array)
array <- matrix(NA, nrow = length(M$x), ncol = 5)
solveGTAeq <- function(Pars) {
  yini <- c(X1 = 1, X2 = 1)
  GTAeq <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dX1 <- r * (1-c) * (1-(X1+X2)/K) * X1 - c * X1 + c * N* X1 * X2 # GTA postive
      dX2 <- r * (1-(X1+X2)/K) * X2 - c * N * X1 * X2     # GTA negative
      return(list(c(dX1, dX2)))
    }) 
  }
  times <- seq(0, 10000, by = 1)
  out <- ode(yini, times, GTAeq, pars), 
  b = 0
  if (is.na(out[length(times), 2])){
    output <- out[length(times),]
    outputarray <- c(output[2], output[3], NA)
    return(outputarray)
    break
  }
  for (a in 1:50){
    deltaX <- abs(out[length(times), 2]-out[(length(times)-a), 2]) 
    if (deltaX < 0.0001 && (out[length(times), 2] + out[length(times), 3]) > 1000) {
      b = b + 1
    }
  }
  if (b == 50){
    output <- out[length(times),]
    X1.ratio <- output[2]/(output[2] + output[3])
    outputarray <- c(output[2], output[3], X1.ratio)
    return(outputarray)
  }
  else {
    times <- seq(0, 100000, by = 1)
    out <- ode(yini, times, GTAeq, pars)
    b = 0
    if (is.na(out[length(times), 2])){
      output <- out[length(times),]
      outputarray <- c(output[2], output[3], NA)
      return(outputarray)
      break
    }
    for (a in 1:50){
      deltaX <- abs(out[length(times), 2]-out[(length(times)-a), 2]) 
      if (deltaX < 0.0001 && (out[length(times), 2] + out[length(times), 3]) > 1000) {
        b = b + 1
      }
    }
    if (b == 50){
      output <- out[length(times),]
      X1.ratio <- output[2]/(output[2] + output[3])
      outputarray <- c(output[2], output[3], X1.ratio)
      return(outputarray)
    }
    else {
      times <- seq(0, 1000000, by = 1)
      out <- ode(yini, times, GTAeq, pars)
      b = 0
      if (is.na(out[length(times), 2])){
        output <- out[length(times),]
        outputarray <- c(output[2], output[3], NA)
        return(outputarray)
        break
      }
      for (a in 1:50){
 
        deltaX <- abs(out[length(times), 2]-out[(length(times)-a), 2]) 
        if (deltaX < 0.0001 && (out[length(times), 2] + out[length(times), 3]) > 1000) {
          b = b + 1
        }
      }
      if (b == 50){
        output <- out[length(times),]
        X1.ratio <- output[2]/(output[2] + output[3])
        outputarray <- c(output[2], output[3], X1.ratio)
        return(outputarray)
      }
      else {
        times <- seq(0, 30000000, by = 1)
        out <- ode(yini, times, GTAeq, pars)
        if (is.na(out[length(times), 2])){
          output <- out[length(times),]
          outputarray <- c(output[2], output[3], NA)
          return(outputarray)
          break
        }
        output <- out[length(times),]
        X1.ratio <- output[2]/(output[2] + output[3])
        outputarray <- c(output[2], output[3], X1.ratio)
        return(outputarray)
      }
    }
  }
}

 
for (j in 1 : length(M$x)){
  pars <- c(r = 0.1, # growth rate
            K = M$y[j], # carry capacity
            c = M$x[j], # cost
            N = 1.2e-9) # N = b*i*ro*mu
  solve.output <- solveGTAeq(pars)
  array[j,] <- c(solve.output, M$x[j], M$y[j])
  print(j)
}


write.table(array, file = outputdir, quote = F, row.names = F, sep = "\t")


