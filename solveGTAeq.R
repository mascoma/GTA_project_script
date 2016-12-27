### if there are error or warnings from solver  if so  return NA

na_check <- function(out, run.length){
  if (is.na(out[run.length, 2])){
    return(1)
  }
  else {
    return(0)
  }
}
###
neg_check <- function(out, run.length){
  if (out[run.length, 2]<0 || out[run.length, 3]<0){
    return(1)
  }
  else {
    return(0)
  }
}


####
solveGTAeq <- function(Pars) { 
  # to solve the ODE and return final poulation size, ratio of X+ 
  # the status of population (stable/unstable), and the function (oscillation or not)
 
  yini <- c(X1 = 10000, X2 = 10000)
  GTAeq <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dX1 <- r * (1-c) * (1-(X1+X2)/K) * X1 - c * X1 + c * N* X1 * X2 # GTA postive
      dX2 <- r * (1-(X1+X2)/K) * X2 - c * N * X1 * X2     # GTA negative
      return(list(c(dX1, dX2)))
    }) 
  }
  times <- seq(0, 10000, by = 1)

  out <- ode(yini, times, GTAeq, Pars, method = "ode45")
  run.length <- length(out[, 1])
  
  ## check if the population sizes are na
  e <- na_check(out, run.length)
  if (e){
    outputarray <- c(rep(NA, 5))
    return(outputarray)
  }
  
  ## check if the population sizes are negative
  f <- neg_check(out, run.length)
  if (f){
    outputarray <- c(rep(-1, 5))
    return(outputarray)
  }
  
  
  
  ## determine if dynamic of population size is oscillation
  fun.status <- "l"
  sign1 = 1
  temp = 0
  for (c in 1: run.length){
    dx <- GTAeq(times[c], out[c, 2:3], Pars)
    #print(dx)
    sign2 = sign1
    sign1 = sign(dx[[1]][2])
    if (sign1 != sign2){
      temp = temp + 1
    }
  }
  if (temp > 5) {
    fun.status <- "o"
  }
  ###
  
  ### determine if population is stable
  b <- 0
  pop.status <- "s"
  for (a in 1:50){
    deltaX1 <- abs(out[run.length, 2]-out[(run.length-a), 2]) 
    deltaX2 <- abs(out[run.length, 3]-out[(run.length-a), 3]) 
    if (deltaX1 < 0.001 && deltaX2 < 0.001 && 
        (out[run.length, 2] + out[run.length, 3]) > 10000) {
      b = b + 1
    }
  }
  if (b == 50){
    output <- out[run.length,]
    X1.ratio <- output[2]/(output[2] + output[3])
    outputarray <- c(output[2], output[3], X1.ratio, fun.status, pop.status)
    return(outputarray)
  }
  else {
    times <- seq(0, 100000, by = 1)
    out <- ode(yini, times, GTAeq, Pars, method = "ode45")
    run.length <- length(out[, 1])
    ## check if the population sizes are na
    e <- na_check(out, run.length)
    if (e){
      outputarray <- c(rep(NA, 5))
      return(outputarray)
    }
    
    ## check if the population sizes are negative
    f <- neg_check(out, run.length)
    if (f){
      outputarray <- c(rep(-1, 5))
      return(outputarray)
    }

    ### determine if population is stable
    b <- 0
    for (a in 1:50){
      deltaX1 <- abs(out[run.length, 2]-out[(run.length-a), 2]) 
      deltaX2 <- abs(out[run.length, 3]-out[(run.length-a), 3]) 
      if (deltaX1 < 0.001 && deltaX2 < 0.001 && 
          (out[run.length, 2] + out[run.length, 3]) > 10000) {
        b = b + 1
      }
    }
    if (b == 50){
      output <- out[run.length,]
      X1.ratio <- output[2]/(output[2] + output[3])
      outputarray <- c(output[2], output[3], X1.ratio, fun.status, pop.status)
      return(outputarray)
    }
    else {
      times <- seq(0, 1000000, by = 1)
      out <- ode(yini, times, GTAeq, Pars, method = "ode45")
      run.length <- length(out[, 1])
      ## check if the population sizes are na
      e <- na_check(out, run.length)
      if (e){
        outputarray <- c(rep(NA, 5))
        return(outputarray)
      }
      
      ## check if the population sizes are negative
      f <- neg_check(out, run.length)
      if (f){
        outputarray <- c(rep(-1, 5))
        return(outputarray)
      }
      
      ### determine if population is stable
      b <- 0
      for (a in 1:50){
        deltaX1 <- abs(out[run.length, 2]-out[(run.length-a), 2]) 
        deltaX2 <- abs(out[run.length, 3]-out[(run.length-a), 3]) 
        if (deltaX1 < 0.001 && deltaX2 < 0.001 && 
            (out[run.length, 2] + out[run.length, 3]) > 10000) {
          b = b + 1
        }
      }
      if (b == 50){
        output <- out[run.length,]
        X1.ratio <- output[2]/(output[2] + output[3])
        outputarray <- c(output[2], output[3], X1.ratio, fun.status, pop.status)
        return(outputarray)
      }
      else {
        times <- seq(0, 30000000, by = 1)
        out <- ode(yini, times, GTAeq, Pars, method = "ode45")
        run.length <- length(out[, 1])
        ## check if the population sizes are na
        e <- na_check(out, run.length)
        if (e){
          outputarray <- c(rep(NA, 5))
          return(outputarray)
        }
        
        ## check if the population sizes are negative
        f <- neg_check(out, run.length)
        if (f){
          outputarray <- c(rep(-1, 5))
          return(outputarray)
        }
        
        ### determine if population is stable
        b <- 0
        for (a in 1:50){
          deltaX1 <- abs(out[run.length, 2]-out[(run.length-a), 2]) 
          deltaX2 <- abs(out[run.length, 3]-out[(run.length-a), 3]) 
          if (deltaX1 < 0.001 && deltaX2 < 0.001 && 
              (out[run.length, 2] + out[run.length, 3]) > 10000) {
            b = b + 1
          }
        }
        if (b == 50){
          output <- out[run.length,]
          X1.ratio <- output[2]/(output[2] + output[3])
          outputarray <- c(output[2], output[3], X1.ratio, fun.status, pop.status)
          return(outputarray)
        }
        else{
          pop.status <- "u"
          output <- out[run.length,]
          X1.ratio <- output[2]/(output[2] + output[3])
          outputarray <- c(output[2], output[3], X1.ratio, fun.status, pop.status)
          return(outputarray)
        }
      }
    }
  }
}