#############################
############################
#############################
#### steup parameters############

parms<- c(K = 10e4, L=2, m= 0.001, mu=10e-3, r=0.1, s=0.02, u=10e-3, c=0.001, v=0.001, g=1, ls=0.01)

parmsNames <- names(parms)
for (i in seq(length(parmsNames))) assign(parmsNames[i],parms[[i]])   ### assign the names to the parameters


######## setup inituial population size for a "L" loci system
##### first define the geontype pool and fragment type pool and 
#### defined the expression of the total number of cell populations and number of fragments given the number of loci

num1<-vector(mode="numeric", length=3*2^L)
j=0
for (i in 1:2^L){
  
  j = 1+3*(i-1)
  num1[j:(j+2)]<-rep(i,3)
  
}

num2<-vector(mode="character", length=2*L)

for (l in 1:(2*L)) {
  
  if ( l %% 2 !=0) {
    temp1 = "0"
    temp2 =(l+1)/2
  } else {temp1 = "1"
          temp2 = l/2
  }
  
  
  temp<-paste(temp1,temp2,sep="")
  num2[l] <- temp
}


fs<-c(paste("f",num2,sep=""))
fs  ### types of environmental fragments
F<-paste(fs, collapse="+")  ### expression of total number of outside fragments


xs<-c(paste(c("X0","Xn","Xr"),num1,sep=""))  
xs   ### genotype of three populations
X<-paste(xs, collapse="+")  ### expression of entire population size


######################## then, we should create a genotype pool given the loci number L
####### if L =2, then we have 4 genotype as show in matrix gt


GT<- function (L) {
  number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    if(missing(noBits)) {
      return(binary_vector)
    } else {
      binary_vector[-(1:(length(binary_vector) - noBits))]
    }
  }
  
  m<-matrix(NA, nrow = 2^L, ncol = L)
  v<-vector(mode="numeric", length = L)
  for (i in 1:(2^L)) {
    
    
    v<-number2binary (i-1, L)
    m[i,]<-v
  }
  
  return(m)
}

gt<-GT(L)





#### define other variables work in stochastic process ######

tf =  100     # number of steps
consoleInterval = 0
censusInterval = 0
verbose = T
maxWallTime = Inf
method = "D"
ignoreNegativeState=T



######## the status of DNA fragments fij will be put after the genotype

X<-3e+03   ####total number of cells
X0<-1e+03  #### pop size of NC group
Xr<-1e+03  #### pop size of CFR group
Xn<-1e+03  #### pop size of CFN group
F <-0      #### assum initial environment fragments are 0

x0 <- c(rep ((1e3-2^L),3), rep(1,((3*2^L-3))),rep(0,2*L))
num1<-vector(mode="numeric", length=3*2^L)
j=0
for (i in 1:2^L){
  
  j = 1+3*(i-1)
  num1[j:(j+2)]<-rep(i,3)
  
}

num2<-vector(mode="character", length=2*L)

for (l in 1:(2*L)) {
  
  if ( l %% 2 !=0) {
    temp1 = "0"
    temp2 =(l+1)/2
  } else {temp1 = "1"
          temp2 = l/2
  }
  
  
  temp<-paste(temp1,temp2,sep="")
  num2[l] <- temp
}

names(x0) <-c(c(paste(c("X0","Xn","Xr"), num1, sep="")),c(paste("f",num2,sep="")))
x0  ### initial status of three populations and the fragemnts


######## set up input the reaction vector and the status vector

#### total length of reaction vector
#### given L loci, X0, Xn, Xr groups will have 2^L types of genotypes
#### fragment f will have 2*L types

A = 4*2^L ### for group X0, each genotype has 4 reactions, 
B = 5*2^L ### for group Xn, each genotype has 5 reactions,
C = 7*2^L  ### for group Xr, each genotype has 7 reactions,
D = 2*L        ### for fragments, each type has 1 reactions.

## total length of reaction vector will be:
reactions=(A+B+C+D)

## the vector order will be 
## genotype 1 for X0, Xn, Xr; genotype 2 for X0, Xn, Xr; genotype 3 for X0, Xn, Xr; ... genotype 2^L for X0, Xn, Xr; 
### fragment type 1 ~ 2*L


####### status matrix ########
#### the row will be all variables in order X01, Xn1, Xr1, X02, Xn2, Xr2, ..... ,f01,f11,f02,f12
## reaction order will be growth, death, mutation plus, mutation lose, lysis lose, recombination plus, recombination lose, 
## fragment lose (fragment could obtain from lysis reaction, so need to manually make this matrix)

nu <- matrix(data=NA, nrow= (3*2^L+2*L), ncol = reactions)
nurow1<- c(1,-1,1,-1)
nurow2<- c(1,-1,1,-1,-1)
nurow3<- c(1,-1,1,-1,-1,1,-1)

nu1 <- matrix (data=NA, nrow=3, ncol= (4+5+7))
nu1[1,(1:4)] <-nurow1 
nu1[1,-(1:4)] <-0 
nu1[2,(5:9)]<-nurow2
nu1[2,-(5:9)]<-0
nu1[3,(10:16)]<-nurow3
nu1[3,-(10:16)]<-0

.m =1

for (.n in seq(1, (3*2^L), by = 3)) {
  
  
  nu[.n,.m:(.m+15)] <- nu1[1,]
  nu[.n,-(.m:(.m+15))] <- 0
  nu[(.n+1),.m:(.m+15)] <- nu1[2,]
  nu[(.n+1),-((.m):(.m+15))] <- 0
  nu[(.n+2),.m:(.m+15)] <- nu1[3,]
  nu[(.n+2),-((.m):(.m+15))] <- 0
  
  .m = .m +16
}


#### for the fragments matrix
### find the lysis reaction and put 1 for the corresponding fragment type increase
nu2<-matrix(data=NA, nrow = (2*L), ncol=reactions, dimnames=list(fs, c(1:reactions)))
for (i in 1: (2^L)) {
  gi<-gt[i,]
  for (l in 1: L){
    if (gi[l] == 1) {rowname<-paste("f1",l,sep="")
                     nu2[rowname, (9+(i-1)*16)]<-1
                     nu2[rowname, (14+(i-1)*16)]<-1
    }
    else{
      rowname<-paste("f0",l,sep="")
      nu2[rowname, (9+(i-1)*16)]<-1
      nu2[rowname, (14+(i-1)*16)]<-1
      
    }
    
  }
}

for (i in 1:(2*L)){
  for (j in 1: reactions){
    if (is.na(nu2[i,j])) {
      nu2[i,j]<-0
      
    }
    
  }
  
}


.m1 = A+B+C
for (j in 1:(2*L)) {
  nu[(3*2^L+j),(1:reactions)]<-nu2[j,]
  nu[(3*2^L+j),(.m1+j)] <- -1
} 


nu

source("~/R/GTArecombmodel2.R")

###############################################

##################### ssa function ########################

#ssa.run <- function(x0,L,nu,tf,method,tau,ignoreNegativeState,consoleInterval,censusInterval,
#                    verbose,maxWallTime) {

# In this function the user defined state variables and model parameters get 
# assigned. To reduce the risk for potential name clashed with internal 
# variables some of the function arguments and internal variables are 
# (re)named using dot notation (i.e. .*).


.x <- x0
varNames <- names(.x)
for (.i in seq(length(varNames))) assign(varNames[.i],.x[[.i]])  ### name the variables


# Initialize miscellaneous counters 
.t <- 0 # Initialize the simulation time
timeOfNextCensus <- .t + censusInterval  # Time of first data census
timeForConsole   <- .t + consoleInterval # Time of first console output
timeToTerminate  <- FALSE

# Add the initial state of the system to the time series matrix and 'pre-grow' 
# with NAs
timeSeries <- c(.t, .x) # First data point
numCols    <- length(timeSeries)
timeSeries <- rbind(timeSeries, matrix(nrow=1000, ncol=(numCols)))

a<-vector(mode= "numeric", length= reactions)


gr<-genotype_reaction(L)  ### for one generation of reaction
fr<-fragment_reaction(L)

a <- c(gr,fr)

eval_a <- a

for (.i in seq(length(eval_a))) if (any(eval_a<0)) stop("negative propensity function")

.nu<-nu

####################### start stochastic run ###################
################################################################  

# Start the timer and make an announcement if running silent
procTimeStart   <- proc.time()
elapsedWallTime <- 0
startWallTime   <- format(Sys.time())
if (verbose) {
  cat("Running ",method,
      " method with console output every ",consoleInterval,
      " time step\n",sep="")
  cat("Start wall time: ",startWallTime,"...\n",sep="")  
  flush.console()
}

if ((verbose) & (consoleInterval>0)) {
  cat("t=",.t," : ",sep="")
  cat(.x,sep=",")
  cat("\n")
  flush.console()
}

stepSize <- NULL
currentRow <- 2 # First row contains (t0,x0)
suspendedTauLeapMethod <- FALSE
nSuspendedTauLeaps <- 0

while( (.t<tf) & (any(.x>0)) & 
         (all(.x>=0)) & (any(eval_a>0)) & 
         (elapsedWallTime<=maxWallTime) ) {
  doCalc <- TRUE
  if ((verbose) & (timeForConsole<=.t)) {
    cat("(",elapsedWallTime,"s) t=",.t," : ",sep="")
    cat(.x,sep=",")
    cat("\n")
    flush.console()
    timeForConsole <- timeForConsole + consoleInterval
  }
  
  out <- ssa.d(eval_a, .nu) 
  
  if (doCalc) {
    .t <- .t + out$tau  # Update the time
    .x <- .x + out$nu_j # Update the state vector
    
    # Check that no states are negative (can occur in some tau-leaping methods)
    if ((any(.x<0)) & (!ignoreNegativeState)) {
      cat("at least one population in 'x' is negative. Bailing to browser...\n")
      browser()
    }
    
    
    stepSize <- c(stepSize, out$tau)
    
    # If it's time record the current state of the system (t,x)
    if (timeOfNextCensus <= .t) { 
      timeSeries[currentRow,] <- c(.t, .x)
      currentRow              <- currentRow + 1
      timeOfNextCensus        <- .t + censusInterval
      
      # If necessary add empty rows to the time series matrix
      if (currentRow > dim(timeSeries)[1]) 
        timeSeries <- rbind(timeSeries, matrix(nrow=1000, ncol=(numCols)))
    } # if()
    
    # Evaluate the transition rates for the next time step
    for (.i in seq(length(varNames))) assign(varNames[.i],.x[[.i]])
    gr<-genotype_reaction(L)  ### for one generation of reaction
    fr<-fragment_reaction(L)
    
    a <- c(gr,fr)
    
    eval_a[is.na(eval_a)] <- 0 # Replace NA with zero (0/0 gives NA)
    if(any(eval_a<0)) 
      warning("negative propensity function - coersing to zero\n")
    eval_a[eval_a<0] <- 0
  } # if (!suspendedTauLeapMethod)
  procTimeEnd <- proc.time()
  elapsedWallTime <- procTimeEnd[3] - procTimeStart[3]
} # while()


if (verbose) {
  cat("t=",.t," : ",sep="")
  cat(.x,sep=",")
  cat("\n")
  flush.console()
}

# Remove all the remaining "pre-grown" NA rows
timeSeries <- timeSeries[!is.na(timeSeries[,1]),]

# Record the final state of the system
timeSeries <- rbind(timeSeries, c(.t, .x))
endWallTime <- format(Sys.time())

#  return(list(timeSeries=timeSeries, eval_a=eval_a, elapsedWallTime=elapsedWallTime, startWallTime=startWallTime, endWallTime=endWallTime, stepSize=stepSize, nSuspendedTauLeaps=nSuspendedTauLeaps))
#}








#ssa.run(x0,L,nu,tf,method,tau,ignoreNegativeState,consoleInterval,censusInterval,verbose,maxWallTime)

