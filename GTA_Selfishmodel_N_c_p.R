library(ggplot2)
library(reshape2)
library(plot3D)
library(pracma)
library(latticeExtra)
library(fields)
library(akima)

 
## analytic solutions for the GTA selfish-model 
#K  ## carrying capacity
#r  ## growth rate 
#n     ## number of GTA released per cell
#g  ## proportion of lollipop GTA containing GTA genome
#i  ## infection rate
#re   ## incoporation rate
#mu ## frequecy of generation "lollipop GTA"
#c  ## proportion of bacteria population expressing GTA
#N= n*g*i*re*mu

## population X1- containg and expression GTA genome
## population X2- without GTA genome, but except GTA transfering

suc_tra<- function(num1, num2){
  c = num1
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  p = num2
  r/(K*(r-c*p-c*r*p))
} 
c = seq(0.0001, 0.1, 0.001)
p = seq(0.001, 1, 0.01)
N1<- vector("numeric", length = 100)
for (a in 1:100){
  N1[a] = suc_tra(c[a], p[a])
}

cp<-meshgrid(c, p)
N <- matrix(NA, 100, 100)
for(i in 1:100){
  for(j in 1:100) 
  N[i, j] = suc_tra(cp$X[i, j], cp$Y[i, j])
}



# A function generating colors
par(mfrow = c(1, 1), mar = c(4,4, 4, 4))
persp3D(z = N, x = c, y = p, main = "volcano", facets = FALSE, scale = TRUE)

# save plotting parameters
pm <- par("mfrow")
## =======================================================================
## Ribbon, persp, color keys, facets
## =======================================================================
par(mfrow = c(2, 2))
# simple, no scaling,
persp3D(z = volcano, main = "volcano", clab = c("height", "m"))
# keep ratios between x- and y (scale = FALSE)
# change ratio between x- and z (expand)
persp3D(z = volcano, x = 1: nrow(volcano), y = 1:ncol(volcano),
        expand = 0.3, main = "volcano", facets = FALSE, scale = FALSE,
        clab = "height, m", colkey = list(side = 1, length = 0.5))
# ribbon, in x--direction
V <- volcano[, seq(1, ncol(volcano), by = 3)]  # lower resolution
ribbon3D(z = V, colkey = list(width = 0.5, length = 0.5,
                              cex.axis = 0.8, side = 2), clab = "m")
# ribbon, in y-direction
Vy <- volcano[seq(1, nrow(volcano), by = 3), ]
ribbon3D(z = Vy, expand = 0.3, space = 0.3, along = "y",
         colkey = list(width = 0.5, length = 0.5, cex.axis = 0.8))
## =======================================================================
## Several ways to visualise 3-D data
## =======================================================================