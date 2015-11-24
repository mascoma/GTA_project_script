library(ggplot2)
library(reshape2)
## analytic solutions for the GTA selfish-model 
#K  ## carrying capacity
#r  ## growth rate 
#n     ## number of GTA released per cell
#g  ## proportion of lollipop GTA containing GTA genome
#i  ## infection rate
#re   ## incoporation rate
#mu ## frequecy of generation "lollipop GTA"
#c  ## proportion of bacteria population expressing GTA

## population X1- containg and expression GTA genome
## population X2- without GTA genome, but except GTA transfering

## c_mu function
## scenario 1: at the equilibrium, X2 takes over X1, so X1/(X1+X2) = 0.01

expression_prop1<- function(int){
  mu =int
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  g = 0.1 ## proportion of lollipop GTA containing GTA genome
  i = 0.001 ## infection rate
  re = 0.001  ## incoporation rate
  100*(K*r*n*g*i*re*mu - r)/(K*n*g*i*re*mu + K*r*n*g*i*re*mu)
} 

mu <-seq(0.00004, 0.0005, 0.000001)
c_01 <- vector(mode= "numeric", length = length(mu))
for (x in 1 : length(mu)){
  c_01[x] <- expression_prop1(mu[x])
}


 
## scenario 2: at the equilibrium, X1 takes over X2, so X1/(X1+X2) = 0.99

expression_prop2<- function(int){
  mu =int
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  g = 0.1 ## proportion of lollipop GTA containing GTA genome
  i = 0.001 ## infection rate
  re = 0.001  ## incoporation rate
  100*(K*r*n*g*i*re*mu - r)/(99*(K*n*g*i*re*mu + K*r*n*g*i*re*mu))
} 

mu <-seq(0.00004, 0.0005, 0.000001)
c_99 <- vector(mode= "numeric", length = length(mu))
for (x in 1 : length(mu)){
  c_99[x] <- expression_prop2(mu[x])
}

# plot the results
c_mu<- cbind(mu, c_01, c_99)
c_mu<-as.data.frame(c_mu) 
head(c_mu)
population<-"c_01"
g <- ggplot(c_mu, aes(x=mu), y=c_01, group=1) 
g <- g +  geom_line(aes(y=c_01, color=population), size=1)  
g
g <- g +  geom_line(aes(y=c_99, color="c_99"),   size=1)  
g
g <- g + ylab("c") + xlab("mu") +ylim(0,0.4)

g

ggsave("c_mu.jpg",plot=g)

## c_K function
## scenario 1: at the equilibrium, X2 takes over X1, so X1/(X1+X2) = 0.01
KK1<- function(int){
  mu =0.0001
  K = int ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  g = 0.1 ## proportion of lollipop GTA containing GTA genome
  i = 0.001 ## infection rate
  re = 0.001  ## incoporation rate
  100*(K*r*n*g*i*re*mu - r)/(K*n*g*i*re*mu + K*r*n*g*i*re*mu)
} 

K <-seq(100000, 1000000000, 500000)
c_01 <- vector(mode= "numeric", length = length(K))
for (x in 1 : length(K)){
  c_01[x] <- KK1(K[x])
}



## scenario 2: at the equilibrium, X1 takes over X2, so X1/(X1+X2) = 0.99

KK2<- function(int){
  mu =0.0001
  K = int ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  g = 0.1 ## proportion of lollipop GTA containing GTA genome
  i = 0.001 ## infection rate
  re = 0.001  ## incoporation rate
  100*(K*r*n*g*i*re*mu - r)/(99*(K*n*g*i*re*mu + K*r*n*g*i*re*mu))
} 

K <-seq(100000, 1000000000, 500000)
c_99 <- vector(mode= "numeric", length = length(K))
for (x in 1 : length(K)){
  c_99[x] <- KK2(K[x])
}

# plot the results
c_K<- cbind(K, c_01, c_99)
c_K<-as.data.frame(c_K) 
head(c_K)
population<-"c_01"
g <- ggplot(c_K, aes(x=K), y=c_01, group=1) 
g <- g +  geom_line(aes(y=c_01, color=population), size=1)  
g
g <- g +  geom_line(aes(y=c_99, color="c_99"),   size=1)  
g
g <- g + ylab("c") + xlab("K") +ylim(0,0.4)

g

ggsave("c_K.jpg",plot=g)

## c_n function
## scenario 1: at the equilibrium, X2 takes over X1, so X1/(X1+X2) = 0.01
num_GTA1<- function(int){
  mu =0.0001
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = int  ## number of GTA released per cell
  g = 0.1 ## proportion of lollipop GTA containing GTA genome
  i = 0.001 ## infection rate
  re = 0.001  ## incoporation rate
  100*(K*r*n*g*i*re*mu - r)/(K*n*g*i*re*mu + K*r*n*g*i*re*mu)
} 

n <-seq(0, 500, 5)
c_01 <- vector(mode= "numeric", length = length(n))
for (x in 1 : length(n)){
  c_01[x] <- num_GTA1(n[x])
}



## scenario 2: at the equilibrium, X1 takes over X2, so X1/(X1+X2) = 0.99

num_GTA2<- function(int){
  mu =0.0001
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = int   ## number of GTA released per cell
  g = 0.1 ## proportion of lollipop GTA containing GTA genome
  i = 0.001 ## infection rate
  re = 0.001  ## incoporation rate
  100*(K*r*n*g*i*re*mu - r)/(99*(K*n*g*i*re*mu + K*r*n*g*i*re*mu))
} 

n <-seq(0, 500, 5)
c_99 <- vector(mode= "numeric", length = length(n))
for (x in 1 : length(n)){
  c_99[x] <- num_GTA2(n[x])
}

# plot the results
c_n<- cbind(n, c_01, c_99)
c_n<-as.data.frame(c_n) 
head(c_n,100)
population<-"c_01"
g <- ggplot(c_n, aes(x=n), y=c_01, group=1) 
g <- g +  geom_line(aes(y=c_01, color=population), size=1)  
g
g <- g +  geom_line(aes(y=c_99, color="c_99"),   size=1)  
g
g <- g + ylab("c") + xlab("n") +ylim(0,1)

g

ggsave("c_n.jpg",plot=g)

## c_tr function let tr =  g*i*re
## scenario 1: at the equilibrium, X2 takes over X1, so X1/(X1+X2) = 0.01


lp_prop1<- function(int){
  mu =0.0001
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200  ## number of GTA released per cell
  tr = int
  100*(K*r*n*tr*mu - r)/(K*n*tr*mu + K*r*n*tr*mu)
} 

tr <-seq(0, 0.00001, 0.000000001)
c_01 <- vector(mode= "numeric", length = length(tr))
for (x in 1 : length(tr)){
  c_01[x] <- lp_prop1(tr[x])
}



## scenario 2: at the equilibrium, X1 takes over X2, so X1/(X1+X2) = 0.99

lp_prop2<- function(int){
  mu =0.0001
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  tr = int
  100*(K*r*n*tr*mu - r)/(99*(K*n*tr*mu + K*r*n*tr*mu))
} 

tr <-seq(0, 0.00001, 0.000000001)
c_99 <- vector(mode= "numeric", length = length(tr))
for (x in 1 : length(tr)){
  c_99[x] <- lp_prop2(tr[x])
}

# plot the results
c_tr<- cbind(tr, c_01, c_99)
c_tr<-as.data.frame(c_tr) 
head(c_tr,100)
population<-"c_01"
g <- ggplot(c_tr, aes(x=tr), y=c_01, group=1) 
g <- g +  geom_line(aes(y=c_01, color=population), size=1)  
g
g <- g +  geom_line(aes(y=c_99, color="c_99"),   size=1)  
g
g <- g + ylab("c") + xlab("tr") +ylim(0,1)

g

ggsave("c_tr.jpg",plot=g)

 
## function of c related to p = X1/(X1+X2):

c_p <- function(p) {
  mu =0.0001
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200  ## number of GTA released per cell
  tr = 0.0000001
  (1-1/(K*n*tr*mu))/((1+1/r)*p)
}


p<-seq(0, 1, 0.00001)
c_out <- vector(mode= "numeric", length = length(p))
for (x in 1 : length(p)){
  c_out[x] <- c_p(p[x])
}

c_p<-cbind(p, c_out)
c_p<-as.data.frame(c_p) 
head(c_p,5000)
tail(c_p,10)
g <- ggplot(c_p, aes(x=p), y=c_out, group=1) 
g <- g +  geom_line(aes(y=c_out), size=1)  
g
 
g <- g + ylab("c") + xlab("p") +ylim(0,1)

g

ggsave("c_p.jpg",plot=g)
