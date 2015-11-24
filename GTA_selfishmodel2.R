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

## combining g, i, re together as a parameter to quantify the successfully transdcuting GTA genome rate 
## using parameter tr 

## c_mu function
## scenario 1: at the equilibrium, X2 takes over X1, so X1/(X1+X2) = 0.01

trans_rate1<- function(int){
  mu =0.0001
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  tr = int
  100*(K*r*n*tr*mu - r)/(K*n*tr*mu + K*r*n*tr*mu)
} 

tr <-seq(0, 0.0001, 0.0000001)
c_01 <- vector(mode= "numeric", length = length(tr))
for (x in 1 : length(tr)){
  c_01[x] <- trans_rate1(tr[x])
}



## scenario 2: at the equilibrium, X1 takes over X2, so X1/(X1+X2) = 0.99

trans_rate2<- function(int){
  mu =int
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  tr = int
  100*(K*r*n*tr*mu - r)/(99*(K*n*tr*mu + K*r*n*tr*mu))
} 

tr <-seq(0, 0.0001, 0.0000001)
c_99 <- vector(mode= "numeric", length = length(tr))
for (x in 1 : length(tr)){
  c_99[x] <- trans_rate2(tr[x])
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
#g <- g + ylab("c") + xlab("tr") +ylim(0,0.1)

#g

ggsave("c_trplot.jpg",plot=g)
## c_mu function
## scenario 1: at the equilibrium, X2 takes over X1, so X1/(X1+X2) = 0.01
lp_freq1<- function(int){
  mu =int
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  tr = 0.0000025
  100*(K*r*n*tr*mu - r)/(K*n*tr*mu + K*r*n*tr*mu)
} 

mu <-seq(0, 0.001, 0.000001)
c_01 <- vector(mode= "numeric", length = length(mu))
for (x in 1 : length(mu)){
  c_01[x] <- lp_freq1(mu[x])
}



## scenario 2: at the equilibrium, X1 takes over X2, so X1/(X1+X2) = 0.99

lp_freq2<- function(int){
  mu =int
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  tr = 0.0000025
  100*(K*r*n*tr*mu - r)/(99*(K*n*tr*mu + K*r*n*tr*mu))
} 

mu <-seq(0, 0.001, 0.000001)
c_99 <- vector(mode= "numeric", length = length(mu))
for (x in 1 : length(mu)){
  c_99[x] <- lp_freq2(mu[x])
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
g <- g + ylab("c") + xlab("mu") +ylim(0,0.1)

g

ggsave("c_muplot.jpg",plot=g)

## c_n function
## scenario 1: at the equilibrium, X2 takes over X1, so X1/(X1+X2) = 0.01
num_GTA1<- function(int){
  mu =int
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = 200   ## number of GTA released per cell
  tr = 0.0000025
  100*(K*r*n*tr*mu - r)/(K*n*tr*mu + K*r*n*tr*mu)
} 

n <- seq(0, 1000, 1)
c_01 <- vector(mode= "numeric", length = length(n))
for (x in 1 : length(n)){
  c_01[x] <- num_GTA1(n[x])
}



## scenario 2: at the equilibrium, X1 takes over X2, so X1/(X1+X2) = 0.99

num_GTA2<- function(int){
  mu = 0.00001
  K = 10^9 ## carrying capacity
  r = 0.1  ## growth rate 
  n = int   ## number of GTA released per cell
  tr = 0.0000025
  100*(K*r*n*tr*mu - r)/(99*(K*n*tr*mu + K*r*n*tr*mu))
} 

n <- seq(0, 2000, 1)
c_99 <- vector(mode= "numeric", length = length(n))
for (x in 1 : length(n)){
  c_99[x] <- num_GTA2(n[x])
}

# plot the results
c_n<- cbind(n, c_01, c_99)
c_n<-as.data.frame(c_n) 
head(c_n)
population<-"c_01"
g <- ggplot(c_n, aes(x=n), y=c_01, group=1) 
g <- g +  geom_line(aes(y=c_01, color=population), size=1)  
g
g <- g +  geom_line(aes(y=c_99, color="c_99"),   size=1)  
g
g <- g + ylab("c") + xlab("n") +ylim(0,0.1)

g
ggsave("c_nplot.jpg",plot=g)


