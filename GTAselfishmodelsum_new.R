library(GillespieSSA)
library(ggplot2)
library(reshape2)


load("/Users/Xin/Desktop/GTA_project/output/Jan192016/selfishmodel_shortrun3.RData")  


lpfreqs<-seq(0.00001, 0.01, 0.00002)
pop <- matrix(NA, nrow=500, ncol=3)
for(i in 1: length(output)) {
  pop_lp <- output[[i]]$data[,1:3]
  colnames(pop_lp) <- c("time", "X1","X2")
  pop[i, ]<-pop_lp[length(pop_lp[,3]),]
}
 
lp_pop <- cbind(lpfreqs, pop[,2:3 ])
colnames(lp_pop) <- c("mu", "X+", "X-")
lp_pop <- as.data.frame(lp_pop)
population<-"X+"
#plot(pop_lp[, 1], pop_lp[,2])
g <- ggplot(lp_pop, aes(x=mu, y=lp_pop[,2], group=1)) 
g <- g +geom_point(aes(y=lp_pop[,2], color=population),shape=19, size=1)   
g
g <- g +geom_point(aes(y=lp_pop[,3], color="X-"),shape=19, size=1)  
g <- g + ylab("pop_size") + xlab("mu") + ylim(0,1.0e+09) + xlim(0, 0.01)
g
ggsave("mu_pop_run3.png",plot=g)
