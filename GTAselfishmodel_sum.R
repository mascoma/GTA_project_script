library(GillespieSSA)
library(ggplot2)
library(reshape2)

   
load("neutralmodel_lprun1.RData")  

for(i in 1: length(output)) {
  pop_lp <- output[[i]]$data[,1:3]
  colnames(pop_lp) <- c("time", "X1","X2")
  pop_lp<-data.frame(pop_lp)
  population<-"X1"
  #plot(pop_lp[, 1], pop_lp[,2])
  g <- ggplot(pop_lp, aes(x=time, y=X1, group=1)) 
  g <- g +geom_point(aes(y=X1, color=population),shape=19, size=2)   
  g
  g <- g +geom_point(aes(y=X2, color="X2"),shape=19, size=2)  
  g <- g + ylab("pop_size") + xlab("time") + ylim(0,1.0e+09) + xlim(0, 500000)
  g
  name <- paste("lp_pop_sum1_", i,".jpg", sep="")
  ggsave(name,plot=g)
}

q()
  


