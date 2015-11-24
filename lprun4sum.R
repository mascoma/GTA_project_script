library(ggplot2)
library(reshape2)

pop_lp <- lpfreq4_out$data[,1:3]
colnames(pop_lp) <- c("time", "X1","X2")
pop_lp<-data.frame(pop_lp)
population<-"X1"
#plot(pop_lp[, 1], pop_lp[,2])
g <- ggplot(pop_lp, aes(x=time, y=X1, group=1)) 
g <- g +geom_point(aes(y=X1, color=population),shape=19, size=1)   
g
g <- g +geom_point(aes(y=X2, color="X2"),shape=19, size=1)  
g <- g + ylab("pop_size") + xlab("time") + ylim(0,1.0e+09) + xlim(0, 500000)
g
 
ggsave("lprun4.jpg",plot=g)