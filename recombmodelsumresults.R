library(GillespieSSA)
library(ggplot2)
library(reshape2)
Xa_run1<-read.table("SSA_Xa_results_run1.txt",sep="\t",head=T)
Xa_run2<-read.table("SSA_Xa_results_run2.txt",sep="\t",head=T)
Xa_run3<-read.table("SSA_Xa_results_run3.txt",sep="\t",head=T)

head(Xa_run3)
Xa_run3<-as.data.frame(Xa_run3) 
population<-"X1"
g <- ggplot(Xa_run2, aes(x=Generation), y=X1, group=1) 
g <- g +  geom_point(aes(y=X1, color=population), shape=19, size=2)  
g
g <- g +  geom_point(aes(y=X2, color="X2"),shape=19, size=2)  
g

g <- g +  geom_point(aes(y=X3, color="X3"),shape=19, size=2)  
g

g <- g + geom_point(aes(y=X4, color="X4"),shape=19, size=2)  
g 
g <- g + ylab("pop_size") + xlab("Time")

g


Xn_run1<-read.table("SSA_Xn_results_run1.txt",sep="\t",head=T)
Xn_run2<-read.table("SSA_Xn_results_run2.txt",sep="\t",head=T)
Xn_run3<-read.table("SSA_Xn_results_run3.txt",sep="\t",head=T)

tail(Xn_run2)
Xn_run3<-as.data.frame(Xn_run3) 
population<-"X1"
g <- ggplot(Xn_run3, aes(x=Generation), y=X1, group=1) 

g <- g +  geom_point(aes(y=X1, color=population), shape=19, size=2)  
g
g <- g +  geom_point(aes(y=X2, color="X2"),shape=19, size=2)  
g

g <- g +  geom_point(aes(y=X3, color="X3"),shape=19, size=2)  
g

g <- g + geom_point(aes(y=X4, color="X4"),shape=19, size=2)  
g 
g <- g + ylab("pop_size") + xlab("Time")

g


Xr_run1<-read.table("SSA_Xr_results_run1.txt",sep="\t",head=T)
Xr_run2<-read.table("SSA_Xr_results_run2.txt",sep="\t",head=T)
Xr_run3<-read.table("SSA_Xr_results_run3.txt",sep="\t",head=T)

Xr_run1<-as.data.frame(Xr_run3) 
population<-"X1"
g <- ggplot(Xr_run1, aes(x=Generation), y=X1, group=1) 

g <- g +  geom_point(aes(y=X1, color=population), shape=19, size=2)  
g
g <- g +  geom_point(aes(y=X2, color="X2"),shape=19, size=2)  
g

g <- g +  geom_point(aes(y=X3, color="X3"),shape=19, size=2)  
g

g <- g + geom_point(aes(y=X4, color="X4"),shape=19, size=2)  
g 
g <- g + ylab("pop_size") + xlab("Time")

g