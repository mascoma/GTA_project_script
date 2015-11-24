library(GillespieSSA)
library(ggplot2)
library(reshape2)
XaXnXr<-read.table("simulation_XaXnXr_2_3_run1.txt",sep="\t",head=T)
#Xa_run2<-read.table("SSA_Xa_results_run2.txt",sep="\t",head=T)
#Xa_run3<-read.table("SSA_Xa_results_run3.txt",sep="\t",head=T)

head(XaXnXr)
XaXnXr<-as.data.frame(XaXnXr) 
population<-"Xa0"
g <- ggplot(XaXnXr, aes(x=Generations), y=Xa0, group=1) 
g <- g +  geom_line(aes(y=Xa0, color=population), linetype="dotted", size=1)  
g
g <- g +  geom_line(aes(y=Xa1, color="Xa1"), linetype="dotted", size=1)  
g

g <- g +  geom_line(aes(y=Xa2, color="Xa2"),linetype="dotted", size=1)  
g

g <- g + geom_line(aes(y=Xa3, color="Xa3"), linetype="dotted", size=1)  
g 
g <- g +  geom_line(aes(y=Xn0, color="Xn0"), linetype="dotted", size=1)  
g

g <- g +  geom_line(aes(y=Xn1, color="Xn1"), linetype="dotted", size=1)  
g

g <- g + geom_line(aes(y=Xn2, color="Xn2"), linetype="dotted", size=1)  
g
g <- g + geom_line(aes(y=Xn3, color="Xn3"), linetype="dotted", size=1)  
g
g <- g +  geom_line(aes(y=Xr0, color="Xr0"), linetype="dotted", size=1)  
g <- g +  geom_line(aes(y=Xr1, color="Xr1"), linetype="dotted", size=1)  
g

g <- g + geom_line(aes(y=Xr2, color="Xr2"), linetype="dotted", size=1)  
g
g <- g + geom_line(aes(y=Xr3, color="Xr3"), linetype="dotted", size=1)  
g <- g + ylab("pop_size") + xlab("Time") +ylim(0, 1e+09)

g
ggsave("XaXnXr_plot1.jpg",plot=g)

XaXr<-read.table("simulation_XaXr2_2_2.txt",sep="\t",head=T)
#Xn_run2<-read.table("SSA_Xn_results_run2.txt",sep="\t",head=T)
#Xn_run3<-read.table("SSA_Xn_results_run3.txt",sep="\t",head=T)

tail(XaXr)
XaXr<-as.data.frame(XaXr) 
population<-"Xa0"
g <- ggplot(XaXr, aes(x=Generations), y=Xa0, group=1) 

g <- g +  geom_line(aes(y=Xa0, color=population), linetype="dotted", size=1)  
g
g <- g +  geom_line(aes(y=Xa1, color="Xa1"), linetype="dotted", size=1)  
g

g <- g +  geom_line(aes(y=Xa2, color="Xa2"),linetype="dotted", size=1)  
g

g <- g + geom_line(aes(y=Xa3, color="Xa3"), linetype="dotted", size=1)  
g 


g <- g +  geom_line(aes(y=Xr0, color="Xr0"), linetype="dotted", size=1)  
g

g <- g +  geom_line(aes(y=Xr1, color="Xr1"), linetype="dotted", size=1)  
g

g <- g + geom_line(aes(y=Xr2, color="Xr2"), linetype="dotted", size=1)  
g
g <- g + geom_line(aes(y=Xr3, color="Xr3"), linetype="dotted", size=1)  
g
g <- g + ylab("pop_size") + xlab("Time") +ylim(0, 1e+09)

g

ggsave("XaXr_plot1.jpg",plot=g)




XnXr<-read.table("simulation_XnXr2_2_3.txt",sep="\t",head=T)
#Xn_run2<-read.table("SSA_Xn_results_run2.txt",sep="\t",head=T)
#Xn_run3<-read.table("SSA_Xn_results_run3.txt",sep="\t",head=T)

tail(XnXr)
XnXr<-as.data.frame(XnXr) 
population<-"Xn0"
g <- ggplot(XnXr, aes(x=Generations), y=Xn0, group=1) 

g <- g +  geom_line(aes(y=Xn0, color=population), linetype="dotted", size=1)  
g
g <- g +  geom_line(aes(y=Xn1, color="Xn1"), linetype="dotted", size=1)  
g

g <- g +  geom_line(aes(y=Xn2, color="Xn2"),linetype="dotted", size=1)  
g

g <- g + geom_line(aes(y=Xn3, color="Xn3"), linetype="dotted", size=1)  
g 


g <- g +  geom_line(aes(y=Xr0, color="Xr0"), linetype="dotted", size=1)  
g

g <- g +  geom_line(aes(y=Xr1, color="Xr1"), linetype="dotted", size=1)  
g

g <- g + geom_line(aes(y=Xr2, color="Xr2"), linetype="dotted", size=1)  
g
g <- g + geom_line(aes(y=Xr3, color="Xr3"), linetype="dotted", size=1)  
g
g <- g + ylab("pop_size") + xlab("Time") +ylim(0, 1e+09)

g

ggsave("XnXr_plot1.jpg",plot=g)
