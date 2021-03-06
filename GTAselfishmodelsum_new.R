library(GillespieSSA)
library(ggplot2)
library(reshape2)
library(plot3D)
library(rgl)
library(rugarch)
library(rgl)
library(fGarch)
load("/Users/Xin/Desktop/GTA_project/output/Jan252016/selfishmodel_newrun3_3.RData")  


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
g <- g +geom_point(aes(y=lp_pop[,2], color=population),shape=19, size=3)   
g
g <- g +geom_point(aes(y=lp_pop[,3], color="X-"),shape=19, size=3)  
g <- g + ylab("poppulation size") + xlab("mu") + ylim(0,1000000000) + xlim(0, 0.01) 
g <- g + scale_colour_brewer(palette = "Set1") + 
  theme(axis.title.y = element_text(size =24), axis.title.x = element_text(vjust = 1,size =24), 
        axis.text.x = element_text(vjust=1, size = 20), axis.text.y = element_text(hjust = 1,size = 20),
        legend.text=element_text(size=18), legend.title = element_text(size = 20),
        panel.background = element_rect(colour = "black", size = 2)) 
g
ggsave("/Users/Xin/Desktop/GTA_project/output/Jan262016/mu_sum.png",plot=g, , width = 12, height = 9)
 
z1 = matrix(NA, nrow = 300, ncol = 500)
z2 = matrix(NA, nrow = 300, ncol = 500)
for (j in 1:500){
  dat = matrix(NA, nrow = 300003, ncol = 3)
  
  len =  length(output[[j]]$data[, 1])
  if (len <  300003){
    for (m in 1:len){
      dat[m,] = output[[j]]$data[m,]
    }
    for (n in (len+1):300003){
     dat[n,] = output[[j]]$data[len,]
    }
  }
  if (len == 300003){
    dat = output[[j]]$data
  }
  test1 <- matrix(NA, nrow = 300, ncol = 3)
  for (i in 1:300) {
   
    test1[i,]<- dat[((i-1)*1000+1),1:3]
  }
  z1[,j] = test1[,2]  
  z2[,j] = test1[,3]
}
  
 

#perspbox(z = volcano, bty = "b2", ticktype = "detailed", d = 2, main  = "bty = 'b2'")
png("/Users/Xin/Desktop/GTA_project/output/Jan252016/3Dsum.png", width = 2500, height=2000)
par(mar = c(1,1,1,1))
col1 = rgb(204,229, 255, alpha = 10, maxColorValue = 255)
col2= rgb(255, 204, 204, alpha = 10, maxColorValue = 255)
#persp3d(x = test1[,1], y= lpfreqs, z = z1,theta=90,phi=60,expand=0.75, zlim = c(0, 10^9), col = col1, ticktype="detailed", 
#        axes=T, zlab = "population size", xlab = "time", ylab = "mu")

persp3D(x = test1[,1], y= lpfreqs, z = z1, bty = "b2", theta=60,phi=5, zlim = c(0, 10^9),xlim=c(0,0.01), col = col1, lighting = T, ltheta = 50, ticktype="detailed", 
        axes=T, zlab = "population size", xlab = "time", ylab = "mu", cex.lab = 4,cex.axis= 2)
persp3D(x = test1[,1], y= lpfreqs, z = z2,col = col2, lighting = T, ltheta = 50, ticktype="detailed", add = T)
dev.off()
 