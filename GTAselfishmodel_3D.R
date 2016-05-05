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
par(mar = c(10,1,1,1))
col1 = rgb(204,229, 255, alpha = 10, maxColorValue = 255)
col2= rgb(255, 204, 204, alpha = 10, maxColorValue = 255)
#persp3d(x = test1[,1], y= lpfreqs, z = z1,theta=90,phi=60,expand=0.75, zlim = c(0, 10^9), col = col1, ticktype="detailed", 
#        axes=T, zlab = "population size", xlab = "time", ylab = "mu")

persp3D(x = test1[,1], y= lpfreqs, z = z1, bty = "b2", theta=60,phi=5, zlim = c(0, 10^9),ylim = c(0,0.01) ,col = col1, lighting = T, ltheta = 50, ticktype="detailed", 
        axes=T, zlab = "population size", xlab = "time", ylab = "mu", cex.lab = 4,cex.axis= 2)
persp3D(x = test1[,1], y= lpfreqs, z = z2,col = col2, lighting = T, ltheta = 50, ticktype="detailed", add = T)
dev.off()