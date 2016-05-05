library(GillespieSSA)
library(ggplot2)
library(reshape2)

load("/Users/Xin/Desktop/GTA_project/output/Jan262016/selfishmodelsum_mu2.RData")
len = length(output[[500]]$data[,1])

output[[499]]$data[len,]
lpfreqs[499]
ip = c(7.5*10^-7, 5*10^-7, 2.5*10^-7, 10^-6, 7.5*10^-8, 5*10^-8, 2.5*10^-8, 10^-7, 7.5*10^-9, 5*10^-9, 2.5*10^-9, 10^-8, 10^-9)
ip_mu = cbind(ip, min_mu)
#ip_mu[13,2] = 0.01
ip_mu = as.data.frame(ip_mu[1:12,])
g <- ggplot(ip_mu, aes(x=ip, y=min_mu)) +geom_line()+geom_point(shape=21, size=5, fill = "royalblue4")  +  
  ylab("mu") + xlab("Infection&Ingration rate") + ylim(10^-5,0.005) + xlim(10^-9, 10^-6) + 
  theme(axis.title.y = element_text(size =24)) + theme(axis.title.x = element_text(vjust = 3, size =24)) +
  theme(axis.text.x = element_text(vjust = 1, size = 20),axis.text.y=element_text(hjust =1, size = 20), panel.background = element_rect(colour = "black", size = 2))
g
 
ggsave(file = "/Users/Xin/Desktop/GTA_project/output/Jan262016/selfishmodelip_mu.png",plot=g, width = 12, height = 9)
