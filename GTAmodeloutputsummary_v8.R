library(ggplot2)
library(plyr)
library(scales)
library(rgl)
library(scatterplot3d)
library(fields) 
library(aqfig)
library(dplyr)
library(deSolve)
library(gridExtra)


dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161213/"
file1 <- "o_group.txt"
file2 <- "ou_group.txt"
file3 <- "lxps_group.txt"
file4 <- "lxpus_group.txt"
input1 <- paste(dir, file1, sep="")
input2 <- paste(dir, file2, sep="")
input3 <- paste(dir, file3, sep="")
input4 <- paste(dir, file4, sep="")

o.group <- read.delim2(input1, sep = "\t", stringsAsFactors = FALSE, 
                            header = T, na.strings = "")

ou.group <- read.delim2(input2, sep = "\t", stringsAsFactors = FALSE, 
                        header = T, na.strings = "")

lxps.group <-  read.delim2(input3, sep = "\t", stringsAsFactors = FALSE, 
                           header = T, na.strings = "")

lxpus.group <-  read.delim2(input4, sep = "\t", stringsAsFactors = FALSE, 
                           header = T, na.strings = "")

lxp.group <- rbind(lxps.group, lxpus.group)

dat1 <- group_by(lxps.group, K, N)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+ sat"

dat1 <- group_by(lxpus.group, K, N)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X+ unsat"

 
g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("logN") + xlim(5, 9) + ylim(-10, -6)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("logN") + xlim(5, 9) + ylim(-10, -6)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

 
output1 <- paste(dir, "lxpsvsus_kn.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()
###################

dat1 <- group_by(lxps.group, K, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+ sat"

dat1 <- group_by(lxpus.group, K, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X+ unsat"

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("r") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("r") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

 
output1 <- paste(dir, "lxpsvsus_kr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

####################
dat1 <- group_by(lxps.group, K, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+ sat"

dat1 <- group_by(lxpus.group, K, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X+ unsat"
 

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("c") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("c") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))
output1 <- paste(dir, "lxpsvsus_kc.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

##################
dat1 <- group_by(lxps.group, N, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+ sat"

dat1 <- group_by(lxpus.group, N, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X+ unsat"
 

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logN") + ylab("c") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logN") + ylab("c") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

 
output1 <- paste(dir, "lxpsvsus_nc.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

###################

dat1 <- group_by(lxps.group, N, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+ sat"

dat1 <- group_by(lxpus.group, N, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X+ unsat"

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logN") + ylab("r") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logN") + ylab("r") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

output1 <- paste(dir, "lxpsvsus_nr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

#################
dat1 <- group_by(lxps.group, c, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+ sat"

dat1 <- group_by(lxpus.group, c, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X+ unsat"

 
g1<-ggplot(lsxpus_kn, aes(x=as.numeric(c), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("c") + ylab("r") + xlim(0, 1) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=as.numeric(c), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("c") + ylab("r") + xlim(0, 1) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

 
output1 <- paste(dir, "lxpsvsus_cr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

#################
################
################
################


dat1 <- group_by(lxp.group, K, N)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(o.group, K, N)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "o"
 

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("logN") + xlim(5, 9) + ylim(-10, -6)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("logN") + xlim(5, 9) + ylim(-10, -6)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

 
output1 <- paste(dir, "lxp_o_kn.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()


###################

dat1 <- group_by(lxp.group, K, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(o.group, K, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "o"

 
g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("r") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("r") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

 
output1 <- paste(dir, "lxp_o_Kr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

#####################

dat1 <- group_by(lxp.group, K, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(o.group, K, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "o"

 
g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("c") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("c") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

output1 <- paste(dir, "lxp_o_Kc.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()


###################

dat1 <- group_by(lxp.group, N, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(o.group, N, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "o"

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logN") + ylab("c") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logN") + ylab("c") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))
 
output1 <- paste(dir, "lxp_o_Nc.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

################

dat1 <- group_by(lxp.group, N, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(o.group, N, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "o"

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logN") + ylab("r") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logN") + ylab("r") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

output1 <- paste(dir, "lxp_o_Nr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()


#################

dat1 <- group_by(lxp.group, c, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(o.group, c, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "o"

g1<-ggplot(lsxpus_kn, aes(x=as.numeric(c), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("c") + ylab("r") + xlim(0, 1) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=as.numeric(c), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("c") + ylab("r") + xlim(0, 1) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))
 
output1 <- paste(dir, "lxp_o_cr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()


#############
#############
#############
#############
 
dat1 <- group_by(lxp.group, K, N)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(ou.group, K, N)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "ou"


g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("logN") + xlim(5, 9) + ylim(-10, -6)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("logN") + xlim(5, 9) + ylim(-10, -6)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))


output1 <- paste(dir, "lxp_ou_kn.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()


###################

dat1 <- group_by(lxp.group, K, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(ou.group, K, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "ou"


g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("r") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("r") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))


output1 <- paste(dir, "lxp_ou_Kr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

#####################

dat1 <- group_by(lxp.group, K, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(ou.group, K, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "ou"


g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logK") + ylab("c") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logK") + ylab("c") + xlim(5, 9) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

output1 <- paste(dir, "lxp_ou_Kc.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()


###################

dat1 <- group_by(lxp.group, N, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(ou.group, N, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "ou"

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logN") + ylab("c") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logN") + ylab("c") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

output1 <- paste(dir, "lxp_ou_Nc.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

################

dat1 <- group_by(lxp.group, N, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(ou.group, N, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "ou"

g1<-ggplot(lsxpus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("logN") + ylab("r") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("logN") + ylab("r") + xlim(-10, -6) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

output1 <- paste(dir, "lxp_ou_Nr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()


#################

dat1 <- group_by(lxp.group, c, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "lX+"

dat1 <- group_by(ou.group, c, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "ou"

g1<-ggplot(lsxpus_kn, aes(x=as.numeric(c), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(3)) + 
  xlab("c") + ylab("r") + xlim(0, 1) + ylim(0, 1)+
  scale_color_manual(values=c('#8B2323'))+
  scale_size_manual(values=c(2))

g2<-ggplot(lsxnus_kn, aes(x=as.numeric(c), y=as.numeric(r), 
                          color=type, shape=type, size=type)) +   
  geom_point(alpha=0.5) + scale_shape_manual(values=c(4)) + 
  xlab("c") + ylab("r") + xlim(0, 1) + ylim(0, 1)+
  scale_color_manual(values=c('#0000FF'))+
  scale_size_manual(values=c(2))

output1 <- paste(dir, "lxp_ou_cr.png")
png(output1, width = 1200, height = 500)
grid.arrange(g1, g2, ncol=2)
dev.off()

