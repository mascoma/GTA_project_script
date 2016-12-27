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

dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161206/"
file1 <- "lsbs_group.txt"
file2 <- "lsbus_group.txt"
file3 <- "lsxns_group.txt"
file4 <- "lsxnus_group.txt"
file5 <- "lsxps_group.txt"
file6 <- "lsxpus_group.txt"
file7 <- "lubs_group.txt"
file8 <- "luxns_group.txt"
file9 <- "luxps_group.txt"
file10 <- "luxpus_group.txt"
input1 <- paste(dir, file1, sep="")
input2 <- paste(dir, file2, sep="")
input3 <- paste(dir, file3, sep="")
input4 <- paste(dir, file4, sep="")
input5 <- paste(dir, file5, sep="")
input6 <- paste(dir, file6, sep="")
input7 <- paste(dir, file7, sep="")
input8 <- paste(dir, file8, sep="")
input9 <- paste(dir, file9, sep="")
input10 <- paste(dir, file10, sep="")
ls.xpos.sat.group <- read.delim2(input5, sep = "\t", stringsAsFactors = FALSE, 
                                 header = T, na.strings = "")
lu.xpos.sat.group <- read.delim2(input9, sep = "\t", stringsAsFactors = FALSE, 
                                  header = T, na.strings = "")
lu.xneg.sat.group <- read.delim2(input8, sep = "\t", stringsAsFactors = FALSE, 
                                 header = T, na.strings = "")
ls.xneg.sat.group <- read.delim2(input3, sep = "\t", stringsAsFactors = FALSE, 
                                 header = T, na.strings = "")
lu.both.sat.group <- read.delim2(input7, sep = "\t", stringsAsFactors = FALSE, 
                                 header = T, na.strings = "")
ls.both.sat.group <- read.delim2(input1, sep = "\t", stringsAsFactors = FALSE, 
                                 header = T, na.strings = "")
ls.xpos.unsat.group <- read.delim2(input6, sep = "\t", stringsAsFactors = FALSE, 
                                   header = T, na.strings = "")
ls.xneg.unsat.group <- read.delim2(input4, sep = "\t", stringsAsFactors = FALSE, 
                                   header = T, na.strings = "")
ls.both.unsat.group <- read.delim2(input2, sep = "\t", stringsAsFactors = FALSE, 
                                   header = T, na.strings = "")
lu.xpos.unsat.group <- read.delim2(input10, sep = "\t", stringsAsFactors = FALSE, 
                                   header = T, na.strings = "")

l.xpos.sat.group <- rbind(lu.xpos.sat.group, ls.xpos.sat.group)
l.xneg.sat.group <- rbind(lu.xneg.sat.group, ls.xneg.sat.group)
l.both.sat.group <- rbind(lu.both.sat.group, ls.both.sat.group)

l.xpos.unsat.group <- rbind(lu.xpos.unsat.group, ls.xpos.unsat.group)
l.xneg.unsat.group <- ls.xneg.unsat.group
l.both.unsat.group <- ls.both.unsat.group

write.table(l.xpos.sat.group, file="/Users/Xin/Desktop/projects/GTA_project/output/20161206/lxps_group.txt", sep = '\t')
write.table(l.xneg.sat.group, file="/Users/Xin/Desktop/projects/GTA_project/output/20161206/lxns_group.txt", sep = '\t')
write.table(l.both.sat.group, file="/Users/Xin/Desktop/projects/GTA_project/output/20161206/lbs_group.txt", sep = '\t')
write.table(l.xpos.unsat.group, file="/Users/Xin/Desktop/projects/GTA_project/output/20161206/lxpus_group.txt", sep = '\t')
write.table(l.xneg.unsat.group, file="/Users/Xin/Desktop/projects/GTA_project/output/20161206/lxnus_group.txt", sep = '\t')
write.table(l.both.unsat.group, file="/Users/Xin/Desktop/projects/GTA_project/output/20161206/lbus_group.txt", sep = '\t')


dat1 <- group_by(l.xpos.sat.group, K, N)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, K, N)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.sat.group, K, N)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"
 
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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + scale_shape_manual(values=c(19)) + 
  xlab("logK") + ylab("logN") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(5, 9) + ylim(-10, -6)

output1 <- paste(dir, "nonos_kn.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()
###################
 
dat1 <- group_by(l.xpos.sat.group, K, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, K, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.sat.group, K, r)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"
 
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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + scale_shape_manual(values=c(19)) + 
  xlab("logK") + ylab("r") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(5, 9) + ylim(0, 1)
output1 <- paste(dir, "nonos_kr.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()

####################
dat1 <- group_by(l.xpos.sat.group, K, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, K, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.sat.group, K, c)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + scale_shape_manual(values=c(19)) + 
  xlab("logK") + ylab("c") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(5, 9) + ylim(0, 1)
output1 <- paste(dir, "nonos_kc.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()

##################
dat1 <- group_by(l.xpos.sat.group, N, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, N, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.sat.group, N, c)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + scale_shape_manual(values=c(19)) + 
  xlab("logN") + ylab("c") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(-10, -6) + ylim(0, 1)
output1 <- paste(dir, "nonos_nc.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()

###################

dat1 <- group_by(l.xpos.sat.group, N, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, N, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.sat.group, N, r)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + scale_shape_manual(values=c(19)) + 
  xlab("logN") + ylab("r") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(-10,-6) + ylim(0, 1)
output1 <- paste(dir, "nonos_nr.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()

#################
dat1 <- group_by(l.xpos.sat.group, c, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, c, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.sat.group, c, r)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(as.numeric(c), y=as.numeric(r), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + scale_shape_manual(values=c(19)) + 
  xlab("c") + ylab("r") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(0, 1) + ylim(0, 1)
output1 <- paste(dir, "nonos_cr.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()

#################

dat1 <- group_by(l.xpos.unsat.group, K, N)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, K, N)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.unsat.group, K, N)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + 
  scale_shape_manual(values=c(19)) + 
  xlab("logK") + ylab("logN") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(5, 9) + ylim(-10, -6)
output1 <- paste(dir, "nonous_kn.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()


###################

dat1 <- group_by(l.xpos.unsat.group, K, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, K, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.unsat.group, K, r)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + 
  scale_shape_manual(values=c(19)) + 
  xlab("logK") + ylab("r") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(5, 9) + ylim(0, 1)
output1 <- paste(dir, "nonous_Kr.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()

#####################

dat1 <- group_by(l.xpos.unsat.group, K, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, K, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.unsat.group, K, c)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + 
  scale_shape_manual(values=c(19)) + 
  xlab("logK") + ylab("c") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(5, 9) + ylim(0, 1)
output1 <- paste(dir, "nonous_Kc.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()


###################

dat1 <- group_by(l.xpos.unsat.group, N, c)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, N, c)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.unsat.group, N, c)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + 
  scale_shape_manual(values=c(19)) + 
  xlab("logN") + ylab("c") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(-10, -6) + ylim(0, 1)
output1 <- paste(dir, "nonous_Nc.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()

################

dat1 <- group_by(l.xpos.unsat.group, N, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, N, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.unsat.group, N, r)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + 
  scale_shape_manual(values=c(19)) + 
  xlab("logN") + ylab("r") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(-10, -6) + ylim(0, 1)
output1 <- paste(dir, "nonous_Nr.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()


#################

dat1 <- group_by(l.xpos.unsat.group, c, r)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, c, r)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(l.both.unsat.group, c, r)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

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

g3 <- ggplot(lsbus_kn, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                           size=type)) + geom_point(alpha=0.5) + 
  scale_shape_manual(values=c(19)) + 
  xlab("c") + ylab("r") + scale_color_manual(values=c('#68229B'))+
  scale_size_manual(values=c(2)) + xlim(0, 1) + ylim(0, 1)
output1 <- paste(dir, "nonous_cr.png")
png(output1, width = 1800, height = 500)
grid.arrange(g1, g2, g3, ncol=3)
dev.off()
