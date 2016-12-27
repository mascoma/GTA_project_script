library(ggplot2)
library(plyr)
library(scales)
library(rgl)
library(scatterplot3d)
library(fields) 
library(aqfig)
library(dplyr)
library(deSolve)

dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161206/"

ou.group <- rbind(ou, lu.xneg.unsat, lu.both.unsat) #1017 no need to split which population dominates because of alwasy oscillation 
output1 <- paste(dir, "ou_group.txt", sep = "")
write.table(ou.group, file = output1, sep = '\t')

os.xpos.sat.group <- 0
os.xneg.sat.group <- os.xneg.sat2
output2 <- paste(dir, "osxns_group.txt", sep = "")
write.table(os.xneg.sat.group, file = output2, sep = '\t')
os.both.sat.group <- 0
os.xpos.unsat.group <- os.xpos.unsat2
output3 <- paste(dir, "osxpus_group.txt", sep = "")
write.table(os.xpos.unsat.group, file = output3, sep = '\t')
os.xneg.unsat.group <- os.xneg.unsat
output4 <- paste(dir, "osxnus_group.txt", sep = "")
write.table(os.xneg.unsat.group, file = output4, sep = '\t')
os.both.unsat.group <- os.both.unsat2
output5 <- paste(dir, "osbus_group.txt", sep = "")
write.table(os.both.unsat.group, file = output5, sep = '\t')

lu.xpos.sat.group <- lu.xpos.sat
output6 <- paste(dir, "luxps_group.txt", sep = "")
write.table(lu.xpos.sat.group, file = output6, sep = '\t')
lu.xneg.sat.group <- lu.xneg.sat
output7 <- paste(dir, "luxns_group.txt", sep = "")
write.table(lu.xneg.sat.group, file = output7, sep = '\t')
lu.both.sat.group <- lu.both.sat
output8 <- paste(dir, "lubs_group.txt", sep = "")
write.table(lu.both.sat.group, file = output8, sep = '\t')
lu.xpos.unsat.group <- lu.xpos.unsat
output9 <- paste(dir, "luxpus_group.txt", sep = "")
write.table(lu.xpos.unsat.group, file = output9, sep = '\t')
lu.xneg.unsat.group <- 0
lu.both.unsat.group <- 0

ls.xpos.sat.group <- rbind(ls.xpos.sat, os.xpos.sat)
output10 <- paste(dir, "lsxps_group.txt", sep = "")
write.table(ls.xpos.sat.group, file = output10, sep = '\t')
ls.xneg.sat.group <- rbind(ls.xneg.sat, ls.xneg.sat2)
output11 <- paste(dir, "lsxns_group.txt", sep = "")
write.table(ls.xneg.sat.group, file = output11, sep = '\t')
ls.both.sat.group <- rbind(ls.both.sat, os.both.sat)
output12 <- paste(dir, "lsbs_group.txt", sep = "")
write.table(ls.both.sat.group, file = output12, sep = '\t')
ls.xpos.unsat.group <- rbind(ls.xpos.unsat, ls.xpos.unsat2)
output13 <- paste(dir, "lsxpus_group.txt", sep = "")
write.table(ls.xpos.unsat.group, file = output13, sep = '\t')
ls.xneg.unsat.group <- ls.xneg.unsat
output14 <- paste(dir, "lsxnus_group.txt", sep = "")
write.table(ls.xneg.unsat.group, file = output14, sep = '\t')
ls.both.unsat.group <- rbind(ls.both.unsat, ls.both.unsat2)
output15 <- paste(dir, "lsbus_group.txt", sep = "")
write.table(ls.both.unsat.group, file = output15, sep = '\t')

o.group <- rbind(ou.group, os.xpos.unsat.group, os.xneg.sat.group, 
                 os.xneg.unsat.group, os.both.unsat.group)
output16 <- paste(dir, "o_group.txt", sep = "")
write.table(o.group, file = output16, sep = '\t')

l.group <- rbind(lu.xpos.sat.group, lu.xneg.sat.group, lu.both.sat.group, 
                 lu.xpos.unsat.group, ls.xpos.sat.group, ls.xneg.sat.group,
                 ls.both.sat.group, ls.xpos.unsat.group, ls.xneg.unsat.group,
                 ls.both.unsat.group)

output17 <- paste(dir, "l_group.txt", sep = "")
write.table(l.group, file = output17, sep = '\t')

sat.group <- rbind(os.xneg.sat.group, lu.xpos.sat.group, lu.xneg.sat.group, 
                   lu.both.sat.group, ls.xpos.sat.group, ls.xneg.sat.group,
                   ls.both.sat.group)

output18 <- paste(dir, "sat_group.txt", sep = "")
write.table(sat.group, file = output18, sep = '\t')

unsat.group<- rbind(os.xpos.unsat.group, os.xneg.unsat.group, os.both.unsat.group, 
                    lu.xpos.unsat.group, ls.xpos.unsat.group, ls.xneg.unsat.group, 
                    ls.both.unsat.group)

output19 <- paste(dir, "unsat_group.txt", sep = "")
write.table(unsat.group, file = output19, sep = '\t')

dat1 <- group_by(ou.group, K, N)
ou_kn <- summarise(dat1, n = n())
ou_kn$propn <- (ou_kn$n/sum(ou_kn$n))*100
ou_kn$type <- "ou"

nonou.group <- rbind(sat.group, unsat.group)

dat1 <- group_by(nonou.group, K, N)
nonou_kn <- summarise(dat1, n = n())
nonou_kn$propn <- (nonou_kn$n/sum(nonou_kn$n))*100
nonou_kn$type <- "nonou"

by_kn <- rbind(nonou_kn, ou_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                size=type)) + geom_point(alpha = 0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
            ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
            scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ou_non_kn.png", width = 8, height = 8)

dat1 <- group_by(ou.group, K, r)
ou_kr <- summarise(dat1, n = n())
ou_kr$propn <- (ou_kr$n/sum(ou_kr$n))*100
ou_kr$type <- "ou"

dat1 <- group_by(nonou.group, K, r)
nonou_kr <- summarise(dat1, n = n())
nonou_kr$propn <- (nonou_kr$n/sum(nonou_kr$n))*100
nonou_kr$type <- "nonou"

by_kr <- rbind(nonou_kr, ou_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ou_non_kr.png", width = 8, height = 8)

dat1 <- group_by(ou.group, K, c)
ou_kc <- summarise(dat1, n = n())
ou_kc$propn <- (ou_kc$n/sum(ou_kc$n))*100
ou_kc$type <- "ou"

dat1 <- group_by(nonou.group, K, c)
nonou_kc <- summarise(dat1, n = n())
nonou_kc$propn <- (nonou_kc$n/sum(nonou_kc$n))*100
nonou_kc$type <- "nonou"

by_kc <- rbind(nonou_kc, ou_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ou_non_kc.png", width = 8, height = 8)

dat1 <- group_by(ou.group, N, c)
ou_nc <- summarise(dat1, n = n())
ou_nc$propn <- (ou_nc$n/sum(ou_nc$n))*100
ou_nc$type <- "ou"

dat1 <- group_by(nonou.group, N, c)
nonou_nc <- summarise(dat1, n = n())
nonou_nc$propn <- (nonou_nc$n/sum(nonou_nc$n))*100
nonou_nc$type <- "nonou"

by_nc <- rbind(nonou_nc, ou_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logN") +
  ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ou_non_nc.png", width = 8, height = 8)

dat1 <- group_by(ou.group, N, r)
ou_nr <- summarise(dat1, n = n())
ou_nr$propn <- (ou_nr$n/sum(ou_nr$n))*100
ou_nr$type <- "ou"

dat1 <- group_by(nonou.group, N, r)
nonou_nr <- summarise(dat1, n = n())
nonou_nr$propn <- (nonou_nr$n/sum(nonou_nr$n))*100
nonou_nr$type <- "nonou"

by_nr <- rbind(nonou_nr, ou_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logN") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ou_non_nr.png", width = 8, height = 8)

dat1 <- group_by(ou.group, c, r)
ou_cr <- summarise(dat1, n = n())
ou_cr$propn <- (ou_cr$n/sum(ou_cr$n))*100
ou_cr$type <- "ou"

dat1 <- group_by(nonou.group, c, r)
nonou_cr <- summarise(dat1, n = n())
nonou_cr$propn <- (nonou_cr$n/sum(nonou_cr$n))*100
nonou_cr$type <- "nonou"

by_cr <- rbind(nonou_cr, ou_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("c") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ou_non_cr.png", width = 8, height = 8)

dat1 <- group_by(o.group, c, r)
o_cr <- summarise(dat1, n = n())
o_cr$propn <- (o_cr$n/sum(o_cr$n))*100
o_cr$type <- "o"

dat1 <- group_by(l.group, c, r)
l_cr <- summarise(dat1, n = n())
l_cr$propn <- (l_cr$n/sum(l_cr$n))*100
l_cr$type <- "l"

by_cr <- rbind(l_cr, o_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("c") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/o_l_cr.png", width = 8, height = 8)

dat1 <- group_by(o.group, N, c)
o_nc <- summarise(dat1, n = n())
o_nc$propn <- (o_nc$n/sum(o_nc$n))*100
o_nc$type <- "o"

dat1 <- group_by(l.group, N, c)
l_nc <- summarise(dat1, n = n())
l_nc$propn <- (l_nc$n/sum(l_nc$n))*100
l_nc$type <- "l"

by_nc <- rbind(l_nc, o_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logN") +
  ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/o_l_nc.png", width = 8, height = 8)

dat1 <- group_by(o.group, N, r)
o_nr <- summarise(dat1, n = n())
o_nr$propn <- (o_nr$n/sum(o_nr$n))*100
o_nr$type <- "o"

dat1 <- group_by(l.group, N, r)
l_nr <- summarise(dat1, n = n())
l_nr$propn <- (l_nr$n/sum(l_nr$n))*100
l_nr$type <- "l"

by_nr <- rbind(l_nr, o_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logN") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/o_l_nr.png", width = 8, height = 8)

dat1 <- group_by(o.group, K, c)
o_kc <- summarise(dat1, n = n())
o_kc$propn <- (o_kc$n/sum(o_kc$n))*100
o_kc$type <- "o"

dat1 <- group_by(l.group, K, c)
l_kc <- summarise(dat1, n = n())
l_kc$propn <- (l_kc$n/sum(l_kc$n))*100
l_kc$type <- "l"

by_kc <- rbind(l_kc, o_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/o_l_kc.png", width = 8, height = 8)

dat1 <- group_by(o.group, K, r)
o_kr <- summarise(dat1, n = n())
o_kr$propn <- (o_kr$n/sum(o_kr$n))*100
o_kr$type <- "o"

dat1 <- group_by(l.group, K, r)
l_kr <- summarise(dat1, n = n())
l_kr$propn <- (l_kr$n/sum(l_kr$n))*100
l_kr$type <- "l"

by_kr <- rbind(l_kr, o_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/o_l_kr.png", width = 8, height = 8)

dat1 <- group_by(o.group, K, N)
o_kn <- summarise(dat1, n = n())
o_kn$propn <- (o_kn$n/sum(o_kn$n))*100
o_kn$type <- "o"

dat1 <- group_by(l.group, K, N)
l_kn <- summarise(dat1, n = n())
l_kn$propn <- (l_kn$n/sum(l_kn$n))*100
l_kn$type <- "l"

by_kn <- rbind(l_kn, o_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/o_l_kn.png", width = 8, height = 8)

dat1 <- group_by(sat.group, K, N)
sat_kn <- summarise(dat1, n = n())
sat_kn$propn <- (sat_kn$n/sum(sat_kn$n))*100
sat_kn$type <- "sat"

dat1 <- group_by(unsat.group, K, N)
unsat_kn <- summarise(dat1, n = n())
unsat_kn$propn <- (unsat_kn$n/sum(unsat_kn$n))*100
unsat_kn$type <- "unsat"

by_kn <- rbind(sat_kn, unsat_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/sat_kn.png", width = 8, height = 8)

dat1 <- group_by(sat.group, K, r)
sat_kr <- summarise(dat1, n = n())
sat_kr$propn <- (sat_kr$n/sum(sat_kr$n))*100
sat_kr$type <- "sat"

dat1 <- group_by(unsat.group, K, r)
unsat_kr <- summarise(dat1, n = n())
unsat_kr$propn <- (unsat_kr$n/sum(unsat_kr$n))*100
unsat_kr$type <- "unsat"

by_kr <- rbind(sat_kr, unsat_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/sat_kr.png", width = 8, height = 8)

dat1 <- group_by(sat.group, K, c)
sat_kc <- summarise(dat1, n = n())
sat_kc$propn <- (sat_kc$n/sum(sat_kc$n))*100
sat_kc$type <- "sat"

dat1 <- group_by(unsat.group, K, c)
unsat_kc <- summarise(dat1, n = n())
unsat_kc$propn <- (unsat_kc$n/sum(unsat_kc$n))*100
unsat_kc$type <- "unsat"

by_kc <- rbind(sat_kc, unsat_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logK") +
  ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/sat_kc.png", width = 8, height = 8)

dat1 <- group_by(sat.group, N, c)
sat_nc <- summarise(dat1, n = n())
sat_nc$propn <- (sat_nc$n/sum(sat_nc$n))*100
sat_nc$type <- "sat"

dat1 <- group_by(unsat.group, N, c)
unsat_nc <- summarise(dat1, n = n())
unsat_nc$propn <- (unsat_nc$n/sum(unsat_nc$n))*100
unsat_nc$type <- "unsat"

by_nc <- rbind(sat_nc, unsat_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logN") +
  ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/sat_nc.png", width = 8, height = 8)

dat1 <- group_by(sat.group, N, r)
sat_nr <- summarise(dat1, n = n())
sat_nr$propn <- (sat_nr$n/sum(sat_nr$n))*100
sat_nr$type <- "sat"

dat1 <- group_by(unsat.group, N, r)
unsat_nr <- summarise(dat1, n = n())
unsat_nr$propn <- (unsat_nr$n/sum(unsat_nr$n))*100
unsat_nr$type <- "unsat"

by_nr <- rbind(sat_nr, unsat_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("logN") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/sat_nr.png", width = 8, height = 8)

dat1 <- group_by(sat.group, c, r)
sat_cr <- summarise(dat1, n = n())
sat_cr$propn <- (sat_cr$n/sum(sat_cr$n))*100
sat_cr$type <- "sat"

dat1 <- group_by(unsat.group, c, r)
unsat_cr <- summarise(dat1, n = n())
unsat_cr$propn <- (unsat_cr$n/sum(unsat_cr$n))*100
unsat_cr$type <- "unsat"

by_cr <- rbind(sat_cr, unsat_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17)) + xlab("c") +
  ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B'))+
  scale_size_manual(values=c(2,2))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/sat_cr.png", width = 8, height = 8)

dat1 <- group_by(lu.xpos.sat.group, c, r)
luxps_cr <- summarise(dat1, n = n())
luxps_cr$propn <- (luxps_cr$n/sum(luxps_cr$n))*100
luxps_cr$type <- "X+"

dat1 <- group_by(lu.xneg.sat.group, c, r)
luxns_cr <- summarise(dat1, n = n())
luxns_cr$propn <- (luxns_cr$n/sum(luxns_cr$n))*100
luxns_cr$type <- "X-"

dat1 <- group_by(lu.both.sat.group, c, r)
lubs_cr <- summarise(dat1, n = n())
lubs_cr$propn <- (lubs_cr$n/sum(lubs_cr$n))*100
lubs_cr$type <- "X+X-"

by_cr <- rbind(luxns_cr, luxps_cr, lubs_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("c") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B' , '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lu_cr.png", width = 8, height = 8)

dat1 <- group_by(lu.xpos.sat.group, N, r)
luxps_nr <- summarise(dat1, n = n())
luxps_nr$propn <- (luxps_nr$n/sum(luxps_nr$n))*100
luxps_nr$type <- "X+"

dat1 <- group_by(lu.xneg.sat.group, N, r)
luxns_nr <- summarise(dat1, n = n())
luxns_nr$propn <- (luxns_nr$n/sum(luxns_nr$n))*100
luxns_nr$type <- "X-"

dat1 <- group_by(lu.both.sat.group, N, r)
lubs_nr <- summarise(dat1, n = n())
lubs_nr$propn <- (lubs_nr$n/sum(lubs_nr$n))*100
lubs_nr$type <- "X+X-"

by_nr <- rbind(luxns_nr, luxps_nr, lubs_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lu_nr.png", width = 8, height = 8)

dat1 <- group_by(lu.xpos.sat.group, N, c)
luxps_nc <- summarise(dat1, n = n())
luxps_nc$propn <- (luxps_nc$n/sum(luxps_nc$n))*100
luxps_nc$type <- "X+"

dat1 <- group_by(lu.xneg.sat.group, N, c)
luxns_nc <- summarise(dat1, n = n())
luxns_nc$propn <- (luxns_nc$n/sum(luxns_nc$n))*100
luxns_nc$type <- "X-"

dat1 <- group_by(lu.both.sat.group, N, c)
lubs_nc <- summarise(dat1, n = n())
lubs_nc$propn <- (lubs_nc$n/sum(lubs_nc$n))*100
lubs_nc$type <- "X+X-"

by_nc <- rbind(luxns_nc, luxps_nc, lubs_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lu_nc.png", width = 8, height = 8)

dat1 <- group_by(lu.xpos.sat.group, K, c)
luxps_kc <- summarise(dat1, n = n())
luxps_kc$propn <- (luxps_kc$n/sum(luxps_kc$n))*100
luxps_kc$type <- "X+"

dat1 <- group_by(lu.xneg.sat.group, K, c)
luxns_kc <- summarise(dat1, n = n())
luxns_kc$propn <- (luxns_kc$n/sum(luxns_kc$n))*100
luxns_kc$type <- "X-"

dat1 <- group_by(lu.both.sat.group, K, c)
lubs_kc <- summarise(dat1, n = n())
lubs_kc$propn <- (lubs_kc$n/sum(lubs_kc$n))*100
lubs_kc$type <- "X+X-"

by_kc <- rbind(luxns_kc, luxps_kc, lubs_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lu_kc.png", width = 8, height = 8)

dat1 <- group_by(lu.xpos.sat.group, K, r)
luxps_kr <- summarise(dat1, n = n())
luxps_kr$propn <- (luxps_kr$n/sum(luxps_kr$n))*100
luxps_kr$type <- "X+"

dat1 <- group_by(lu.xneg.sat.group, K, r)
luxns_kr <- summarise(dat1, n = n())
luxns_kr$propn <- (luxns_kr$n/sum(luxns_kr$n))*100
luxns_kr$type <- "X-"

dat1 <- group_by(lu.both.sat.group, K, r)
lubs_kr <- summarise(dat1, n = n())
lubs_kr$propn <- (lubs_kr$n/sum(lubs_kr$n))*100
lubs_kr$type <- "X+X-"

by_kr <- rbind(luxns_kr, luxps_kr, lubs_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lu_kr.png", width = 8, height = 8)

dat1 <- group_by(lu.xpos.sat.group, K, N)
luxps_kn <- summarise(dat1, n = n())
luxps_kn$propn <- (luxps_kn$n/sum(luxps_kn$n))*100
luxps_kn$type <- "X+"

dat1 <- group_by(lu.xneg.sat.group, K, N)
luxns_kn <- summarise(dat1, n = n())
luxns_kn$propn <- (luxns_kn$n/sum(luxns_kn$n))*100
luxns_kn$type <- "X-"

dat1 <- group_by(lu.both.sat.group, K, N)
lubs_kn <- summarise(dat1, n = n())
lubs_kn$propn <- (lubs_kn$n/sum(lubs_kn$n))*100
lubs_kn$type <- "X+X-"

by_kn <- rbind(luxns_kn, luxps_kn, lubs_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lu_kn.png", width = 8, height = 8)



dat1 <- group_by(ls.xpos.sat.group, c, r)
lsxps_cr <- summarise(dat1, n = n())
lsxps_cr$propn <- (lsxps_cr$n/sum(lsxps_cr$n))*100
lsxps_cr$type <- "X+"

dat1 <- group_by(ls.xneg.sat.group, c, r)
lsxns_cr <- summarise(dat1, n = n())
lsxns_cr$propn <- (lsxns_cr$n/sum(lsxns_cr$n))*100
lsxns_cr$type <- "X-"

dat1 <- group_by(ls.both.sat.group, c, r)
lsbs_cr <- summarise(dat1, n = n())
lsbs_cr$propn <- (lsbs_cr$n/sum(lsbs_cr$n))*100
lsbs_cr$type <- "X+X-"

by_cr <- rbind(lsxns_cr, lsxps_cr, lsbs_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("c") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B' , '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ls_cr.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.sat.group, N, r)
lsxps_nr <- summarise(dat1, n = n())
lsxps_nr$propn <- (lsxps_nr$n/sum(lsxps_nr$n))*100
lsxps_nr$type <- "X+"

dat1 <- group_by(ls.xneg.sat.group, N, r)
lsxns_nr <- summarise(dat1, n = n())
lsxns_nr$propn <- (lsxns_nr$n/sum(lsxns_nr$n))*100
lsxns_nr$type <- "X-"

dat1 <- group_by(ls.both.sat.group, N, r)
lsbs_nr <- summarise(dat1, n = n())
lsbs_nr$propn <- (lsbs_nr$n/sum(lsbs_nr$n))*100
lsbs_nr$type <- "X+X-"

by_nr <- rbind(lsxns_nr, lsxps_nr, lsbs_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ls_nr.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.sat.group, N, c)
lsxps_nc <- summarise(dat1, n = n())
lsxps_nc$propn <- (lsxps_nc$n/sum(lsxps_nc$n))*100
lsxps_nc$type <- "X+"

dat1 <- group_by(ls.xneg.sat.group, N, c)
lsxns_nc <- summarise(dat1, n = n())
lsxns_nc$propn <- (lsxns_nc$n/sum(lsxns_nc$n))*100
lsxns_nc$type <- "X-"

dat1 <- group_by(ls.both.sat.group, N, c)
lsbs_nc <- summarise(dat1, n = n())
lsbs_nc$propn <- (lsbs_nc$n/sum(lsbs_nc$n))*100
lsbs_nc$type <- "X+X-"

by_nc <- rbind(lsxns_nc, lsxps_nc, lsbs_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ls_nc.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.sat.group, K, c)
lsxps_kc <- summarise(dat1, n = n())
lsxps_kc$propn <- (lsxps_kc$n/sum(lsxps_kc$n))*100
lsxps_kc$type <- "X+"

dat1 <- group_by(ls.xneg.sat.group, K, c)
lsxns_kc <- summarise(dat1, n = n())
lsxns_kc$propn <- (lsxns_kc$n/sum(lsxns_kc$n))*100
lsxns_kc$type <- "X-"

dat1 <- group_by(ls.both.sat.group, K, c)
lsbs_kc <- summarise(dat1, n = n())
lsbs_kc$propn <- (lsbs_kc$n/sum(lsbs_kc$n))*100
lsbs_kc$type <- "X+X-"

by_kc <- rbind(lsxns_kc, lsxps_kc, lsbs_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ls_kc.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.sat.group, K, r)
lsxps_kr <- summarise(dat1, n = n())
lsxps_kr$propn <- (lsxps_kr$n/sum(lsxps_kr$n))*100
lsxps_kr$type <- "X+"

dat1 <- group_by(ls.xneg.sat.group, K, r)
lsxns_kr <- summarise(dat1, n = n())
lsxns_kr$propn <- (lsxns_kr$n/sum(lsxns_kr$n))*100
lsxns_kr$type <- "X-"

dat1 <- group_by(ls.both.sat.group, K, r)
lsbs_kr <- summarise(dat1, n = n())
lsbs_kr$propn <- (lsbs_kr$n/sum(lsbs_kr$n))*100
lsbs_kr$type <- "X+X-"

by_kr <- rbind(lsxns_kr, lsxps_kr, lsbs_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ls_kr.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.sat.group, K, N)
lsxps_kn <- summarise(dat1, n = n())
lsxps_kn$propn <- (lsxps_kn$n/sum(lsxps_kn$n))*100
lsxps_kn$type <- "X+"

dat1 <- group_by(ls.xneg.sat.group, K, N)
lsxns_kn <- summarise(dat1, n = n())
lsxns_kn$propn <- (lsxns_kn$n/sum(lsxns_kn$n))*100
lsxns_kn$type <- "X-"

dat1 <- group_by(ls.both.sat.group, K, N)
lsbs_kn <- summarise(dat1, n = n())
lsbs_kn$propn <- (lsbs_kn$n/sum(lsbs_kn$n))*100
lsbs_kn$type <- "X+X-"

by_kn <- rbind(lsxns_kn, lsxps_kn, lsbs_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/ls_kn.png", width = 8, height = 8)




dat1 <- group_by(ls.xpos.unsat.group, c, r)
lsxpus_cr <- summarise(dat1, n = n())
lsxpus_cr$propn <- (lsxpus_cr$n/sum(lsxpus_cr$n))*100
lsxpus_cr$type <- "X+"

dat1 <- group_by(ls.xneg.unsat.group, c, r)
lsxnus_cr <- summarise(dat1, n = n())
lsxnus_cr$propn <- (lsxnus_cr$n/sum(lsxnus_cr$n))*100
lsxnus_cr$type <- "X-"

dat1 <- group_by(ls.both.unsat.group, c, r)
lsbus_cr <- summarise(dat1, n = n())
lsbus_cr$propn <- (lsbus_cr$n/sum(lsbus_cr$n))*100
lsbus_cr$type <- "X+X-"

by_cr <- rbind(lsxnus_cr, lsxpus_cr, lsbus_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("c") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B' , '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lus_cr.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.unsat.group, N, r)
lsxpus_nr <- summarise(dat1, n = n())
lsxpus_nr$propn <- (lsxpus_nr$n/sum(lsxpus_nr$n))*100
lsxpus_nr$type <- "X+"

dat1 <- group_by(ls.xneg.unsat.group, N, r)
lsxnus_nr <- summarise(dat1, n = n())
lsxnus_nr$propn <- (lsxnus_nr$n/sum(lsxnus_nr$n))*100
lsxnus_nr$type <- "X-"

dat1 <- group_by(ls.both.unsat.group, N, r)
lsbus_nr <- summarise(dat1, n = n())
lsbus_nr$propn <- (lsbus_nr$n/sum(lsbus_nr$n))*100
lsbus_nr$type <- "X+X-"

by_nr <- rbind(lsxnus_nr, lsxpus_nr, lsbus_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lus_nr.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.unsat.group, N, c)
lsxpus_nc <- summarise(dat1, n = n())
lsxpus_nc$propn <- (lsxpus_nc$n/sum(lsxpus_nc$n))*100
lsxpus_nc$type <- "X+"

dat1 <- group_by(ls.xneg.unsat.group, N, c)
lsxnus_nc <- summarise(dat1, n = n())
lsxnus_nc$propn <- (lsxnus_nc$n/sum(lsxnus_nc$n))*100
lsxnus_nc$type <- "X-"

dat1 <- group_by(ls.both.unsat.group, N, c)
lsbus_nc <- summarise(dat1, n = n())
lsbus_nc$propn <- (lsbus_nc$n/sum(lsbus_nc$n))*100
lsbus_nc$type <- "X+X-"

by_nc <- rbind(lsxnus_nc, lsxpus_nc, lsbus_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lus_nc.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.unsat.group, K, c)
lsxpus_kc <- summarise(dat1, n = n())
lsxpus_kc$propn <- (lsxpus_kc$n/sum(lsxpus_kc$n))*100
lsxpus_kc$type <- "X+"

dat1 <- group_by(ls.xneg.unsat.group, K, c)
lsxnus_kc <- summarise(dat1, n = n())
lsxnus_kc$propn <- (lsxnus_kc$n/sum(lsxnus_kc$n))*100
lsxnus_kc$type <- "X-"

dat1 <- group_by(ls.both.unsat.group, K, c)
lsbus_kc <- summarise(dat1, n = n())
lsbus_kc$propn <- (lsbus_kc$n/sum(lsbus_kc$n))*100
lsbus_kc$type <- "X+X-"

by_kc <- rbind(lsxnus_kc, lsxpus_kc, lsbus_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lus_kc.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.unsat.group, K, r)
lsxpus_kr <- summarise(dat1, n = n())
lsxpus_kr$propn <- (lsxpus_kr$n/sum(lsxpus_kr$n))*100
lsxpus_kr$type <- "X+"

dat1 <- group_by(ls.xneg.unsat.group, K, r)
lsxnus_kr <- summarise(dat1, n = n())
lsxnus_kr$propn <- (lsxnus_kr$n/sum(lsxnus_kr$n))*100
lsxnus_kr$type <- "X-"

dat1 <- group_by(ls.both.unsat.group, K, r)
lsbus_kr <- summarise(dat1, n = n())
lsbus_kr$propn <- (lsbus_kr$n/sum(lsbus_kr$n))*100
lsbus_kr$type <- "X+X-"

by_kr <- rbind(lsxnus_kr, lsxpus_kr, lsbus_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lus_kr.png", width = 8, height = 8)

dat1 <- group_by(ls.xpos.unsat.group, K, N)
lsxpus_kn <- summarise(dat1, n = n())
lsxpus_kn$propn <- (lsxpus_kn$n/sum(lsxpus_kn$n))*100
lsxpus_kn$type <- "X+"

dat1 <- group_by(ls.xneg.unsat.group, K, N)
lsxnus_kn <- summarise(dat1, n = n())
lsxnus_kn$propn <- (lsxnus_kn$n/sum(lsxnus_kn$n))*100
lsxnus_kn$type <- "X-"

dat1 <- group_by(ls.both.unsat.group, K, N)
lsbus_kn <- summarise(dat1, n = n())
lsbus_kn$propn <- (lsbus_kn$n/sum(lsbus_kn$n))*100
lsbus_kn$type <- "X+X-"

by_kn <- rbind(lsxnus_kn, lsxpus_kn, lsbus_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lus_kn.png", width = 8, height = 8)


l.xpos.sat.group <- rbind(lu.xpos.sat.group, ls.xpos.sat.group)
output <- paste(dir, "l_satxp.txt", sep="")
write.table(l.xpos.sat.group, file = output, sep = '\t')


l.xneg.sat.group <- rbind(lu.xneg.sat.group, ls.xneg.sat.group)
l.both.sat.group <- rbind(lu.both.sat.group, ls.both.sat.group)

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

by_kn <- rbind(lsxnus_kn, lsxpus_kn, lsbus_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lsat_kn.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.sat.group, c, r)
lsxpus_cr <- summarise(dat1, n = n())
lsxpus_cr$propn <- (lsxpus_cr$n/sum(lsxpus_cr$n))*100
lsxpus_cr$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, c, r)
lsxnus_cr <- summarise(dat1, n = n())
lsxnus_cr$propn <- (lsxnus_cr$n/sum(lsxnus_cr$n))*100
lsxnus_cr$type <- "X-"

dat1 <- group_by(l.both.sat.group, c, r)
lsbus_cr <- summarise(dat1, n = n())
lsbus_cr$propn <- (lsbus_cr$n/sum(lsbus_cr$n))*100
lsbus_cr$type <- "X+X-"

by_cr <- rbind(lsxnus_cr, lsxpus_cr, lsbus_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("c") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B' , '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lsat_cr.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.sat.group, N, r)
lsxpus_nr <- summarise(dat1, n = n())
lsxpus_nr$propn <- (lsxpus_nr$n/sum(lsxpus_nr$n))*100
lsxpus_nr$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, N, r)
lsxnus_nr <- summarise(dat1, n = n())
lsxnus_nr$propn <- (lsxnus_nr$n/sum(lsxnus_nr$n))*100
lsxnus_nr$type <- "X-"

dat1 <- group_by(l.both.sat.group, N, r)
lsbus_nr <- summarise(dat1, n = n())
lsbus_nr$propn <- (lsbus_nr$n/sum(lsbus_nr$n))*100
lsbus_nr$type <- "X+X-"

by_nr <- rbind(lsxnus_nr, lsxpus_nr, lsbus_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lsat_nr.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.sat.group, N, c)
lsxpus_nc <- summarise(dat1, n = n())
lsxpus_nc$propn <- (lsxpus_nc$n/sum(lsxpus_nc$n))*100
lsxpus_nc$type <- "X+"

dat1 <- group_by(l.xnegsat.group, N, c)
lsxnus_nc <- summarise(dat1, n = n())
lsxnus_nc$propn <- (lsxnus_nc$n/sum(lsxnus_nc$n))*100
lsxnus_nc$type <- "X-"

dat1 <- group_by(l.both.sat.group, N, c)
lsbus_nc <- summarise(dat1, n = n())
lsbus_nc$propn <- (lsbus_nc$n/sum(lsbus_nc$n))*100
lsbus_nc$type <- "X+X-"

by_nc <- rbind(lsxnus_nc, lsxpus_nc, lsbus_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lsat_nc.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.sat.group, K, c)
lsxpus_kc <- summarise(dat1, n = n())
lsxpus_kc$propn <- (lsxpus_kc$n/sum(lsxpus_kc$n))*100
lsxpus_kc$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, K, c)
lsxnus_kc <- summarise(dat1, n = n())
lsxnus_kc$propn <- (lsxnus_kc$n/sum(lsxnus_kc$n))*100
lsxnus_kc$type <- "X-"

dat1 <- group_by(l.both.sat.group, K, c)
lsbus_kc <- summarise(dat1, n = n())
lsbus_kc$propn <- (lsbus_kc$n/sum(lsbus_kc$n))*100
lsbus_kc$type <- "X+X-"

by_kc <- rbind(lsxnus_kc, lsxpus_kc, lsbus_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lsat_kc.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.sat.group, K, r)
lsxpus_kr <- summarise(dat1, n = n())
lsxpus_kr$propn <- (lsxpus_kr$n/sum(lsxpus_kr$n))*100
lsxpus_kr$type <- "X+"

dat1 <- group_by(l.xneg.sat.group, K, r)
lsxnus_kr <- summarise(dat1, n = n())
lsxnus_kr$propn <- (lsxnus_kr$n/sum(lsxnus_kr$n))*100
lsxnus_kr$type <- "X-"

dat1 <- group_by(l.both.sat.group, K, r)
lsbus_kr <- summarise(dat1, n = n())
lsbus_kr$propn <- (lsbus_kr$n/sum(lsbus_kr$n))*100
lsbus_kr$type <- "X+X-"

by_kr <- rbind(lsxnus_kr, lsxpus_kr, lsbus_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lsat_kr.png", width = 8, height = 8)



l.xpos.unsat.group <- rbind(lu.xpos.unsat.group, ls.xpos.unsat.group)
output <- paste(dir, "l_unsatxp.txt", sep="")
write.table(l.xpos.unsat.group, file = output, sep = '\t')
l.xneg.unsat.group <- ls.xneg.unsat.group
l.both.unsat.group <- ls.both.unsat.group

 
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

by_kn <- rbind(lsxnus_kn, lsxpus_kn, lsbus_kn)

g <- ggplot(by_kn, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("logN") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lunsat_kn.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.unsat.group, c, r)
lsxpus_cr <- summarise(dat1, n = n())
lsxpus_cr$propn <- (lsxpus_cr$n/sum(lsxpus_cr$n))*100
lsxpus_cr$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, c, r)
lsxnus_cr <- summarise(dat1, n = n())
lsxnus_cr$propn <- (lsxnus_cr$n/sum(lsxnus_cr$n))*100
lsxnus_cr$type <- "X-"

dat1 <- group_by(l.both.unsat.group, c, r)
lsbus_cr <- summarise(dat1, n = n())
lsbus_cr$propn <- (lsbus_cr$n/sum(lsbus_cr$n))*100
lsbus_cr$type <- "X+X-"

by_cr <- rbind(lsxnus_cr, lsxpus_cr, lsbus_cr)

g <- ggplot(by_cr, aes(x=as.numeric(c), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("c") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B' , '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lunsat_cr.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.unsat.group, N, r)
lsxpus_nr <- summarise(dat1, n = n())
lsxpus_nr$propn <- (lsxpus_nr$n/sum(lsxpus_nr$n))*100
lsxpus_nr$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, N, r)
lsxnus_nr <- summarise(dat1, n = n())
lsxnus_nr$propn <- (lsxnus_nr$n/sum(lsxnus_nr$n))*100
lsxnus_nr$type <- "X-"

dat1 <- group_by(l.both.unsat.group, N, r)
lsbus_nr <- summarise(dat1, n = n())
lsbus_nr$propn <- (lsbus_nr$n/sum(lsbus_nr$n))*100
lsbus_nr$type <- "X+X-"

by_nr <- rbind(lsxnus_nr, lsxpus_nr, lsbus_nr)

g <- ggplot(by_nr, aes(x=log10(as.numeric(N)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lunsat_nr.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.unsat.group, N, c)
lsxpus_nc <- summarise(dat1, n = n())
lsxpus_nc$propn <- (lsxpus_nc$n/sum(lsxpus_nc$n))*100
lsxpus_nc$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, N, c)
lsxnus_nc <- summarise(dat1, n = n())
lsxnus_nc$propn <- (lsxnus_nc$n/sum(lsxnus_nc$n))*100
lsxnus_nc$type <- "X-"

dat1 <- group_by(l.both.unsat.group, N, c)
lsbus_nc <- summarise(dat1, n = n())
lsbus_nc$propn <- (lsbus_nc$n/sum(lsbus_nc$n))*100
lsbus_nc$type <- "X+X-"

by_nc <- rbind(lsxnus_nc, lsxpus_nc, lsbus_nc)

g <- ggplot(by_nc, aes(x=log10(as.numeric(N)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logN") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lunsat_nc.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.unsat.group, K, c)
lsxpus_kc <- summarise(dat1, n = n())
lsxpus_kc$propn <- (lsxpus_kc$n/sum(lsxpus_kc$n))*100
lsxpus_kc$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, K, c)
lsxnus_kc <- summarise(dat1, n = n())
lsxnus_kc$propn <- (lsxnus_kc$n/sum(lsxnus_kc$n))*100
lsxnus_kc$type <- "X-"

dat1 <- group_by(l.both.unsat.group, K, c)
lsbus_kc <- summarise(dat1, n = n())
lsbus_kc$propn <- (lsbus_kc$n/sum(lsbus_kc$n))*100
lsbus_kc$type <- "X+X-"

by_kc <- rbind(lsxnus_kc, lsxpus_kc, lsbus_kc)

g <- ggplot(by_kc, aes(x=log10(as.numeric(K)), y=as.numeric(c), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("c") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lunsat_kc.png", width = 8, height = 8)

dat1 <- group_by(l.xpos.unsat.group, K, r)
lsxpus_kr <- summarise(dat1, n = n())
lsxpus_kr$propn <- (lsxpus_kr$n/sum(lsxpus_kr$n))*100
lsxpus_kr$type <- "X+"

dat1 <- group_by(l.xneg.unsat.group, K, r)
lsxnus_kr <- summarise(dat1, n = n())
lsxnus_kr$propn <- (lsxnus_kr$n/sum(lsxnus_kr$n))*100
lsxnus_kr$type <- "X-"

dat1 <- group_by(l.both.unsat.group, K, r)
lsbus_kr <- summarise(dat1, n = n())
lsbus_kr$propn <- (lsbus_kr$n/sum(lsbus_kr$n))*100
lsbus_kr$type <- "X+X-"

by_kr <- rbind(lsxnus_kr, lsxpus_kr, lsbus_kr)

g <- ggplot(by_kr, aes(x=log10(as.numeric(K)), y=as.numeric(r), color=type, shape=type,
                       size=type)) + geom_point(alpha=0.3) + scale_shape_manual(values=c(1, 17, 3)) + 
  xlab("logK") + ylab("r") + scale_color_manual(values=c('#8B2323', '#104E8B', '#000000'))+
  scale_size_manual(values=c(2,2, 1))

ggsave("/Users/Xin/Desktop/projects/GTA_project/output/20161206/lunsat_kr.png", width = 8, height = 8)







