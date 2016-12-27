library(ggplot2)
library(plyr)
library(scales)
library(rgl)
library(scatterplot3d)
library(fields) 
library(aqfig)
library(dplyr)
library(deSolve)

dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161026/"
file1 <- "GTAmodel_all.txt"
input1 <- paste(dir, file1, sep = "")

model.output <- read.delim2(input1, sep = "\t", stringsAsFactors = FALSE, 
                            header = F, na.strings = "")
names(model.output) <- c("Xpos","Xneg","p","curveshape", 
                         "stable", "r", "K", "c", "N")
model.output$saturation <- (as.numeric(model.output$Xpos) + 
                              as.numeric(model.output$Xneg))/as.numeric(model.output$K)
nonsolve <- subset(model.output, p == -1) # 722

ou <- subset(model.output, curveshape == "o" & stable == "u") # 232
ou.xpos <- subset(ou, as.numeric(ou$p) >=0.99) #0
ou.xneg <- subset(ou, as.numeric(ou$p) <=0.01) #229
ou.both <- subset(ou, as.numeric(p) > 0.01 & as.numeric(p) < 0.99) #3

os <-  subset(model.output, curveshape == "o" & stable == "s") # 1045940
os.xpos.unsat <- subset(os, as.numeric(os$p) >=0.99 & saturation < 0.9) #28605
os.xpos.sat <- subset(os, as.numeric(os$p) >=0.99 & saturation >= 0.9) #16405
os.xneg.unsat <- subset(os, as.numeric(os$p) <=0.01 &  saturation < 0.9) #33927
os.xneg.sat <- subset(os, as.numeric(os$p) <=0.01 & saturation >= 0.9) #496184
os.both.unsat <- subset(os, as.numeric(os$p) > 0.01 & as.numeric(os$p) < 0.99 & saturation < 0.9) #431034
os.both.sat <- subset(os, as.numeric(os$p) > 0.01 & as.numeric(os$p) < 0.99 & saturation >= 0.9) #39785

lu <- subset(model.output, curveshape == "l" & stable == "u") # 6017
lu.xpos.unsat <- subset(lu, as.numeric(lu$p) >=0.99 & saturation < 0.9) #67
lu.xpos.sat <- subset(lu, as.numeric(lu$p) >=0.99 & saturation >= 0.9) #838
lu.xneg.unsat <- subset(lu, as.numeric(lu$p) <=0.01 & saturation < 0.9) #287
lu.xneg.sat <- subset(lu, as.numeric(lu$p) <=0.01 & saturation >= 0.9) #2794
lu.both.unsat <- subset(lu, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation < 0.9) #498
lu.both.sat <- subset(lu, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation >= 0.9) #1533

ls <-  subset(model.output, curveshape == "l" & stable == "s") # 5197089
ls.xpos.unsat <- subset(ls, as.numeric(ls$p) >=0.99 & saturation < 0.90) #126578
ls.xpos.sat <- subset(ls, as.numeric(ls$p) >=0.99 & saturation >= 0.90) #322142
ls.xneg.unsat <- subset(ls, as.numeric(ls$p) <=0.01 & saturation < 0.90) #15503
ls.xneg.sat <- subset(ls, as.numeric(ls$p) <=0.01 & saturation >= 0.90) #4324475
ls.both.unsat <- subset(ls, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation < 0.90) #177519
ls.both.sat <- subset(ls, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation >= 0.90) #230872

ou.group <- rbind(ou, lu.xneg.unsat, lu.both.unsat) #1017
os.xneg.unsat.group <- rbind(os.xneg.unsat,  ls.xneg.unsat) #49430
os.both.unsat.group <- os.both.unsat  #431034

group <- function(dat, var1, var2, type){
  dat1 <- group_by(dat, var1, var2)
  dat2 <- summarise(dat1, n = n())
  dat2$propn <- (dat2$n/sum(dat2$n))*100
  dat2$type <- type
}


ls.xpos.sat.group <- rbind(os.xpos.sat, ls.xpos.sat)  #338547
ls.xneg.sat.group <- rbind(os.xneg.sat, ls.xneg.sat) #4820659
ls.both.sat.group <- rbind(os.both.sat, ls.both.sat) #270657

lu.xpos.sat.group <- lu.xpos.sat #838
lu.xneg.sat.group <- lu.xneg.sat #2794
lu.both.sat.group <- lu.both.sat #1533

ls.xpos.unsat.group <- rbind(os.xpos.unsat, ls.xpos.unsat) #155183
ls.both.unsat.group <- ls.both.unsat #177519

lu.xpos.unsat.group <- lu.xpos.unsat #67

o.group <- rbind(ou.group, os.xneg.unsat.group, os.both.unsat.group)

l.group <- rbind(ls.xpos.sat.group, ls.xneg.sat.group, ls.both.sat.group, 
                 lu.xpos.sat.group, lu.xneg.sat.group, lu.both.sat.group,
                 ls.xpos.unsat.group, ls.both.unsat.group, lu.xpos.unsat.group)

o.heat <- cbind(as.numeric(o.group$c), as.numeric(o.group$r), 
                as.numeric(o.group$K)/1e9, as.numeric(o.group$N)/1e-06)
colnames(o.heat) <- c("c", "r", "K", "N")

png("/Users/Xin/Desktop/projects/GTA_project/output/20161128/o_heat.png")
nba_heatmap <- heatmap(o.heat, Rowv=NA, Colv=NA, 
                       col = cm.colors(256), scale = "none")
dev.off()

l.heat <- cbind(as.numeric(l.group$c), as.numeric(l.group$r), 
                as.numeric(l.group$K)/1e9, as.numeric(l.group$N)/1e-06)
colnames(l.heat) <- c("c", "r", "K", "N")

png("/Users/Xin/Desktop/projects/GTA_project/output/20161128/l_heat.png")
l_heatmap <- heatmap(l.heat, Rowv=NA, Colv=NA, 
                       col = cm.colors(256), scale = "none")
dev.off()

nba_heatmap <- heatmap(o.heat[1:10,], Rowv=NA, Colv=NA, 
                       col = cm.colors(1000), scale = "none")


by_K_N_r_o <- group_by(o.group, K, N, r)
by_Knr_o <- summarise(by_K_N_r_o, n = n())
by_Knr_o$type <- "o"

by_K_N_r_l <- group_by(l.group, K, N, r)
by_Knr_l <- summarise(by_K_N_r_l, n = n())
by_Knr_l$type <- "l"
by_Knr <- rbind(by_Knr_o, by_Knr_l)

mcol = by_Knr$type
i = 1
while (i <= length(mcol)){
  if (mcol[i] == "l"){
    mcol[i] = "red"
  }else {
    mcol[i] = "blue"
  }
  i = i +1
}

plot3d(log10(as.numeric(by_Knr$K)), log10(as.numeric(by_Knr$N)), as.numeric(by_Knr$r), 
       col=mcol, xlab="logK", ylab="logN", zlab="r")


by_K_N_c_o <- group_by(o.group, K, N, c)
by_Knc_o <- summarise(by_K_N_c_o, n = n())
by_Knc_o$type <- "o"

by_K_N_c_l <- group_by(l.group, K, N, c)
by_Knc_l <- summarise(by_K_N_c_l, n = n())
by_Knc_l$type <- "l"
by_Knc <- rbind(by_Knc_o, by_Knc_l)

mcol = by_Knc$type
i = 1
while (i <= length(mcol)){
  if (mcol[i] == "l"){
    mcol[i] = "red"
  }else {
    mcol[i] = "blue"
  }
  i = i +1
}

plot3d(log10(as.numeric(by_Knc$K)), log10(as.numeric(by_Knc$N)), as.numeric(by_Knc$c), 
       col=mcol, xlab="logK", ylab="logN", zlab="c")


by_K_r_c_o <- group_by(o.group, K, r, c)
by_Krc_o <- summarise(by_K_r_c_o, n = n())
by_Krc_o$type <- "o"

by_K_r_c_l <- group_by(l.group, K, r, c)
by_Krc_l <- summarise(by_K_r_c_l, n = n())
by_Krc_l$type <- "l"
by_Krc <- rbind(by_Krc_o, by_Krc_l)

mcol = by_Krc$type
i = 1
while (i <= length(mcol)){
  if (mcol[i] == "l"){
    mcol[i] = "red"
  }else {
    mcol[i] = "blue"
  }
  i = i +1
}

plot3d(log10(as.numeric(by_Krc$K)), as.numeric(by_Krc$r), as.numeric(by_Krc$c), 
       col=mcol, xlab="logK", ylab="r", zlab="c")

by_N_r_c_o <- group_by(o.group, N, r, c)
by_Nrc_o <- summarise(by_N_r_c_o, n = n())
by_Nrc_o$type <- "o"

by_N_r_c_l <- group_by(l.group, N, r, c)
by_Nrc_l <- summarise(by_N_r_c_l, n = n())
by_Nrc_l$type <- "l"
by_Nrc <- rbind(by_Nrc_o, by_Nrc_l)

mcol = by_Nrc$type
i = 1
while (i <= length(mcol)){
  if (mcol[i] == "l"){
    mcol[i] = "red"
  }else {
    mcol[i] = "blue"
  }
  i = i +1
}

plot3d(log10(as.numeric(by_Nrc$N)), as.numeric(by_Nrc$r), as.numeric(by_Nrc$c), 
       col=mcol, xlab="logN", ylab="r", zlab="c")

dir.create("animation")
for (i in 1:90) {
  view3d(userMatrix=rotationMatrix(2*pi * i/90, 1, -1, -1))
  rgl.snapshot(filename=paste("animation/frame-",
                              sprintf("%03d", i), ".png", sep=""))
}



plot(table(as.numeric(l.group$c)),main = "c dist", ylab = "count")
lines(table(as.numeric(o.group$c)), col = "red")

plot(table(round(as.numeric(l.group$r), 3)),main = "r dist", ylab = "count")
lines(table(round(as.numeric(o.group$r), 3)), col = "red")

plot(table(round(as.numeric(l.group$K), 1)),main = "K dist", ylab = "count")
lines(table(round(as.numeric(o.group$K), 1)), col = "red")

plot(table(as.numeric(l.group$N)),main = "N dist", ylab = "count")
lines(table(as.numeric(o.group$N)), col = "red")

by_K_N_o <- group_by(o.group, K, N)
by_K_o <- summarise(by_K_N_o, n = n())
by_K_o$propn <- (by_K_o$n/sum(by_K_o$n))*100
by_K_o$type <- "o"

by_K_N_l <- group_by(l.group, K, N)
by_K_l <- summarise(by_K_N_l, n = n())
by_K_l$propn <- (by_K_l$n/sum(by_K_l$n))*100
by_K_l$type <- "l"
by_K <- rbind(by_K_l, by_K_o)

mcol = by_K$type
i = 1
while (i <= length(mcol)){
  if (mcol[i] == "l"){
    mcol[i] = "red"
  }else {
    mcol[i] = "blue"
  }
  i = i +1
}
scatterplot3d(log10(as.numeric(by_K$K)), log10(as.numeric(by_K$N)), by_K$n, 
              type = "h")

plot3d(log10(as.numeric(by_K$K)), log10(as.numeric(by_K$N)), by_K$propn, 
       col=mcol, size = 3)

ggplot(by_K, aes(x=log10(as.numeric(K)), y=log10(as.numeric(N)), color = type, 
                 shape=type)) + geom_point() + scale_shape_manual(values=c(4, 1))


by_c_r_o <- group_by(o.group, c, r)
by_c_o <- summarise(by_c_r_o, n = n())
by_c_o$propn <- (by_c_o$n/sum(by_c_o$n))*100
by_c_o$type <- "o"

by_c_r_l <- group_by(l.group, c, r)
by_c_l <- summarise(by_c_r_l, n = n())
by_c_l$propn <- (by_c_l$n/sum(by_c_l$n))*100
by_c_l$type <- "l"
by_c <- rbind(by_c_l, by_c_o)

mcol = by_c$type
i = 1
while (i <= length(mcol)){
  if (mcol[i] == "l"){
    mcol[i] = "red"
  }else {
    mcol[i] = "blue"
  }
  i = i +1
}

plot3d(as.numeric(by_c$c), as.numeric(by_c$r), by_c$propn, col=mcol, size = 3)

ggplot(by_c, aes(x=as.numeric(c), y=as.numeric(r), color = type, 
                 shape=type)) + geom_point() + scale_shape_manual(values=c(4, 1))

by_c_K_o <- group_by(o.group, c, K)
by_cK_o <- summarise(by_c_K_o, n = n())
by_cK_o$propn <- (by_cK_o$n/sum(by_cK_o$n))*100
by_cK_o$type <- "o"

by_c_K_l <- group_by(l.group, c, K)
by_cK_l <- summarise(by_c_K_l, n = n())
by_cK_l$propn <- (by_cK_l$n/sum(by_cK_l$n))*100
by_cK_l$type <- "l"
by_cK <- rbind(by_cK_l, by_cK_o)

ggplot(by_cK, aes(x=as.numeric(c), y=log10(as.numeric(K)), color = type, 
                 shape=type)) + geom_point() + scale_shape_manual(values=c(4, 1))

by_c_N_o <- group_by(o.group, c, N)
by_cN_o <- summarise(by_c_N_o, n = n())
by_cN_o$propn <- (by_cN_o$n/sum(by_cN_o$n))*100
by_cN_o$type <- "o"

by_c_N_l <- group_by(l.group, c, N)
by_cN_l <- summarise(by_c_N_l, n = n())
by_cN_l$propn <- (by_cN_l$n/sum(by_cN_l$n))*100
by_cN_l$type <- "l"
by_cN <- rbind(by_cN_l, by_cN_o)

ggplot(by_cN, aes(x=as.numeric(c), y=log10(as.numeric(N)), color = type, 
                  shape=type)) + geom_point() + scale_shape_manual(values=c(4, 1))

by_r_N_o <- group_by(o.group, r, N)
by_rN_o <- summarise(by_r_N_o, n = n())
by_rN_o$propn <- (by_rN_o$n/sum(by_rN_o$n))*100
by_rN_o$type <- "o"

by_r_N_l <- group_by(l.group, r, N)
by_rN_l <- summarise(by_r_N_l, n = n())
by_rN_l$propn <- (by_rN_l$n/sum(by_rN_l$n))*100
by_rN_l$type <- "l"
by_rN <- rbind(by_rN_l, by_rN_o)

ggplot(by_rN, aes(x=as.numeric(r), y=log10(as.numeric(N)), color = type, 
                  shape=type)) + geom_point() + scale_shape_manual(values=c(4, 1))

by_r_K_o <- group_by(o.group, r, K)
by_rK_o <- summarise(by_r_K_o, n = n())
by_rK_o$propn <- (by_rK_o$n/sum(by_rK_o$n))*100
by_rK_o$type <- "o"

by_r_K_l <- group_by(l.group, r, K)
by_rK_l <- summarise(by_r_K_l, n = n())
by_rK_l$propn <- (by_rK_l$n/sum(by_rK_l$n))*100
by_rK_l$type <- "l"
by_rK <- rbind(by_rK_l, by_rK_o)

ggplot(by_rK, aes(x=as.numeric(r), y=log10(as.numeric(K)), color = type, 
                  shape=type)) + geom_point() + scale_shape_manual(values=c(4, 1))

plot.3dcolor <- function(outputdir, outputfilename, x, y, z, x.lab, y.lab){
  output <- paste(outputdir, outputfilename, sep = "")
  png(output)
  col.source <- kristen.colors(n=50)
  plot3d.points(x, y, z, col=col.source, xlab = x.lab, ylab = y.lab)
  vertical.image.legend(col=col.source, zlim=c(min(z), max(z)))
  dev.off()
}
outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161128"
plot.3dcolor(outputdir, "lupossat.png", as.numeric(lu.xpos.sat.group$c), 
             as.numeric(lu.xpos.sat.group$r), log10(as.numeric(lu.xpos.sat.group$K)), 
             "c", "r")



