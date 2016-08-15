library(ggplot2)
library(plyr)
library(scales)
library(rgl)
library(scatterplot3d)
library(fields) 
library(aqfig)
dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20160722/"
file <- "GTAmodels_para50.txt"
input1 <- paste(dir, file, sep = "")
outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20160804/"
model.output <- read.delim(input1, sep = "\t", stringsAsFactors = FALSE, 
                           header = T, na.strings = "")
names(model.output) <- c("Xpos","Xneg","p","curveshape", 
                         "stable", "r", "K", "c", "N")
os <- subset(model.output, curveshape == "o" & stable == "s") # 1244248 records
ou <- subset(model.output, curveshape == "o" & stable == "u") # 1180
ls <- subset(model.output, curveshape == "l" & stable == "s") # 4284663
lu <- subset(model.output, curveshape == "l" & stable == "u") # 4875

nonsolve <- subset(model.output, p == "NA" & c != "NA") # 590034

lu.xpos <- subset(lu, p!="NA" & as.numeric(p) >=0.99)  #861
lu.xneg <- subset(lu, p!="NA" & as.numeric(p) <=0.01)  #2586
lu.both <- subset(lu, p!="NA" & as.numeric(p) > 0.01 & as.numeric(p) < 0.99)  # 1428

os.xpos <- subset(os, p!="NA" & as.numeric(p) >=0.99)  #26321
os.xneg <- subset(os, p!="NA" & as.numeric(p) <=0.01)  #797228
os.both <- subset(os, p!="NA" & as.numeric(p) > 0.01 & as.numeric(p) < 0.99)  # 420699

ls.xpos <- subset(ls, p!="NA" & as.numeric(p) >=0.99)   ##401314
ls.xneg <- subset(ls, p!="NA" & as.numeric(p) <=0.01)  ##3537862
ls.both <- subset(ls, p!="NA" & as.numeric(p) > 0.01 & as.numeric(p) < 0.99)  ## 345487

Xpos.dom <- subset(model.output, p!="NA" & as.numeric(p) >=0.99 & stable == "s") # 427635
Xneg.dom <- subset(model.output, p!="NA" & as.numeric(p) <=0.01 & stable == "s") # 9 
Xpos.Xneg <- subset(model.output, p!="NA" & as.numeric(p) > 0.01 
                    &  as.numeric(p) < 0.99 & stable == "s") # 766186

## plot the results using 3d plot 
## plot x = c, y = r, z = N or logN, color = K 
## plot ou, lu, os.xpos, os.xneg, os.both, ls.xpos, ls.xneg, ls.both

plot.3dcolor <- function(outputdir, outputfilename, x, y, z, x.lab, y.lab){
  output <- paste(outputdir, outputfilename, sep = "")
  png(output)
  col.source <- kristen.colors(n=50)
  plot3d.points(x, y, z, col=col.source, xlab = x.lab, ylab = y.lab)
  vertical.image.legend(col=col.source, zlim=c(min(z), max(z)))
  dev.off()
}
## ou system
plot.3dcolor(outputdir, "ou.png", as.numeric(ou$c), as.numeric(ou$r), 
             log10(as.numeric(ou$K)), "c", "r")

## lu system

plot.3dcolor(outputdir, "lu_rn.png", as.numeric(lu$r), log10(as.numeric(lu$N)), 
             log10(as.numeric(lu$K)), "r", "logN")
plot.3dcolor(outputdir, "lu_rc.png", as.numeric(lu$r), as.numeric(lu$c), 
             log10(as.numeric(lu$K)), "r", "c")
plot.3dcolor(outputdir, "lu_cn.png", as.numeric(lu$c), log10(as.numeric(lu$N)), 
             log10(as.numeric(lu$K)), "c", "logN")

plot.3dcolor(outputdir, "lu.xpos_rn.png", as.numeric(lu.xpos$r), 
             log10(as.numeric(lu.xpos$N)), log10(as.numeric(lu.xpos$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "lu.xpos_rc.png", as.numeric(lu.xpos$r), 
             as.numeric(lu.xpos$c), log10(as.numeric(lu.xpos$K)), 
             "r", "c")
plot.3dcolor(outputdir, "lu.xpos_cn.png", as.numeric(lu.xpos$c), 
             log10(as.numeric(lu.xpos$N)), log10(as.numeric(lu.xpos$K)), 
             "c", "logN")

plot.3dcolor(outputdir, "lu.xneg_rn.png", as.numeric(lu.xneg$r), 
             log10(as.numeric(lu.xneg$N)), log10(as.numeric(lu.xneg$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "lu.xneg_rc.png", as.numeric(lu.xneg$r), 
             as.numeric(lu.xneg$c), log10(as.numeric(lu.xneg$K)), 
             "r", "c")
plot.3dcolor(outputdir, "lu.xneg_cn.png", as.numeric(lu.xneg$c), 
             log10(as.numeric(lu.xneg$N)), log10(as.numeric(lu.xneg$K)), 
             "c", "logN")

plot.3dcolor(outputdir, "lu.both_rn.png", as.numeric(lu.both$r), 
             log10(as.numeric(lu.both$N)), log10(as.numeric(lu.both$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "lu.both_rc.png", as.numeric(lu.both$r), 
             as.numeric(lu.both$c), log10(as.numeric(lu.both$K)), 
             "r", "c")
plot.3dcolor(outputdir, "lu.both_cn.png", as.numeric(lu.both$c), 
             log10(as.numeric(lu.both$N)), log10(as.numeric(lu.both$K)), 
             "c", "logN")

## os system
plot.3dcolor(outputdir, "os.xpos_rn.png", as.numeric(os.xpos$r), 
             log10(as.numeric(os.xpos$N)), log10(as.numeric(os.xpos$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "os.xpos_rc.png", as.numeric(os.xpos$r), 
             as.numeric(os.xpos$c), log10(as.numeric(os.xpos$K)), 
             "r", "c")
plot.3dcolor(outputdir, "os.xpos_cn.png", as.numeric(os.xpos$c), 
             log10(as.numeric(os.xpos$N)), log10(as.numeric(os.xpos$K)), 
             "c", "logN")

plot.3dcolor(outputdir, "os.xneg_rn.png", as.numeric(os.xneg$r), 
             log10(as.numeric(os.xneg$N)), log10(as.numeric(os.xneg$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "os.xneg_rc.png", as.numeric(os.xneg$r), 
             as.numeric(os.xneg$c), log10(as.numeric(os.xneg$K)), 
             "r", "c")
plot.3dcolor(outputdir, "os.xneg_cn.png", as.numeric(os.xneg$c), 
             log10(as.numeric(os.xneg$N)), log10(as.numeric(os.xneg$K)), 
             "c", "logN")

plot.3dcolor(outputdir, "os.both_rn.png", as.numeric(os.both$r), 
             log10(as.numeric(os.both$N)), log10(as.numeric(os.both$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "os.both_rc.png", as.numeric(os.both$r), 
             as.numeric(os.both$c), log10(as.numeric(os.both$K)), 
             "r", "c")
plot.3dcolor(outputdir, "os.both_cn.png", as.numeric(os.both$c), 
             log10(as.numeric(os.both$N)), log10(as.numeric(os.both$K)), 
             "c", "logN")

## ls system

plot.3dcolor(outputdir, "ls.xpos_rn.png", as.numeric(ls.xpos$r), 
             log10(as.numeric(ls.xpos$N)), log10(as.numeric(ls.xpos$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "ls.xpos_rc.png", as.numeric(ls.xpos$r), 
             as.numeric(ls.xpos$c), log10(as.numeric(ls.xpos$K)), 
             "r", "c")
plot.3dcolor(outputdir, "ls.xpos_cn.png", as.numeric(ls.xpos$c), 
             log10(as.numeric(ls.xpos$N)), log10(as.numeric(ls.xpos$K)), 
             "c", "logN")

plot.3dcolor(outputdir, "ls.xneg_rn.png", as.numeric(ls.xneg$r), 
             log10(as.numeric(ls.xneg$N)), log10(as.numeric(ls.xneg$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "ls.xneg_rc.png", as.numeric(ls.xneg$r), 
             as.numeric(ls.xneg$c), log10(as.numeric(ls.xneg$K)), 
             "r", "c")
plot.3dcolor(outputdir, "ls.xneg_cn.png", as.numeric(ls.xneg$c), 
             log10(as.numeric(ls.xneg$N)), log10(as.numeric(ls.xneg$K)), 
             "c", "logN")

plot.3dcolor(outputdir, "ls.both_rn.png", as.numeric(ls.both$r), 
             log10(as.numeric(ls.both$N)), log10(as.numeric(ls.both$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "ls.both_rc.png", as.numeric(ls.both$r), 
             as.numeric(ls.both$c), log10(as.numeric(ls.both$K)), 
             "r", "c")
plot.3dcolor(outputdir, "ls.both_cn.png", as.numeric(ls.both$c), 
             log10(as.numeric(ls.both$N)), log10(as.numeric(ls.both$K)), 
             "c", "logN")


## na system
plot.3dcolor(outputdir, "nonsolve_rn.png", as.numeric(nonsolve$r), 
             log10(as.numeric(nonsolve$N)), log10(as.numeric(nonsolve$K)), 
             "r", "logN")
plot.3dcolor(outputdir, "nonsolve_rc.png", as.numeric(nonsolve$r), 
             as.numeric(nonsolve$c), log10(as.numeric(nonsolve$K)), 
             "r", "c")
plot.3dcolor(outputdir, "nonsolve_cn.png", as.numeric(nonsolve$c), 
             log10(as.numeric(nonsolve$N)), log10(as.numeric(nonsolve$K)), 
             "c", "logN")


