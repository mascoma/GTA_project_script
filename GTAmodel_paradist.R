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

nonsolve <- subset(model.output, p == "NA" & c != "NA") # 589834

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


### plot the distribution of each parameter
output <- paste(outputdir, "parameter_dist.png", sep = "")
png(output, width = 600, height = 1200)
par(mfrow=c(10, 4), mar = c(2,2,2,2))
plot(density(as.numeric(model.output$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (all)", xlim = c(0, 1))
plot(density(as.numeric(model.output$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (all)", xlim = c(0, 1))
plot(density(log10(as.numeric(model.output$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (all)", xlim = c(-10, -7))
plot(density(log10(as.numeric(model.output$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (all)", xlim = c(5, 9))

plot(density(as.numeric(ou$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (ou)", xlim = c(0, 1))
plot(density(as.numeric(ou$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (ou)", xlim = c(0, 1))
plot(density(log10(as.numeric(ou$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (ou)", xlim = c(-10, -7))
plot(density(log10(as.numeric(ou$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (ou)", xlim = c(5, 9))

plot(density(as.numeric(lu$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (lu)", xlim = c(0, 1))
plot(density(as.numeric(lu$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (lu)", xlim = c(0, 1))
plot(density(log10(as.numeric(lu$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (lu)", xlim = c(-10, -7))
plot(density(log10(as.numeric(lu$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (lu)", xlim = c(5, 9))

plot(density(as.numeric(os.xpos$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (os.xpos)", xlim = c(0, 1))
plot(density(as.numeric(os.xpos$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (os.xpos)", xlim = c(0, 1))
plot(density(log10(as.numeric(os.xpos$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (os.xpos)", xlim = c(-10, -7))
plot(density(log10(as.numeric(os.xpos$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (os.xpos)", xlim = c(5, 9))

plot(density(as.numeric(os.xneg$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (os.xneg)", xlim = c(0, 1))
plot(density(as.numeric(os.xneg$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (os.xneg)", xlim = c(0, 1))
plot(density(log10(as.numeric(os.xneg$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (os.xneg)", xlim = c(-10, -7))
plot(density(log10(as.numeric(os.xneg$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (os.xneg)", xlim = c(5, 9))

plot(density(as.numeric(os.both$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (os.both)", xlim = c(0, 1))
plot(density(as.numeric(os.both$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (os.both)", xlim = c(0, 1))
plot(density(log10(as.numeric(os.both$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (os.both)", xlim = c(-10, -7))
plot(density(log10(as.numeric(os.both$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (os.both)", xlim = c(5, 9))


plot(density(as.numeric(ls.xpos$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (ls.xpos)", xlim = c(0, 1))
plot(density(as.numeric(ls.xpos$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (ls.xpos)", xlim = c(0, 1))
plot(density(log10(as.numeric(ls.xpos$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (ls.xpos)", xlim = c(-10, -7))
plot(density(log10(as.numeric(ls.xpos$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (ls.xpos)", xlim = c(5, 9))

plot(density(as.numeric(ls.xneg$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (ls.xneg)", xlim = c(0, 1))
plot(density(as.numeric(ls.xneg$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (ls.xneg)", xlim = c(0, 1))
plot(density(log10(as.numeric(ls.xneg$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (ls.xneg)", xlim = c(-10, -7))
plot(density(log10(as.numeric(ls.xneg$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (ls.xneg)", xlim = c(5, 9))

plot(density(as.numeric(ls.both$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (ls.both)", xlim = c(0, 1))
plot(density(as.numeric(ls.both$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (ls.both)", xlim = c(0, 1))
plot(density(log10(as.numeric(ls.both$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (ls.both)", xlim = c(-10, -7))
plot(density(log10(as.numeric(ls.both$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (ls.both)", xlim = c(5, 9))

plot(density(as.numeric(nonsolve$c), na.rm=T, bw = 1e-4), 
     main = "c distribution (nonsolve)", xlim = c(0, 1))
plot(density(as.numeric(nonsolve$r), na.rm=T, bw = 1e-4), 
     main = "r distribution (nonsolve)", xlim = c(0, 1))
plot(density(log10(as.numeric(nonsolve$N)), na.rm=T, bw = 0.01), 
     main = "logN distribution (nonsolve)", xlim = c(-10, -7))
plot(density(log10(as.numeric(nonsolve$K)), na.rm=T, bw = 0.01), 
     main = "logK distribution (nonsolve)", xlim = c(5, 9))
dev.off()

