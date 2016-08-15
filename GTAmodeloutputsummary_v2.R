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
outputrecheck <- "/Users/Xin/Desktop/projects/GTA_project/output/20160807/modelrecheck_group.txt"
model.output <- read.delim(input1, sep = "\t", stringsAsFactors = FALSE, 
                           header = T, na.strings = "")

names(model.output) <- c("Xpos","Xneg","p","curveshape", 
                         "stable", "r", "K", "c", "N")
model.output$saturation <- (as.numeric(model.output$Xpos) + 
                             as.numeric(model.output$Xneg))/as.numeric(model.output$K)

## split the output to NA group  and non-NA group
nonsolve <- subset(model.output, p == "NA") # 590034
model.output.rmna <- subset(model.output, p != "NA")
plot(density(model.output.rmna$saturation, bw = 0.01))
recheck <- subset(model.output.rmna, saturation < 0.95 & curveshape == "l" & stable == "s")
write.table(recheck, file = outputrecheck, quote = F, sep='\t')
model.o <- subset(model.output, curveshape == "o") # 1244248 records
model.l <- subset(model.output, curveshape == "l") # 1180
model.os <- subset(model.o, stable == "s") # 4284663
model.ou <- subset(model.o, stable == "u") # 4875
model.ls <- subset(model.l, stable == "s") # 4284663
model.lu <- subset(model.l, stable == "u") # 4875

model.os.Xpos <- subset(model.os, as.numeric(p)>=0.99) # 427635
model.os.Xpos.sat <- subset(model.os.Xpos, saturation>=0.9) # 427635
model.os.Xpos.unsat <- subset(model.os.Xpos, saturation<0.9)
model.os.Xneg <- subset(model.os, as.numeric(p)<=0.01) # 9 
model.os.Xneg.sat <- subset(model.os.Xneg, saturation>=0.9) # 427635
model.os.Xneg.unsat <- subset(model.os.Xneg, saturation<0.9)
model.os.both <- subset(model.os, as.numeric(p)>0.01 & as.numeric(p)<0.99)
model.os.Xneg.sat <- subset(model.os.Xneg, saturation>=0.9) # 427635
model.os.Xneg.unsat <- subset(model.os.Xneg, saturation<0.9)

                    
model.ou.Xpos <- subset(model.ou, as.numeric(p)>=0.99) # 427635
model.ou.Xneg <- subset(model.ou, as.numeric(p)<=0.01) # 9 
model.ou.both <- subset(model.ou, as.numeric(p)>0.01 & as.numeric(p)<0.99)


