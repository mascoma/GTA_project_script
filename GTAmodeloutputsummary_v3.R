library(ggplot2)
library(plyr)
library(scales)
library(rgl)
library(scatterplot3d)
library(fields) 
library(aqfig)
library(dplyr)

dir1 <- "/Users/Xin/Desktop/projects/GTA_project/output/20160722/"
dir2 <- "/Users/Xin/Desktop/projects/GTA_project/output/20160820/"
dir3 <- "/Users/Xin/Desktop/projects/GTA_project/output/20160821/"
file1 <- "GTAmodels_para50.txt"
file2 <- "GTArecheckmodels.txt"
file3 <- "GTArecheckmodels_1100.txt" 
file4 <- "GTAmodel_na.txt"
input1 <- paste(dir1, file1, sep = "")
input2 <- paste(dir2, file2, sep = "")
input3 <- paste(dir3, file3, sep = "")

output1 <- paste(dir3, file4, sep = "")
model.output <- read.delim2(input1, sep = "\t", stringsAsFactors = FALSE, 
                           header = F, na.strings = "")
names(model.output) <- c("Xpos","Xneg","p","curveshape", 
                         "stable", "r", "K", "c", "N")
model.output$saturation <- (as.numeric(model.output$Xpos) + 
                              as.numeric(model.output$Xneg))/as.numeric(model.output$K)
nonsolve <- subset(model.output, p == "NA") # 589874
model.output.rmna <- subset(model.output, p != "NA") # 5535126
recheck <- subset(model.output.rmna, saturation < 0.95 & 
                    curveshape == "l" & stable == "s") #611873
rest <- setdiff(model.output.rmna, recheck) # 4923253

rechecked.output <- read.delim2(input2, sep = "\t", stringsAsFactors = FALSE, 
                                header = F, na.strings = "")
names(rechecked.output) <- c("Xpos","Xneg","p","curveshape", 
                         "stable", "r", "K", "c", "N")
rechecked.output$saturation <- (as.numeric(rechecked.output$Xpos) + 
                              as.numeric(rechecked.output$Xneg))/as.numeric(rechecked.output$K)
rechecked.nodup <- rechecked.output[!duplicated(rechecked.output), ] #610773

rechecked_1100.output <- read.delim2(input3, sep = "\t", stringsAsFactors = FALSE, 
                                header = F, na.strings = "")
names(rechecked_1100.output) <- c("Xpos","Xneg","p","curveshape", 
                             "stable", "r", "K", "c", "N")
rechecked_1100.output$saturation <- (as.numeric(rechecked_1100.output$Xpos) + 
                                  as.numeric(rechecked_1100.output$Xneg))/as.numeric(rechecked_1100.output$K)
rechecked.all <- rbind(rechecked.nodup, rechecked_1100.output)
rechecked.na <- subset(rechecked.all, p == "NA")
rechecked.nona <- subset(rechecked.all, p != "NA")
na.all <- rbind(nonsolve, rechecked.na) # 597153
rest.all <- rbind(rest, rechecked.nona) # 5527847 
#write.table(na.all, file = output1, sep = "\t", quote = F)



# check the duplicates in rechecked group1 
#recheck1 <- rechecked.output[, 6:9] #611886
#recheck2 <- recheck[, 6:9] #611873
#rechecked1.diff <- setdiff(recheck1, recheck2) # in recheck1 but not recheck2 0
#rechecked2.diff <- setdiff(recheck2, recheck1) # in recheck2 but not recheck1 1100
#rechecked.ins <- intersect(recheck1, recheck2) # in both sets 610773
#rechecked.dup <- rechecked.output[duplicated(rechecked.output), ] #1113

# check the rechecked2.diff group
#write.table(rechecked2.diff, file = outputrecheck, quote = F, sep='\t')

 