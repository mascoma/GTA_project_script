#!/usr/bin/env Rscript
### pass working directory, input filename, output filename to three arguments
library(ggplot2)
library(reshape2)
args = commandArgs(trailingOnly=TRUE)
if (length(args)!=3) {
  stop("Missing arguments", call.=FALSE)
}  

dir <- args[1]
input <- args[2]
output <- args[3]
inputfile <- paste(dir, input, sep = "")
dat <- read.delim2(inputfile, sep = '\t', header = T)
pop <- dat[1:length(dat[,1]), c(1, 3:4)]
names(pop[, 1]) <- "Time"
pop <- as.data.frame(pop)
pop.melt <- melt(pop, id.var = "Generations")
names(pop.melt) <- c("time", "genotype", "size")
g <- ggplot(pop.melt, aes(x = time, y = size, color = genotype)) + 
  geom_point(size = 1) + ylab("population size") + 
  scale_color_manual(values=c('#8B2323', '#104E8B'), labels=c("GTA+", "GTA-")) +
  guides(colour = guide_legend(override.aes = list(size = 4))) 
g
ggsave(plot = g, file = output, path = dir, device = "png", 
       width = 13, height = 9)