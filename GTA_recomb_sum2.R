library(GillespieSSA)
library(ggplot2)
library(reshape2)
inputfile <- "/isi/olga/xin/GTA_project/output/20160411/simulation_Xr_run1.txt"
outputdir <- "/isi/olga/xin/GTA_project/output/20160418/"
outputfile <- "Xr_run1.jpeg"
dat <- read.delim2(inputfile, sep = '\t', header = T)
pop <- dat[1:40000, c(1, 3:6)]
names(pop[, 1]) <- "Time"
pop <- as.data.frame(pop)
pop.melt <- melt(pop, id.var = "Generations")
names(pop.melt) <- c("time", "genotype", "size")
g <- ggplot(pop.melt, aes(x = time, y = size, color = genotype)) + 
    geom_point(size = 1) + ylab("population size") + 
    scale_fill_brewer(palette="Set2") +
    guides(colour = guide_legend(override.aes = list(size = 4))) 
g
ggsave(plot = g, file = outputfile, path = outputdir, device = "jpeg", 
         width = 13, height = 9)