library(GillespieSSA)
library(ggplot2)
library(reshape2)
for (i in 1:50){
  inputdir <- "/isi/olga/xin/GTA_project/output/20160411/"
  inputfile <- "simulation_Xr_run"
  outputdir <- "/isi/olga/xin/GTA_project/output/20160412/"
  input <- paste(inputdir, inputfile, i, ".txt", sep = '')
  filename <- paste("Xr_run", i, ".jpeg", sep = '')
  dat <- read.delim2(input, sep = '\t', header = T)
  pop <- dat[, c(1, 3:6)]
  names(pop[, 1]) <- "Time"
  pop <- as.data.frame(pop)
  pop.melt <- melt(pop, id.var = "Generations")
  names(pop.melt) <- c("time", "genotype", "size")
  g <- ggplot(pop.melt, aes(x = time, y = size, color = genotype)) + 
    geom_point(size = 1) + ylab("population size") + 
    scale_fill_brewer(palette="Set2") +
    guides(colour = guide_legend(override.aes = list(size = 4))) 
  g
  ggsave(plot = g, file = filename, path = outputdir, device = "jpeg", 
    width = 13, height = 9)
}
