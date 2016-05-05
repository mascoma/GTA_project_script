library(GillespieSSA)
library(ggplot2)
library(reshape2)
 
inputdir <- "/isi/olga/xin/GTA_project/output/20160407/selfishmodel_midc.RData" 
load(inputdir)  
pop2 <- matrix(NA, nrow = length(mu), ncol = 3)

for(i in 1: length(output)) {
  pop <- output[[i]]$data[, 1:3]
  colnames(pop) <- c("time", "X1", "X2")
  pop2[i, ] <- pop[length(pop[, 3]), ]
}
pop_mu <- cbind(mu, pop2[ , 2:3])
colnames(pop_mu) <- c("mu", "X+", "X-")
pop_mu <- as.data.frame(pop_mu)
n <- length(pop_mu[, 1])
for (j in 1: length(pop_mu[, 1])){
  if ((pop_mu[j, 2] > pop_mu[j, 3]) && (n > j)) {  
    n <- j
  }
}

mpop_mu <- melt(pop_mu, id = "mu")
colnames(mpop_mu) <- c("mu", "subpopulation", "size")

g <- ggplot(mpop_mu, aes(x = mu, y = size, color = subpopulation)) + 
  geom_point(size = 1) + ylab("population size") +
  ggtitle(expression(atop(bold("Dynamic of X+ and X-"), 
                          atop(italic("b = 100, c = 0.01, ro = 0.0001"), 
                               "")))) + 
  theme(plot.title = element_text(size = 18, face = "bold", colour = "black", 
                                  vjust = -1)) +
  geom_vline(xintercept = mu[n]) + 
  theme(legend.title = element_text(size  =14)) + 
  guides(colour = guide_legend(override.aes = list(size = 4))) +
  geom_text(aes(mu[n], 1.1*10^9 , label = mu[n], hjust = -0.2), color = "black", 
            family = "Courier")

g
ggsave(plot = g, file = "/isi/olga/xin/GTA_project/output/20160407/selfishmodel_midc.jpeg", 
       device = "jpeg")
 


