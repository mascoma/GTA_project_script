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

model.output <- read.delim(input1, sep = "\t", stringsAsFactors = FALSE, 
                           header = T, na.strings = "")

names(model.output) <- c("Xpos","Xneg","p","curveshape", 
                         "stable", "r", "K", "c", "N")
model.output$satuation <- (as.numeric(model.output$Xpos) + 
                             as.numeric(model.output$Xneg))/as.numeric(model.output$K)

model.output.rmna <- subset(model.output, p != "NA")
plot(density(model.output.rmna$satuation, bw = 0.01))
sat <- subset(model.output, satuation >= 0.9)

os <- subset(model.output, curveshape == "o" & stable == "s") # 1244248 records
ou <- subset(model.output, curveshape == "o" & stable == "u") # 1180
ls <- subset(model.output, curveshape == "l" & stable == "s") # 4284663
lu <- subset(model.output, curveshape == "l" & stable == "u") # 4875

nonsolve <- subset(model.output, p == "NA") # 590034

Xpos.dom <- subset(model.output, p!="NA" & as.numeric(p) >=0.99 & stable == "s") # 427635
Xneg.dom <- subset(model.output, p!="NA" & as.numeric(p) <=0.01 & stable == "s") # 9 
Xpos.Xneg <- subset(model.output, p!="NA" & as.numeric(p) > 0.01 
                    &  as.numeric(p) < 0.99 & stable == "s") # 766186



## plot the results using 3d plot 
## plot x = c, y = r, z = N or logN, color = K 
## plot ou, lu, os.xpos, os.xneg, os.both, ls.xpos, ls.xneg, ls.both
col_generator <- function(array) {
  K.array <- c(1e5, 1e6, 1e7, 2e7, 5e7, 8e7, seq(1e8, 1e9, length.out = 44))
  K.array <- as.character(K.array)
  index <- match(array, K.array)
  col.source <- kristen.colors(n=50)
  col.array <- col.source[index]
  return(col.array)
}

plot_parameters <- function(input, output, title) {
  col.array <- col_generator(input$K)
  
  png(output, width = 900, height = 800)
  scatterplot3d(as.numeric(input$c), as.numeric(input$r), 
                log10(as.numeric(input$N)), color = col.array, pch = 16, 
                xlab = "c", ylab = "r", zlab = "logN", 
                main = title, mar=c(5, 3, 5, 7)+0.1)
  par(mar=c(5, 4, 4, 2) + 0.1)
  K.array <- c(1e5, 1e6, 1e7, 2e7, 5e7, 8e7, seq(1e8, 1e9, length.out = 44))
  col.source <- kristen.colors(n=50)
  image.plot(legend.only=TRUE, 
             zlim= c(min(log10(K.array)), max(log10(K.array))),
             nlevel=length(K.array),
             col=col.source)
  dev.off()
}

###### plot the entire parameter space ####
plot_parameters(model.output, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/parameterspace.png", 
                "Parameter space")

par(mar=c(5, 3, 5, 7)+0.1)
col.array <- col_generator(model.output$K)
plot3d(as.numeric(model.output$c), as.numeric(model.output$r), 
       log10(as.numeric(model.output$N)), col = col.array, size = 2, xlab = "c", 
       ylab = "r", zlab = "logN")
rgl.postscript("/Users/Xin/Desktop/projects/GTA_project/output/20160722/parameter_space.pdf", "pdf") 
       

 
rgl.open()
plot3d(as.numeric(model.output$c), as.numeric(model.output$r), 
       log10(as.numeric(model.output$N)), col = col.array, size = 2, xlab = "c", 
       ylab = "r", zlab = "logN")

for (i in 1:10) {
  rgl.viewpoint(i, 36*i)
  filename <- paste("/Users/Xin/Desktop/projects/GTA_project/output/20160722/parameter_space", 
                    formatC(i, digits = 1, flag = "0"), ".pdf", sep = "") 
  rgl.postscript(filename, fmt = "pdf")
}


 
######## no solution ########
range(as.numeric(nonsolve$c), na.rm = T) # 0.001 1.000
range(as.numeric(nonsolve$r), na.rm = T) # 0.001296 1.000000
range(as.numeric(nonsolve$N), na.rm = T) # 1e-010 1e-07  
range(as.numeric(nonsolve$K), na.rm = T) # 1e+05 1e+09

nonsolve.c.length <- table(nonsolve$c) # length 47

nonsolve.r.length <- table(nonsolve$r) # length 50

nonsolve.N.length <- table(nonsolve$N) # length 50

nonsolve.k.length <- table(nonsolve$K) # length 51


plot_parameters(nonsolve, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/nosolution.png", 
                "No-solution group")

######## ou system

range(as.numeric(ou$c)) # 0.7, 1
range(as.numeric(ou$r)) # 0.08 1.00
range(as.numeric(ou$N)) # 1e-07 1e-07 !! N only has one value in this group
range(as.numeric(ou$K)) # 393023256 1000000000

ou.c.length <- table(ou$c) # length 4

ou.r.length <- table(ou$r) # length 16
 
ou.k.length <- table(ou$K) # length 30

ou.xpos <- subset(ou, p!="NA" & as.numeric(p) >=0.99)  #0
ou.xneg <- subset(ou, p!="NA" & as.numeric(p) <=0.01)  #0
ou.both <- subset(ou, p!="NA" & as.numeric(p) > 0.01 & as.numeric(p) < 0.99)  #1180

# ggplot(ou, aes(x = as.numeric(c), y = as.numeric(r), fill = as.numeric(K))) + 
#   geom_point(shape = 21, size = 5, alpha = 0.2) + 
#   scale_fill_gradient(low="white", high="black", name = "K") + 
#   xlab("c") + ylab("r") 
# plot3d(as.numeric(ou$c), as.numeric(ou$r), as.numeric(ou$K), size = 5, 
#        xlab = "c", ylab = "r", zlab = "K", 
#        main = "Oscillation unstable system (N = 1e-7)")
# rgl.postscript("/Users/Xin/Desktop/projects/GTA_project/output/20160722/Krc_ou.pdf","pdf") 
# 
png("/Users/Xin/Desktop/projects/GTA_project/output/20160722/Krc_ou.png", 
     width = 600, height = 600)
scatterplot3d(as.numeric(ou$c), as.numeric(ou$r), log10(as.numeric(ou$K)), pch = 5, 
       xlab = "c", ylab = "r", zlab = "logK", 
       main = "Oscillation unstable system (N = 1e-7)")
dev.off()


########## lu system
#######################################
range(as.numeric(lu$c)) # 1e-04 1e-01
range(as.numeric(lu$r)) # 1e-04 1e+00
range(as.numeric(lu$N)) # 1e-09 1e-07  
range(as.numeric(lu$K)) # 1e+07 1e+09

lu.c.length <- table(lu$c) # length 41

lu.r.length <- table(lu$r) # length 50

lu.N.length <- table(lu$N) # length 47

lu.k.length <- table(lu$K) # length 30

lu.xpos <- subset(lu, p!="NA" & as.numeric(p) >=0.99)  #861
lu.xneg <- subset(lu, p!="NA" & as.numeric(p) <=0.01)  #2586
lu.both <- subset(lu, p!="NA" & as.numeric(p) > 0.01 & as.numeric(p) < 0.99)  # 1428

# plot3d(as.numeric(lu$c), as.numeric(lu$r), log10(as.numeric(lu$N)), col = col.array, size = 5, 
#        xlab = "c", ylab = "r", zlab = "logN", 
#        main = "Oscillation stable system")
#plot_parameters(lu, 
#                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/lu.png", 
#                "Non-oscillation unstable system")

range(as.numeric(lu.xpos$c)) # 0.00010000 0.04934286
range(as.numeric(lu.xpos$r)) # 0.001296 1.000000
range(as.numeric(lu.xpos$N)) # 1.012195e-09 8.000000e-09
range(as.numeric(lu.xpos$K)) # 141860465 1000000000

lu.xpos.c.length <- table(as.numeric(lu.xpos$c)) # 16
lu.xpos.r.length <- table(as.numeric(lu.xpos$r)) # 49
lu.xpos.K.length <- table(as.numeric(lu.xpos$K)) # 20
lu.xpos.N.length <- table(as.numeric(lu.xpos$N)) # 44

plot_parameters(lu.xpos, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/luxpos.png", 
                "Non-oscillation unstable system (X+)")

range(as.numeric(lu.xneg$c)) # 1e-04 1e-01
range(as.numeric(lu.xneg$r)) #  1e-04 1e+00
range(as.numeric(lu.xneg$N)) # 1e-09 1e-07
range(as.numeric(lu.xneg$K)) #  1e+07 1e+09

lu.xneg.c.length <- table(as.numeric(lu.xneg$c)) # 41
lu.xneg.r.length <- table(as.numeric(lu.xneg$r)) # 50
lu.xneg.K.length <- table(as.numeric(lu.xneg$K)) # 20
lu.xneg.N.length <- table(as.numeric(lu.xneg$N)) # 26


plot_parameters(lu.xneg, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/luxneg.png", 
                "Non-oscillation unstable system (X-)")

range(as.numeric(lu.both$c)) # 0.0001 0.0015
range(as.numeric(lu.both$r)) # 1e-04 1e+00
range(as.numeric(lu.both$N)) # 1e-09 1e-07
range(as.numeric(lu.both$K)) #  1e+07 1e+09

lu.both.c.length <- table(as.numeric(lu.both$c)) # 6
lu.both.r.length <- table(as.numeric(lu.both$r)) # 50
lu.both.K.length <- table(as.numeric(lu.both$K)) # 21
lu.both.N.length <- table(as.numeric(lu.both$N)) # 46


plot_parameters(lu.both, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/luboth.png", 
                "Non-oscillation unstable system (X+, X-)")



############# os system ##############
os.xpos <- subset(os, p!="NA" & as.numeric(p) >=0.99)  #26321
os.xneg <- subset(os, p!="NA" & as.numeric(p) <=0.01)  #797228
os.both <- subset(os, p!="NA" & as.numeric(p) > 0.01 & as.numeric(p) < 0.99)  # 420699

range(as.numeric(os.xpos$c)) # 0.001 0.500
range(as.numeric(os.xpos$r)) # 0.004884 1.000000
range(as.numeric(os.xpos$N)) # 5e-09 1e-07
range(as.numeric(os.xpos$K)) # 5e+07 1e+09

os.xpos.c.length <- table(as.numeric(os.xpos$c)) # 41
os.xpos.r.length <- table(as.numeric(os.xpos$r)) # 46
os.xpos.K.length <- table(as.numeric(os.xpos$K)) # 46
os.xpos.N.length <- table(as.numeric(os.xpos$N)) # 4

plot_parameters(os.xpos, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/osxpos.png", 
                "Oscillation stable system (X+)")

range(as.numeric(os.xneg$c)) # 0.002492 1.000000
range(as.numeric(os.xneg$r)) #  0.004314286 1.000000000
range(as.numeric(os.xneg$N)) # 1e-10 1e-07
range(as.numeric(os.xneg$K)) #  1e+05 1e+09

os.xneg.c.length <- table(as.numeric(os.xneg$c)) # 44
os.xneg.r.length <- table(as.numeric(os.xneg$r)) # 48
os.xneg.K.length <- table(as.numeric(os.xneg$K)) # 50
os.xneg.N.length <- table(as.numeric(os.xneg$N)) # 49


plot_parameters(os.xneg, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/osxneg.png", 
                "Oscillation stable system (X-)")

range(as.numeric(os.both$c)) # 0.004314286 1.000000000
range(as.numeric(os.both$r)) # 0.002492 1.000000
range(as.numeric(os.both$N)) # 1.012195e-09 1.000000e-07
range(as.numeric(os.both$K)) #  2e+07 1e+09

os.both.c.length <- table(as.numeric(os.both$c)) # 44
os.both.r.length <- table(as.numeric(os.both$r)) # 48
os.both.K.length <- table(as.numeric(os.both$K)) # 47
os.both.N.length <- table(as.numeric(os.both$N)) # 46


plot_parameters(os.both, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/osboth.png", 
                "Oscillation stable system (X+, X-)")

############# ls system ##############
ls.xpos <- subset(ls, p!="NA" & as.numeric(p) >=0.99)   ##401314
ls.xneg <- subset(ls, p!="NA" & as.numeric(p) <=0.01)  ##3537862
ls.both <- subset(ls, p!="NA" & as.numeric(p) > 0.01 & as.numeric(p) < 0.99)  ## 345487


range(as.numeric(ls.xpos$c)) #  1e-04 4e-01
range(as.numeric(ls.xpos$r)) # 0.001296 1.000000
range(as.numeric(ls.xpos$N)) # 1.012195e-09 1.000000e-07
range(as.numeric(ls.xpos$K)) # 2e+07 1e+09

ls.xpos.c.length <- table(ls.xpos$c) # length 44

ls.xpos.r.length <- table(ls.xpos$r) # length 49

ls.xpos.N.length <- table(ls.xpos$N) # length 46

ls.xpos.k.length <- table(ls.xpos$K) # length 47



plot_parameters(ls.xpos, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/lsxpos.png", 
                "Non-oscillation stable system (X+)")

range(as.numeric(ls.xneg$c)) # 1e-04 1e+00
range(as.numeric(ls.xneg$r)) #  1e-04 1e+00
range(as.numeric(ls.xneg$N)) # 1e-10 1e-07
range(as.numeric(ls.xneg$K)) #  1e+05 1e+09

ls.xneg.c.length <- table(ls.xneg$c) # length 50

ls.xneg.r.length <- table(ls.xneg$r) # length 50

ls.xneg.N.length <- table(ls.xneg$N) # length 49

ls.xneg.k.length <- table(ls.xneg$K) # length 50


plot_parameters(ls.xneg, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/lsxneg.png", 
                "Non-oscillation stable system (X-)")


range(as.numeric(ls.both$c)) # 1e-04 1e+00
range(as.numeric(ls.both$r)) #  1e-04 1e+00
range(as.numeric(ls.both$N)) # 1.012195e-09 1.000000e-07
range(as.numeric(ls.both$K)) #  2e+07 1e+09

ls.both.c.length <- table(ls.both$c) # length 50

ls.both.r.length <- table(ls.both$r) # length 50

ls.both.N.length <- table(ls.both$N) # length 46

ls.both.k.length <- table(ls.both$K) # length 47


plot_parameters(ls.both, 
                "/Users/Xin/Desktop/projects/GTA_project/output/20160722/lsboth.png", 
                "Non-oscillation stable system (X+, X-)")

 