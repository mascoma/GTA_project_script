library(ggplot2)
library(plyr)
library(scales)
library(rgl)
library(scatterplot3d)
library(fields) 
library(aqfig)
library(deSolve)


GTAmodel_plot <- function(dat, outputdir, fig1, fig2, group, time, i){
  yini <- c(X1 = 10000, X2 = 10000)
  GTAeq <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
      dX1 <- r * (1-c) * (1-(X1+X2)/K) * X1 - c * X1 + c * N* X1 * X2 # GTA postive
      dX2 <- r * (1-(X1+X2)/K) * X2 - c * N * X1 * X2     # GTA negative
      return(list(c(dX1, dX2)))
    }) 
  }
  times <- seq(0, time, by = 1)
  Pars <- c(c = as.numeric(dat$c[i]), 
            r = as.numeric(dat$r[i]), 
            N = as.numeric(dat$N[i]), 
            K = as.numeric(dat$K[i]))
  out <- ode(yini, times, GTAeq, Pars)
  output1 <- paste(outputdir, fig1, sep = "")
  png(output1, width = 600, height = 600)
  title1 <- paste("population dynamic of ", group, sep = "")
  matplot(out[ , 1], out[ , 2:3], type = "l", xlab = "time", xlim = c(0, 1.5*time),
          ylab = "population size", main = title1, lwd = 2)
  legend("topright", c("X+", "X-"), col = 1:2, lty = 1:2)
  dev.off()
  
  output2 <- paste(outputdir, fig2, sep = "")
  png(output2, width = 1200, height = 300)
  par(mfrow=c(1, 4), mar = c(2,2,2,2))
  plot(density(as.numeric(dat$c), na.rm=T, bw = 1e-4), 
       main = paste("c distribution ", group ,sep=""), xlim = c(0, 1))
  plot(density(as.numeric(dat$r), na.rm=T, bw = 1e-4), 
       main = paste("r distribution ", group ,sep=""), xlim = c(0, 1))
  plot(density(log10(as.numeric(dat$N)), na.rm=T, bw = 0.01), 
       main = paste("logN distribution ", group ,sep=""), xlim = c(-10, -7))
  plot(density(log10(as.numeric(dat$K)), na.rm=T, bw = 0.01), 
       main = paste("logK distribution ", group ,sep=""), xlim = c(5, 9))
  dev.off()
  par(mfrow=c(1,1))
}

dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20160823/"
file1 <- "group_no_solution.txt"
file2 <- "group_negative.txt"
file3 <- "group_notna.txt"
outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20160824/"

input1 <- paste(dir, file1, sep = "")
input2 <- paste(dir, file2, sep = "")
input3 <- paste(dir, file3, sep = "")
no.solution <- read.delim(input1, sep = "\t", stringsAsFactors = FALSE, 
                          header = T, na.strings = "") # 269760
neg.solution <- read.delim(input2, sep = "\t", stringsAsFactors = FALSE, 
                           header = T, na.strings = "") # 251800
real.solution <- read.delim(input3, sep = "\t", stringsAsFactors = FALSE, 
                            header = T, na.strings = "")

unsat <- subset(real.solution, real.solution$saturation < 0.95)
unstable <- subset(real.solution, stable == "u")

GTAmodel_plot(no.solution, outputdir, "popplot_nosolve.png", 
              "parameter_dist_nosolve.png", "no.solution", 10000, 580)

GTAmodel_plot(neg.solution, outputdir, "popplot_negsolve.png", 
              "parameter_dist_negsolve.png", "neg.solution", 20000, 580)

GTAmodel_plot(neg.solution, outputdir, "popplot_negsolve.png", 
              "parameter_dist_negsolve.png", "neg.solution", 10000, 60165)

GTAmodel_plot(neg.solution, outputdir, "popplot_negsolve.png", 
              "parameter_dist_negsolve.png", "neg.solution", 10000, 1189)

l <-subset(real.solution, curveshape == "l") #
o <-subset(real.solution, curveshape == "o") #
ou <- subset(real.solution, curveshape == "o" & stable == "u") # 1150
ou.xpos <- subset(ou, as.numeric(ou$p) >=0.99) #0
ou.xneg <- subset(ou, as.numeric(ou$p) <=0.01) #0
ou.both <- subset(ou, as.numeric(p) > 0.01 & as.numeric(p) < 0.99) # 1150
ou.unsat <- subset(ou, saturation < 0.90) # 1150

os <- subset(real.solution, curveshape == "o" & stable == "s") # 1256116 records
os.xpos <- subset(os, as.numeric(os$p) >=0.99) #27570
os.xpos.unsat <- subset(os.xpos, os.xpos$saturation < 0.90) #  21757
os.xneg <- subset(os, as.numeric(os$p) <=0.01) #798799
os.xneg.unsat <- subset(os.xneg, os.xneg$saturation < 0.90) #  26511
os.both <- subset(os, as.numeric(p) > 0.01 & as.numeric(p) < 0.99) # 429747
os.both.unsat <- subset(os.both, os.both$saturation < 0.90) # 367741

ls <- subset(real.solution, curveshape == "l" & stable == "s") # 4341206
ls.xpos <- subset(ls, as.numeric(ls$p) >=0.99) # 402433
ls.xpos.unsat <- subset(ls.xpos, ls.xpos$saturation < 0.90) #100810
ls.xpos.sat <- subset(ls.xpos, ls.xpos$saturation >= 0.90) #301623
ls.xneg <- subset(ls, as.numeric(ls$p) <=0.01) # 3591661
ls.xneg.unsat <- subset(ls.xneg, ls.xneg$saturation < 0.90) #7779
ls.xneg.sat <- subset(ls.xneg, ls.xneg$saturation >= 0.90) #3583882

ls.both <- subset(ls, as.numeric(ls$p) > 0.01 & as.numeric(ls$p) < 0.99) # 347112
ls.both.unsat <- subset(ls.both, ls.both$saturation < 0.90) #141132
ls.both.sat <- subset(ls.both, ls.both$saturation >= 0.90) #205980

GTAmodel_plot(ls.xpos, outputdir, "popplot_ls.xpos.png", 
              "parameter_dist_ls.xpos.png", "ls.xpos", 1000000, 5000)

GTAmodel_plot(ls.xneg, outputdir, "popplot_ls.xneg.png", 
              "parameter_dist_ls.xneg.png", "ls.xneg", 1000000, 5000)

GTAmodel_plot(ls.both, outputdir, "popplot_ls.both.png", 
              "parameter_dist_ls.both.png", "ls.both", 10000, 5000)

GTAmodel_plot(ls.xpos.unsat, outputdir,  "poplot_ls.xpos.unsat.png", 
              "parameter_dist_ls.xpos.unsat.png", "ls.xpos.unsat", 500, 100500)
GTAmodel_plot(ls.xpos.sat, outputdir,  "poplot_ls.xpos.sat.png", 
              "parameter_dist_ls.xpos.sat.png", "ls.xpos.sat", 1000000, 100)

GTAmodel_plot(ls.xneg.unsat, outputdir,  "poplot_ls.xneg.unsat.png", 
              "parameter_dist_ls.xneg.unsat.png", "ls.xneg.unsat", 1000000, 1)
GTAmodel_plot(ls.xneg.sat, outputdir,  "poplot_ls.xneg.sat.png", 
              "parameter_dist_ls.xneg.sat.png", "ls.xneg.sat", 10000, 100)

GTAmodel_plot(ls.both.unsat, outputdir,  "poplot_ls.both.unsat.png", 
              "parameter_dist_ls.both.unsat.png", "ls.both.unsat", 10000000, 1)
GTAmodel_plot(ls.both.sat, outputdir,  "poplot_ls.both.sat.png", 
              "parameter_dist_ls.both.sat.png", "ls.both.sat", 1000000, 100)



lu <- subset(real.solution, curveshape == "l" & stable == "u") # 4968
lu.xpos <- subset(lu, as.numeric(lu$p) >=0.99) # 862
lu.xpos.unsat <- subset(lu.xpos, lu.xpos$saturation < 0.90) # 25
lu.xpos.sat <- subset(lu.xpos, lu.xpos$saturation >= 0.90) # 837
lu.xneg <- subset(lu, as.numeric(lu$p) <=0.01) # 2618
lu.xneg.unsat <- subset(lu.xneg, lu.xneg$saturation < 0.90) # 31
lu.xneg.sat <- subset(lu.xneg, lu.xneg$saturation >= 0.90) # 2587
lu.both <- subset(lu, as.numeric(lu$p) > 0.01 & as.numeric(lu$p) < 0.99) # 1488
lu.both.unsat <- subset(lu.both, lu.both$saturation < 0.90) #95
lu.both.sat <- subset(lu.both, lu.both$saturation >= 0.90) #1393


GTAmodel_plot(lu.xpos, outputdir, "popplot_lu.xpos.png", 
              "parameter_dist_lu.xpos.png", "lu.xpos", 10000, 2)

GTAmodel_plot(lu.xneg, outputdir, "popplot_lu.xneg.png", 
              "parameter_dist_lu.xneg.png", "lu.xneg", 10000, 2)


GTAmodel_plot(lu.both, outputdir, "popplot_lu.both.png", 
              "parameter_dist_lu.both.png", "lu.both", 10000, 2)

GTAmodel_plot(lu.xpos.unsat, outputdir,  "poplot_lu.xpos.unsat.png", 
              "parameter_dist_lu.xpos.unsat.png", "lu.xpos.unsat", 1000000, 10)
GTAmodel_plot(lu.xpos.sat, outputdir,  "poplot_lu.xpos.sat.png", 
              "parameter_dist_lu.xpos.sat.png", "lu.xpos.sat", 10000000, 100)

GTAmodel_plot(lu.xneg.unsat, outputdir,  "poplot_lu.xneg.unsat.png", 
              "parameter_dist_lu.xneg.unsat.png", "lu.xneg.unsat", 1000000, 10)
GTAmodel_plot(lu.xneg.sat, outputdir,  "poplot_lu.xneg.sat.png", 
              "parameter_dist_lu.xneg.sat.png", "lu.xneg.sat", 10000000, 100)

GTAmodel_plot(lu.both.unsat, outputdir,  "poplot_lu.both.unsat.png", 
              "parameter_dist_lu.both.unsat.png", "lu.both.unsat", 1000000, 10)
GTAmodel_plot(lu.both.sat, outputdir,  "poplot_lu.both.sat.png", 
              "parameter_dist_lu.both.sat.png", "lu.both.sat", 10000000, 100)






GTAmodel_plot(ou, outputdir, "popplot_ou.png", 
              "parameter_dist_ou.png", "ou", 8000, 1)
 
GTAmodel_plot(os.xpos, outputdir, "popplot_os.xpos.png", 
              "parameter_dist_os.xpos.png", "os.xpos", 500, 2)


GTAmodel_plot(os.xneg, outputdir, "popplot_os.xneg.png", 
              "parameter_dist_os.xneg.png", "os.xneg", 10000, 2)
GTAmodel_plot(os.both, outputdir, "popplot_os.both.png", 
              "parameter_dist_os.both.png", "os.both", 10000, 2)







GTAmodel_plot(unsat, outputdir, "popplot_unsat.png", 
              "parameter_dist_unsat.png", "unsaturated", 10000, 100)
 
c.array <- c(0.0001, 0.0002, 0.0005, 0.0008, 0.001, 
             seq(0.0015, 0.1, length.out = 36), 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
             0.8, 0.9, 1)
K.array <- c(1e5, 1e6, 1e7, 2e7, 5e7, 8e7, seq(1e8, 1e9, length.out = 44))
r.array <- c(seq(0.0001, 0.03, length.out = 26), 0.05, 0.06, 0.08, 0.09,
             seq(0.1, 1, length.out = 20))
N.array <- c(1e-10, 5e-10, seq(1e-9, 1.5e-9, length.out = 42), 
             2e-9, 5e-9, 8e-9, 1e-8, 1e-7)
outfile <- paste(outputdir, "allparadist.png", sep = '')
png(outfile, width = 1200, height = 300)
par(mfrow=c(1, 4), mar = c(2,2,2,2))
plot(density(c.array, na.rm=T, bw = 1e-4), 
     main = paste("c distribution ", ".all" ,sep=""), xlim = c(0, 1))
plot(density(r.array, na.rm=T, bw = 1e-4), 
     main = paste("r distribution ", ".all" ,sep=""), xlim = c(0, 1))
plot(density(log10(N.array), na.rm=T, bw = 0.01), 
     main = paste("logN distribution ", ".all" ,sep=""), xlim = c(-10, -7))
plot(density(log10(K.array), na.rm=T, bw = 0.01), 
     main = paste("logK distribution ", ".all" ,sep=""), xlim = c(5, 9))
dev.off()
par(mfrow=c(1,1))

