library(ggplot2)
library(plyr)
library(scales)
library(rgl)
library(scatterplot3d)
library(fields) 
library(aqfig)
library(dplyr)
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
  out <- ode(yini, times, GTAeq, Pars, method = "ode45")
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
       main = paste("logN distribution ", group ,sep=""), xlim = c(-10, -6))
  plot(density(log10(as.numeric(dat$K)), na.rm=T, bw = 0.01), 
       main = paste("logK distribution ", group ,sep=""), xlim = c(5, 9))
  dev.off()
  par(mfrow=c(1,1))
}

GTAmodel_plot2 <- function(dat,outputdir, time, i){
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
  out <- ode(yini, times, GTAeq, Pars, method = "ode45")
  output1 <- paste(outputdir, i, sep = "_")
  png(output1, width = 600, height = 600)
  matplot(out[ , 1], out[ , 2:3], type = "l", xlab = "time", xlim = c(0, 1.5*time),
          ylab = "population size", main = i, lwd = 2)
  legend("topright", c("X+", "X-"), col = 1:2, lty = 1:2)
  dev.off()
}

dir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161026/"
file1 <- "GTAmodel_all.txt"
input1 <- paste(dir, file1, sep = "")
file2 <- "ls_xpos_sat.txt"
output1 <- paste(dir, file2, sep = "")
file3 <- "lu_xpos_sat.txt"
output2 <- paste(dir, file3, sep = "")
file4 <- "ls_xpos_unsat.txt"
output3 <- paste(dir, file4, sep = "")
file5 <- "lu_xpos_unsat.txt"
output4 <- paste(dir, file5, sep = "")


model.output <- read.delim2(input1, sep = "\t", stringsAsFactors = FALSE, 
                            header = F, na.strings = "")
names(model.output) <- c("Xpos","Xneg","p","curveshape", 
                         "stable", "r", "K", "c", "N")
model.output$saturation <- (as.numeric(model.output$Xpos) + 
                              as.numeric(model.output$Xneg))/as.numeric(model.output$K)
nonsolve <- subset(model.output, p == -1) # 722



ou <- subset(model.output, curveshape == "o" & stable == "u") # 232
ou.xpos <- subset(ou, as.numeric(ou$p) >=0.99) #0
ou.xneg <- subset(ou, as.numeric(ou$p) <=0.01) #229
ou.both <- subset(ou, as.numeric(p) > 0.01 & as.numeric(p) < 0.99) #3

outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161130/ouboth"
for (i in 1:length(ou.both$p)){
  GTAmodel_plot2(ou.both, outputdir, 1000, i)
  print(i)
}


GTAmodel_plot(ou, dir, "popplot_ou.png", 
              "parameter_dist_ou.png", "ou", 10000, 200)

GTAmodel_plot(ou.xneg, dir, "popplot_ouxnegunsat.png", 
              "parameter_dist_ouxnegunsat.png", "ou_xneg_unsat", 10000, 229)  # all o
GTAmodel_plot(ou.both, dir, "popplot_oubothunsat.png", 
              "parameter_dist_oubothunsat.png", "ou_both_unsat", 1000, 3) # all o

os <-  subset(model.output, curveshape == "o" & stable == "s") # 1045940
os.xpos.unsat <- subset(os, as.numeric(os$p) >=0.99 & saturation < 0.9) #28605
os.xpos.sat <- subset(os, as.numeric(os$p) >=0.99 & saturation >= 0.9) #16405
os.xneg.unsat <- subset(os, as.numeric(os$p) <=0.01 &  saturation < 0.9) #33927
os.xneg.sat <- subset(os, as.numeric(os$p) <=0.01 & saturation >= 0.9) #496184
os.both.unsat <- subset(os, as.numeric(os$p) > 0.01 & as.numeric(os$p) < 0.99 & saturation < 0.9) #431034
os.both.sat <- subset(os, as.numeric(os$p) > 0.01 & as.numeric(os$p) < 0.99 & saturation >= 0.9) #39785


 




GTAmodel_plot(os.xpos.unsat, dir, "popplot_osxposunsat.png", 
              "parameter_dist_osxposunsat.png", "os_xpos_unsat", 2000, 28605) # all l
GTAmodel_plot(os.xpos.sat, dir, "popplot_osxpossat.png", 
              "parameter_dist_osxpossat.png", "os_xpos_sat", 200, 16405) # all l
GTAmodel_plot(os.xneg.unsat, dir, "popplot_osxnegunsat.png", 
              "parameter_dist_osxnegunsat.png", "os_xneg_unsat", 10000, 30007) # all o
GTAmodel_plot(os.xneg.sat, dir, "popplot_osxnegsat.png", 
              "parameter_dist_osxnegsat.png", "os_xneg_sat", 50000, 496182) # all l
GTAmodel_plot(os.both.unsat, dir, "popplot_osbothunsat.png", 
              "parameter_dist_osbothunsat.png", "os_both_unsat", 300000, 430000) # all o
GTAmodel_plot(os.both.sat, dir, "popplot_osbothsat.png", 
              "parameter_dist_osbothsat.png", "os_both_sat", 5000, 39785) #all l

lu <- subset(model.output, curveshape == "l" & stable == "u") # 6017
lu.xpos.unsat <- subset(lu, as.numeric(lu$p) >=0.99 & saturation < 0.9) #67
lu.xpos.sat <- subset(lu, as.numeric(lu$p) >=0.99 & saturation >= 0.9) #838
lu.xneg.unsat <- subset(lu, as.numeric(lu$p) <=0.01 & saturation < 0.9) #287
lu.xneg.sat <- subset(lu, as.numeric(lu$p) <=0.01 & saturation >= 0.9) #2794
lu.both.unsat <- subset(lu, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation < 0.9) #498
lu.both.sat <- subset(lu, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation >= 0.9) #1533

outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161130/luxnus"
i = 1
while (i <=287){
  GTAmodel_plot2(lu.xneg.unsat, outputdir, 1000000, i)
  i = i+10
  print(i)
}

outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161130/luxns"
i = 1
while (i <=2794){
  GTAmodel_plot2(lu.xneg.sat, outputdir, 1000000, i)
  i = i+100
  print(i)
}

outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161130/lubus"
i = 1
while (i <=498){
  GTAmodel_plot2(lu.both.unsat, outputdir, 1000000, i)
  i = i+10
  print(i)
}

outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161130/lubs"
i = 1
while (i <=1533){
  GTAmodel_plot2(lu.both.sat, outputdir, 1000000, i)
  i = i+20
  print(i)
}


GTAmodel_plot(lu.xpos.unsat, dir, "popplot_luxposunsat.png", 
              "parameter_dist_luxposunsat.png", "lu_xpos_unsat", 200000, 60) # no o
GTAmodel_plot(lu.xpos.sat, dir, "popplot_luxpossat.png", 
              "parameter_dist_luxpossat.png", "lu_xpos_sat", 100000, 838) # no o
GTAmodel_plot(lu.xneg.unsat, dir, "popplot_luxnegunsat.png", 
              "parameter_dist_luxnegunsat.png", "lu_xneg_unsat", 200000, 287) # all o
GTAmodel_plot(lu.xneg.sat, dir, "popplot_luxnegsat.png", 
              "parameter_dist_luxnegsat.png", "lu_xneg_sat", 200000, 2794)
GTAmodel_plot(lu.both.unsat, dir, "popplot_lubothunsat.png", 
              "parameter_dist_lubothunsat.png", "lu_both_unsat", 200000, 498) # all o
GTAmodel_plot(lu.both.sat, dir, "popplot_lubothsat.png", 
              "parameter_dist_lubothsat.png", "lu_both_sat", 100000, 1533)


ls <-  subset(model.output, curveshape == "l" & stable == "s") # 5197089
ls.xpos.unsat <- subset(ls, as.numeric(ls$p) >=0.99 & saturation < 0.90) #126578
ls.xpos.sat <- subset(ls, as.numeric(ls$p) >=0.99 & saturation >= 0.90) #322142
ls.xneg.unsat <- subset(ls, as.numeric(ls$p) <=0.01 & saturation < 0.90) #15503
ls.xneg.sat <- subset(ls, as.numeric(ls$p) <=0.01 & saturation >= 0.90) #4324475
ls.both.unsat <- subset(ls, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation < 0.90) #177519
ls.both.sat <- subset(ls, as.numeric(p) > 0.01 & as.numeric(p) < 0.99 & saturation >= 0.90) #230872


outputdir <- "/Users/Xin/Desktop/projects/GTA_project/output/20161130/ls"
i = 1
while (i <=5197089){
  GTAmodel_plot2(ls, outputdir, 10000, i)
  i = i+20000
  print(i)
}



GTAmodel_plot(ls.xpos.unsat, dir, "popplot_lsxposunsat.png", 
              "parameter_dist_lsxposunsat.png", "ls_xpos_unsat", 10000, 100000)
GTAmodel_plot(ls.xpos.sat, dir, "popplot_lsxpossat.png", 
              "parameter_dist_lsxpossat.png", "ls_xpos_sat", 20000, 300000)
GTAmodel_plot(ls.xneg.unsat, dir, "popplot_lsxnegunsat.png", 
              "parameter_dist_lsxnegunsat.png", "ls_xneg_unsat", 50000, 10000)
GTAmodel_plot(ls.xneg.sat, dir, "popplot_lsxnegsat.png", 
              "parameter_dist_lsxnegsat.png", "ls_xneg_sat", 3000, 4000000)
GTAmodel_plot(ls.both.unsat, dir, "popplot_lsbothunsat.png", 
              "parameter_dist_lsbothunsat.png", "ls_both_unsat", 500000, 5)
GTAmodel_plot(ls.both.sat, dir, "popplot_lsbothsat.png", 
              "parameter_dist_lsbothsat.png", "ls_both_sat", 5000, 200000)

ou.group <- rbind(ou, lu.xneg.unsat, lu.both.unsat)

os.xpos.unsat2 <-rbind(os.xpos.unsat[19:24,], os.xpos.unsat[36:38,], 
                       os.xpos.unsat[27801,], os.xpos.unsat[27809,],
                       os.xpos.unsat[27817,], os.xpos.unsat[28001,],
                       os.xpos.unsat[28009,], os.xpos.unsat[28017,],
                       os.xpos.unsat[28121,])
ls.xpos.unsat2 <-rbind(os.xpos.unsat[1:18,], os.xpos.unsat[25:35,], 
                       os.xpos.unsat[39:27800,], os.xpos.unsat[27802:27808,],
                       os.xpos.unsat[27810:27816,], os.xpos.unsat[27818:28000,],
                       os.xpos.unsat[28002:28008,], os.xpos.unsat[28010:28016,],
                       os.xpos.unsat[28018:28120,], os.xpos.unsat[28122:28605,])

os.xneg.sat2 <-rbind(os.xneg.sat[105879:106100,], os.xneg.sat[106102:106150,], 
                     os.xneg.sat[106152:106175,], os.xneg.sat[106183:106242,],
                     os.xneg.sat[203768:203987,], os.xneg.sat[203996:204006,],
                     os.xneg.sat[204012:204023,], os.xneg.sat[204030:204036,],
                     os.xneg.sat[204046:204048,],
                     os.xneg.sat[315768:316088,], os.xneg.sat[431522:431791,], 
                     os.xneg.sat[431967:432948,])

ls.xneg.sat2 <-rbind(os.xneg.sat[1:105878,], os.xneg.sat[106101,], 
                     os.xneg.sat[106151,], os.xneg.sat[106176:106182,],
                     os.xneg.sat[106243:203767,], os.xneg.sat[203988:203995,],
                     os.xneg.sat[204007:204011,], os.xneg.sat[204024:204029,],
                     os.xneg.sat[204037:204045,], os.xneg.sat[204049:315767,],
                     os.xneg.sat[316089:431521,], os.xneg.sat[431792:431966,],
                     os.xneg.sat[432949:496184,])

os.both.unsat2 <-rbind(os.both.unsat[1:8139,], os.both.unsat[8152:8156,],
                       os.both.unsat[8165:8176,], os.both.unsat[8185:8199,],
                       os.both.unsat[8205:8224,], os.both.unsat[8237:8259,],
                       os.both.unsat[8266:8306,], os.both.unsat[8319:8320,], 
                       os.both.unsat[8345:8390,], os.both.unsat[8394:8398,],
                       os.both.unsat[8406:8499,], os.both.unsat[8500:8507,],
                       os.both.unsat[8509:8569,], os.both.unsat[8573:8577,],
                       os.both.unsat[8581:9109,], os.both.unsat[9113:9136,],
                       os.both.unsat[9141:9269,], os.both.unsat[9273:9298,],
                       os.both.unsat[9301:9328,], os.both.unsat[9333:9348,], 
                       os.both.unsat[9353:9958,], os.both.unsat[9971:9976,],
                       os.both.unsat[9985:9997,], os.both.unsat[10201:10205,], 
                       os.both.unsat[10212:10248,],
                       os.both.unsat[10252:11139,], os.both.unsat[11242:11808,],
                       os.both.unsat[11813:11837,], os.both.unsat[11841:11867,],
                       os.both.unsat[11871:11897,], os.both.unsat[11903:11920,],
                       os.both.unsat[11924:11946,], os.both.unsat[11949:11974,],
                       os.both.unsat[11977:18570,], os.both.unsat[18573:18600,],
                       os.both.unsat[18626:18643,], os.both.unsat[18646:18669,],
                       os.both.unsat[18673:19170,], os.both.unsat[19173:19199,],
                       os.both.unsat[19207:19920,], os.both.unsat[19926:19940,],
                       os.both.unsat[19944:19996,], os.both.unsat[20002:20014,], 
                       os.both.unsat[20027:20035,], os.both.unsat[20039:20042,],
                       os.both.unsat[20044:20077,], os.both.unsat[20081:20089,],
                       os.both.unsat[20091:20096,], os.both.unsat[20098:20303,],
                       os.both.unsat[20305:20311,], os.both.unsat[20543:24005,],
                       os.both.unsat[24008:24012,], os.both.unsat[24030:24033,],
                       os.both.unsat[24038:24040,], os.both.unsat[24051:24052,],
                       os.both.unsat[24057:24058,], os.both.unsat[24062:24064,],
                       os.both.unsat[24084:24086,], os.both.unsat[24223:53904,],
                       os.both.unsat[54201:128600,], os.both.unsat[128602:129396,],
                       os.both.unsat[129404:129410,], os.both.unsat[129414:129429,],
                       os.both.unsat[129432:130000,], os.both.unsat[130002:130083,],
                       os.both.unsat[130086:130097,], os.both.unsat[130104:130140,],
                       os.both.unsat[130143:130820,], os.both.unsat[130823:130880,],
                       os.both.unsat[130883:431034,])

ls.both.unsat2 <-rbind(os.both.unsat[8140:8151,], os.both.unsat[8157:8164,],
                       os.both.unsat[8177:8184,], os.both.unsat[8200:8204,],
                       os.both.unsat[8225:8236,], os.both.unsat[8260:8265,],
                       os.both.unsat[8307:8318,], os.both.unsat[8321:8344,],
                       os.both.unsat[8391:8393,], os.both.unsat[8399:8405,],
                       os.both.unsat[8508,], os.both.unsat[8570:8572,],
                       os.both.unsat[8578:8580,], os.both.unsat[9110:9112,],
                       os.both.unsat[9137:9140,], os.both.unsat[9270:9272,],
                       os.both.unsat[9299:9300,], os.both.unsat[9329:9332,], 
                       os.both.unsat[9349:9352,], os.both.unsat[9959:9970,],
                       os.both.unsat[9977:9984,], os.both.unsat[9998:10200,], 
                       os.both.unsat[10206:10211,], os.both.unsat[10249:10251,],
                       os.both.unsat[11140:11241,], os.both.unsat[11809:11812,],
                       os.both.unsat[11838:11840,], os.both.unsat[11868:11870,],
                       os.both.unsat[11898:11902,], os.both.unsat[11921:11923,],
                       os.both.unsat[11947:11948,], os.both.unsat[11975:11976,],
                       os.both.unsat[18571:18572,], os.both.unsat[18601:18625,],
                       os.both.unsat[18644:18645,], os.both.unsat[18670:18672,],
                       os.both.unsat[19171:19172,], os.both.unsat[19200:19206,],
                       os.both.unsat[19921:19925,], os.both.unsat[19941:19943,],
                       os.both.unsat[19997:20001,], os.both.unsat[20015:20026,], 
                       os.both.unsat[20036:20038,], os.both.unsat[20043,],
                       os.both.unsat[20078:20080,], os.both.unsat[20090,],
                       os.both.unsat[20097,], os.both.unsat[20304,],
                       os.both.unsat[20312:20542,], os.both.unsat[24006,],
                       os.both.unsat[24007,], os.both.unsat[24013:24029,],
                       os.both.unsat[24034:24037,], os.both.unsat[24041:24050,],
                       os.both.unsat[24053:24056,], os.both.unsat[24059:24061,],
                       os.both.unsat[24065:24083,], os.both.unsat[24087:24222,],
                       os.both.unsat[53905:54200,], os.both.unsat[128601,],
                       os.both.unsat[129397:129403,], os.both.unsat[129411:129413,],
                       os.both.unsat[129430:129431,], os.both.unsat[130001,],
                       os.both.unsat[130084:130085,], os.both.unsat[130098:130103,], 
                       os.both.unsat[130141:130142,], os.both.unsat[130821:130822,], 
                       os.both.unsat[130881:130882,])


ou.group <- rbind(ou, lu.xneg.unsat, lu.both.unsat) #1017
GTAmodel_plot(ou.group, dir, "popplot_ougroup.png", 
              "parameter_dist_ougroup.png", "ou_unsat", 10000, 200)
os.xneg.unsat.group <- rbind(os.xneg.unsat,  ls.xneg.unsat) #49430
GTAmodel_plot(os.xneg.unsat.group, dir, "popplot_osxnegunsatgroup.png", 
              "parameter_dist_osxnegunsatgroup.png", "os_xneg_unsat", 10000, 200)
os.both.unsat.group <- os.both.unsat  #431034
GTAmodel_plot(os.both.unsat.group, dir, "popplot_osbothunsatgroup.png", 
              "parameter_dist_osbothunsatgroup.png", "os_both_unsat", 50000, 400000)

ls.xpos.sat.group <- rbind(os.xpos.sat, ls.xpos.sat)  #338547
write.table(ls.xpos.sat.group, file = output1, sep = '\t')

png("/Users/Xin/Desktop/projects/GTA_project/output/20161115/ls-sat-xpos-para.png", width = 1000, height = 200)
par(mfrow=c(1, 4), mar = c(2,2,2,2))
plot(table(round(as.numeric(ls.xpos.sat.group$c), 5)), na.rm=T, main = "c dist ls-sat-xpos")
plot(table(round(as.numeric(ls.xpos.sat.group$r), 5)), na.rm=T, main = "r dist ls-sat-xpos", xlim = c(0, 1))
plot(table(round(log10(as.numeric(ls.xpos.sat.group$N)), 2)), na.rm=T, main = "logN dist ls-sat-xpos")
plot(table(round(log10(as.numeric(ls.xpos.sat.group$K)), 2)), na.rm=T, main = "logK dist ls-sat-xpos")
dev.off()

GTAmodel_plot(ls.xpos.sat.group, dir, "popplot_lsxpossatgroup.png", 
              "parameter_dist_lsxpossatgroup.png", "ls_xpos_sat", 100, 1000)
ls.xneg.sat.group <- rbind(os.xneg.sat, ls.xneg.sat) #4820659

GTAmodel_plot(ls.xneg.sat.group, dir, "popplot_lsxnegsatgroup.png", 
              "parameter_dist_lsxnegsatgroup.png", "ls_xneg_sat", 1000, 1000)
ls.both.sat.group <- rbind(os.both.sat, ls.both.sat) #270657
GTAmodel_plot(ls.both.sat.group, dir, "popplot_lsbothsatgroup.png", 
              "parameter_dist_lsbothsatgroup.png", "ls_both_sat", 1000, 10000)
lu.xpos.sat.group <- lu.xpos.sat #838
write.table(lu.xpos.sat.group, file = output2, sep = '\t')

png("/Users/Xin/Desktop/projects/GTA_project/output/20161115/lu-sat-xpos-para.png", width = 1000, height = 200)
par(mfrow=c(1, 4), mar = c(2,2,2,2))
plot(table(round(as.numeric(lu.xpos.sat.group$c), 5)), na.rm=T, main = "c dist lu-sat-xpos")
plot(table(round(as.numeric(lu.xpos.sat.group$r), 5)), na.rm=T, main = "r dist lu-sat-xpos", xlim = c(0, 1))
plot(table(round(log10(as.numeric(lu.xpos.sat.group$N)), 2)), na.rm=T, main = "logN dist lu-sat-xpos")
plot(table(round(log10(as.numeric(lu.xpos.sat.group$K)), 2)), na.rm=T, main = "logK dist lu-sat-xpos")
dev.off()



GTAmodel_plot(lu.xpos.sat.group, dir, "popplot_luxpossatgroup.png", 
              "parameter_dist_luxpossatgroup.png", "lu_xpos_sat", 400000, 400)
lu.xneg.sat.group <- lu.xneg.sat #2794
GTAmodel_plot(lu.xneg.sat.group, dir, "popplot_luxnegsatgroup.png", 
              "parameter_dist_luxnegsatgroup.png", "lu_xneg_sat", 40000, 2000)
lu.both.sat.group <- lu.both.sat #1533
GTAmodel_plot(lu.both.sat.group, dir, "popplot_lubothsatgroup.png", 
              "parameter_dist_lubothsatgroup.png", "lu_both_sat", 40000, 1000)
ls.xpos.unsat.group <- rbind(os.xpos.unsat, ls.xpos.unsat) #155183
write.table(ls.xpos.unsat.group, file = output3, sep = '\t')

png("/Users/Xin/Desktop/projects/GTA_project/output/20161115/ls-unsat-xpos-para.png", width = 1000, height = 200)
par(mfrow=c(1, 4), mar = c(2,2,2,2))
plot(table(round(as.numeric(ls.xpos.unsat.group$c), 5)), na.rm=T, main = "c dist ls-unsat-xpos")
plot(table(round(as.numeric(ls.xpos.unsat.group$r), 5)), na.rm=T, main = "r dist ls-unsat-xpos", xlim = c(0, 1))
plot(table(round(log10(as.numeric(ls.xpos.unsat.group$N)), 2)), na.rm=T, main = "logN dist ls-unsat-xpos")
plot(table(round(log10(as.numeric(ls.xpos.unsat.group$K)), 2)), na.rm=T, main = "logK dist ls-unsat-xpos")
dev.off()


GTAmodel_plot(ls.xpos.unsat.group, dir, "popplot_lsxposunsatgroup.png", 
              "parameter_dist_lsxposunsatgroup.png", "ls_xpos_unsat", 400, 500)
ls.both.unsat.group <- ls.both.unsat #177519
GTAmodel_plot(ls.both.unsat.group, dir, "popplot_lsbothunsatgroup.png", 
              "parameter_dist_lsbothunsatgroup.png", "ls_both_unsat", 40000, 100000)
lu.xpos.unsat.group <- lu.xpos.unsat #67
write.table(lu.xpos.unsat.group, file = output4, sep = '\t')
png("/Users/Xin/Desktop/projects/GTA_project/output/20161115/lu-unsat-xpos-para.png", width = 1000, height = 200)
par(mfrow=c(1, 4), mar = c(2,2,2,2))
plot(table(round(as.numeric(lu.xpos.unsat.group$c), 5)), na.rm=T, main = "c dist lu-unsat-xpos")
plot(table(round(as.numeric(lu.xpos.unsat.group$r), 5)), na.rm=T, main = "r dist lu-unsat-xpos", xlim = c(0, 1))
plot(table(round(log10(as.numeric(lu.xpos.unsat.group$N)), 2)), na.rm=T, main = "logN dist lu-unsat-xpos")
plot(table(round(log10(as.numeric(lu.xpos.unsat.group$K)), 2)), na.rm=T, main = "logK dist lu-unsat-xpos")
dev.off()

GTAmodel_plot(lu.xpos.unsat.group, dir, "popplot_luxposunsatgroup.png", 
              "parameter_dist_luxposunsatgroup.png", "lu_xpos_unsat", 3000000, 60)
