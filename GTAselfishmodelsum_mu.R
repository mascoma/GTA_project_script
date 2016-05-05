library(GillespieSSA)
library(ggplot2)
library(reshape2)

files = list.files(path = "/isi/olga/xin/GTA_project/output/selfish_model/newoutput")
path = "/isi/olga/xin/GTA_project/output/selfish_model/newoutput"

 
lpfreqs = seq(0.00001, 0.01, 0.00002)
min_mu = vector(mode = "numeric", length = length(files))
for (j in 1:length(files)){
  dir = paste(path, files[j], sep="/")
  load(dir) 
  for (i in 1:500) {
    len = length(output[[i]]$data[,1])
    x1 = output[[i]]$data[len, 2]
    x2 = output[[i]]$data[len, 3]
    x = x1+x2
    if (x1 >x2){
      mu = lpfreqs[i]
      rm(output)
      min_mu[j] = mu 
      break
    } 
  }
}
save.image("/isi/olga/xin/GTA_project/output/selfish_model/selfishmodelsum_mu2.RData")
q()
