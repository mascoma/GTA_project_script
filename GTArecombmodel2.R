



####################### reactions ##############
######### general equations for each reatcion and the function to evaluate the equations
################ regular growth ##################

Gv<- vector(mode="character",length=3*2^L)

for (i in 1:(2^L)){
  temp1<-paste("(1-(",X,")/K)*r*X0",i,sep="")
  temp2<-paste("(1-(",X,")/K)*(r-c+(",F,")*v*u)*Xn",i,sep="")
  temp3<-paste("(1-(",X,")/K)*(r-c+(",F,")*v*u)*Xr",i,sep="")
  Gv[(1+3*(i-1))]<-temp1
  Gv[(2+3*(i-1))]<-temp2
  Gv[(3+3*(i-1))]<-temp3
  
}

Gv


gv<-function (y) {    #### y has to be 1, 4, 7... 
  vector<-vector(mode="numeric", length=3)
  parse_G<-parse(text=Gv)
  vector[1]<- eval(parse_G[y])
  vector[2]<- eval(parse_G[(y+1)])
  vector[3]<- eval(parse_G[(y+2)])
  return (vector)
}


################################################
######### death ################################
Dv <- vector(mode="character", length = 3*2^L) ### reaction vector should be in order like reactions for geontype 1 CFN group 
### and then the next...
for (i in seq(1,2^L)){
  
  Dv[(1+3*(i-1))] <- paste("(m+s*(L-sum(gt[", i,",])))*X0", i, sep="")
  Dv[(2+3*(i-1))] <- paste("(m+s*(L-sum(gt[", i,",])))*Xn", i, sep="")
  Dv[(3+3*(i-1))] <- paste("(m+s*(L-sum(gt[", i,",])))*Xr", i, sep="")
}


Dv 

dv<-function (y) {    #### y has to be 1, 4, 7... 
  dv<-vector(mode="numeric", length=3)
  parse_D<-parse(text=Dv)
  dv[1]<- eval(parse_D[y])
  dv[2]<- eval(parse_D[(y+1)])
  dv[3]<- eval(parse_D[(y+2)])
  return (dv)
}



########### mutation #########################

########### convert to i

mv <-function (i, L) {
  Mn=0
  Mr=0
  M0=0
  for (j in 1:(2^L)) {   # genotype j
    temp1<-as.character(gt[i,])
    temp2<-as.character(gt[j,])
    if (sum(stringdist(temp1,temp2,method="hamming"))==1){  # assuming mutation could only happen once to convert to genotype i
      .o<-paste("Xn",j,sep="","*mu")
      .p<-paste("Xr",j,sep="","*mu")
      .q<-paste("X0",j,sep="","*mu")
      Mv <- c(.o, .p, .q)
      parse_M<-parse(text=Mv)
      Mun<- eval(parse_M[1])
      Mur<- eval(parse_M[2])
      Mu0<- eval(parse_M[3])
      
    } else {Mun= 0  
            Mur= 0
            Mu0= 0
            
            
    }
    Mn=Mn+Mun
    Mr=Mr+Mur
    M0=M0+Mu0      
    
  }
  Mv<-c(M0, Mn, Mr)
  return(Mv)
}


############ convert from i
Lmv <- vector(mode="character", length = 3*2^L) ### reaction vector should be in order like reactions for geontype 1 CFN group 
### and then the next...
for (i in seq(1,2^L)){
  
  Lmv[(1+3*(i-1))] <- paste("L*mu*X0", i, sep="")
  Lmv[(2+3*(i-1))] <- paste("L*mu*Xn", i, sep="")
  Lmv[(3+3*(i-1))] <- paste("L*mu*Xr", i, sep="")
}


Lmv

lmv<-function (y) {    #### y has to be 1, 4, 7... 
  lmv<-vector(mode="numeric", length=3)
  parse_LM<-parse(text=Lmv)
  lmv[1]<- eval(parse_LM[y])
  lmv[2]<- eval(parse_LM[(y+1)])
  lmv[3]<- eval(parse_LM[(y+2)])
  return (lmv)
}

######### recombination##########  will be added after mutation
########## covert to i



recomb<-function(i){
  R = 0
  gi<-gt[i,]
  for (j in 1:(2^L)) {  
    gj<-gt[j,] # l represent the loci number
    temp1<-as.character(gi)
    temp2<-as.character(gj)
    
    
    for (l in 1:L) {
      if (gi[l] != gj[l] && sum(stringdist(temp1,temp2,method="hamming"))==1) { 
        .o<-paste("u*g*f",gi[l],l,"*","Xr", j,sep="")
        dR<- eval(parse(text=.o))
      } else{dR=0}
      R=R+dR
    }
  }
  return(R)
}


########## convert from i 
Lrv <- vector(mode="character", length = 2^L) ### reaction vector should be in order like reactions for geontype 1 CFN group 

for (i in 1:(2^L)){
  vector<-vector(mode="character", length=L)
  for (x in 1:L){
    temp1<-gt[i,x]
    temp<-paste(temp1,x,sep="")
    vector[x]<-temp
  }
  
  ff<-paste(c(paste("f",vector,sep="")),collapse="+")
  
  Lrv[i]<-paste("u*g*(",F, "-", ff,")*Xr", i, sep="")
}


Lrv

lrv<-function (y) {    
  parse_LR<-parse(text=Lrv)
  lrv<- eval(parse_LR[y])
  
  return (lrv)
}

#############################
######### lysis ##############

##### define lysis proportion ls ######

Lyv<- vector(mode="character",length=2*2^L)

for (i in 1:(2^L)){
  #temp1<-paste("ls*","X0",i,sep="")
  temp2<-paste("ls*","Xn",i,sep="")
  temp3<-paste("ls*","Xr",i,sep="")
  #Lyv[(1+3*(i-1))]<-temp1
  Lyv[(1+2*(i-1))]<-temp2
  Lyv[(2+2*(i-1))]<-temp3
  
}

Lyv

lyv<-function (y) {    #### y has to be 1, 4, 7... 
  vector<-vector(mode="numeric", length=2)
  parse_Ly<-parse(text=Lyv)
  vector[1]<- eval(parse_Ly[y])
  vector[2]<- eval(parse_Ly[(y+1)])
  
  return (vector)
}


############## dynamic of fragments##############
######### fragment increase by cell lysis ########

#lyfv<-function (L) {
#fragmenttype<-vector(mode="numeric", length=2*L)
#varNames <- vector(mode="character", length=2*L)
#for (l in 1:L) {
#  tem0=0
#  tem1=0 
#  for (i in 1:(2^L)){
#    if (gt[i,][l] == 0) {
#      temp1<-paste("Xn",i, "+","Xr", i, sep="")
#      temp11 <- eval(parse(text=temp1))
#      tem0<-tem0+temp11
#    }
#    else if (gt[i,][l]==1){
#      temp2<-paste("Xn",i, "+","Xr",i, sep="")
#      temp22 <- eval(parse(text=temp2))
#      tem1<-tem1+temp22
#    }
#  }
#  tm0<-tem0*ls
#  varName0<-paste("f0",l,sep="")

#  tm1<-tem1*ls
#  varName1<-paste("f1",l,sep="")

#  fragmenttype[(1+2*(l-1)):(2+2*(l-1))]<-c(tm0,tm1)
#  varNames[(1+2*(l-1)):(2+2*(l-1))] <- c(varName0,varName1)

#}
#names(fragmenttype)<-varNames
#return(fragmenttype)
#}




######### fragment taken ########

Tfv<- vector(mode = "character", length = length(fs))
tempv1<-vector(mode="character",length=2^L)
tempv2<-vector(mode="character",length=2^L)
for (j in 1: (2^L)) {
  tempv1[j]<-xs[(2+3*(j-1))]
  
  tempv2[j]<-xs[(3+3*(j-1))]
  
}
Xn<-paste(tempv1, collapse="+")
Xr<-paste(tempv2, collapse="+")

for (i in 1: length(fs)){
  
  Tfv[i]<-paste("u*", fs[i],"*(",Xn,"+",Xr,")",sep="")
  
}

Tfv

tfv<-function (y) {    
  parse_TF<-parse(text=Tfv)
  tf<- eval(parse_TF[y])
  
  return (tf)
}





############ input fragment from cell death #######

#Gfv <- vector(mode="character", length = length(fs)) ### reaction vector should be in order like reactions for geontype 1 CFN group 

#for (i in 1: length(fs)){
#  temp<-NULL
#fi<-unlist(strsplit(fs[i],""))[2:3] ## two numbers define fragment type, the first number represents the genotype status (0 or 1)
## the second number represents the number of loci
#fi<-as.numeric(fi)
#for (j in 1:2^L){
#gj<-gt[j,] ## for any genotype, we comepare the status at each locus with status of fragment i

#if (gj[fi[2]]==fi[1]){
#  temp1<-paste("(m+s*(L-sum(gt[", j,",])))*(X0",j,"+Xn",j,"+Xr",j,")", sep="")
#  temp<-c(temp, temp1)}  else {temp1=NULL}

#}
#Gfv[i]<-paste("(",paste(temp, collapse="+"),")" ,"/L",sep="")
#}


#Gfv  


#gfv<-function (y) {    
#  parse_GF<-parse(text=Gfv)
#  gf<- eval(parse_GF[y])

#  return (gf)
#}




###### fragment degradation ############  (in GTA system, could be ignored)
#Dfv<- vector(mode = "character", length = length(fs))

#for (i in 1:length(fs)){

#  Dfv[i]<- paste("d*",fs[i],sep="")
#}


#Dfv

###############################
#####################################################
###############################################################################

genotype_reaction<-function(L) {
  N <- (4+5+7)
  temp <- vector(mode= "numeric", length= N)
  for (i in 1:(2^L)) {
    y <-1+3*(i-1)
    z <-1+2*(i-1)
    gv <-gv(y)
    dv <-dv(y)
    mv <-mv(i, L)
    lmv <-lmv(y)
    re <-recomb(i)
    lrv <-lrv(i)
    lyv<-lyv(z)
    temp1 <-c(gv[1], dv[1], mv[1], lmv[1])
    temp2 <-c(gv[2], dv[2], mv[2], lmv[2], lyv[1])
    temp3 <-c(gv[3], dv[3], mv[3], lmv[3], lyv[2], re, lrv)
    temp4 <-c (temp1, temp2, temp3)  
    temp[(1+(i-1)*N):(i*N)]<- temp4  
  }
  
  return (temp)
}




fragment_reaction<-function(L) {
  temp<- vector(mode="numeric", length=2*L)
  #temp1<-lyfv(L)
  for (i in 1:(2*L)) {
    #temp11<- temp1[i]
    temp[i]<-tfv(i)
  }
  return (temp)
}


