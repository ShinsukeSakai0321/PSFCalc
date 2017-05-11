#
infile <- "example.csv"
outfile <- "example-res.csv"
source("Distribution.r")
source("Limit.r")
source("PSFlib.r")

#### 一ケースのチェック
Di <- 500; tr <- 10
Cvmu <- 0.2; Cvcov<- 0.5
######################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
psfcal$PSF()
gamma <- 4
cat("MAWP=",psfcal$MAWP(),",Su=",psfcal$CalcSu(gamma))
cat("λ=",psfcal$Shell(),",Rt=",psfcal$Rt())
psfcal$Out(outfile)

############### シェルパラメータとRtの二次元空間の等高線 #######################
Di <- 2400; tr <- 13
Cvmu <- 0.2; Cvcov<- 0.8
######################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
clevel <- c(1,1e-4,1e-6)　　#等高線を表示させるレベル
ndiv <- 10  #x軸，y軸の分割数
psfcal$Contour(ndiv,clevel)
png("Di2400tr13Cvmu0_2Cvcov0_8.png",width=500,height=400)
psfcal$DrawContour(clevel)
dev.off()

###############特定のλに対する目標信頼性を満足するRtの計算(GA利用)#####
Di <- 500; tr <- 10
Pft <- 1e-6; Cvmu <- 0.2; Cvcov<- 0.5
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
lambda <- 2;
Rt <- psfcal$Target(lambda,Pft)
c(Di,tr,lambda,Rt,Cvmu,Cvcov,psfcal$ShellRt(lambda,Rt))

######特定のλ、Rtに対する計算###
Di <- 500; tr <- 10
Cvmu <- 0.2; Cvcov<- 0.5
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
lambda <- 3.0; Rt <- 0.3
psfcal$ShellRt(lambda,Rt)

###### 複数ケースの計算 #####
outfile <- "Di10000tr50-res2.csv"
lambda <- c(1,2,4,6,8,10,13,15)
n <- length(lambda)
Rt <- numeric(n)
####################
Di <- 10000; tr <- 50
#####################################
Pft <- 1e-4; Cvmu <- 0.1; Cvcov<- 0.5
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=F)
#####################################
Pft <- 1e-6; Cvmu <- 0.1; Cvcov<- 0.5
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=T)
#####################################
Pft <- 1e-4; Cvmu <- 0.2; Cvcov<- 0.5
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=T)
#####################################
Pft <- 1e-6; Cvmu <- 0.2; Cvcov<- 0.5
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=T)
#####################################
Pft <- 1e-4; Cvmu <- 0.1; Cvcov<- 0.8
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=T)
#####################################
Pft <- 1e-6; Cvmu <- 0.1; Cvcov<- 0.8
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=T)
#####################################
Pft <- 1e-4; Cvmu <- 0.2; Cvcov<- 0.8
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=T)
#####################################
Pft <- 1e-6; Cvmu <- 0.2; Cvcov<- 0.8
#####################################
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust()  #Pa=MAWP, SuのSaからの計算
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
for(i in 1:n){
  Rt[i] <- psfcal$Target(lambda[i],Pft)
}
i <- 1
dd <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
for(i in 2:n){
  da <- c(Di,tr,lambda[i],Rt[i],Cvmu,Cvcov,psfcal$ShellRt(lambda[i],Rt[i]))
  dd <- rbind(dd,da)
}
colnames(dd) <- c("Di","tr","λ","Rt","Cv","Cvcov","Pf","PSF-Cv", "PSF-Pa", "PSF-tr", "PSF-no", "PSF-Di", "PSF-Su", "PSF-s", "PSF-tmm")
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=T)

###### 複数ケースの計算 new#####
source("Distribution.r")
source("Limit.r")
source("PSFlib.r")
infile="example.csv"
#outfile="Di2400tr13test-res.csv"
Di=2400; tr=13
#########################################
### 解析条件は必要に応じて，増加削減のこと
Pft <- c(1e-4,1e-6)
Cvmu <- c(0.1,0.2)
Cvcov <- c(0.5,0.8)
#########################################
outfile <-paste("Di",as.character(Di),"tr",as.character(tr),"-res.csv",sep="")
i <- 0
for(iCvmu in 1:length(Cvmu)){
  for(iCvcov in 1:length(Cvcov)){
    for(iPft in 1:length(Pft)){
      i <- i+1; if(i==1){iflag <- FALSE}else{iflag <- TRUE}
      indata <- data.frame(Di=Di,tr=tr,Pft=Pft[iPft],Cvmu=Cvmu[iCvmu],Cvcov=Cvcov[iCvcov])
      aa <- read.csv(infile)
      psfcal <- PSFControl$new(aa)
      write.table(psfcal$PSFEval(indata),outfile,sep=",",quote=F,row.names=F,append=iflag)
    }
  }
}

###check
iCvmu=1;iCvcov=1;iPft=1
indata <- data.frame(Di=Di,tr=tr,Pft=Pft[iPft],Cvmu=Cvmu[iCvmu],Cvcov=Cvcov[iCvcov])
aa <- read.csv(infile)
psfcal <- PSFControl$new(aa)
dd <-psfcal$PSFEval(indata)
write.table(dd,outfile,sep=",",quote=F,row.names=F,append=F)
