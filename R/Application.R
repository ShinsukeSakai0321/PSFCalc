PSFEval<-function(Di=2400,tr=13,Pft=1e-4,Cvmu=0.1,Cvcov=0.5,Sa=100){
  infile <- system.file("example.csv",package="PSFCalc")
  aa <- read.csv(infile)
  psfcal<-PSFCalc::PSFControl$new(aa)
  psfcal$PSFEval(Di=Di,tr=tr,Pft=Pft,Cvmu=Cvmu,Cvcov=Cvcov,Sa=Sa)
}
PSFContour<-function(Di=2400,tr=13,Pft=1e-4,Cvmu=0.1,Cvcov=0.5,Sa=100,clevel=c(1,1e-4,1e-6),ndiv=10){
  ##############λとRt 座標上の等高線############
  infile <- system.file("example.csv",package="PSFCalc")
  aa <- read.csv(infile)
  psfcal <- PSFCalc::PSFControl$new(aa)
  psfcal$SetDi_tr(Di,tr)
  psfcal$Adjust() #Pa=MAWP, Su のSa からの計算
  psfcal$SetCv_mu_cov(Cvmu,Cvcov)
  #ndiv <- 10 #x 軸，y 軸の分割数　10 × 10 の点
  psfcal$Contour(ndiv,clevel)#等高線描画のための計算
  psfcal$DrawContour(clevel)#等高線描画
}
