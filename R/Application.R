PSFEval<-function(Di=2400,tr=13,Pft=1e-4,Cvmu=0.1,Cvcov=0.5,Sa=100){
  infile <- system.file("example.csv",package="PSFCalc")
  aa <- read.csv(infile)
  psfcal<-PSFCalc::PSFControl$new(aa)
  psfcal$PSFEval(Di=Di,tr=tr,Pft=Pft,Cvmu=Cvmu,Cvcov=Cvcov,Sa=Sa)
}
