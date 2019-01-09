PSFEval<-function(Di=2400,tr=13,Pft=1e-4,Cvmu=0.1,Cvcov=0.5,Sa=100){
  infile <- system.file("example.csv",package="PSFCalc")
  aa <- read.csv(infile)
  aa$Dimu<-Di; aa$trmu<-tr; aa$Cvmu<-Cvmu; aa$Cvcov<-Cvcov;
  psfcal<-PSFCalc::PSFControl$new(aa)
  psfcal$Adjust(Sa=Sa)
  psfcal$PSFEval(Di=Di,tr=tr,Pft=Pft,Cvmu=Cvmu,Cvcov=Cvcov,Sa=Sa)
}
PSFContour<-function(Di=2400,tr=13,Cvmu=0.1,Cvcov=0.5,Sa=100,clevel=c(1,1e-4,1e-6),ndiv=10){
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
ShellRt<-function(lambda=5,Rt=0.5,Di=2400,tr=13,Cvmu=0.1,Cvcov=0.5,Sa=100){
  infile <- system.file("example.csv",package="PSFCalc")
  aa <- read.csv(infile)
  aa$Dimu<-Di; aa$trmu<-tr; aa$Cvmu<-Cvmu; aa$Cvcov<-Cvcov;
  psfcal<-PSFCalc::PSFControl$new(aa)
  psfcal$Adjust(Sa=Sa)
  psfcal$ShellRt(lambda,Rt)
}
Target <- function(lambda=5,Pft=1e-4,Di=2400,tr=13,Cvmu=0.1,Cvcov=0.5,Sa=100){
  infile <- system.file("example.csv",package="PSFCalc")
  aa <- read.csv(infile)
  aa$Dimu<-Di; aa$trmu<-tr; aa$Cvmu<-Cvmu; aa$Cvcov<-Cvcov;
  psfcal<-PSFCalc::PSFControl$new(aa)
  psfcal$Adjust(Sa)  #Pa=MAWP, SuのSaからの計算
  psfcal$Adjust(Sa=Sa)
  psfcal$Target(lambda,Pft)
}
PSFTable<-function(tr=15,Pft=1e-4,Cv=0.1,CvCOV=0.5){
  cc<-list(PSFCv=0,PSFPa=0,PSFSu=0)
  #1
  if(Pft==1e-4 && Cv==0.1 && CvCOV==0.5){
    if(tr>=4 && tr<16){
      cc<-list(Cv=4.4,Pa=1.2,Su=1.0)
    }
    if(tr>=16 && tr<30){
      cc<-list(Cv=4.4,Pa=1.6,Su=1.1)
    }
    if(tr>=30){
      cc<-list(Cv=1.7,Pa=1.6,Su=1.1)
    }
    return(cc)
  }
  #2
  if(Pft==1e-6 && Cv==0.1 && CvCOV==0.5){
    if(tr>=6 && tr<14){
      cc<-list(Cv=6.2,Pa=1.1,Su=1.0)
    }
    if(tr>=14 && tr<32){
      cc<-list(Cv=6.2,Pa=1.9,Su=1.1)
    }
    if(tr>=32){
      cc<-list(Cv=2.3,Pa=1.9,Su=1.1)
    }
    return(cc)
  }
  #3
  if(Pft==1e-4 && Cv==0.2 && CvCOV==0.5){
    if(tr>=8 && tr<32){
      cc<-list(Cv=4.4,Pa=1.2,Su=1.0)
    }
    if(tr>=32){
      cc<-list(Cv=4.4,Pa=1.6,Su=1.1)
    }
    return(cc)
  }
  #4
  if(Pft==1e-6 && Cv==0.2 && CvCOV==0.5){
    if(tr>=8 && tr<32){
      cc<-list(Cv=6.2,Pa=1.3,Su=1.1)
    }
    if(tr>=32){
      cc<-list(Cv=6.2,Pa=1.9,Su=1.1)
    }
    return(cc)
  }
  #5
  if(Pft==1e-4 && Cv==0.1 && CvCOV==0.8){
    if(tr>=6 && tr<24){
      cc<-list(Cv=6.4,Pa=1.1,Su=1.0)
    }
    if(tr>=24){
      cc<-list(Cv=6.4,Pa=1.6,Su=1.1)
    }
    return(cc)
  }
  #6
  if(Pft==1e-6 && Cv==0.1 && CvCOV==0.8){
    if(tr>=8 && tr<28){
      cc<-list(Cv=9.3,Pa=1.3,Su=1.1)
    }
    if(tr>=28 && tr<44){
      cc<-list(Cv=9.3,Pa=1.9,Su=1.1)
    }
    if(tr>=44){
      cc<-list(Cv=6.0,Pa=1.9,Su=1.1)
    }
    return(cc)
  }
  #7
  if(Pft==1e-4 && Cv==0.2 && CvCOV==0.8){
    if(tr>=12 && tr<30){
      cc<-list(Cv=6.4,Pa=1.1,Su=1.0)
    }
    if(tr>=30){
      cc<-list(Cv=6.4,Pa=1.2,Su=1.1)
    }
    return(cc)
  }
  #8
  if(Pft==1e-6 && Cv==0.2 && CvCOV==0.8){
    if(tr>=18 && tr<44){
      cc<-list(Cv=9.3,Pa=1.1,Su=1.0)
    }
    if(tr>=44){
      cc<-list(Cv=9.3,Pa=1.3,Su=1.1)
    }
    return(cc)
  }

  cat("not available\n")
  cat("Pft=",Pft,":Cv=",Cv,":CvCOV=",CvCOV,"\n")
}
