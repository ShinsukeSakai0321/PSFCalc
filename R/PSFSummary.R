PSFSummary <- function(outfile="example-res.csv",indata,Pft=c(1e-4,1e-6),Cvmu=c(0.1,0.2),Cvcov=c(0.5,0.8),Di=2400,tr=13,Sa=100){
  i <- 0
  for(iCvmu in 1:length(Cvmu)){
    for(iCvcov in 1:length(Cvcov)){
      for(iPft in 1:length(Pft)){
        i <- i+1; if(i==1){iflag <- FALSE}else{iflag <- TRUE}
        psfcal <- PSFControl$new(indata)
        rescalc <- psfcal$PSFEval(Di=Di,tr=tr,Pft=Pft[iPft],Cvmu=Cvmu[iCvmu],Cvcov=Cvcov[iCvcov],Sa=Sa)
        options(warn=-1)
        write.table(rescalc,outfile,sep=",",quote=F,row.names=F,append=iflag)
        options(warn=0)
      }
    }
  }
}
PSFThickVar <- function(outfile="example-res.csv",Pft=1e-4,Cvmu=0.1,Cvcov=0.5,Di=2400,tr=c(10,20),Sa=100){
  i <- 0
  for(itr in 1:length(tr)){
    i <- i+1; if(i==1){iflag <- FALSE}else{iflag <- TRUE}
    infile <- system.file("example.csv",package="PSFCalc")
    aa <- read.csv(infile)
    aa$Dimu<-Di; aa$trmu<-tr[itr]; aa$Cvmu<-Cvmu; aa$Cvcov<-Cvcov;
    psfcal<-PSFCalc::PSFControl$new(aa)
    psfcal$Adjust(Sa=Sa)
    rescalc <- psfcal$PSFEval(Di=Di,tr=tr[itr],Pft=Pft,Cvmu=Cvmu,Cvcov=Cvcov,Sa=Sa)
    options(warn=-1) #Disappear warning message
    write.table(rescalc,outfile,sep=",",quote=F,row.names=F,append=iflag)
    options(warn=0)
  }
}

