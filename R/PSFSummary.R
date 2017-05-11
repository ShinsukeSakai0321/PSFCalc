PSFSummary <- function(outfile="example-res.csv",indata,Pft=c(1e-4,1e-6),Cvmu=c(0.1,0.2),Cvcov=c(0.5,0.8),Di=2400,tr=13,Sa=100){
  i <- 0
  for(iCvmu in 1:length(Cvmu)){
    for(iCvcov in 1:length(Cvcov)){
      for(iPft in 1:length(Pft)){
        i <- i+1; if(i==1){iflag <- FALSE}else{iflag <- TRUE}
        psfcal <- PSFControl$new(indata)
        rescalc <- psfcal$PSFEval(Di=Di,tr=tr,Pft=Pft[iPft],Cvmu=Cvmu[iCvmu],Cvcov=Cvcov[iCvcov],Sa=Sa)
        write.table(rescalc,outfile,sep=",",quote=F,row.names=F,append=iflag)
      }
    }
  }
}
