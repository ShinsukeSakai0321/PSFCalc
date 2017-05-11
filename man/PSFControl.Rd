\name{PSFControl}
\alias{PSFControl}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
PSFControl
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
This class treats various PSF-related supports
}
\usage{PSFControl$initialize(aa) }
\arguments{
  \item{aa}{data format such as example.csv}
%%     ~~Describe \code{x} here~~
}

\section{Public function}{
Standard reference class is R6 class \cr
Public function
\tabular{lll}{
\bold{function} \tab \bold{contents} \tab \bold{variable(s)}\cr
    initialize(aa) \tab initializes the object \tab aa:data format such as example.csv\cr
    PSF() \tab Executes reliability analysis for current aa-data and returns PSF list.\tab \cr
    lambda_Rt2s_tmm(lambda, Rt) \tab converts (lambda,Rt) to (s,tmm) \tab lambda,Rt \cr
    s_tmm2lambda_Rt(s, tmm) \tab converts (s,tmm) to (lambda,Rt)\tab s,tmm \cr
    ShellRt(lambda, Rt) \tab returns Pf+PSF list at (lambda,Rt) \tab lambda,Rt \cr
    MAWP() \tab calculates MAWP from aa data and returns the value \tab \cr
    SetPa(xval) \tab sets xval to the variable Pamu \tab xval: value to be set for Pa \cr
    SetDi_tr(Di, tr) \tab sets (Di,tr) to (Dimu,trmu)\tab Di,tr:values to be set \cr
    CalcSu(gamma) \tab calculates Su from Sa value for the safety factor gamma and returns the value \tab gamma:safety factor \cr
    SetSu() \tab Calculates Su value from aa using CalcSu function and sets it to Sumu \tab \cr
    GetSu() \tab gets Sumu value \tab \cr
    SetSa(x) \tab sets x to Sa \tab x:value to be set \cr
    GetMu() \tab gets current mean value list of objet metal \tab \cr
    Shell() \tab calculates shell parameter from aa-data and returns it \tab \cr
    Rt() \tab calculates Rt value after inspection time and returns it \tab \cr
    Pf() \tab returns Pf value of current object metal \tab \cr
    Adjust() \tab sets value of 100 to Sa, calculates Su and sets it to Sumu, calculates MAWP and sets it to Pumu \tab \cr
    Out(outfile) \tab write current resuts related to object metal to outfile \tab outfile: name of output file \cr
    Contour(nn, clevel) \tab draws contour line on the lambda-Rt figure \tab nn:number of grids for evaluation,clevel:list of contour levels \cr
    DrawContour(clevel) \tab draws contour line accordint to current private$x,private$y,private$z \tab clevel:list of contour levels \cr
    Target(lambda, Pft) \tab returns Rt value with target Pft value for lambda value \tab lambda,Pft \cr
    SetCv_mu_cov(mu, cov) \tab sets (mu,cov) to (Cvmu,Cvcov) \tab mu,cov \cr
    PSFEval(indata) \tab returns data.frame of results for the condition written in indata \tab indata:condition for calculation \cr

 }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
#If it is necessary, install package "LimitState" as follows
#install.packages("devtools")
#devtools::install_github("ShinsukeSakai0321/LimitState")
library(LimitState)
library(PSFCalc)
infile <- system.file("example.csv",package="PSFCalc")
fdata <- read.csv(infile)
####Checking for one case ###
Di <- 500; tr <- 10
Cvmu <- 0.2; Cvcov<- 0.5
######################
aa <- read.csv(infile)
psfcal <- PSFCalc::PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust(Sa=100)  #Pa=MAWP and evaluates Su from Sa
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
psfcal$PSF()
#Correct answer
#Pf,PSF1,PSF2,....
#[1] 0.0004616347 3.7410414030 1.0114521033 1.0000000000 1.0000000000 1.0000000000
#[7] 1.0109882772 1.0000000000 1.0011929134
###########################
# Calculation of Shell & Rt
###########################
Di <- 500; tr <- 10
infile <- system.file("example.csv",package="PSFCalc")
fdata <- read.csv(infile)
aa <- read.csv(infile)
psfcal <- PSFCalc::PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
gamma <- 4
psfcal$SetSa(100)
cat("MAWP=",psfcal$MAWP(),",Su=",psfcal$CalcSu(gamma))
cat("λ=",psfcal$Shell(),",Rt=",psfcal$Rt())
############### Drawing contour on lamgda-Rt figure#######################
Di <- 2400; tr <- 13
Cvmu <- 0.1; Cvcov<- 0.5
######################
infile <- system.file("example.csv",package="PSFCalc")
fdata <- read.csv(infile)
aa <- read.csv(infile)
psfcal <- PSFCalc::PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust(Sa=100)  #Pa=MAWP, Calculates Su from Sa
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
clevel <- c(1,1e-4,1e-6)# contour levles for Pf
ndiv <- 10  #division of x and y axis
psfcal$Contour(ndiv,clevel)
psfcal$DrawContour(clevel)
#############Evaluates Rt for specific lambda satisfying target reliability #######
Di <- 2400; tr <- 13
Pft <- 1e-6; Cvmu <- 0.1; Cvcov<- 0.5
#####################################
library(GA)
infile <- system.file("example.csv",package="PSFCalc")
fdata <- read.csv(infile)
aa <- read.csv(infile)
psfcal <- PSFCalc::PSFControl$new(aa)
psfcal$SetDi_tr(Di,tr)
psfcal$Adjust(Sa=100)  #Pa=MAWP, Calculates Su from Sa
psfcal$SetCv_mu_cov(Cvmu,Cvcov)
lambda <- 5;
Rt <- psfcal$Target(lambda,Pft)
c(Di,tr,lambda,Rt,Cvmu,Cvcov,psfcal$ShellRt(lambda,Rt))
############################
###Candidate of answer
# [1] 2.400000e+03 1.300000e+01 5.000000e+00 5.172694e-01 1.000000e-01 5.000000e-01
# [7] 9.979141e-07 5.960800e+00 1.073792e+00 1.000000e+00 1.000000e+00 1.000000e+00
#[13] 1.028481e+00 1.000000e+00 1.002066e+00
###########################
# Example of PSFEval
###########################
infile <- system.file("example.csv",package="PSFCalc")
indata <- read.csv(infile)
psfcal <- PSFControl$new(indata)
rescalc <- psfcal$PSFEval(Di=2400,tr=13,Pft=1e-4,Cvmu=0.1,Cvcov=0.5,Sa=100)
rescalc
}
\keyword{datasets}
