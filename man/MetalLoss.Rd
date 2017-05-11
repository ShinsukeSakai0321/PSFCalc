\name{MetalLoss}
\alias{MetalLoss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
MetalLoss
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Class definition for metal loss analysis
}
\usage{
MetalLoss$new(name,n,Mu,sigmmaX,dist,ttime)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{name of analysis}
  \item{n}{number of random variables}
  \item{Mu}{list of mean values of variables}
  \item{sigmmaX}{list of std. values of variables}
  \item{dist}{list of distribution type}
  \item{ttime}{inspection time}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\section{Public function}{
%%  ~~ If necessary, more details than the description above ~~
Standard reference class is R6 class \cr
Public function
\tabular{lll}{
\bold{function} \tab \bold{contents} \tab \bold{variable(s)}\cr
initialize(name,n,Mu,sigmmaX,dist,ttime) \tab initializes the object \tab name:name of the subject,n:number of variables,Mu:list of mean of variables,sigmmaX:list of std. of variables,dist:list of name of variable distributions,ttime:inspection time\cr
Calc() \tab executes reliability analysis \cr
PSF() \tab returns list of PSF values \cr
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
This is inheritted class of LSFM. As for Class LSFM, see LimitState::LSFM.
}
\examples{
#If it is necessary, install package "LimitState" as follows
#install.packages("devtools")
#devtools::install_github("ShinsukeSakai0321/LimitState")
library(LimitState)
infile <- system.file("example.csv",package="PSFCalc")
fdata <- read.csv(infile)
muX <-  c(fdata$Cvmu, fdata$Pamu, fdata$trmu, fdata$nomu, fdata$Dimu, fdata$Sumu, fdata$smu, fdata$tmmmu)
COVX <- c(fdata$Cvcov,fdata$Pacov,fdata$trcov,fdata$nocov,fdata$Dicov,fdata$Sucov,fdata$scov,fdata$tmmcov)
sigmmaX <- muX * COVX
dist <- c("gumbel", "gumbel", "normal", "normal" ,"normal" ,"normal" ,"normal" ,"normal")
metal <- PSFCalc::MetalLoss$new("MetalLoss",8,muX,sigmmaX,dist,fdata$ttime)
metal$Calc()
Pf <- metal$GetPOF()
cat("Pf=",Pf,"\n")
#correct answer=2.144935e-07
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
