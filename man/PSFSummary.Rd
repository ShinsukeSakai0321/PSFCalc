\name{PSFSummary}
\alias{PSFSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
PSFSummary
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PSFSummary(outfile="example-res.csv",infile="example.csv",Pft=c(1e-4,1e-6),Cvmu=c(0.1,0.2),Cvcov=c(0.5,0.8),Di=2400,tr=13)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
}
\examples{
library(PSFCalc)
library(LimitState)
Di=2400; tr=13
#########################################
###You can modify if necessary
Pft <- c(1e-4,1e-6)
Cvmu <- c(0.1,0.2)
Cvcov <- c(0.5,0.8)
#########################################
outfile <-paste("Di",as.character(Di),"tr",as.character(tr),"-res.csv",sep="")
infile <- system.file("example.csv",package="PSFCalc")
aa <- read.csv(infile)
PSFSummary(outfile=outfile,indata=aa,Pft=Pft,Cvmu=Cvmu,Cvcov=Cvcov,Di=Di,tr=tr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
