\name{PSFContour}
\alias{PSFContour}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw contour for specific PSF condition for PSF
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PSFContour(Di = 2400, tr = 13, Pft = 1e-04, Cvmu = 0.1, Cvcov = 0.5, Sa = 100, clevel = c(1, 1e-04, 1e-06), ndiv = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Di}{
%%     ~~Describe \code{Di} here~~
}
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{Pft}{
%%     ~~Describe \code{Pft} here~~
}
  \item{Cvmu}{
%%     ~~Describe \code{Cvmu} here~~
}
  \item{Cvcov}{
%%     ~~Describe \code{Cvcov} here~~
}
  \item{Sa}{
%%     ~~Describe \code{Sa} here~~
}
  \item{clevel}{
Target reliabilities to be drawn
}
  \item{ndiv}{
ndiv*ndiv points on the Rt-lambda graph are evaluated for drawing countour
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Di = 2400, tr = 13, Pft = 1e-04, Cvmu = 0.1, Cvcov = 0.5,
    Sa = 100, clevel = c(1, 1e-04, 1e-06), ndiv = 10)
{
    infile <- system.file("example.csv", package = "PSFCalc")
    aa <- read.csv(infile)
    psfcal <- PSFCalc::PSFControl$new(aa)
    psfcal$SetDi_tr(Di, tr)
    psfcal$Adjust()
    psfcal$SetCv_mu_cov(Cvmu, Cvcov)
    psfcal$Contour(ndiv, clevel)
    psfcal$DrawContour(clevel)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
