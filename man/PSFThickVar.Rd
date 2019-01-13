\name{PSFThickVar}
\alias{PSFThickVar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculate PSF table for variouts thickness values and write the result to specified file
}
\description{
Calculate PSF table for variouts thickness values and write the result to specified file
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PSFThickVar(outfile = "example-res.csv", Pft = 1e-04, Cvmu = 0.1, Cvcov = 0.5, Di = 2400, tr = c(10, 20), Sa = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
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
  \item{Di}{
%%     ~~Describe \code{Di} here~~
}
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{Sa}{
%%     ~~Describe \code{Sa} here~~
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
function (outfile = "example-res.csv", Pft = 1e-04, Cvmu = 0.1,
    Cvcov = 0.5, Di = 2400, tr = c(10, 20), Sa = 100)
{
    i <- 0
    for (itr in 1:length(tr)) {
        i <- i + 1
        if (i == 1) {
            iflag <- FALSE
        }
        else {
            iflag <- TRUE
        }
        infile <- system.file("example.csv", package = "PSFCalc")
        aa <- read.csv(infile)
        aa$Dimu <- Di
        aa$trmu <- tr[itr]
        aa$Cvmu <- Cvmu
        aa$Cvcov <- Cvcov
        psfcal <- PSFCalc::PSFControl$new(aa)
        psfcal$Adjust(Sa = Sa)
        rescalc <- psfcal$PSFEval(Di = Di, tr = tr[itr], Pft = Pft,
            Cvmu = Cvmu, Cvcov = Cvcov, Sa = Sa)
        options(warn = -1)
        write.table(rescalc, outfile, sep = ",", quote = F, row.names = F,
            append = iflag)
        options(warn = 0)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
