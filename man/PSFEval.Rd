\name{PSFEval}
\alias{PSFEval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate PSF table for specific coditions with target reliability
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate PSF table for the case with example case modifying the values specified by arguments. Shell parameter is varied from 1 to 15.
}
\usage{
PSFEval(Di=2400,tr=13,Pft=1e-4,Cvmu=0.1,Cvcov=0.5,Sa=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Di}{
%%     ~~Describe \code{x} here~~
}
\item{tr}{}
\item{Pft}{}
\item{Cvmu}{}
\item{Cvcov}{}
\item{Sa}{Allowable stress for the material}
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
