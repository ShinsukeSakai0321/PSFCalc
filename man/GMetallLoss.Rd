\name{GMetalLoss}
\alias{GMetalLoss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
GMetalLoss
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Class definition of limit state function for metal loss analysis
}
\usage{
GMetalLoss$new(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of variables
%%     ~~Describe \code{x} here~~
}
}
\section{Public function}{
%%  ~~ If necessary, more details than the description above ~~
Standard reference class is R6 class \cr
Public function
\tabular{lll}{
\bold{function} \tab \bold{contents} \tab \bold{variable(s)}\cr
initialize(n) \tab initializes the object \tab n:number of variables\cr
gcalc() \tab calculates g-value at evaluation point \tab \cr
Setttime(t) \tab sets inspection time \tab t:inspection time(year)\cr
dGdXcalc() \tab returns differential vector list at the evaluation point \tab \cr
 \cr

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
Shinsuke Sakai
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
This is inheritted class of Lbase. As for Class Lbase, see LimitState::Lbase.
}
\examples{
library(LimitState)
lim <- LimitState::Lbase
lim <- PSFCalc::GMetalLoss$new(n)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
