\name{complete}
\alias{complete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Completing The Resultmatrices In \code{nN9Var}
%%  ~~function to do ... ~~
}
\description{When \code{nN9Var} compares two images of the same size every shift appears twice (shift A to B or shift Bto A). The algorithm computes only one of the two shifts. \code{complete} completes the resultsmatrix. This is an intern function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
complete(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an incomplete resultmatrix of \code{nN9Var}}
%%     ~~Describe \code{x} here~~
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \code{complete} returns the completed Resultmatrix \code{x}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Lars Koppers
%%  ~~who you are~~
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
result <- matrix(c(1,2,3,3,2,6:15, rep(0,10)), 5, 5, byrow = TRUE)
complete(result)

## The function is currently defined as
function (x) 
{
    n <- nrow(x)
    m <- ncol(x)
    for (i in (floor(n/2 + 1) + 1):n) x[i, ] <- x[n - i + 2, 
        c(1, m:2)]
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}

