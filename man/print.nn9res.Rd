\name{print.nn9res}
\alias{print.nn9res}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Printing nn9res Objects
%%  ~~function to do ... ~~
}
\description{Resultmatrices from nn9res objects are typicaly to big for
  printing. \code{print.nn9res} only prints \code{str(x)}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{print}{nn9res}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a nn9res object
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{optional parameters
%%     ~~Describe \code{\dots} here~~
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Lars Koppers
%%  ~~who you are~~
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{print}}, \code{\link{str}} 
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

## The function is currently defined as
function (x, ...) 
{
    if (!class(x) == "nn9res") {
        print("x is no nn9res object")
    }
    str(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}

