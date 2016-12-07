\name{shifts}
\alias{shifts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Printing The Relevant Results Of \code{nN9Var}
%%  ~~function to do ... ~~
}
\description{Returns the shifts over or under a cutpoint. With this
  function it is possible to print the interesting shifts pointet out by
  the different algorithm of \code{nN9Var}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shifts(x, CP, lower = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{one of the resultmatrices of \code{nN9Var}
%%     ~~Describe \code{x} here~~
}
  \item{CP}{a cutpoint for Printing the results.
%%     ~~Describe \code{CP} here~~
}
  \item{lower}{logical, if \code{TRUE} the shifts below the cutpoint
    will be printed (e.G. for Var). 
%%     ~~Describe \code{lower} here~~
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{\code{shifts} returns a matrix containing three columns:
  \code{row} and \code{col} give the position of the shift, \code{quan} the result of the algorithrem for the shift.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
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

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
tenebrio <- nN9Var(molitor100, molitor100[1:8,100:108])
shifts(tenebrio$nnN, 47, lower = FALSE)

## The function is currently defined as
function (x, CP, lower = FALSE) 
{
    if (!is.logical(lower)) {
        stop("lower must be logical")
    }
    if (!is.numeric(CP)) {
        stop("cutpoint must be numeric")
    }
    if (lower == TRUE) {
        Inter <- which(x < CP, arr.ind = TRUE, useNames = FALSE)
    }
    else {
        Inter <- which(x > CP, arr.ind = TRUE, useNames = FALSE)
    }
    Z <- cbind(Inter, x[Inter])
    colnames(Z) <- c("row", "col", "value")
    return(Z)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}