\name{summary.nn9res}
\alias{summary.nn9res}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary Of The Nine Next Neighbour Algorithm
%%  ~~function to do ... ~~
}
\description{A Summary of the nn9res objects.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{summary}{nn9res}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a nn9res object
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{optional parameters
%%     ~~Describe \code{\dots} here~~
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
  \item{pV}{A vector, containing the $75\%$ and the $90\%$ Quantile, entry 30, 20 and 10 of the decreasing sortet values and the maximum of the resultmatrix for the pixelwise comparison.}
  \item{nnN}{A vector, containing the $75\%$ and the $90\%$ Quantile, entry 30, 20 and 10 of the decreasing sorted values and the maximum of the resultmatrix for the nearest neighbour algorithm.}
  \item{nnNVar}{A vector, containing the minimum, entry 10, 20 and 30 of the sorted values and the $10\%$ and the $25\%$ Quantile of the resultmatrix for the nearest neighbour variance algorithm}
  \item{Varlim}{A vector, containing the $75\%$ and the $90\%$ Quantile, entry 30, 20 and 10 of the decreasing sorted values and the maximum of the resultmatrix for the nearest neighbour variance algorithm using a cutpoint.}
  \item{loc9nN}{\code{summary()} of the resultmatrix for the local 9nN}
  \item{locVar}{\code{summary()} of the resultmatrix for the local 9nNVar}

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

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
tenebrio <- nN9Var(molitor100, molitor100[1:8,100:108])
summary(tenebrio)

## The function is currently defined as
function (object, ...) 
{
    if (!class(object) == "nn9res") {
        stop("object must have class nn9res")
    }
    n <- length(object$pV)
    SpV <- sort(object$pV)
    SuMpV <- c(quantile(object$pV, 0.75), quantile(object$pV, 
        0.9), SpV[n - 29], SpV[n - 19], SpV[n - 9], max(object$pV))
    names(SuMpV)[3:6] <- c("30", "20", "10", "Max")
    S9nN <- sort(object$nnN)
    SuM9nN <- c(quantile(object$nnN, 0.75), quantile(object$nnN, 
        0.9), S9nN[n - 29], S9nN[n - 19], S9nN[n - 9], max(object$nnN))
    names(SuM9nN)[3:6] <- c("30", "20", "10", "Max")
    SVar <- sort(object$nnNVar)
    SuMVar <- c(min(object$nnNVar), SVar[10], SVar[20], SVar[30], 
        quantile(object$nnNVar, 0.1), quantile(object$nnNVar, 
            0.25))
    names(SuMVar)[1:4] <- c("Min", "10", "20", "30")
    SVarlim <- sort(object$Varlim)
    SuMVarlim <- c(quantile(object$Varlim, 0.75), quantile(object$Varlim, 
        0.9), SVarlim[n - 29], SVarlim[n - 19], SVarlim[n - 9], 
        max(object$Varlim))
    names(SuMVarlim)[3:6] <- c("30", "20", "10", "Max")
    SuMloc9nN <- summary(as.vector(object$loc9nN))
    SuMlocVar <- summary(as.vector(object$locVar))
    return(list(pV = SuMpV, nnN = SuM9nN, nnNVar = SuMVar, Varlim = SuMVarlim, 
        loc9nN = SuMloc9nN, locVar = SuMlocVar))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{univar}