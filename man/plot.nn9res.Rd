\name{plot.nn9res}
\alias{plot.nn9res}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting Interesting Shifts Or Areas
%%  ~~function to do ... ~~
}
\description{Plots shifts or areas of interest in the original Image.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{plot}{nn9res}(x, Image, CP, result = "pV", equal = FALSE, lower = FALSE, asp = 1, col = "black", border = "transparent", axes = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a class nn9res Object containing resultmatrices from
    \code{nN9Var} or a matrix.
%%     ~~Describe \code{x} here~~
}
  \item{Image}{the imagematrix which was used for \code{x} in
    \code{nN9Var}.
%%     ~~Describe \code{Image} here~~
}
  \item{CP}{cutpoint selects the plotted pixel.
%%     ~~Describe \code{CP} here~~
}
  \item{result}{caracter, the resultmatrix which should be plotted, will
    be ignored if \code{x} is a matrix.
%%     ~~Describe \code{result} here~~
}
  \item{equal}{if \code{equal = TRUE} only values equal to the cutpoint
    will be plotted 
%%     ~~Describe \code{equal} here~~
}
  \item{lower}{if \code{lower = TRUE} values below the cutpoint will be
    plotted, values above otherwise.
%%     ~~Describe \code{lower} here~~
}
  \item{asp}{numeric, giving the aspect ratio y/x, see \code{\link{plot.window}}
%%     ~~Describe \code{asp} here~~
}
  \item{col}{The color of the marked pixel
%%     ~~Describe \code{col} here~~
}
  \item{border}{optional, the color of the border from the marks
%%     ~~Describe \code{border} here~~
}
  \item{axes}{logical, should axes be drawn?
%%     ~~Describe \code{axes} here~~
}
  \item{\dots}{additional graphic parameters for \code{\link{plot.window}} and \code{\link{title}} can be added.
%%     ~~Describe \code{\dots} here~~
}
}
\author{Lars Koppers
%%  ~~who you are~~
}
\note{Most result matrices plotting shifts. A point in the graphic marks
  a shift of the top left edge of the second image by this point. The
  \code{loc} matrices marking not shifts but interesting blocks for any shift.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{plot}},\code{\link{points}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
tenebrio <- nN9Var(molitor100, molitor100[1:8,100:108])
par(mfrow = c(1,2))
plot(tenebrio, Image= molitor100, result = "loc9nN", CP=1)
plot(tenebrio, Image= molitor100, result = "nnNVar", CP=1e-03, lower=TRUE)

## The function is currently defined as
function (x, Image, CP, result = "pV", equal = FALSE, lower = FALSE, 
    asp = 1, col = "black", border = "transparent", axes = FALSE, 
    ...) 
{
    if (!any(result == c("pV", "nnN", "nnNVar", "Varlim", "loc9nN", 
        "locVar"))) {
        stop("result must be the name of a resultmatrix")
    }
    if (!class(x) == "nn9res" && !class(x) == "matrix") {
        stop("x must have class nn9res")
    }
    if (class(x) == "matrix") {
        if (!all(dim(x) == dim(Image))) {
            stop("the resultmatrix and Image must have the same size")
        }
    }
    else {
        if (!all(dim(x[[result]]) == dim(Image))) {
            stop("the resultmatrix and Image must have the same size")
        }
    }
    if (!is.logical(equal)) {
        stop("equal must be logical")
    }
    if (!is.logical(lower)) {
        stop("lower must be logical")
    }
    if (!is.numeric(CP)) {
        stop("cutpoint must be numeric")
    }
    Dim <- dim(Image)
    if (class(x) == "matrix") {
        if (equal == TRUE) {
            Z <- which(x == CP, arr.ind = TRUE)
        }
        else {
            if (lower == TRUE) {
                Z <- which(x < CP, arr.ind = TRUE)
            }
            else {
                Z <- which(x > CP, arr.ind = TRUE)
            }
        }
    }
    else {
        if (equal == TRUE) {
            Z <- which(x[[result]] == CP, arr.ind = TRUE)
        }
        else {
            if (lower == TRUE) {
                Z <- which(x[[result]] < CP, arr.ind = TRUE)
            }
            else {
                Z <- which(x[[result]] > CP, arr.ind = TRUE)
            }
        }
    }
    Z <- cbind(Z[, 2], Z[, 1])
    Z[, 2] <- Dim[1] - Z[, 2] + 1
    plot.new()
    plot.window(xlim = c(0, Dim[2]), ylim = c(0, Dim[1]), asp = asp, 
        ...)
    title(...)
    if (axes) {
        axis(1)
        aT <- axTicks(side = 2, axp = NULL, usr = c(0, Dim[1]), 
            log = NULL, nintLog = NULL)
        axis(2, at = aT, labels = rev(aT))
    }
    COL <- as.numeric(Image)
    Igrid <- expand.grid(Dim[1]:1, 1:Dim[2])
    rect(xleft = (Igrid[, 2] - 1), ybottom = (Igrid[, 1] - 1), 
        xright = (Igrid[, 2]), ytop = (Igrid[, 1]), border = "transparent", 
        col = gray(COL))
    rect(xleft = (Z[, 1] - 1), ybottom = (Z[, 2] - 1), xright = (Z[, 
        1]), ytop = (Z[, 2]), border = border, col = col)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
