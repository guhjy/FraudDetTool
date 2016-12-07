\name{nN9Var}
\alias{nN9Var}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nearest Neighbour Algorithm To Find Manipulated Data
%%  ~~function to do ... ~~
}
\description{Nearest neighbour algorithm to find identical areas.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nN9Var(x, y = x, CP = 3.844675e-05, Nullshift = !(all(dim(x) == dim(y)) && all(x == y)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an imagematrix
%%     ~~Describe \code{x} here~~
}
  \item{y}{optional. A second image to compare with \code{x}. The
    number of rows and columbs must be smaller or equal to \code{x} 
%%     ~~Describe \code{y} here~~
}
  \item{CP}{numeric. A cutpoint which defines the small block-variances.
%%     ~~Describe \code{CP} here~~
}
  \item{Nullshift}{should the nullshift \code{[1,1]} be deleted? Deletes
    the nullshift if \code{Nullshift = FALSE}. Deletes by default only
    for images of the same size.
%%     ~~Describe \code{Nullshift} here~~
}
}
\details{
The Algorithm compares every possible shift between code{x} and
\code{y}. parts of \code{y} which are not lying over \code{x} would be
shiftet to the other side of the imagematrix ((n+1,m+1) -> (1,1)). For
every shift three different types of simillarity will be considered:

1. Pixelwise comparison: Every overlaping pixelpair will be controlled
if the shades of grey are the same.
2. Nearest neighbor: Counts only similaritys if the pixelpair and the
eight neighboring pixelpairs are pairwise identical.
3. Nearest neighbor variances: Looks for the variance of the pairwise
differences.


  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a list of following items. Every resultmatrix has the
  dimension of \code{x}. A cell of the first four resultmatrices shows
  the value for the shift by it's position minus one, e.g. cell (4,5)
  shows the result for the shift by 3 rows and 4 columbs. The last two
  location-matrices show the the number of similaritys for the pixel
  over all shifts.
  \item{pV}{resultmatrix of pixelwise comparison. Counts the number of
    identical pixelpairs for the shifts.}
  \item{nnN}{resultmatrix of the nearest neighbor comparison. Counts
    the number of identical 3 x 3 neighborhoods.}
  \item{nnNVar}{resultmatrix of the nearest neighbor variance
    comparison. For every shift the sum of the neighborhood-variances
    will be stored.}
  \item{Varlim}{resultmatrix for the number of block-variances below the
  cutpoint \code{CP}.}
  \item{loc9nN}{localisationmatrix for the identical nearest neighbor
    areas. Counts the number of shifts in which the marked pixel and its
  neighborhood is identical.}
  \item{locVar}{localisationmatrix for the low variances areas. Counts the
    number of shifts in which the neighbourhoodvariance is below the cutpoint.}
  \item{CP}{The Cutpoint which defines the low variances.}
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

## The function is currently defined as
function (x, y = x, CP = 3.844675e-05, Nullshift = !(all(dim(x) == 
    dim(y)) && all(x == y))) 
{
    if (!all(dim(x) >= dim(y))) {
        stop("number of rows and columbs in y must be smaller or equal to the number in x")
    }
    if (!is.logical(Nullshift)) {
        stop("Nullshift must be logical")
    }
    if (!is.numeric(CP)) {
        stop("cutpoint must be numeric")
    }
    Dimx <- dim(x)
    Dimy <- dim(y)
    pV <- matrix(NA, Dimx[1], Dimx[2])
    nnN <- matrix(NA, Dimx[1], Dimx[2])
    nnNVar <- matrix(NA, Dimx[1], Dimx[2])
    Varlim <- matrix(NA, Dimx[1], Dimx[2])
    if (all(dim(x) == dim(y))) {
        loc9nN <- matrix(0, Dimx[1], Dimx[2])
        locVar <- matrix(0, Dimx[1], Dimx[2])
    }
    else {
        loc9nN <- matrix(0, Dimx[1] + Dimy[1], Dimx[2] + Dimy[2])
        locVar <- matrix(0, Dimx[1] + Dimy[1], Dimx[2] + Dimy[2])
    }
    X <- cbind(x, x)
    X <- rbind(X, X)
    if (all(dim(x) == dim(y))) {
        n <- floor(Dimx[1]/2 + 1)
    }
    else {
        n <- Dimx[1]
    }
    for (i in 1:n) {
        for (j in 1:(Dimx[2])) {
            Z <- y - X[i:(i + (Dimy[1] - 1)), j:(j + (Dimy[2] - 
                1))]
            ZZ <- cbind(Z, Z[, 1:2])
            ZZ <- rbind(ZZ, ZZ[1:2, ])
            Z2 <- abs(Z) + abs(ZZ[2:(Dimy[1] + 1), 1:(Dimy[2])]) + 
                abs(ZZ[3:(Dimy[1] + 2), 1:(Dimy[2])]) + abs(ZZ[1:(Dimy[1]), 
                2:(Dimy[2] + 1)]) + abs(ZZ[2:(Dimy[1] + 1), 2:(Dimy[2] + 
                1)]) + abs(ZZ[3:(Dimy[1] + 2), 2:(Dimy[2] + 1)]) + 
                abs(ZZ[1:(Dimy[1]), 3:(Dimy[2] + 2)]) + abs(ZZ[2:(Dimy[1] + 
                1), 3:(Dimy[2] + 2)]) + abs(ZZ[3:(Dimy[1] + 2), 
                3:(Dimy[2] + 2)])
            T <- c(Z, ZZ[2:(Dimy[1] + 1), 1:(Dimy[2])], ZZ[3:(Dimy[1] + 
                2), 1:(Dimy[2])], ZZ[1:(Dimy[1]), 2:(Dimy[2] + 
                1)], ZZ[2:(Dimy[1] + 1), 2:(Dimy[2] + 1)], ZZ[3:(Dimy[1] + 
                2), 2:(Dimy[2] + 1)], ZZ[1:(Dimy[1]), 3:(Dimy[2] + 
                2)], ZZ[2:(Dimy[1] + 1), 3:(Dimy[2] + 2)], ZZ[3:(Dimy[1] + 
                2), 3:(Dimy[2] + 2)])
            T2 <- array(T, dim = c(Dimy, 9))
            VarM2 <- ((T2[, , 1]^2 + T2[, , 2]^2 + T2[, , 3]^2 + 
                T2[, , 4]^2 + T2[, , 5]^2 + T2[, , 6]^2 + T2[, 
                , 7]^2 + T2[, , 8]^2 + T2[, , 9]^2)/9 - ((T2[, 
                , 1] + T2[, , 2] + T2[, , 3] + T2[, , 4] + T2[, 
                , 5] + T2[, , 6] + T2[, , 7] + T2[, , 8] + T2[, 
                , 9])/9)^2) * 9/8
            pVM <- Z == 0
            nnNM <- Z2 == 0
            pV[i, j] <- sum(pVM)
            nnN[i, j] <- sum(nnNM)
            nnNVar[i, j] <- sum(VarM2)
            Varlim[i, j] <- sum(VarM2 < CP)
            if (all(dim(x) == dim(y))) {
                loc9nN <- loc9nN + (Z2 == 0)
                locVar <- locVar + (VarM2 < CP)
            }
            else {
                loc9nN[i:(i + nrow(y) - 1), j:(j + ncol(y) - 
                  1)] <- loc9nN[i:(i + nrow(y) - 1), j:(j + ncol(y) - 
                  1)] + (Z2 == 0)
                locVar[i:(i + nrow(y) - 1), j:(j + ncol(y) - 
                  1)] <- locVar[i:(i + nrow(y) - 1), j:(j + ncol(y) - 
                  1)] + (VarM2 < CP)
            }
        }
    }
    if (Nullshift == FALSE) {
        pV[1, 1] <- 0
        nnN[1, 1] <- 0
        nnNVar[1, 1] <- max(nnNVar, na.rm = TRUE)
        Varlim[1, 1] <- 0
    }
    if (all(Dimx == Dimy)) {
        pV <- complete(pV)
        nnN <- complete(nnN)
        nnNVar <- complete(nnNVar)
        Varlim <- complete(Varlim)
    }
    else {
        loc9nN[, 1:ncol(y)] <- loc9nN[, 1:ncol(y)] + loc9nN[, 
            (Dimx[2] + 1):(Dimx[2] + ncol(y))]
        locVar[, 1:ncol(y)] <- locVar[, 1:ncol(y)] + locVar[, 
            (Dimx[2] + 1):(Dimx[2] + ncol(y))]
        loc9nN[1:nrow(y), ] <- loc9nN[1:nrow(y), ] + loc9nN[(Dimx[1] + 
            1):(Dimx[1] + nrow(y)), ]
        locVar[1:nrow(y), ] <- locVar[1:nrow(y), ] + locVar[(Dimx[1] + 
            1):(Dimx[1] + nrow(y)), ]
        loc9nN <- loc9nN[1:Dimx[1], 1:Dimx[2]]
        locVar <- locVar[1:Dimx[1], 1:Dimx[2]]
    }
    results <- list(pV = pV, nnN = nnN, nnNVar = nnNVar, Varlim = Varlim, 
        loc9nN = loc9nN, locVar = locVar, CP = CP)
    class(results) <- "nn9res"
    return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}

