\name{FraudDetTools-package}
\alias{FraudDetTools-package}
\alias{FraudDetTools}
\docType{package}
\title{Tools For Detection Of Image Manipulation}
\description{this package contains an algorithm which compares shifts of
  two images (or of one with itself) and search for similar
  areas. Pixelwise Comparison, nearest neighbor comparison and nearest
  neighbor variance comparison are possible. There also some functions
  to analyse and visualise the results.
}
\details{This packege allows to compare one image-matrix with itself or two
  different image-matrices. If they are not greyscaled they have to convert
  to greyscale by \code{readImage()} first. The analysis is exectuted by
  \code{nN9Var}. The result of the analysis is a class nn9res object,
  containing a list of matrices, all  in the
  dimension of the first image-matrix. Each matrix is the result of a
  different search-algorithm. To evaluate the results \code{summary()} or \code{shifts()} is
  used. It is possible to visualize the results with \code{plot}
\tabular{ll}{
Package: \tab nnNTest\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-02-25\cr
License: \tab GPL-2 or later\cr
}
}
\author{Lars Koppers}
% \references{
% ~~ Literature or other references for background information ~~
% }
% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
% ~~ the R documentation directory ~~
\keyword{ package }
% \seealso{
% ~~ Optional links to other man pages, e.g. ~~
% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
% }
% \examples{
% ~~ simple examples of the most important functions ~~
% }
