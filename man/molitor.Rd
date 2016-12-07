\name{molitor}
\alias{molitor100}
\alias{molitor50}
\alias{molitor0}
\docType{data}
\title{Tenebrio Molitor Test Image
%%   ~~ data name/kind ... ~~
}
\description{An image matrix of a detail image in three different jpeg qualities. On the left side
  there is a very light and monoton area, other parts of the image are structured.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(molitor100)}
\format{
  The format is:
 num [1:250, 1:400] 
}
\details{This data can be used to understand the procedure of
  \code{nN9Var}. There are no image manipulation in that picture, but
  there is a very light and uniform area. The algorithm finds many
  similar areas in the light part and no ones in the structured part of
  the image. For the low quality versions of the image the number of identical areas rises.
%%  ~~ If necessary, more details than the __description__ above ~~
}
% \source{
% %%  ~~ reference to a publication or URL from which the data were obtained ~~
% }
% \references{
% %%  ~~ possibly secondary sources and usages ~~
% }
\examples{
data(molitor100)
str(molitor100)
plot(c(0,400),c(0,250),xlab="",ylab="", pch="") 
rasterImage(molitor100,0,0,400,250)
}

\keyword{datasets}
