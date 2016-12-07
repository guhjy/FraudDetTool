\name{readImage}
\alias{readImage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reading And Greyscaling Of Images
%%  ~~function to do ... ~~
}
\description{This function reads .jpg and .png images and transformes
  them into greyscaled images if nessesary. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readImage(file, type, rgb = c(0.3, 0.59, 0.11))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{the name of the file which the image are to be read from.
%%     ~~Describe \code{file} here~~
}
  \item{type}{the type of the image: png, jpg or jpeg
%%     ~~Describe \code{type} here~~
}
  \item{rgb}{the paramater of the greyscale transformation. By default
    r = 0.3, g = 0.59, b = 0.11
%%     ~~Describe \code{rgb} here~~
}
}
\details{
The alpha channel of the images will be ignored.
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns an imagematrix. Each Cell represents one pixel.
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

## The function is currently defined as
function (file, type, rgb = c(0.3, 0.59, 0.11)) 
{
    if (type != "jpg" && type != "jpeg" && type != "png") {
        stop("type is no known graphic format")
    }
    if (type == "jpg" || type == "jpeg") {
        Image <- readJPEG(file)
    }
    if (type == "png") {
        Image <- readPNG(file)
    }
    if (length(dim(Image)) > 2) {
        if (dim(Image)[3] == 2) {
            Image <- Image[, , 1]
        }
        else {
            Image <- round((Image[, , 1] * rgb[1] + Image[, , 
                2] * rgb[2] + Image[, , 3] * rgb[3]) * 255)/255
        }
    }
    return(Image)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{file}
% __ONLY ONE__ keyword per line
