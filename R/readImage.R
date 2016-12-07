readImage <-
function(file,type,rgb=c(0.3,0.59,0.11)){# Standardparam CCIR Recommendation 601 (rounded) 0.299 0.587 0.114
if(type!="jpg"&&type!="jpeg"&&type!="png"){stop("type is no known graphic format")}
if(type=="jpg"||type=="jpeg"){
  Image <- readJPEG(file) # readJPEG from package jpeg
  }
if(type=="png"){
  Image <- readPNG(file) # readPNG from package png
  }
if(length(dim(Image))>2){ # correction if two or more matrices where read (rgb, alpha-channel)
  if(dim(Image)[3]==2){ # The second matric is the alpha-channel and will be removed
  Image <- Image[,,1]
  }else{ # If there are 3 or 4 matrices the first three where the r-, g- and b-matrices. They will be transformt to greyscale by the rgb-parameters
  Image <- round((Image[,,1]*rgb[1]+Image[,,2]*rgb[2]+Image[,,3]*rgb[3])*255)/255}}
return(Image)
}
