plot.nn9res <-
function(x,Image,CP,result="pV",equal=FALSE,lower=FALSE,asp=1,col="black", border="transparent",axes=FALSE,...){
if(!any(result==c("pV","nnN","nnNVar","Varlim","loc9nN","locVar"))){stop("result must be the name of a resultmatrix")}
if(!class(x)=="nn9res" && !class(x)=="matrix"){stop("x must have class nn9res")}

if(class(x)=="matrix"){
  if(!all(dim(x)==dim(Image))){stop("the resultmatrix and Image must have the same size")}
}else{
  if(!all(dim(x[[result]])==dim(Image))){stop("the resultmatrix and Image must have the same size")}}
if(!is.logical(equal)){stop("equal must be logical")}
if(!is.logical(lower)){stop("lower must be logical")}
if(!is.numeric(CP)){stop("cutpoint must be numeric")}
Dim <- dim(Image)
if(class(x)=="matrix"){
  if(equal==TRUE){
    Z <- which(x==CP,arr.ind=TRUE) # if equal=TRUE only x=Cutpoint will be plotted
  }else{
      if(lower==TRUE){
        Z <- which(x<CP,arr.ind=TRUE) # if equal=FALSE and lower=TRUE x<Cutpoint will be plotted
      }else{
        Z <- which(x>CP,arr.ind=TRUE)}} # if equal=FALSE and lower=FALSE x>Cutpoint will be plotted
}else{
  if(equal==TRUE){
    Z <- which(x[[result]]==CP,arr.ind=TRUE) # if equal=TRUE only x=Cutpoint will be plotted
  }else{
      if(lower==TRUE){
        Z <- which(x[[result]]<CP,arr.ind=TRUE) # if equal=FALSE and lower=TRUE x<Cutpoint will be plotted
      }else{
        Z <- which(x[[result]]>CP,arr.ind=TRUE)}} # if equal=FALSE and lower=FALSE x>Cutpoint will be plotted
}
Z <- cbind(Z[,2],Z[,1])    # modification vrom Matrix (row,col) to coordinates (x,y)
Z[,2] <- Dim[1]-Z[,2]+1
plot.new()
plot.window(xlim=c(0,Dim[2]),ylim=c(0,Dim[1]),asp=asp,...)
title(...)
if(axes){
axis(1)
aT <- axTicks(side=2, axp = NULL, usr = c(0,Dim[1]), log = NULL, nintLog = NULL)
axis(2, at=aT, labels=rev(aT))}
COL <- as.numeric(Image)
Igrid <- expand.grid(Dim[1]:1,1:Dim[2])
rect(xleft=(Igrid[,2]-1), ybottom=(Igrid[,1]-1), xright=(Igrid[,2]), ytop=(Igrid[,1]), border="transparent", col=gray(COL))
## rasterImage(Image,0,0,Dim[2],Dim[1])
rect(xleft=(Z[,1]-1), ybottom=(Z[,2]-1), xright=(Z[,1]), ytop=(Z[,2]), border=border, col=col)
}
