shifts <-
function(x, CP, lower=FALSE){
if(!is.logical(lower)){stop("lower must be logical")}
if(!is.numeric(CP)){stop("cutpoint must be numeric")}
  if(lower==TRUE){Inter <- which(x<CP,arr.ind=TRUE,useNames=FALSE) # Which shifts have values below the cutpoint
  }else {Inter <- which(x>CP,arr.ind=TRUE,useNames=FALSE)} # Which shifts have values above the cutpoint
Z <- cbind(Inter,x[Inter])
colnames(Z) <- c("row","col","value")
return(Z) # returns row, col and the value for the shift
}
