summary.nn9res <-
function(object,...){
if(!class(object)=="nn9res"){stop("object must have class nn9res")}
n <- length(object$pV)
    SpV <- sort(object$pV)
  SuMpV <-  c(quantile(object$pV,0.75),quantile(object$pV,0.9),SpV[n-29],SpV[n-19],SpV[n-9],max(object$pV))
names(SuMpV)[3:6] <- c("30","20","10","Max")
    S9nN <- sort(object$nnN)
  SuM9nN <-  c(quantile(object$nnN,0.75),quantile(object$nnN,0.9),S9nN[n-29],S9nN[n-19],S9nN[n-9],max(object$nnN))
names(SuM9nN)[3:6] <- c("30","20","10","Max")
    SVar <- sort(object$nnNVar)
  SuMVar <- c(min(object$nnNVar),SVar[10],SVar[20],SVar[30],quantile(object$nnNVar,0.1),quantile(object$nnNVar,0.25))
names(SuMVar)[1:4] <- c("Min","10","20","30")
    SVarlim <- sort(object$Varlim)
  SuMVarlim <-  c(quantile(object$Varlim,0.75),quantile(object$Varlim,0.9),SVarlim[n-29],SVarlim[n-19],SVarlim[n-9],max(object$Varlim))
names(SuMVarlim)[3:6] <- c("30","20","10","Max")
  SuMloc9nN <- summary(as.vector(object$loc9nN))
  SuMlocVar <- summary(as.vector(object$locVar))
return(list("pV"=SuMpV,"nnN"=SuM9nN,"nnNVar"=SuMVar,"Varlim"=SuMVarlim,"loc9nN"=SuMloc9nN,"locVar"=SuMlocVar))
}
