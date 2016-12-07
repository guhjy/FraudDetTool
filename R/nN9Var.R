nN9Var <-
function(x,y=x, CP=3.844675e-05, Nullshift=!(all(dim(x)==dim(y))&&all(x==y))){                  # x imagematrix, y optionale second image, CP for the Variance
if(!all(dim(x)>=dim(y))){stop("number of rows and columbs in y must be smaller or equal to the number in x")}
if(!is.logical(Nullshift)){stop("Nullshift must be logical")}
if(!is.numeric(CP)){stop("cutpoint must be numeric")}
Dimx <- dim(x)                               # Dimension for the image(s)
Dimy <- dim(y)
pV  <- matrix(NA,Dimx[1],Dimx[2])            # resultmatrix for pixelwise comparison. pV[i,j] = number of idetical pixel for shift (i-1,j-1)
nnN  <- matrix(NA,Dimx[1],Dimx[2])           # resultmatrix for nine next neighbour. nnN[i,j] = number of idetical 3x3 blocks for shift (i-1,j-1)
nnNVar <- matrix(NA,Dimx[1],Dimx[2])         # resultmatrix for 9nN sum of variances. nnNVar[i,j] = sum of 3x3 block-variances for shift (i-1,j-1)
Varlim <- matrix(NA,Dimx[1],Dimx[2])         # resultmatrix for numbers of variances below CP. Varlim[i,j] = number of 3x3 block-variances below the cutpoint CP for shift (i-1,j-1)
if(all(dim(x)==dim(y))){
   loc9nN <- matrix(0,Dimx[1],Dimx[2])       # resultmatrix for the location of identical 3x3 blocks
                                             # loc9nN[i,j] = number of shifts in which x[i:(i+2),j,(j+2)] is identical to the shiftet y
   locVar <- matrix(0,Dimx[1],Dimx[2])       # resultmatrix for number of shifts in which the 3x3 block-variance is below the cutpoint
}else{        
   loc9nN <- matrix(0,Dimx[1]+Dimy[1],Dimx[2]+Dimy[2])     # if x neq y it is nessesariy to create bigger resultmatrices and reduce they at the end of the algorithm
   locVar <- matrix(0,Dimx[1]+Dimy[1],Dimx[2]+Dimy[2])}    
X  <- cbind(x,x)                             # quadruple x
X  <- rbind(X,X)                             # now y can compare with x by any shift

if(all(dim(x)==dim(y))){                     # if x=y every shift is duplicated, so adequate to compute anloy the half of the results and complete() later
n <- floor(Dimx[1]/2+1)
}else{
n <- Dimx[1]}

   for(i in 1:n){                                    # loop for the rows
   for(j in 1:(Dimx[2])){                            # loop for the columbs
      Z  <- y-X[i:(i+(Dimy[1]-1)),j:(j+(Dimy[2]-1))] # subtraction of the matrices. shift of y by (i-1,j-1) the unused values of X will be ignored
      ZZ <- cbind(Z,Z[,1:2])                         # n+2 variation of Z to build 3x3 blocks for all pixel in x
      ZZ <- rbind(ZZ,ZZ[1:2,])                       
      Z2 <- abs(Z) + abs(ZZ[2:(Dimy[1]+1),1:(Dimy[2])]) + abs(ZZ[3:(Dimy[1]+2),1:(Dimy[2])]) + abs(ZZ[1:(Dimy[1]),2:(Dimy[2]+1)]) + abs(ZZ[2:(Dimy[1]+1),2:(Dimy[2]+1)]) + abs(ZZ[3:(Dimy[1]+2),2:(Dimy[2]+1)]) + abs(ZZ[1:(Dimy[1]),3:(Dimy[2]+2)]) + abs(ZZ[2:(Dimy[1]+1),3:(Dimy[2]+2)]) + abs(ZZ[3:(Dimy[1]+2),3:(Dimy[2]+2)])                                         # each cell consist the sum of all Z-values [i:(i+2),j,(j+2)]
      T <- c(Z,ZZ[2:(Dimy[1]+1),1:(Dimy[2])],ZZ[3:(Dimy[1]+2),1:(Dimy[2])],ZZ[1:(Dimy[1]),2:(Dimy[2]+1)],ZZ[2:(Dimy[1]+1),2:(Dimy[2]+1)],ZZ[3:(Dimy[1]+2),2:(Dimy[2]+1)],ZZ[1:(Dimy[1]),3:(Dimy[2]+2)],ZZ[2:(Dimy[1]+1),3:(Dimy[2]+2)],ZZ[3:(Dimy[1]+2),3:(Dimy[2]+2)])   
      T2 <- array(T,dim = c(Dimy,9))                 # creating a n,m,9 array for all the 3x3 blocks
      VarM2 <- ((T2[,,1]^2+T2[,,2]^2+T2[,,3]^2+T2[,,4]^2+T2[,,5]^2+T2[,,6]^2+T2[,,7]^2+T2[,,8]^2+T2[,,9]^2)/9 - ((T2[,,1]+T2[,,2]+T2[,,3]+T2[,,4]+T2[,,5]+T2[,,6]+T2[,,7]+T2[,,8]+T2[,,9])/9)^2)*9/8            # calculate all 3x3 blockvariances
      pVM <- Z==0                                    # if Z[i,j]=0, this pixelpair is identical for this shift
      nnNM <- Z2==0                                  # if Z2[i,j]=0, the 3x3 block [i:(i+2),j,(j+2)] is idetical for this shift
      pV[i,j] <- sum(pVM)                            # number of the identical pixel for shift (i-1,j-1)
      nnN[i,j] <- sum(nnNM)                          # number of the identical 3x3 blocks for the shift (i-1,j-1)
      nnNVar[i,j] <- sum(VarM2)                      # sum of the 3x3 block variances for shift (i-1,j-1)
      Varlim[i,j] <- sum(VarM2<CP)                   # number of 3x3 block variances below CP for shift (i-1,j-1)
         if(all(dim(x)==dim(y))){
            loc9nN <- loc9nN + (Z2==0)               # for each identical 3x3 block [i:(i+2),j,(j+2)] loc9nN (i,j) would be increased by 1.
            locVar <- locVar + (VarM2<CP)            # for each 3x3 block [i:(i+2),j,(j+2)] Variance below the cutpoint (i,j) would be increased by 1.
         }else{ # the same for x neq y
            loc9nN[i:(i+nrow(y)-1),j:(j+ncol(y)-1)] <- loc9nN[i:(i+nrow(y)-1),j:(j+ncol(y)-1)] + (Z2==0)
            locVar[i:(i+nrow(y)-1),j:(j+ncol(y)-1)] <- locVar[i:(i+nrow(y)-1),j:(j+ncol(y)-1)] + (VarM2<CP)
}}}

if(Nullshift==FALSE){ # Deleting of all null-shifts
   pV[1,1]     <- 0 
   nnN[1,1]    <- 0
   nnNVar[1,1] <- max(nnNVar,na.rm=TRUE) # replace with max, because we search for small variances
   Varlim[1,1] <- 0}

if(all(Dimx==Dimy)){ # completing the resultmatrices if dim(x)==dim(y)
   pV     <- complete(pV)
   nnN    <- complete(nnN)
   nnNVar <- complete(nnNVar)
   Varlim <- complete(Varlim)
}else{ # otherwise reducing the loc-matrices
   loc9nN[,1:ncol(y)] <- loc9nN[,1:ncol(y)] + loc9nN[,(Dimx[2]+1):(Dimx[2]+ncol(y))]
   locVar[,1:ncol(y)] <- locVar[,1:ncol(y)] + locVar[,(Dimx[2]+1):(Dimx[2]+ncol(y))]
   loc9nN[1:nrow(y),] <- loc9nN[1:nrow(y),] + loc9nN[(Dimx[1]+1):(Dimx[1]+nrow(y)),]
   locVar[1:nrow(y),] <- locVar[1:nrow(y),] + locVar[(Dimx[1]+1):(Dimx[1]+nrow(y)),]
   loc9nN <- loc9nN[1:Dimx[1],1:Dimx[2]]
   locVar <- locVar[1:Dimx[1],1:Dimx[2]]}

results <- list("pV"=pV,"nnN"=nnN,"nnNVar"=nnNVar,"Varlim"=Varlim,"loc9nN"=loc9nN,"locVar"=locVar,"CP"=CP)
class(results) <- "nn9res"
return(results)
}
