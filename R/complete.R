complete <-
function(x){     # Completes the nN9 mtrices
n <- nrow(x)
m <- ncol(x)
for(i in (floor(n/2+1)+1):n) # for the second half of the matrix
x[i,] <- x[n-i+2,c(1,m:2)]   # take the line mirrowed at floor(n/2+1)+1 and reverse it +1 
return(x)
}
