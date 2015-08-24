
iQuantile <- function (x, breaks=15) {
  #indices in x[] of percentile steps by 1/breaks
  xo <- order(x)  #sort indices
  n <- length(x)
  r <- rep(0, breaks+1)
  r[1]<- 1
  r[breaks+1]<- n
  r[2:breaks]<- round(1:(breaks-1)*n/breaks)
  return(list(index=r,cuts=x[xo[r]]))
}
