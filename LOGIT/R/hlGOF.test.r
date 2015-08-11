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
  hlGOF.test <- function (observed, predicted, breaks=15) {
  #H-L GOF test for logistic regression
  #observed and predicted should not have missing values and match by index
  cat('\n', 'Hosmer-Lemeshow GOF test', '\n')
  ndata <- length(predicted)
  cuts <- c(round(.75*breaks), breaks, round(1.25*breaks))
  pvals <- rep(1,cuts[3]) #p-values
  for (nCuts in cuts) {
    ip <- order(predicted)
    iq <- iQuantile(predicted, nCuts) #indices for cuts
    iqInd<- iq$index
    cat('\n','For # Cuts =',nCuts,'  # Data =',ndata,'\n')
    cat('Cut  # Total #Patterns # Resp.    # Pred.  Mean Resp. Mean Pred.','\n')  

    x2 <- 0
    ntot <- 0
    for (i in 1:nCuts) {
      if (i==1) {
        isubs <- ip[1:iqInd[2]]
      } else {
        isubs <- ip[(iqInd[i]+1):iqInd[i+1]]
      }
      nsubs <- length(isubs)
      ntot <- ntot + nsubs
      aobs <- mean(observed[isubs])
      mobs <- sum(observed[isubs])
      ncvp <- length(unique(predicted[isubs]))
      apred <- mean(predicted[isubs])
      mpred <- apred*nsubs
      x2 <- x2 + (mobs-mpred)^2/mpred + ((nsubs-mobs) - (nsubs-mpred))^2/(nsubs-mpred)
      cat(sprintf('%3d',i), sprintf('%8d', nsubs), sprintf('%8d', ncvp), sprintf('%8d', mobs), 
        sprintf('%10.2f',mpred), sprintf('%8.5f', aobs), sprintf('%8.5f',apred), '\n')
    }
    cat('Total # Data:',ndata,' Total over cuts:',ntot,'\n')
    pvals[nCuts] <- pchisq(x2,nCuts-2,lower.tail=FALSE)
    cat('Chisq:', x2, '  d.f.:', sprintf('%d',nCuts-2), ' P-value:', 
      sprintf('%8.5f', pvals[nCuts]),'\n')
  }
  cat('\n','Minimum P-value: ',sprintf('%8.5f',min(pvals)),'\n')
}
