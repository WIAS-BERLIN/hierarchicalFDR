hierasymptkappa <- function(pValues, 
                            alpha = 0.05,
                            kappa = 0.001) {

  result <- list()
  if (is.list(pValues) == FALSE) { # no family structure, test as one single family
    fpValues <- sort(pValues)
    m <- length(pValues)
    u <- as.integer(kappa*m) + 1
    conjpValues <- numeric()
    for (i in 1:(m-u+1)) {
      locpvalue <- fpValues[[(u-1+i)]]*(m-u+1)/i
      conjpValues <- c(conjpValues, locpvalue)
    }
    fpvalue <- min(conjpValues) # family p-value
    if (fpvalue <= alpha*kappa) { # pretest of the one family p-value
      hresult <- aorc(pValues, alpha, startIDX_SUD = u, betaAdjustment = 0, silent = TRUE) #local result in the family by AORC test
      result <- list(hresult)
    }
    else { # no rejections made
      result <- list(fpValues != fpValues)
    } 
  } else {
    k <- length(pValues)
    if (1/k <= kappa) kappa <- 1/k # For larger families obtain a smaller kappa than the preset
    for (i in 1:k) {
      if (length(pValues[[i]]) == 0) {
        lresult <- list(FALSE)
        result <- c(result, lresult)
        next
      }
      fpValues <- pValues[[i]] # family p-values
      sortfpValues <- sort(fpValues) # sorted family p-values
      fm <- length(fpValues)
      fu <- as.integer(kappa*fm) # determine u_i
      if (fu < kappa*fm) fu <- fu + 1
      conjpValues <- numeric()
      for (i in 1:(fm-fu+1)) {
        locpvalue <- sortfpValues[[(fu-1+i)]]*(fm-fu+1)/i
        conjpValues <- c(conjpValues, locpvalue)
      }
      fpvalue <- min(conjpValues) # family p-value
      if (fpvalue <= alpha*kappa) { # pretest of the family p-value
        lresult <- aorc(fpValues, alpha, startIDX_SUD = fu, betaAdjustment = 0, silent = TRUE) # local test of the family by AORC SUD procedure
        lresult <- list(lresult$rejected)
      } else { # no rejection in this family is made
        lresult <- list(fpValues!=fpValues)
      } 
      result <- c(result, lresult)
    }
  }
  return(result)
}
