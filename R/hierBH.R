hierBH <- function(pValues,
                   alpha = 0.05) { 
  # The hierarchical testing procedure developed by Bogomolov
  result<-list()
  k <- length(pValues)
  flpvalues <- numeric() # family level p-values
  for (i in 1:k) {
    fpValues <- pValues[[i]]
    sortfpValues <- sort(fpValues)
    fm <- length(fpValues)
    interpValues <- numeric()
    for (i in 1:fm) {
      locpvalue <- sortfpValues[[i]]*fm/i
      interpValues <- c(interpValues, locpvalue)
    }
    flpvalues <- c(flpvalues, min(interpValues))
  }
  famlevel <- BH(flpvalues, alpha, silent = TRUE)$rejected # BH test on the family level
  for (i in 1:length(famlevel)) {
    R <- length(famlevel[famlevel[[i]] == TRUE])
    if (famlevel[[i]] == TRUE) {
      lresult <- BH(pValues[[i]], alpha = R/k*alpha, silent = TRUE)$rejected # BH test on the local p-values within a family
    } else {
      lresult <- pValues[[i]] != pValues[[i]]
    }
    result <- c(result, list(lresult))
  }
  return(result)
}
