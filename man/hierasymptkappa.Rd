\name{hierasymptkappa}
\alias{hierasymptkappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hierasymptkappa(pValues, alpha = 0.05, kappa = 0.001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pValues}{
%%     ~~Describe \code{pValues} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{kappa}{
%%     ~~Describe \code{kappa} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (pValues, alpha = 0.05, kappa = 0.001) 
{
    result <- list()
    if (is.list(pValues) == FALSE) {
        fpValues <- sort(pValues)
        m <- length(pValues)
        u <- as.integer(kappa * m) + 1
        conjpValues <- numeric()
        for (i in 1:(m - u + 1)) {
            locpvalue <- (fpValues[[(u - 1 + i)]] * (m - u + 
                1)/i)
            conjpValues <- c(conjpValues, locpvalue)
        }
        fpvalue <- min(conjpValues)
        if (fpvalue <= alpha * kappa) {
            hresult <- aorc(pValues, alpha, startIDX_SUD = u, 
                betaAdjustment = 0, silent = TRUE)
            result <- list(hresult)
        }
        else {
            result <- (list(fpValues != fpValues))
        }
    }
    else {
        k <- length(pValues)
        if ((1/k) <= kappa) {
            kappa <- (1/k)
        }
        for (i in 1:k) {
            if (length(pValues[[i]]) == 0) {
                lresult <- list(FALSE)
                result <- c(result, lresult)
                next
            }
            fpValues <- pValues[[i]]
            sortfpValues <- sort(fpValues)
            fm <- length(fpValues)
            fu <- as.integer(kappa * fm)
            if (fu < (kappa * fm)) {
                fu <- fu + 1
            }
            conjpValues <- numeric()
            for (i in 1:(fm - fu + 1)) {
                locpvalue <- (sortfpValues[[(fu - 1 + i)]] * 
                  (fm - fu + 1)/i)
                conjpValues <- c(conjpValues, locpvalue)
            }
            fpvalue <- min(conjpValues)
            if (fpvalue <= (alpha * kappa)) {
                lresult <- aorc(fpValues, alpha, startIDX_SUD = fu, 
                  betaAdjustment = 0, silent = TRUE)
                lresult <- list(lresult$rejected)
            }
            else {
                lresult <- list(fpValues != fpValues)
            }
            result <- c(result, lresult)
        }
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
