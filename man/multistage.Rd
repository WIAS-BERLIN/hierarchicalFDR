\name{multistage}
\alias{multistage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  some title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multistage(pValues, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pValues}{
%%     ~~Describe \code{pValues} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (pValues, alpha = 0.05) 
{
    result <- list()
    k <- length(pValues)
    flpvalues <- numeric()
    for (i in 1:k) {
        fpValues <- pValues[[i]]
        sortfpValues <- sort(fpValues)
        fm <- length(fpValues)
        conjpValues <- numeric()
        for (i in 1:fm) {
            locpvalue <- (sortfpValues[[i]] * fm/i)
            conjpValues <- c(conjpValues, locpvalue)
        }
        flpvalues <- c(flpvalues, min(conjpValues))
    }
    ktilde <- c(1, 1)
    R <- c(k, k)
    famlevel <- BH(flpvalues, alpha = alpha, silent = TRUE)$rejected
    R[[2]] <- length(famlevel[famlevel == TRUE])
    x <- 0
    if (famlevel[[1]] == FALSE && famlevel[[2]] == FALSE) {
        x <- 1
    }
    u <- 0
    while (x == 0) {
        u <- u + 1
        if (famlevel[[1]] == TRUE) {
            fpValues <- pValues[[1]]
            sortfpValues <- sort(fpValues)
            fm <- length(fpValues)
            fu <- min(c(fm, u))
            conjpValues <- numeric()
            for (i in 1:(fm - fu + 1)) {
                locpvalue <- (sortfpValues[[(fu - 1 + i)]] * 
                  (fm - fu + 1)/i)
                conjpValues <- c(conjpValues, locpvalue)
            }
            flpvalues[[1]] <- min(conjpValues)
            ktilde[[1]] <- u
        }
        if (famlevel[[1]] == FALSE) {
            flpvalues[[1]] <- 1
        }
        if (famlevel[[2]] == TRUE) {
            fpValues <- pValues[[2]]
            sortfpValues <- sort(fpValues)
            fm <- length(fpValues)
            fu <- min(c(fm, u))
            conjpValues <- numeric()
            for (i in 1:(fm - fu + 1)) {
                locpvalue <- (sortfpValues[[(fu - 1 + i)]] * 
                  (fm - fu + 1)/i)
                conjpValues <- c(conjpValues, locpvalue)
            }
            flpvalues[[2]] <- min(conjpValues)
            ktilde[[2]] <- u
        }
        if (famlevel[[2]] == FALSE) {
            flpvalues[[2]] <- 1
        }
        famlevel <- BH(flpvalues, alpha = alpha * (R[[2]]/R[[1]]), 
            silent = TRUE)$rejected
        R[[1]] <- R[[2]]
        R[[2]] <- length(famlevel[famlevel == TRUE])
        if (famlevel[[1]] == FALSE && famlevel[[2]] == FALSE) {
            x <- 1
        }
        if (u == max(length(pValues[[1]]), length(pValues[[2]]))) {
            x <- 1
        }
    }
    gone <- sort(pValues[[1]])
    ktildo <- ktilde[[1]]
    grenzone <- gone[[ktildo]]
    rejfamone <- (pValues[[1]] <= grenzone)
    gtwo <- sort(pValues[[2]])
    ktildtwo <- ktilde[[2]]
    grenztwo <- gtwo[[ktildtwo]]
    rejfamtwo <- (pValues[[2]] <= grenztwo)
    result <- c(list(rejfamone), list(rejfamtwo))
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
