\name{hierBH}
\alias{hierBH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hierBH(pValues, alpha = 0.05)
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
        interpValues <- numeric()
        for (i in 1:fm) {
            locpvalue <- (sortfpValues[[i]] * fm/i)
            interpValues <- c(interpValues, locpvalue)
        }
        flpvalues <- c(flpvalues, min(interpValues))
    }
    famlevel <- BH(flpvalues, alpha, silent = TRUE)$rejected
    for (i in 1:length(famlevel)) {
        R <- length(famlevel[famlevel[[i]] == TRUE])
        if (famlevel[[i]] == TRUE) {
            lresult <- BH(pValues[[i]], alpha = (R/k * alpha), 
                silent = TRUE)$rejected
        }
        else {
            lresult <- (pValues[[i]] != pValues[[i]])
        }
        result <- c(result, list(lresult))
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
