\name{ame.boot}
\alias{ame.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ame.boot(data.name, rep = 100, meth = "dt", func, var.name, fromtoby)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.name}{
%%     ~~Describe \code{data.name} here~~
}
  \item{rep}{
%%     ~~Describe \code{rep} here~~
}
  \item{meth}{
%%     ~~Describe \code{meth} here~~
}
  \item{func}{
%%     ~~Describe \code{func} here~~
}
  \item{var.name}{
%%     ~~Describe \code{var.name} here~~
}
  \item{fromtoby}{
%%     ~~Describe \code{fromtoby} here~~
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
function (data.name, rep = 100, meth = "dt", func, var.name, 
    fromtoby) 
{
    ame.boot <- replicate(rep, unlist(ame(sample_n(data.name, 
        nrow(data.name), replace = T), meth, func, var.name, 
        fromtoby)[1]))
    mean <- mean(ame.boot)
    se <- sd(ame.boot)
    error <- qnorm(0.975) * se
    output <- c(ame = mean, se = se, lci = mean - error, uci = mean + 
        error)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
