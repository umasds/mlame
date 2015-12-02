\name{ame}
\alias{ame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ame(data.name, meth = "dt", func, var.name, fromtoby = NULL, plotTree = FALSE, plotPV = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.name}{
%%     ~~Describe \code{data.name} here~~
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
  \item{plotTree}{
%%     ~~Describe \code{plotTree} here~~
}
  \item{plotPV}{
%%     ~~Describe \code{plotPV} here~~
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
function (data.name, meth = "dt", func, var.name, fromtoby = NULL, 
    plotTree = FALSE, plotPV = FALSE) 
{
    if (meth == "lm") {
        fit <- glm(func, family = gaussian(link = "identity"), 
            data = data.name)
        sfit <- summary(fit)
    }
    if (meth == "dt") {
        fit <- rpart(func, cp = 1e-04, method = "anova", data = data.name)
        if (plotTree == "TRUE") {
            plot(fit, uniform = TRUE, main = "Pruned Regression Tree")
            text(fit, cex = 0.6)
        }
        sfit <- fit
    }
    if (meth == "dtt") {
        t0.fit <- rpart(func, method = "anova", data = subset(data.name, 
            data.name[var.name] == 0))
        p0.fit <- prune(t0.fit, cp = t0.fit$cptable[which.min(t0.fit$cptable[, 
            "xerror"]), "CP"])
        t1.fit <- rpart(func, method = "anova", data = subset(data.name, 
            data.name[var.name] == 1))
        p1.fit <- prune(t1.fit, cp = t1.fit$cptable[which.min(t1.fit$cptable[, 
            "xerror"]), "CP"])
        sfit <- list(p0.fit, p1.fit)
    }
    if (meth == "rf") {
        fit <- randomForest(func, data = data.name)
        sfit <- importance(fit)
    }
    if (meth == "rftt") {
        p0.fit <- randomForest(func, method = "anova", data = subset(data.name, 
            data.name[var.name] == 0))
        p1.fit <- randomForest(func, method = "anova", data = subset(data.name, 
            data.name[var.name] == 1))
        sfit <- list(importance(p0.fit), importance(p1.fit))
    }
    if (nrow(unique(data.name[var.name])) == 2) {
        if (meth == "dtt" | meth == "rftt") {
            y.pred.fit.1 <- predict(p0.fit, data.name)
            y.pred.fit.2 <- predict(p1.fit, data.name)
        }
        else {
            data.1 <- data.name
            data.1[var.name] <- 0
            data.2 <- data.name
            data.2[var.name] <- 1
            y.pred.fit.1 <- predict(fit, data.1)
            y.pred.fit.2 <- predict(fit, data.2)
        }
        ame <- mean(y.pred.fit.1) - mean(y.pred.fit.2)
        output <- list(ame, sfit)
    }
    else {
        if (is.null(fromtoby)) {
            print("Sequence from to by needed")
        }
        else {
            data <- data.name
            pv <- NULL
            counter <- 1
            steps <- fromtoby
            for (i in steps) {
                data[var.name] <- i
                pv[counter] <- mean(predict(fit, data))
                counter <- sum(counter, 1)
            }
            slope <- tail(pv, length(pv) - 1) - head(pv, length(pv) - 
                1)
            if (plotPV == "TRUE") {
                plot(steps, pv, type = "l")
            }
            ame <- mean(slope)
            output <- list(ame, pv, sfit)
        }
    }
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
