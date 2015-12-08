#' Bootrapping zur Bestimmung  der Standardfehler und 95\%-Konfidenzintervalle
#'
#' @examples
#' \dontrun{
#' ame.boot(test.data, 10, "lm", y ~ alter + geschl + bildung, "geschl")
#' ame.boot(test.data, 10, "dtt", y ~ alter+ bildung, "geschl")
#' ame.boot(test.data, 10, "dt", y ~ alter + geschl + bildung, "bildung")
#' ame.boot(test.data, 10, "dt", y ~ alter + geschl, "alter", seq(20,60,1))
#' }
#'
#' @export
ame.boot <- function(data.name, rep=100, meth="dt", func, var.name, fromtoby) {
  if (is.factor(data.name[[var.name]]) & length(levels(data.name[[var.name]])>2)) {
    ame.boot <- replicate(rep, ame(sample_n(data.name, nrow(data.name), replace=T), meth, func, var.name, fromtoby)[1])
    #mean <- Reduce("+", ame.boot) / length(ame.boot)
    mean.vec <- apply(simplify2array(ame.boot), 1, mean)
    mean <- matrix(0, length(mean.vec), length(mean.vec))  
    mean[lower.tri(mean, diag=FALSE)] <- mean.vec    
    se.vec <- apply(simplify2array(ame.boot), 1, sd)
    se <- matrix(0, length(se.vec), length(se.vec))  
    se[lower.tri(se, diag=FALSE)] <- se.vec
    p <- round(2*pt(-abs(mean/se), df=rep-1), 3)
    output <- list(ame=mean, se=se, p=p)    
  }
  else{
    ame.boot <- replicate(rep, unlist(ame(sample_n(data.name, nrow(data.name), replace=T), meth, func, var.name, fromtoby)[1]))
    mean <- round(mean(ame.boot), 3)
    se <- round(sd(ame.boot), 3)
    error <- round((qnorm(0.975) * se), 3)
    p <- round(2*pt(-abs(mean/se), df=rep-1), 3) # 2*pnorm(-abs(mean/se))
    output <- c(ame=mean, se=se, p=p, lci=mean-error, uci=mean+error)
  }
  return(output)
}