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
  ame.boot <- replicate(rep, unlist(ame(sample_n(data.name, nrow(data.name), replace=T), meth, func, var.name, fromtoby)[1]))
  mean <- round(mean(ame.boot), 3)
  se <- round(sd(ame.boot), 3)
  error <- round((qnorm(0.975) * se), 3)
  p <- round(2*pnorm(-abs(mean/se)), 3)
  output <- c(ame=mean, se=se, p=p, lci=mean-error, uci=mean+error)
  return(output)
}
