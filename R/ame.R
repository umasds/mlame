#' Average marginal effects for ML algorithms
#'
#' \code{ame} estimates average marginal effects (AME) for models fitted using
#' different machine learning (ML) algorithms. AMEs can be estimated for
#' continuous and binary independent variables / features on continuous
#' dependent variables / response.
#'
#' @param data.name a data frame containing the variables in the model.
#' @param meth ML algorithm to be used; currently \code{"lm"}, \code{"dt"},
#'     \code{"dtt"}, \code{"rf"}, and \code{"rftt"} are implemented. For more
#'     information see 'Details'.
#' @param func an object of class "\code{\link{formula}}" specifying the
#'     model to be fitted. For example: \code{y ~ x + z}
#' @param var.name name of independent variable / feature AME is to be
#'     estimated for.
#' @param fromtoby range predicted values are to be esitmated for. Only
#'     necessary for continuous independent variables / features. Usually
#'     given as \code{seq(from, to, by)} Where \code{from} and \code{to}
#'     are the interval boundaries and \code{by} is the step width.
#' @param plotTree plot the resulting decision tree (only for \code{meth =
#'     "dt"}).
#' @param plotPV plot predicted values (only for continuous independent
#'     variables / features).
#'
#' @section Details:
#'     The data frame \code{data.name} is used to train a ML model using one of
#'     five algorithms:
#'     \describe{
#'     \item{\code{method = "lm"}}{an ordinary linear model (yes, this is also considered a ML
#'         algorithm ;)}
#'     \item{\code{method = "dt"}}{an ordinary regression tree implemented via the
#'         \code{\link[rpart]{rpart}} function.}
#'     \item{\code{method = "dtt"}}{two tree algorithm}
#'     \item{\code{method = "rf"}}{a random forest}
#'     \item{\code{method = "rftt"}}{random forest two tree}
#'     }
#'
#'     The formula \code{func} is used to specify the dependent variable /
#'     response and the independent variables / features that will be used for
#'     learning the model.
#'
#' @section Value:
#' \code{ame} returns a \code{list} of two (for binary independent variables /
#'     features) or three objects (for continuous independent variables /
#'     features):
#'     \enumerate{
#'         \item AME estimate
#'         \item predicted values (continuous only)
#'         \item model information
#'     }
#'
#' @author Jonas Beste (\email{jonas.beste@@iab.de}) and Arne Bethmann (\email{bethmann@@uni-mannheim.de}).
#'
#' @seealso \code{\link{ame.boot}}
#'
#' @examples
#' ## Estimate AMEs for Species on Petal.Length in iris data
#' set.seed(1)
#'
#' ## Using a linear model
#' ame(iris, "lm", Petal.Length ~ Petal.Width + Species, "Species")[1:2]
#'
#' ## Using a decision tree
#' ame(iris, "dt", Petal.Length ~ Petal.Width + Species, "Species")[1:2]
#'
#' @importFrom rpart rpart
#' @importFrom randomForest randomForest
#' @importFrom randomForest importance
#'
#' @export
ame <- function(data.name, meth="dt", func, var.name, fromtoby=NULL, plotTree=FALSE, plotPV=FALSE){
  # Linear Regression
  if (meth == "lm") {
    fit <- glm(func, family=gaussian(link = "identity"), data=data.name)
    sfit <- summary(fit)
  }
  # Decision Tree
  if (meth == "dt") {
    fit <- rpart(func, cp=0.0001, method="anova", data=data.name) # t.fit
    #fit<- prune(t.fit, cp= t.fit$cptable[which.min(t.fit$cptable[,"xerror"]),"CP"])
    if (plotTree == "TRUE") {
      plot(fit, uniform=TRUE, main="Regression Tree")
      text(fit, cex=.6)
    }
    sfit <- fit
  }
  # Decision Two Tree
  if (meth == "dtt") {
    if ((is.factor(data.name[[var.name]]) & length(levels(data.name[[var.name]]))==2) | (nrow(unique(data.name[var.name]))==2)) { # hier weitere (u.w.u.) oder Bedingungen
      t0.fit <- rpart(func, method="anova", data=subset(data.name, data.name[var.name]==0))
      p0.fit <- prune(t0.fit, cp= t0.fit$cptable[which.min(t0.fit$cptable[,"xerror"]),"CP"])
      t1.fit <- rpart(func, method="anova", data=subset(data.name, data.name[var.name]==1))
      p1.fit <- prune(t1.fit, cp= t1.fit$cptable[which.min(t1.fit$cptable[,"xerror"]),"CP"])
      sfit <- list(p0.fit, p1.fit)
    }
    else {
      print("ToDo")
    }
  }
  # Random Forest
  if (meth == "rf") {
    fit <- randomForest(func, data=data.name)
    sfit <- importance(fit)
  }
  # Random Forest Two Trees
  if (meth == "rftt") {
    p0.fit <- randomForest(func, method="anova", data=subset(data.name, data.name[var.name]==0))
    p1.fit <- randomForest(func, method="anova", data=subset(data.name, data.name[var.name]==1))
    sfit <- list(importance(p0.fit), importance(p1.fit))
  }
  # Type of Variable
  if ((length(levels(data.name[[var.name]]))==2) | (nrow(unique(data.name[var.name]))==2)) {
    if (meth == "dtt" | meth == "rftt") {
      # Predictions
      pv.1 <- mean(predict(p0.fit, data.name))
      pv.2 <- mean(predict(p1.fit, data.name))
    }
    else {
      # Assigning values
      data.1 <- data.name
      data.1[var.name] <- 0
      data.2 <- data.name
      data.2[var.name] <- 1
      # Predictions
      pv.1 <- mean(predict(fit, data.1))
      pv.2 <- mean(predict(fit, data.2))
    }
    # AMEs
    pv <- c("0"=pv.1, "1"=pv.2)
    ame <- pv.2 - pv.1
    # Output
    output <- list(ame=ame, pv=pv, fit=sfit)
  }
  if (is.factor(data.name[[var.name]]) & length(levels(data.name[[var.name]])>2)) {
    if (meth == "dtt" | meth == "rftt") {
      # Assigning values and predict for multiple trees
      pv <- NULL
      counter <- 1
      for (i in levels(data.name[[var.name]])) {
        data <- data.name
        data[var.name] <- i
        pv[counter] <- mean(predict(fit, data))
        counter <- sum(counter, 1)
      }
    }
    else {
      # Assigning values and predict
      pv <- NULL
      counter <- 1
      for (i in levels(data.name[[var.name]])) {
        data <- data.name
        data[var.name] <- i
        pv[counter] <- mean(predict(fit, data))
        counter <- sum(counter, 1)
      }
    }
    # AMEs
    names(pv) <- levels(data.name[[var.name]])
    ame <- as.dist(replicate(length(pv), pv) - t(replicate(length(pv), pv)))
    # Output
    output <- list(ame=ame, pv=pv, fit=sfit)
  }
  if (!is.factor(data.name[[var.name]]) & (nrow(unique(data.name[var.name]))>2)) {
    if (is.null(fromtoby)) {
      print("Sequence from to by needed")
    }
    else{
      # Assigning values
      data <- data.name
      pv <- NULL
      counter <- 1
      # Predictions
      steps <- fromtoby
      for (i in steps) {
        data[var.name] <- i
        pv[counter] <- mean(predict(fit, data)) #  - data.name[,noquote(all.vars(formula(fit))[1])]
        counter <- sum(counter, 1)
      }
      # Slope
      slope <- tail(pv, length(pv)-1) - head(pv, length(pv)-1)
      if (plotPV == "TRUE") {
        plot(steps, pv, type="l")
        #plot(tail(steps, length(steps)-1), slope)
      }
      # AMEs
      names(pv) <- steps
      ame <- mean(slope)
      # Output
      output <- list(ame=ame, pv=pv, fit=sfit)
    }
  }
  return(output)
}
