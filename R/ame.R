# Funktion AME

# Pakete einlesen
library(rpart)
library(randomForest)
library(ggplot2)
library(dplyr)

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
      plot(fit, uniform=TRUE, main="Pruned Regression Tree")
      text(fit, cex=.6)
    }
    sfit <- fit
  }
  # Decision Two Tree
  if (meth == "dtt") {
    t0.fit <- rpart(func, method="anova", data=subset(data.name, data.name[var.name]==0))
    p0.fit <- prune(t0.fit, cp= t0.fit$cptable[which.min(t0.fit$cptable[,"xerror"]),"CP"])
    t1.fit <- rpart(func, method="anova", data=subset(data.name, data.name[var.name]==1))
    p1.fit <- prune(t1.fit, cp= t1.fit$cptable[which.min(t1.fit$cptable[,"xerror"]),"CP"])
    sfit <- list(p0.fit, p1.fit)
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
  if (nrow(unique(data.name[var.name]))==2) {
    if (meth == "dtt" | meth == "rftt") {
      # Predictions
      y.pred.fit.1 <- predict(p0.fit, data.name)
      y.pred.fit.2 <- predict(p1.fit, data.name)    
    }
    else {
      # Assigning values
      data.1 <- data.name
      data.1[var.name] <- 0
      data.2 <- data.name
      data.2[var.name] <- 1
      # Predictions
      y.pred.fit.1 <- predict(fit, data.1)
      y.pred.fit.2 <- predict(fit, data.2)
    }
    # AMEs
    ame <- mean(y.pred.fit.1) - mean(y.pred.fit.2)
    # Output
    output <- list(ame, sfit)
  }
  else{
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
      ame <- mean(slope)
      # Output
      output <- list(ame, pv, sfit)
    }
  }
  return(output)
}

# Bootstrapping
ame.boot <- function(data.name, rep=100, meth="dt", func, var.name, fromtoby) {
  ame.boot <- replicate(rep, unlist(ame(sample_n(data.name, nrow(data.name), replace=T), meth, func, var.name, fromtoby)[1]))
  mean <- mean(ame.boot)
  se <- sd(ame.boot)
  error <- qnorm(0.975) * se
  output <- c(ame=mean, se=se, lci=mean-error, uci=mean+error)
  return(output)
}
