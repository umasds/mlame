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

# Bootstrapping
ame.boot <- function(data.name, rep=100, meth="dt", func, var.name, fromtoby) {
  ame.boot <- replicate(rep, unlist(ame(sample_n(data.name, nrow(data.name), replace=T), meth, func, var.name, fromtoby)[1]))
  mean <- round(mean(ame.boot), 3)
  se <- round(sd(ame.boot), 3)
  error <- round((qnorm(0.975) * se), 3)
  p <- round(2*pnorm(mean/-abs(se)), 3)
  output <- c(ame=mean, se=se, p=p, lci=mean-error, uci=mean+error)
  return(output)
}
