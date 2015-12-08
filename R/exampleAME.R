# Example for using the ame() function

# Install packages if not available
if (!"rpart" %in% installed.packages()[,1]) install.packages("rpart")
if (!"randomForest" %in% installed.packages()[,1]) install.packages("randomForest")
if (!"ggplot2" %in% installed.packages()[,1]) install.packages("ggplot2")
if (!"dplyr" %in% installed.packages()[,1]) install.packages("dplyr")

# Set seed for reproducible results
set.seed(1)

# Generate data
n <- 10000
a <- 150
b1 <- 120
b1.sqr <- -0.7
b2 <- -200
b1.2 <- -30
b3.1 <- 150
b3.2 <- 250
s <- 0.75

# Variables
alter <- round(runif(n, 15, 64), 0)
geschl <- rbinom(n, 1, .5)
bildung <- as.factor(rbinom(n, 2, .6))
real <- ifelse(bildung==1, 1, 0)
abi <- ifelse(bildung==2, 1, 0)

# Dependent variable
y <- round(exp(rnorm(n , mean=log(a + b1*alter + b1.sqr*alter*alter + b2*geschl 
     + b1.2*alter*geschl + b3.1*real + b3.2*abi), sd=s)))
mean(y)
# Plot
library(ggplot2)
qplot(alter, y, geom=c("point", "smooth"), group = geschl, method="gam"
      , formula=y~poly(x,2)) + ylim(0, 5000)

# Merge data
test.data <- data.frame(cbind(y, alter, geschl, bildung))
#test.data$geschl <- as.factor(test.data$geschl)
test.data$bildung <- as.factor(test.data$bildung)
rm(y, alter, geschl, bildung)

# Load function (this will be made into a package later on)
source("R/ame.R")

# Average marginal effects fuer metrische abhaengige Variablen
# Es koennen der Effekt fuer metrische und binaere unabhaengige Variablen geschaetzt werden
# Funktionsaufbau:
# ame(data.name, meth, func, var.name, fromtoby=NULL, plotTree=FALSE, plotSlope=FALSE)
# data.name = Name des Datenframes
# meth = anzuwendendes Verfahren (in Anfuehrungszeichen): 
  # "lm", lineare regression
  # "dt", decision tree, option plotTree
  # "dtt", decision two tree (nur bei binaeren unabhaengigen Variablen), 
    # hier die Variable nicht in die Funktion mitaufnehmen
  # "rf", random forest
  # "rftt", random forest two tree (nur bei binaeren unabhaengigen Variablen)
    # hier die Variable nicht in die Funktion mitaufnehmen
# func = zu berechnende Funktion, z.B. y ~ x
# var.name = Name der Variable fuer die der AME berechnet werden soll (in Anfuehrungszeichen)
# fromtoby = Wertebereich f?r den Predicted values bestimmt werdne sollen.
  # nur bei metrischen variablen notwendig. Ueblicherweise ueber die Function seq(from,to,by)
# plotTree = Plot des Entscheidungsbaums bei meth = "dt" (optional)
# plotPV = Plot der Predicted Values (optional; nur bei metrischen unabhaenigen variablen)
# Ausgabe: Liste mit 2 (binaere V.) bzw. 3 (metrische V.) Objekten
  # 1. AME-Schaetzer
  # 2. Predicted Values (nur bei metr. V.)
  # 2./3. Informationen zur Modell

# Anwendung bei binaerer Variable Geschlecht
ame(test.data, "lm", y ~ alter + I(alter^2) + geschl + I(alter*geschl) + bildung, "geschl")
ame(test.data, "dt", y ~ alter + geschl + bildung, "geschl", plotTree=TRUE)[1]
ame(test.data, "dtt", y ~ alter + bildung, "geschl")[1]

# Anwendung bei kategorialer Variable Bildung
ame(test.data, "lm", y ~ alter + I(alter^2) + geschl + I(alter*geschl) + bildung, "bildung")[c(1,2)]
ame(test.data, "dt", y ~ alter + geschl + bildung, "bildung", plotTree=TRUE)[c(1,2)]
ame(test.data, "dtt", y ~ alter + geschl, "bildung", plotTree=TRUE)[2]

# Anwendung bei metrischer Variable Alter
ame(test.data, "lm", y ~ alter + I(alter^2) + geschl + I(alter*geschl), "alter", seq(20,60,5))[c(1,2)]
ame(test.data, "dt", y ~ alter + geschl, "alter", seq(20,60,5), plotPV=TRUE)

# Bootrapping zur Bestimmung  der Standardfehler und 95%-Konfidenzintervalle
# Funktionsaufbau
# ame.boot(data.name, rep, n, meth, func, var.name, fromtoby)
# Bedeutung der Argumente wie bei Funktion AME
# rep = Anzahl der Wiederholungen (default = 100)
# n = Anzahl der Faelle pro Sample (default = 1000)
# Ausgabe: Vektor mit 4 Elementen:
  # 1. AME-Schaetzer
  # 2. SE
  # 3. Lower Bound (95%-CI)
  # 4. Upper Bound (95%-CI)

# Anwendung
ame.boot(test.data, 10, "lm", y ~ alter + geschl + bildung, "geschl")
ame.boot(test.data, 10, "dtt", y ~ alter+ bildung, "geschl")
ame.boot(test.data, 10, "dt", y ~ alter + geschl + bildung, "bildung")
ame.boot(test.data, 10, "dt", y ~ alter + geschl, "alter", seq(20,60,1))
