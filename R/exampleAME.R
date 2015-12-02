# Beispiel fuer die verwendung der AME-Funktion

# Pakete installieren
install.packages("rpart")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("dplyr")

# Seed setzen
set.seed(1)

# Daten erzeugen
n <- 10000
a <- 150
b1 <- 120
b1.sqr <- -0.7
b2 <- -200
b1.2 <- -30
s <- 0.75

# Variablen
alter <- round(runif(n, 15, 64), 0)
geschl <- rbinom(n, 1, .5)

# Abhaengige Variable
y <- round(exp(rnorm(n , mean=log(a + b1*alter + b1.sqr*alter*alter + b2*geschl 
     + b1.2*alter*geschl), sd=s)))
mean(y)
# Graphik
qplot(alter, y, geom=c("point", "smooth"), group = geschl, method="gam"
      , formula=y~poly(x,2)) + ylim(0, 5000)

# Daten zusammenfuegen
test.data <- data.frame(cbind(y, alter, geschl))
rm(y, alter, geschl)

# Funktion einlesen
setwd("Pfad in dem ame.R liegt")
source("ame.R")

# Average marginal effects fuer metrische abhaengige Variablen
# Es k?nnen der Effekt fuer metrische und binaere unabhaengige Variablen geschaetzt werden
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

# Anwendung bei bin?rer Variable Geschlecht
ame(test.data, "lm", y ~ alter + I(alter^2) + geschl + I(alter*geschl), "geschl")
ame(test.data, "dt", y ~ alter + geschl, "geschl", plotTree=TRUE)[1]
ame(test.data, "dtt", y ~ alter, "geschl")[1]

# Anwendung bei metrischer Variable Alter
ame(test.data, "lm", y ~ alter + I(alter^2) + geschl + I(alter*geschl), "alter", seq(20,60,5))[1]
ame(test.data, "rf", y ~ alter + I(alter^2) + geschl + I(alter*geschl), "alter", seq(20,60,5)
    , plotPV=TRUE)[1]

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
ame.boot(test.data, 10, "lm", y ~ alter + geschl, "geschl")
ame.boot(test.data, 10, "dtt", y ~ alter, "geschl")
ame.boot(test.data, 10, "dt", y ~ alter + geschl, "alter", seq(20,60,5))
