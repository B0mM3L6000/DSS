#setwd("/home/hl/Documents/Uni/DSS/Uebung2/CaseStudy/")
#setwd("D:/Projekte/DSS/DSS/CaseStudy")
setwd("/home/martin/Projects/DSS/Casestudy")


library(corrplot)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(lattice)
library(caret)
library(xgboost)

#einlesen der csv files

data <- "2007.csv"
#airports <- "airports.csv"
#carriers <- "carriers.csv"
#planes <- "plane-data.csv"

#einlesen in dataframes

datadf <- read.csv(data, sep=",", header=TRUE, encoding="UTF-8")
#airportsdf <- read.csv(airports, sep=",", header=TRUE, encoding="UTF-8")
#carriersdf <- read.csv(carriers, sep=",", header=TRUE, encoding="UTF-8")
#planesdf <- read.csv(planes, sep=",", header=TRUE, encoding="UTF-8")


#entfernen der NA Zeilen
datadf <- na.omit(datadf)

#zufügen neuer spalte mit ID (wichtig für spliten in test und traning später)
datadf$ID <- seq.int(nrow(datadf))

#bereinigen des delays vom wetter delay (hoehere Macht und keine daten über wetter für den forecast vorhanden)
datadf$Delay <- datadf$ArrDelay - datadf$WeatherDelay

#auswählen der relevanten spalten für den forecast
newdf <- subset(datadf,select=c("ID","UniqueCarrier", "DayOfWeek",
                                "Origin", "CRSArrTime", "CRSDepTime", "Distance", "Delay"  ))

#reduzieren auf die top20 flughaefen (ausgewaehlt anhand der anzahl der abfluege in 2007 pro flughafen)
v <- c("ATL","ORD", "DFW", "DEN", "LAX", "PHX", "IHH", "LAS", "DTW", "MSP", "EWR",
       "SLC", "SFO", "MCO", "BOS", "CLT", "JFK", "LGA", "CVG", "BWI")
newdf2 <- subset(newdf, newdf$Origin %in% v)



# Gruppieren Abflug Uhrzeit in 24 Gruppen (stundenweise)
newdf2$Abflug <- 0
i <- 000
for (a in 0:23)
{
  newdf2$Abflug[newdf2$CRSDepTime >= i & newdf2$CRSDepTime <= (i+59)] = a
  i <- i+100
}

# Gruppieren Ankunft Uhrzeit in 24 Gruppen (stundenweise)
newdf2$Ankunft <- 0
i <- 000
for (a in 0:23)
{
  newdf2$Ankunft[newdf2$CRSArrTime >= i & newdf2$CRSArrTime <= (i+59)] = a
  i <- i+100
}


#Dummyvariablen Flughäfen erstellen
for(level in unique(newdf2$Origin)){
  newdf2[paste("dummy", level, sep = "_")] <- ifelse(newdf2$Origin == level, 1, 0)
}

#Dummyvariablen Airlines (Unique Carrier) erstellen
for(level in unique(newdf2$UniqueCarrier)){
  newdf2[paste("dummy", level, sep = "_")] <- ifelse(newdf2$UniqueCarrier == level, 1, 0)
}

#Dummyvariablen DayOfWeek erstellen
for(level in unique(newdf2$DayOfWeek)){
  newdf2[paste("day", level, sep = "_")] <- ifelse(newdf2$DayOfWeek == level, 1, 0)
}

#Dummyvariablen Ankunft erstellen
for(level in unique(newdf2$Ankunft)){
  newdf2[paste("Ankunft", level, sep = "_")] <- ifelse(newdf2$Ankunft == level, 1, 0)
}

#Dummyvariablen Abflug erstellen
for(level in unique(newdf2$Abflug)){
  newdf2[paste("Abflug", level, sep = "_")] <- ifelse(newdf2$Abflug == level, 1, 0)
}

#neue spalte erstellen die kategorisiert, ob der flug verspaetet ist oder nicht, wobei wir einen flug als verspaetet ansehen wenn der delay 15 min oder mehr betraegt
newdf2$is_delayed <- ifelse(newdf2$Delay>15 ,1,0)


# Streichen nicht gebrauchter Variablen, die bereits durch Dummyvariablen ausgedrückt sind
# GGf. direkt nach Erstellen der jeweiligen Dummy-Variablen löschen # FREERAM
newdf2$UniqueCarrier <- NULL
newdf2$DayOfWeek <- NULL
newdf2$Origin <- NULL
newdf2$CRSArrTime <- NULL
newdf2$CRSDepTime <- NULL
newdf2$Delay <- NULL
newdf2$Abflug <- NULL
newdf2$Ankunft <- NULL

#abspeichern des tables in csv, um spätere anwendung einfacher zu machen
write.table(newdf2, "regressionsdataframe.csv", sep=",",row.names = F)
#neues einlesen
newdf2 <- read.csv("regressionsdataframe.csv", sep=",",header=TRUE, encoding="UFT-8")

#entfernen nicht mehr benötigter dataframes und mem clearnen mit gc() # FREERAM
rm("newdf")
rm("datadf")
#rm("planesdf")
#rm("carriersdf")
#rm("airportsdf")

gc()

# Set seed zum Reproduzieren der Ergebnisse
set.seed(54321)

#aufteilen in test- und trainset
inTrain  <-createDataPartition(newdf2$ID,p=0.75,list = FALSE)
Training <- newdf2[inTrain,]
Testing  <- newdf2[-inTrain,]
#length(inTrain)/length(newdf2[,1])

#entfernen der nicht mehr menötigten spalten und dataframes
Training$ID <- NULL
Testing$ID <- NULL
rm("inTrain")
rm("newdf2")

gc()

#zuweisen von labels und input
y_train <- Training$is_delayed
x_train <- model.matrix(is_delayed~., data=Training, all = T)[,-1]

y_test <- Testing$is_delayed
x_test <- model.matrix(is_delayed~., data=Testing, all = T)[,-1]

#entfernen von testing und training für ram freiheit:
rm("Testing")
rm("Training")
gc()

#ab hier forecast versuche:


##############################################################
####nicht teil der praesentation, da schlechte ergebnisse und sehr schlechte runtime/ramoptimierung
#normales lineares model mit standart r
#traininieren:
m1 = glm(is_delayed~., family=binomial,data = data.frame(is_delayed = y_train, x_train))
gc()

#falls etwas entfernt werden muss für mehr ramfreiheit nach dem training (nur das gewuenschte jeweils ausführen) 
#rm("x_train")
#rm("y_train")
gc()

#testen:

ptest <- predict(m1, newdata = data.frame(x_test), type ="response")
#data.frame(y_test,ptest)

prediction <- as.numeric(ptest > 0.4)

class <- floor(prediction+0.4)
real <- table(y_test,class)
real #confusion matrix

error = (real[1,2]+real[2,1]) / length(y_test)
error #error



#plotten von roc und auc:

library(pROC)
auc <- auc(y_test,prediction)
auc
p <- plot(roc(y_test,prediction))
p

#falls etwas entfernt werden muss für mehr ramfreiheit nach dem testen (nur das gewuenschte jeweils ausführen)
rm("x_test")
rm("y_test")
rm("class")
rm("ptest")
rm("prediction")
rm("m1")
gc()


######################################################################
#focus der praesentation:

#falls oben geloescht hier nochmals training und test daten einlesen! 

#xgboost mit tree hist:
#trainieren:
#hyperparameter: 64 tiefe baum, 200 baeume, learningrate 0.1, 10 threads parallel, 250 epochen, treemethode: histogramm, ziel: binary logistic
#ausgewaehlt durch ausprobieren
bst <- xgboost(data = x_train, label = y_train, n_estimators = 200, max.depth = 64, eta = 0.1, nthread = 10, nround = 250, objective = "binary:logistic", tree_method = "hist")
gc()

#falls etwas entfernt werden muss für mehr ramfreiheit nach dem training (nur das gewuenschte jeweils ausführen)
#rm("x_train")
#rm("y_train")
gc()

#testen:
pred <- predict(bst, x_test)
pred

# size of the prediction vector
#print(length(pred))
#print(head(pred))

prediction <- as.numeric(pred > 0.5)

#print(head(prediction))

#err <- mean(as.numeric(pred > 0.5) != y_test)
#print(paste("test-error=", err))

class <- floor(prediction+0.5)
real <- table(y_test,class)
real #confusion matrix

error = (real[1,2]+real[2,1]) / length(y_test) 
error #testerror

#auc und roc:

library(pROC)
auc <- auc(y_test,prediction)
auc
p <- plot(roc(y_test,prediction))
p
