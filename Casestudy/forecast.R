#setwd("/home/hl/Documents/Uni/DSS/Uebung2/CaseStudy/")
#setwd("D:/Projekte/DSS/DSS/CaseStudy")
setwd("/home/martin/Projects/DSS/Casestudy")


library(corrplot)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(caret)
library(lattice)
library(xgboost)

#einlesen der csv files

data <- "2007.csv"
airports <- "airports.csv"
carriers <- "carriers.csv"
planes <- "plane-data.csv"

#einlesen in dataframes

datadf <- read.csv(data, sep=",", header=TRUE, encoding="UTF-8")
airportsdf <- read.csv(airports, sep=",", header=TRUE, encoding="UTF-8")
carriersdf <- read.csv(carriers, sep=",", header=TRUE, encoding="UTF-8")
planesdf <- read.csv(planes, sep=",", header=TRUE, encoding="UTF-8")



datadf <- na.omit(datadf)


datadf$ID <- seq.int(nrow(datadf))

datadf$Delay <- datadf$ArrDelay - datadf$WeatherDelay
newdf <- subset(datadf,select=c("ID","UniqueCarrier", "DayOfWeek",
                                "Origin", "CRSArrTime", "CRSDepTime", "Distance", "Delay"  ))


v <- c("ATL","ORD", "DFW", "DEN", "LAX", "PHX", "IHH", "LAS", "DTW", "MSP", "EWR",
       "SLC", "SFO", "MCO", "BOS", "CLT", "JFK", "LGA", "CVG", "BWI")
newdf2 <- subset(newdf, newdf$Origin %in% v)



# Gruppieren Abflug Uhrzeit
newdf2$Abflug <- 0
i <- 000
for (a in 0:23)
{
  newdf2$Abflug[newdf2$CRSDepTime >= i & newdf2$CRSDepTime <= (i+59)] = a
  i <- i+100
}

# Gruppieren Ankunft Uhrzeit
newdf2$Ankunft <- 0
i <- 000
for (a in 0:23)
{
  newdf2$Ankunft[newdf2$CRSArrTime >= i & newdf2$CRSArrTime <= (i+59)] = a
  i <- i+100
}


#Dummyvariablen Flughäfen
for(level in unique(newdf2$Origin)){
  newdf2[paste("dummy", level, sep = "_")] <- ifelse(newdf2$Origin == level, 1, 0)
}

#Dummyvariablen Airlines (Unique Carrier)
for(level in unique(newdf2$UniqueCarrier)){
  newdf2[paste("dummy", level, sep = "_")] <- ifelse(newdf2$UniqueCarrier == level, 1, 0)
}

#Dummyvariablen DayOfWeek
for(level in unique(newdf2$DayOfWeek)){
  newdf2[paste("day", level, sep = "_")] <- ifelse(newdf2$DayOfWeek == level, 1, 0)
}

#Dummyvariablen Ankunft
for(level in unique(newdf2$Ankunft)){
  newdf2[paste("Ankunft", level, sep = "_")] <- ifelse(newdf2$Ankunft == level, 1, 0)
}

#Dummyvariablen Abflug
for(level in unique(newdf2$Abflug)){
  newdf2[paste("Abflug", level, sep = "_")] <- ifelse(newdf2$Abflug == level, 1, 0)
}


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

write.table(newdf2, "regressionsdataframe.csv", sep=",",row.names = F)
newdf2 <- read.csv("regressionsdataframe.csv", sep=",",header=TRUE, encoding="UFT-8")

rm("newdf")
rm("datadf")
rm("planesdf")
rm("carriersdf")
rm("airportsdf")

# Set seed zum Reproduzieren der Ergebnisse, Brudi
set.seed(54321)
inTrain  <-createDataPartition(newdf2$ID,p=0.75,list = FALSE)
Training <- newdf2[inTrain,]
Testing  <- newdf2[-inTrain,]
length(inTrain)/length(newdf2[,1])

Training$ID <- NULL
Testing$ID <- NULL

y_train <- Training$is_delayed

y_test <- Testing$is_delayed

x_train <- model.matrix(is_delayed~., data=Training, all = T)[,-1]
x_test <- model.matrix(is_delayed~., data=Testing, all = T)[,-1]


m1 = glm(is_delayed~., family=binomial,data = data.frame(is_delayed = y_train, x_train))

rm("x_train")
rm("y_train")
rm("Training")
rm("inTrain")
rm("newdf2")

ptest <- predict(m1, newdata = data.frame(x_test), type ="response")
data.frame(y_test,ptest)

class <- floor(ptest+0.5)
real <- table(y_test,class)

error = (real[1,2]+real[2,1]) / length(Testing$Distance)
error

rm("m1")
rm("Testing")
rm("x_test")
rm("y_test")
rm("class")
rm("ptest")


#xgboost:

bst <- xgboost(data = x_train, label = y_train, max.depth = 32, eta = 0.1, nthread = 10, nround = 500, objective = "binary:logistic", tree_method = "hist")
gc()  #cleart ram

pred <- predict(bst, x_test)

# size of the prediction vector
#print(length(pred))

#print(head(pred))

prediction <- as.numeric(pred > 0.5)
print(head(prediction))

err <- mean(as.numeric(pred > 0.5) != y_test)
print(paste("test-error=", err))


class <- floor(prediction+0.5)
real <- table(y_test,class)
real
