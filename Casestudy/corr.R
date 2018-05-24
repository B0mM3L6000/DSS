#setwd("/home/hl/Documents/Uni/DSS/Uebung2/CaseStudy/")
#setwd("D:/Projekte/DSS/DSS/CaseStudy")
setwd("/home/martin/Projects/DSS/Casestudy")

library(corrplot)

#################datapreprocessing####################


datadf <- read.csv(file="data.csv", header=TRUE, encoding="UTF-8")

Vars <- colnames(datadf)
Vars
Namen <- c("TailNum", "Month", "DayofMonth","DayOfWeek", "DepTime", "CRSDepTime",
           "ArrTime","CRSArrTime","UniqueCarrier","CRSElapsedTime","AirTime", "ActualElapsedTime", "Origin",
           "Dest", "Distance", "Cancelled", "CancellationCode",
           "Diverted", "manufacturer", "model", "aircraft_type", "engine_type", "year")
datadf <- datadf[, !(Vars %in% Namen)]
datadf <- na.omit(datadf)

#######corrplot f?r versp?tungsfaktoren###############


c <- cor(datadf)
corrplot(c, method = "number")

