#setwd("/home/hl/Documents/Uni/DSS/Uebung2/CaseStudy/")
#setwd("D:/Projekte/DSS/DSS/CaseStudy")
setwd("/home/martin/Projects/DSS/Casestudy")

#################datapreprocessing####################


datadf <- read.csv(file="data.csv", header=TRUE, encoding="UTF-8")

Vars <- colnames(datadf)
Vars
Namen <- c("TailNum", "Month", "DayofMonth","DayOfWeek", "DepTime", "CRSDepTime",
           "ArrTime","CRSArrTime","UniqueCarrier","CRSElapsedTime","AirTime", "ActualElapsedTime", "Origin",
           "Dest", "Distance", "TaxiIn", "TaxiOut", "Cancelled", "CancellationCode",
           "Diverted", "manufacturer", "model", "aircraft_type", "engine_type", "year")
datadf <- datadf[, !(Vars %in% Namen)]
datadf <- na.omit(datadf)

#######corrplot f?r versp?tungsfaktoren###############


c <- cor(datadf)
corrplot(c, method = "number")


###########anteil versp?tungen########################

#anteile an arrdelay:

datadf$anteilCarrier <- ifelse(datadf$ArrDelay == 0, 0, datadf$CarrierDelay/datadf$ArrDelay)
datadf$anteilWetter <- ifelse(datadf$ArrDelay == 0, 0, datadf$WeatherDelay/datadf$ArrDelay)
datadf$anteilNAS <- ifelse(datadf$ArrDelay == 0, 0, datadf$NASDelay/datadf$ArrDelay)
datadf$anteilSec <- ifelse(datadf$ArrDelay == 0, 0, datadf$SecurityDelay/datadf$ArrDelay)
datadf$anteilLateAC <- ifelse(datadf$ArrDelay == 0, 0, datadf$LateAircraftDelay/datadf$ArrDelay)

fluegeDelayArr <- subset(datadf, datadf$ArrDelay >0)

meanArrCar <- mean(fluegeDelayArr$anteilCarrier)
meanArrWetter <- mean(fluegeDelayArr$anteilWetter)
meanArrNAS <- mean(fluegeDelayArr$anteilNAS)
meanArrSec <- mean(fluegeDelayArr$anteilSec)
meanArrLateAC <- mean(fluegeDelayArr$anteilLateAC)

meansArr <- c(meanArrCar, meanArrWetter, meanArrNAS, meanArrSec, meanArrLateAC)
namesArr <- c("CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")
plotArrdf <- data.frame(namesArr, meansArr)

library(ggplot2)
plot <- ggplot(plotArrdf, aes(x=namesArr, y=meansArr, fill = namesArr)) + geom_bar(stat = "identity") + ggtitle("ArrivalDelay Share")  + xlab("Delay") + ylab("Share") +scale_fill_discrete(name="") + theme(axis.text.x=element_blank(),
                                                                                                                                                                                                             axis.ticks.x=element_blank()) 
plot

#anteile an depdelay:

datadf$anteilCarrier <- ifelse(datadf$DepDelay == 0, 0, datadf$CarrierDelay/datadf$DepDelay)
datadf$anteilWetter <- ifelse(datadf$DepDelay == 0, 0, datadf$WeatherDelay/datadf$DepDelay)
datadf$anteilNAS <- ifelse(datadf$DepDelay == 0, 0, datadf$NASDelay/datadf$DepDelay)
datadf$anteilSec <- ifelse(datadf$DepDelay == 0, 0, datadf$SecurityDelay/datadf$DepDelay)
datadf$anteilLateAC <- ifelse(datadf$DepDelay == 0, 0, datadf$LateAircraftDelay/datadf$DepDelay)

fluegeDelayDep <- subset(datadf, datadf$DepDelay >0)

meanDepCar <- mean(fluegeDelayDep$anteilCarrier)
meanDepWetter <- mean(fluegeDelayDep$anteilWetter)
meanDepNAS <- mean(fluegeDelayDep$anteilNAS)
meanDepSec <- mean(fluegeDelayDep$anteilSec)
meanDepLateAC <- mean(fluegeDelayDep$anteilLateAC)

meansDep <- c(meanDepCar, meanDepWetter, meanDepNAS, meanDepSec, meanDepLateAC)
namesDep <- c("CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")
plotDepdf <- data.frame(namesDep, meansDep)

library(ggplot2)
plot <- ggplot(plotDepdf, aes(x=namesDep, y=meansDep, fill = namesDep)) + geom_bar(stat = "identity") + ggtitle("DepartureDelay Share")  + xlab("Delay") + ylab("Share") +scale_fill_discrete(name="") + theme(axis.text.x=element_blank(),
                                                                                                                                                                                                             axis.ticks.x=element_blank()) 
plot
