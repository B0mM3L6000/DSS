#setwd("D:/Projekte/DSS/DSS/CaseStudy")
setwd("/home/martin/Projects/DSS/Casestudy")

#einlesen der csv files

data <- "2007.csv"
airports <- "airports.csv"
carriers <- "carriers.csv"
planes <- "plane-data.csv"
terror <- "terror.csv"

#einlesen in dataframes

datadf <- read.csv(data, sep=",", header=TRUE, encoding="UTF-8")
airportsdf <- read.csv(airports, sep=",", header=TRUE, encoding="UTF-8")
carriersdf <- read.csv(carriers, sep=",", header=TRUE, encoding="UTF-8")
planesdf <- read.csv(planes, sep=",", header=TRUE, encoding="UTF-8")
terrordf <- read.csv(terror, sep=",",header=TRUE, encoding="UFT-8")


Vars <- colnames(datadf)
Namen <- c("Year", "FlightNum")
datadf <- datadf[, !(Vars %in% Namen)]

Vars <- colnames(planesdf)
Namen <- c("type", "issue_date", "status")
planesdf <- planesdf[, !(Vars %in% Namen)]

datadf <- merge(datadf, planesdf, by.x=c("TailNum"), by.y=c("tailnum"), all = TRUE)



#########################################################################################
#Terroruntersuchung:


#subframe erstellen mit nur datum und sicherheitsdelay im mittel
#(eventuell noch trend über jahr und saisonalitäten über tag und uhrzeit entfernen)


Vars <- colnames(datadf)
Vars
Namen <- c("Year", "FlightNum","DepTime","CRSDepTime","ArrTime","CRSArrTime","UniqueCarrier","FlightNum","TailNum", "ActualElapsedTime","CRSElapsedTime","AirTime","ArrDelay","DepDelay","Origin","Dest","Distance","TaxiIn","TaxiOut","Cancelled","CancellationCode","Diverted","CarrierDelay","WeatherDelay","NASDelay","LateAircraftDelay")
subdataterrordf <- datadf[, !(Vars %in% Namen)]

subdataterrordf <- aggregate(subdataterrordf[,c("SecurityDelay")], by=list(Monat=subdataterrordf$Month, Tag=subdataterrordf$DayofMonth, Wochentag=subdataterrordf$DayOfWeek), mean) #mean, max, min, sd etc.
subdataterrordfordered <- subdataterrordf[order(subdataterrordf$Monat, subdataterrordf$Tag),] 

#saisonalität und trend entfernen:
library(fpp)

#start:
plot(as.ts(subdataterrordfordered$x))

##trend:
#library(forecast)
#library(fpp)
#trend_terror = ma(subdataterrordfordered$x, order = 7, centre = T)
#plot(as.ts(subdataterrordfordered$x))
#lines(trend_terror)
#plot(as.ts(trend_terror))

##trend entfernen:
#detrend_terror = subdataterrordfordered$x - trend_terror
#plot(as.ts(detrend_terror))

##saisionalität: #52,1428571
#m_terror = t(matrix(data = detrend_terror, nrow = 7.00000001))
#seasonal_terror = colMeans(m_terror, na.rm = T)
#plot(as.ts(rep(seasonal_terror,52)))


ts_terror = ts(subdataterrordfordered$x, frequency = 7)
decompose_terror = decompose(ts_terror, "additive")

plot(as.ts(decompose_terror$seasonal))
plot(as.ts(decompose_terror$trend))
plot(as.ts(decompose_terror$random))
plot(decompose_terror)



#nächster schritt:

stl_terror = stl(ts_terror, "periodic")
seasonal_stl_terror   <- stl_terror$time.series[,1]
trend_stl_terror     <- stl_terror$time.series[,2]
random_stl_terror  <- stl_terror$time.series[,3]

plot(ts_terror)
plot(as.ts(seasonal_stl_terror))
plot(trend_stl_terror)
plot(random_stl_terror)
plot(stl_terror)

#anbinden von den bereinigten werden an das geordnete subdataterror dataframe

test <- subdataterrordfordered

length(random_stl_terror)
random_stl_terror

#in vektor umformen:
bereinigt <- as.numeric(random_stl_terror)
bereinigt

test$bereinigt <- bereinigt

plot(as.ts(test$bereinigt))


#anschläge aggregieren bei selben tag und vorerst land vernachlässigen:

Vars <- colnames(terrordf)
Vars
Namen <- c("Land")
anschlaegedf <- terrordf[, !(Vars %in% Namen)]

#nicht nötig da oben bereits automatisch aggregiert
#anschlaegedf <- aggregate(anschlaegedf[,c("Tote","Verletzte")], by=list(Monat=anschlaegedf$Monat, Tag=anschlaegedf$Tag), sum) #mean, max, min, sd etc.
anschlaegedfordered <- anschlaegedf[order(anschlaegedf$Monat, anschlaegedf$Tag),] 

#mergen mit der bereinigten tabelle von oben:

finaldf <- merge(test, anschlaegedfordered, by.x=c("Tag","Monat"), by.y=c("Tag","Monat"), all = TRUE)

# mergen von terror ins dataframe (problem: merged nur die spalten wo es auch einen anschlag gab an dem tag)
#dataterrordf <- merge(datadf, terrordf, by.x=c("DayofMonth","Month"), by.y=c("Tag","Monat"))
#datadf

#####################################################################################################################################


write.table(datadf, "data.csv", sep=",",row.names = F)
#write.table(dataterrordf,"dataterror.csv", sep=",",row.names = F)

dataexperiment <- read.csv(file="data.csv")

##### FLughafen frequencys nach flügen:

tmp <- as.data.frame(table(dataexperiment$Origin))
tmp2 <- tmp[tmp$Freq>10000,]
hist(tmp$Freq)
hist(log(tmp$Freq))

airportstmp <- merge(tmp, airportsdf, by.x=c("Var1"), by.y=c("iata"))
airportstmp2 <- merge(tmp2, airportsdf, by.x=c("Var1"), by.y=c("iata"))

library(ggplot2)
ggplot(airportstmp, aes(long,lat)) + geom_point()
ggplot(airportstmp2, aes(long,lat, colour = Var1)) + geom_point()
