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

# hier gehen spalten verloren bei beiden operationen. Vermutlich genau die Spalten fÃ¼r die kein Eintrage in der entsprechenden anderen Tabelle 
# zu dem jeweiligen merge schlÃ¼ssel existiert.
datadf <- merge(datadf, planesdf, by.x=c("TailNum"), by.y=c("tailnum"))

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

# mergen von terror ins dataframe (problem: merged nur die spalten wo es auch einen anschlag gab an dem tag)
dataterrordf <- merge(datadf, terrordf, by.x=c("DayofMonth","Month"), by.y=c("Tag","Monat"))
#datadf

write.table(datadf, "data.csv", sep=",",row.names = F)
write.table(dataterrordf,"dataterror.csv", sep=",",row.names = F)

dataexperiment <- read.csv(file="data.csv", nrows = 4000)

