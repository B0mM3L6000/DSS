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

# hier gehen spalten verloren bei beiden operationen. Vermutlich genau die Spalten für die kein Eintrage in der entsprechenden anderen Tabelle 
# zu dem jeweiligen merge schlüssel existiert.
datadf <- merge(datadf, planesdf, by.x=c("TailNum"), by.y=c("tailnum"))
dataterrordf <- merge(datadf, terrordf, by.x=c("DayofMonth","Month"), by.y=c("Tag","Monat"))
#datadf

write.table(datadf, "data.csv", sep=",",row.names = F)
write.table(dataterrordf,"dataterror.csv", sep=",",row.names = F)

dataexperiment <- read.csv(file="data.csv", nrows = 4000)

