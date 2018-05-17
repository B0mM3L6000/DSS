setwd("D:/Projekte/DSS/DSS/CaseStudy")

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


Vars <- colnames(datadf)
Namen <- c("Year", "FlightNum")
datadf <- datadf[, !(Vars %in% Namen)]

Vars <- colnames(planesdf)
Namen <- c("type", "issue_date", "status")
planesdf <- planesdf[, !(Vars %in% Namen)]

datadf <- merge(datadf, planesdf, by.x=c("TailNum"), by.y=c("tailnum"))
datadf

write.table(datadf, "data.csv", sep=",",row.names = F)


dataexperiment <- read.csv(file="data.csv", nrows = 4000)
