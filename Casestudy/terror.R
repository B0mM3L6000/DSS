#setwd("D:/Projekte/DSS/DSS/CaseStudy")
setwd("/home/martin/Projects/DSS/Casestudy")

#Tabelle erstellen mit Spalten: Tag, Monat, Tote, Verletzte, Land, Art
#und füllen mit den Daten von https://en.wikipedia.org/wiki/List_of_terrorist_incidents_in_2007

tag <- 
monat <-
tote <-
verletzte <-
land <-
art <-
  
terrordata <- data.frame(tag,monat,tote,verletzte,land,art)
colnames(terrordata)  <- c("Tag","Monat","Tote","Verletzte","Land","Art")

write.table(terrordata,"terror.csv",sep=",",row.names=F)    # Schreibe Daten in CSV-Datei