setwd("D:/Projekte/DSS/DSS/CaseStudy")
#setwd("/home/martin/Projects/DSS/Casestudy")

#Tabelle erstellen mit Spalten: Tag, Monat, Tote, Verletzte, Land
#und füllen mit den Daten von https://en.wikipedia.org/wiki/List_of_terrorist_incidents_in_2007

tag <- c(4, 4,5,5,6,6,6,7,10,10,10,13,14,14,15,15,16,16,17,17,17,17,18,18,18,22,26,27,29,3,3,17,19)
monat <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2)
tote <- c(13,1,6,0,15,1,3,2,7,5,2,2,1,4,0,0,70,6,9,2,17,0,0,4,1,131,1,14,3,135,23,15,68)
verletzte <- c(25,1,30,3,24,3,4,11,27,10,2,3,6,5,1,2,170,11,43,12,33,2,2,10,1,186,5,30,0,305,20,24,49)
land <- c("iraq","thailand","srilanka","philippines","srilanka","iraq","iraq","iraq","philippines","iraq","iraq","iraq","iraq","pakistan","myanmar", "thailand","iraq","iraq","iraq","india","iraq","thailand","afghanistan","iraq","thailand","iraq","pakistan","pakistan","israel","iraq","afghanistan","pakistan","india")

length(tag)
length(monat)
length(tote)
length(verletzte)
length(land)



terrordata <- data.frame(tag,monat,tote,verletzte,land)
colnames(terrordata)  <- c("Tag","Monat","Tote","Verletzte","Land")

write.table(terrordata,"terror.csv",sep=",",row.names=F)    # Schreibe Daten in CSV-Datei
