setwd("D:/Projekte/DSS/DSS/CaseStudy")
#setwd("/home/martin/Projects/DSS/Casestudy")

#Tabelle erstellen mit Spalten: Tag, Monat, Tote, Verletzte, Land
#und füllen mit den Daten von https://en.wikipedia.org/wiki/List_of_terrorist_incidents_in_2007

tag <- c(4, 4,5,5,6,6,6,7,10,10,10,13,14,14,15,15,16,16,17,17,17,17,18,18,18,22,26,27,29,3,3,17,19,1,3,5,6,16,27,28,8,9,10,11,
         12,14,14,18,18,20,25,28,28,3,6,6,12,13,15,18,18,20,22,3,4,8,8,8,10,11,14,19,24,29,29,2,19,21,14,14,25,4,4,4,6,29,1,2,
         3,11,11,14,14,18,21,26,31,6,6,13,23,23,1,6,11,12,12,21,24,27)
monat <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,
           5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,8,8,8,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,12,12,12,12,
           12,12,12,12)
tote <- c(13,1,6,0,15,1,3,2,7,5,2,2,1,4,0,0,70,6,9,2,17,0,0,4,1,131,1,14,3,135,23,15,68,0,5,2,114,16,152,0,1,1,4,33,8,65,1,200,
          3,1,11,28,28,1,35,1,1,50,24,1,16,6,9,7,1,1,2,0,0,1,10,75,6,4,0,10,2,0,0,796,44,21,0,0,0,0,15,13,0,3,3,1,6,136,12,0,8,
          0,80,2,15,5,2,1,37,2,40,50,0,24)
verletzte <- c(25,1,30,3,24,3,4,11,27,10,2,3,6,5,1,2,170,11,43,12,33,2,2,10,1,186,5,30,0,305,20,24,49,10,0,0,147,16,347,10,0,30,
               23,222,20,100,3,251,0,0,0,35,35,0,80,8,14,115,25,37,100,50,121,20,0,28,5,10,14,37,0,204,2,10,5,12,0,0,60,1562,54,
               74,0,0,0,12,22,0,1,0,30,2,20,387,16,2,50,1,0,9,80,13,0,0,177,0,125,100,0,46)
land <- c("iraq","thailand","srilanka","philippines","srilanka","iraq","iraq","iraq","philippines","iraq","iraq","iraq","iraq",
          "pakistan","myanmar", "thailand","iraq","iraq","iraq","india","iraq","thailand","afghanistan","iraq","thailand","iraq",
          "pakistan","pakistan","israel","iraq","afghanistan","pakistan","india","columbia", "columbia", "afghanistan", "iraq",
          "columbia","iraq","columbia","afghanistan", "columbia","morocco","algeria","iraq", "iraq","morocco","iraq","turkey",
          "somalia","somalia","pakistan","iraq","cuba","iraq","israel","turkey","iraq","pakistan","philippines","india","peru",
          "turkey","somalia","somalia","thailand","pakistan","philippines","turkey","kenya","lebanon","iraq","lebanon","ivorycoast",
          "uk","yemen","afghanistan","pakistan","russia","iraq","india","pakistan","germany","denmark","algeria","maledives",
          "pakistan","afghanistan","iraq","somalia","india","netherlands","india","pakistan","turkey","usa","russia","haiti",
          "afghanistan","philippines","india","russia","france","france","algeria","lebanon","iraq","pakistan","mauritania","pakistan")

length(tag)
length(monat)
length(tote)
length(verletzte)
length(land)



terrordata <- data.frame(tag,monat,tote,verletzte,land)
colnames(terrordata)  <- c("Tag","Monat","Tote","Verletzte","Land")

write.table(terrordata,"terror.csv",sep=",",row.names=F)    # Schreibe Daten in CSV-Datei
