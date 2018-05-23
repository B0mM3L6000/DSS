#setwd("/home/hl/Documents/Uni/DSS/Uebung2/CaseStudy/")
setwd("D:/Projekte/DSS/DSS/CaseStudy")
#setwd("/home/martin/Projects/DSS/Casestudy")


####################Data Preprocessing#####################

dataexperiment <- read.csv(file="data.csv", header=TRUE, encoding="UTF-8", nrows = 10000)
airportsdf <- read.csv(file="airports.csv", header=TRUE, encoding="UTF-8")



tmp <- as.data.frame(table(dataexperiment$Origin))
tmp2 <- tmp[tmp$Freq>10000,]
hist(tmp$Freq)
hist(log(tmp$Freq))

airportstmp <- merge(tmp, airportsdf, by.x=c("Var1"), by.y=c("iata"))
airportstmp2 <- merge(tmp2, airportsdf, by.x=c("Var1"), by.y=c("iata"))

library(ggplot2)
ggplot(airportstmp, aes(long,lat)) + geom_point()




##########################Clustert#########################

#####CLustern:

clusters <- kmeans(airportstmp[,7:8], 10)  #nach geographischer lage in 10 Bereiche geclustert

airportstmp$cluster <- as.factor(clusters$cluster)

#####Plotten der Cluster:

ggplot(airportstmp, aes(long,lat, colour= cluster)) + geom_point()



###########################Untersuchung####################
