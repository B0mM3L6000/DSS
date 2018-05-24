#setwd("/home/hl/Documents/Uni/DSS/Uebung2/CaseStudy/")
#setwd("D:/Projekte/DSS/DSS/CaseStudy")
setwd("/home/martin/Projects/DSS/Casestudy")

library(RColorBrewer)
library(wesanderson)

####################Data Preprocessing#####################

dataexperiment <- read.csv(file="data.csv", header=TRUE, encoding="UTF-8")
airportsdf <- read.csv(file="airports.csv", header=TRUE, encoding="UTF-8")



tmp <- as.data.frame(table(dataexperiment$Origin))

airportstmp <- merge(tmp, airportsdf, by.x=c("Var1"), by.y=c("iata"))
airportstmp <- na.omit(airportstmp)

#maps <- get_map(location = "United States", zoom = 3)
library(ggmap)
library(RgoogleMaps)
lat <- c(20,75) #lat f?r ganz
lat <- c(25,55) #lat f?r mittel usa
long <- c(-165,-55) #long f?r ganz
long <- c(-125,-70) #long f?r mittel usa
bbox <- make_bbox(long,lat,f=0.05)
map <- get_map(bbox, maptype = "satellite", source="google")

ggmap(map) + geom_point(data = airportstmp, aes(long,lat))+xlab("Laengengrad")+ylab("Breitengrad")+ggtitle("Flughaefen")


#library(ggplot2)
#ggplot(airportstmp, aes(long,lat)) + geom_point()




##########################Clustert#########################

#####CLustern:

set.seed(1) #zur reproduzierbarkeit gleicher cluster
clusters <- kmeans(airportstmp[,7:8], 10)  #nach geographischer lage in 10 Bereiche geclustert

airportstmp$cluster <- as.factor(clusters$cluster)

#speichern da er jedes mal anders clustert
write.table(airportstmp, "cluster.csv", sep=",",row.names = F)

#####Plotten der Cluster:

#ggplot(airportstmp, aes(long,lat, colour= cluster)) + geom_point()

ggmap(map) + geom_point(data = airportstmp, aes(long,lat, colour = cluster))+xlab("Laengengrad")+ylab("Breitengrad")+ggtitle("Flughaefen Cluster") + guides(color="none")+ scale_color_manual(values = wes_palette(n=10, name="FantasticFox1", type="continuous")) 

###########################Untersuchung####################
