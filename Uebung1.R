#Aufgabe 1:

#setzten des working directories

setwd("D:/Projekte/CI")

#einlesen der csv files

beschaeftigte <- "Koeln_Beschaeftigte.csv"
bewegung <- "Koeln_Bevoelkerungsbewegung.csv"
dichte <- "Koeln_Flaeche_Dichte_Einwohner.csv"
geburtentode <- "Koeln_GeburtenSterbefaelle.csv"
haushalte <- "Koeln_Haushalte.csv"

#einlesen in dataframes

beschaeftigtedf <- read.csv(beschaeftigte, sep=",", header=TRUE, encoding="UTF-8")
bewegungdf <- read.table(bewegung, sep=";", header=TRUE, encoding="UTF-8")
dichtedf <- read.table(dichte, sep=",", header=TRUE, encoding="UTF-8")
geburtentodedf <- read.table(geburtentode, sep=",", header=TRUE, encoding="UTF-8")
haushaltedf <- read.table(haushalte, sep=";", header=TRUE, encoding="UTF-8")

#ausgeben (zu testzwecken)

beschaeftigtedf
bewegungdf
dichtedf
geburtentodedf
haushaltedf

############################################################################


#Aufgabe 2:

#löschen unnötiger spalten aus haushaltedf für aufgabe2.1

Vars <- colnames(haushaltedf)
Vars
Namen <- c("X.U.FEFF.id", "kap_id", "kapitel", "tabelle", "zeile", "quelle", "linktabellenkommentar")
haushaltedfNeu <- haushaltedf[, !(Vars %in% Namen)]

#neues dataframe

haushaltedfNeu


#tab_id gibt an ob haushalte oder haushalte mit kindern
#jahr gibt das jahr an
#raum den stadtteilnamen
#raum_id den stadtteil (wobei 1-9 bezirke und keine stadtteile sind)
#zei_id gibt an wieviele menschen in einem haushalt sind:
# für haushalte gesamt:(10=kommulierte zahl, 20=1 person, 30=2 Personen, 40=3 Personen, 50=4 Personen, 60=5oder mehr Personen)
# für haushalte mit kindern: (10=kommuliert, 20=1 kind, 30=2 oder mehr Kinder, 40-60 irrelevant)
#zahl ist die anzahl des jeweiligen haushalttypen

#als nächstes nach stadtteil und jahr gruppieren (für 2.1 bei tab_id 10, für 2.2 bei tab_id 20) und dann die haushalte 
#mit zei_id 20-60 (bei 2.2 20-30) jeweils ihr Wert mit der zahl mutiplizieren und summieren

#Aufgabe 2.1:

#entfernen der bezirke und nur betrachten von haushalten (kinderhaushalte vernachlässigt). Außerdem irrelevante einträge der zei_id entfernen:

haushaltedfNeu2 <- haushaltedfNeu[haushaltedfNeu$tab_id == 10,]
haushaltedfNeu2 <- haushaltedfNeu2[haushaltedfNeu2$raum_id > 9,]
haushaltedfNeu2 <- haushaltedfNeu2[haushaltedfNeu2$zei_id > 10,]
haushaltedfNeu2

# ersetzten der id für bewohneranzahl pro haushalt durch tatsächliche anzahl

haushaltedfNeu2[haushaltedfNeu2$zei_id == 20,"zei_id"]   <- 1 
haushaltedfNeu2[haushaltedfNeu2$zei_id == 30,"zei_id"]   <- 2  
haushaltedfNeu2[haushaltedfNeu2$zei_id == 40,"zei_id"]   <- 3  
haushaltedfNeu2[haushaltedfNeu2$zei_id == 50,"zei_id"]   <- 4 
haushaltedfNeu2[haushaltedfNeu2$zei_id == 60,"zei_id"]   <- 5  
haushaltedfNeu2

#hinzufügen neuer spalte: produkt von zei_id und zahl (sind einwohner gesamt für die kategorie/jahr/stadtteil)

haushaltedfNeu2 <- cbind(haushaltedfNeu2, "Summe"=haushaltedfNeu2$zei_id * haushaltedfNeu2$zahl)
haushaltedfNeu2

# summieren der Zeilen geordnet durch jahr und stadtteil

haushaltedfAgg2 <- aggregate(haushaltedfNeu2[,c("Summe")], by=list(ID=haushaltedfNeu2$raum, haushaltedfNeu2$jahr), sum)
haushaltedfAgg2

#was sind die top 10 im jahr 2012?
#erst nur alle 2012 jahre betrachten -> sortieren nach einwohnern -> abschneiden aller unter top10 -> ausgeben

top10_2 <- haushaltedfAgg2[haushaltedfAgg2$Group.2 == 2012,]
top10_2 <- top10_2[order(top10_2$x),] 
top10_2 <- tail(top10_2, 10)
top10_2

Vars <- colnames(top10_2)
Namen <- c("Group.2", "x")
top10_2 <- top10_2[, !(Vars %in% Namen)]

#Vektor mit den top10 in 2012 nach einwohnerzahl:

top10_2

#alles außer top10 aus dem datenframe entfernen

test2 <- subset(haushaltedfAgg2,haushaltedfAgg2$ID == top10_2[1] | haushaltedfAgg2$ID == top10_2[2] | haushaltedfAgg2$ID == top10_2[3] | haushaltedfAgg2$ID == top10_2[4] | haushaltedfAgg2$ID == top10_2[5] | haushaltedfAgg2$ID == top10_2[6] | haushaltedfAgg2$ID == top10_2[7] | haushaltedfAgg2$ID == top10_2[8] | haushaltedfAgg2$ID == top10_2[9] | haushaltedfAgg2$ID == top10_2[10])
test2
names(test2)[names(test2)=="ID"] <- "Stadtteil"

test2


library(ggplot2)

grafik1 <- ggplot(test2, aes(x=Group.2, y=x, group = Stadtteil, colour = Stadtteil)) + geom_line(size=1) + geom_point(size=2) + xlab("Jahr") + ylab("Einwohnerzahl") + ggtitle("Entwicklung der TOP 10 Stadtteile")  + theme_classic()
grafik1 


#Aufgabe 2.2:

#entfernen der bezirke und nur betrachten von haushalten mit kindern. Außerdem irrelevante einträge der zei_id entfernen:

haushaltedfNeu3 <- haushaltedfNeu[haushaltedfNeu$tab_id == 20,]
haushaltedfNeu3 <- haushaltedfNeu3[haushaltedfNeu3$raum_id > 9,]
haushaltedfNeu3 <- haushaltedfNeu3[haushaltedfNeu3$zei_id > 10,]
haushaltedfNeu3 <- haushaltedfNeu3[haushaltedfNeu3$zei_id < 40,]
haushaltedfNeu3

# ersetzten der id für kinderanzahl pro haushalt durch tatsächliche anzahl

haushaltedfNeu3[haushaltedfNeu3$zei_id == 20,"zei_id"]   <- 1 
haushaltedfNeu3[haushaltedfNeu3$zei_id == 30,"zei_id"]   <- 2  
haushaltedfNeu3

#hinzufügen neuer spalte: produkt von zei_id und zahl (sind einwohner gesamt für die kategorie/jahr/stadtteil)

haushaltedfNeu3 <- cbind(haushaltedfNeu3, "Summe"=haushaltedfNeu3$zei_id * haushaltedfNeu3$zahl)
haushaltedfNeu3

# summieren der Zeilen geordnet durch jahr und stadtteil

haushaltedfAgg3 <- aggregate(haushaltedfNeu3[,c("Summe")], by=list(ID=haushaltedfNeu3$raum, haushaltedfNeu3$jahr), sum)
haushaltedfAgg3

#was sind die top 10 im jahr 2012?
#erst nur alle 2012 jahre betrachten -> sortieren nach einwohnern -> abschneiden aller unter top10 -> ausgeben

top10_3 <- haushaltedfAgg3[haushaltedfAgg3$Group.2 == 2012,]
top10_3 <- top10_3[order(top10_3$x),] 
top10_3 <- tail(top10_3, 10)
top10_3

Vars <- colnames(top10_3)
Namen <- c("Group.2", "x")
top10_3 <- top10_3[, !(Vars %in% Namen)]

#Vektor mit den top10 in 2012 nach einwohnerzahl:

top10_3

#alles außer top10 aus dem datenframe entfernen

test3 <- subset(haushaltedfAgg3,haushaltedfAgg3$ID == top10_3[1] | haushaltedfAgg3$ID == top10_3[2] | haushaltedfAgg3$ID == top10_3[3] | haushaltedfAgg3$ID == top10_3[4] | haushaltedfAgg3$ID == top10_3[5] | haushaltedfAgg3$ID == top10_3[6] | haushaltedfAgg3$ID == top10_3[7] | haushaltedfAgg3$ID == top10_3[8] | haushaltedfAgg3$ID == top10_3[9] | haushaltedfAgg3$ID == top10_3[10])
test3
names(test3)[names(test3)=="ID"] <- "Stadtteil"

test3


#plotten:

library(ggplot2)

grafik2 <- ggplot(test3, aes(x=Group.2, y=x, group = Stadtteil, colour = Stadtteil)) + geom_line(size=1) + geom_point(size=2) + xlab("Jahr") + ylab("Kinderzahl") + ggtitle("Entwicklung der TOP 10 Stadtteile")  + theme_classic()
grafik2 




############################################################################


#Aufgabe 3:

#löschen unnötiger spalten aus haushaltedf für aufgabe 3

Vars <- colnames(bewegungdf)
Vars
Namen <- c("X.U.FEFF.id", "kap_id", "kapitel", "tabelle", "zeile", "quelle", "linktabellenkommentar")
bewegungdfNeu <- bewegungdf[, !(Vars %in% Namen)]

#neues dataframe

bewegungdfNeu







############################################################################

#Aufgabe 4:

