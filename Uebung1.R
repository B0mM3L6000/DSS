#Aufgabe 1:

#setzten des working directories

setwd("/home/martin/Projects/DSS")

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

#l?schen unnoetiger spalten aus haushaltedf fuer aufgabe2.1

Vars <- colnames(haushaltedf)
Vars
Namen <- c("id", "kap_id", "kapitel", "tabelle", "zeile", "quelle", "linktabellenkommentar")
haushaltedfNeu <- haushaltedf[, !(Vars %in% Namen)]

#neues dataframe

haushaltedfNeu


#tab_id gibt an ob haushalte oder haushalte mit kindern
#jahr gibt das jahr an
#raum den stadtteilnamen
#raum_id den stadtteil (wobei 1-9 bezirke und keine stadtteile sind)
#zei_id gibt an wieviele menschen in einem haushalt sind:
# fuer haushalte gesamt:(10=kommulierte zahl, 20=1 person, 30=2 Personen, 40=3 Personen, 50=4 Personen, 60=5oder mehr Personen)
# fuer haushalte mit kindern: (10=kommuliert, 20=1 kind, 30=2 oder mehr Kinder, 40-60 irrelevant)
#zahl ist die anzahl des jeweiligen haushalttypen

#als n?chstes nach stadtteil und jahr gruppieren (f?r 2.1 bei tab_id 10, f?r 2.2 bei tab_id 20) und dann die haushalte 
#mit zei_id 20-60 (bei 2.2 20-30) jeweils ihr Wert mit der zahl mutiplizieren und summieren

#Aufgabe 2.1:

#entfernen der bezirke und nur betrachten von haushalten (kinderhaushalte vernachlaessigt). Ausserdem irrelevante eintraege der zei_id entfernen:

haushaltedfNeu2 <- haushaltedfNeu[haushaltedfNeu$tab_id == 10,]
haushaltedfNeu2 <- haushaltedfNeu2[haushaltedfNeu2$raum_id > 9,]
haushaltedfNeu2 <- haushaltedfNeu2[haushaltedfNeu2$zei_id > 10,]
haushaltedfNeu2

# ersetzten der id fuer bewohneranzahl pro haushalt durch tatsaechliche anzahl

haushaltedfNeu2[haushaltedfNeu2$zei_id == 20,"zei_id"]   <- 1 
haushaltedfNeu2[haushaltedfNeu2$zei_id == 30,"zei_id"]   <- 2  
haushaltedfNeu2[haushaltedfNeu2$zei_id == 40,"zei_id"]   <- 3  
haushaltedfNeu2[haushaltedfNeu2$zei_id == 50,"zei_id"]   <- 4 
haushaltedfNeu2[haushaltedfNeu2$zei_id == 60,"zei_id"]   <- 5  
haushaltedfNeu2

#hinzufuegen neuer spalte: produkt von zei_id und zahl (sind einwohner gesamt fuer die kategorie/jahr/stadtteil)

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

#alles au?er top10 aus dem datenframe entfernen

test2 <- subset(haushaltedfAgg2,haushaltedfAgg2$ID == top10_2[1] | haushaltedfAgg2$ID == top10_2[2] | haushaltedfAgg2$ID == top10_2[3] | haushaltedfAgg2$ID == top10_2[4] | haushaltedfAgg2$ID == top10_2[5] | haushaltedfAgg2$ID == top10_2[6] | haushaltedfAgg2$ID == top10_2[7] | haushaltedfAgg2$ID == top10_2[8] | haushaltedfAgg2$ID == top10_2[9] | haushaltedfAgg2$ID == top10_2[10])
test2
names(test2)[names(test2)=="ID"] <- "Stadtteil"

test2


library(ggplot2)

grafik1 <- ggplot(test2, aes(x=Group.2, y=x, group = Stadtteil, colour = Stadtteil)) + geom_line(size=1) + geom_point(size=2) + xlab("Jahr") + ylab("Einwohnerzahl") + ggtitle("Entwicklung der TOP 10 Stadtteile")  + theme_classic()
grafik1 


#Aufgabe 2.2:

#entfernen der bezirke und nur betrachten von haushalten mit kindern. Au?erdem irrelevante eintraege der zei_id entfernen:

haushaltedfNeu3 <- haushaltedfNeu[haushaltedfNeu$tab_id == 20,]
haushaltedfNeu3 <- haushaltedfNeu3[haushaltedfNeu3$raum_id > 9,]
haushaltedfNeu3 <- haushaltedfNeu3[haushaltedfNeu3$zei_id > 10,]
haushaltedfNeu3 <- haushaltedfNeu3[haushaltedfNeu3$zei_id < 40,]
haushaltedfNeu3

# ersetzten der id f?r kinderanzahl pro haushalt durch tatsaechliche anzahl

haushaltedfNeu3[haushaltedfNeu3$zei_id == 20,"zei_id"]   <- 1 
haushaltedfNeu3[haushaltedfNeu3$zei_id == 30,"zei_id"]   <- 2  
haushaltedfNeu3

#hinzuf?gen neuer spalte: produkt von zei_id und zahl (sind einwohner gesamt f?r die kategorie/jahr/stadtteil)

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

#alles au?er top10 aus dem datenframe entfernen

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

#loeschen unnoetiger spalten aus haushaltedf fuer aufgabe 3

Vars <- colnames(bewegungdf)
Vars
Namen <- c("id", "kap_id", "kapitel", "tabelle", "zeile", "quelle", "linktabellenkommentar")
bewegungdfNeu <- bewegungdf[, !(Vars %in% Namen)]

#neues dataframe

bewegungdfNeu

#entfernen der Stadtbezirke + gesamt koeln

bewegungdfNeu <- bewegungdfNeu[bewegungdfNeu$raum_id > 9,]

bewegungdfNeu

#bedeutungen:
#tab_id: 10 = umzuege (verschiedener sorten)  /   20 = sterbe- und geburtenzahlen
#bei tab_id 10 -> zei_id:
# 10: Fortzug ueber Stadtgrenze (Aussen)
# 20: Fortzug innerhalb (Binnen)
# 30: Zuzug ueber Stadtgrenze (Aussen)
# 40: Zuzug innerhalb (Binnen)
# 50: innerhalb des Stadtteils selber
#bei tab_id 20 -> zei_id:
# 10: Sterbefall
# 20: Geburt

#Raum und Raum_id sind name des Stadtteils + ID
#Jahr ist das Jahr
#Zahl is die Anzahl des Falls der entsprechenden zei_id


# Als naechstes muessen fuer jeden der gefragten 7 Faelle subsets auf das Dataframe gemacht werden

#Fall 1: Aussenfortzug (zei_id: 10 und tab_id: 10):

aufgabe31df <- subset(bewegungdfNeu, bewegungdfNeu$zei_id == 10 & bewegungdfNeu$tab_id == 10)
aufgabe31df


#entfernen der unwichtigen spalten:
Vars <- colnames(aufgabe31df)
Vars
Namen <- c("tab_id", "raum_id", "zei_id")
aufgabe31df <- aufgabe31df[, !(Vars %in% Namen)]
aufgabe31df

#mergen mit dataframe aus aufgabe2 für gesamteinwohner:

aufgabe31mergedf <- merge(aufgabe31df, haushaltedfAgg2, by.x=c("raum", "jahr"), by.y=c("ID", "Group.2"))
aufgabe31mergedf

#neue spalte mit Aussenfortzuege pro Einwohner des Stadtteils im jeweiligen Jahr:
#(diese Spalte ist gleichzeitig die Lösung)

aufgabe31mergedf <- cbind(aufgabe31mergedf, "Anteil"=aufgabe31mergedf$zahl / aufgabe31mergedf$x)
aufgabe31mergedf

#Fall 2: Binnenfortzug (zei_id: 20 und tab_id: 10):

aufgabe32df <- subset(bewegungdfNeu, bewegungdfNeu$zei_id == 20 & bewegungdfNeu$tab_id == 10)
aufgabe32df


#entfernen der unwichtigen spalten:
Vars <- colnames(aufgabe32df)
Vars
Namen <- c("tab_id", "raum_id", "zei_id")
aufgabe32df <- aufgabe32df[, !(Vars %in% Namen)]
aufgabe32df

#mergen mit dataframe aus aufgabe2 für gesamteinwohner:

aufgabe32mergedf <- merge(aufgabe32df, haushaltedfAgg2, by.x=c("raum", "jahr"), by.y=c("ID", "Group.2"))
aufgabe32mergedf

#neue spalte mit Aussenfortzuege pro Einwohner des Stadtteils im jeweiligen Jahr:
#(diese Spalte ist gleichzeitig die Lösung)

aufgabe32mergedf <- cbind(aufgabe32mergedf, "Anteil"=aufgabe32mergedf$zahl / aufgabe32mergedf$x)
aufgabe32mergedf

#Fall 3: Aussenzuzug (zei_id: 30 und tab_id: 10):

aufgabe33df <- subset(bewegungdfNeu, bewegungdfNeu$zei_id == 30 & bewegungdfNeu$tab_id == 10)
aufgabe33df


#entfernen der unwichtigen spalten:
Vars <- colnames(aufgabe33df)
Vars
Namen <- c("tab_id", "raum_id", "zei_id")
aufgabe33df <- aufgabe33df[, !(Vars %in% Namen)]
aufgabe33df

#mergen mit dataframe aus aufgabe2 für gesamteinwohner:

aufgabe33mergedf <- merge(aufgabe33df, haushaltedfAgg2, by.x=c("raum", "jahr"), by.y=c("ID", "Group.2"))
aufgabe33mergedf

#neue spalte mit Aussenfortzuege pro Einwohner des Stadtteils im jeweiligen Jahr:
#(diese Spalte ist gleichzeitig die Lösung)

aufgabe33mergedf <- cbind(aufgabe33mergedf, "Anteil"=aufgabe33mergedf$zahl / aufgabe33mergedf$x)
aufgabe33mergedf

#Fall 4: Binnenzuzug (zei_id: 40 und tab_id: 10):

aufgabe34df <- subset(bewegungdfNeu, bewegungdfNeu$zei_id == 40 & bewegungdfNeu$tab_id == 10)
aufgabe34df


#entfernen der unwichtigen spalten:
Vars <- colnames(aufgabe34df)
Vars
Namen <- c("tab_id", "raum_id", "zei_id")
aufgabe34df <- aufgabe34df[, !(Vars %in% Namen)]
aufgabe34df

#mergen mit dataframe aus aufgabe2 für gesamteinwohner:

aufgabe34mergedf <- merge(aufgabe34df, haushaltedfAgg2, by.x=c("raum", "jahr"), by.y=c("ID", "Group.2"))
aufgabe34mergedf

#neue spalte mit Aussenfortzuege pro Einwohner des Stadtteils im jeweiligen Jahr:
#(diese Spalte ist gleichzeitig die Lösung)

aufgabe34mergedf <- cbind(aufgabe34mergedf, "Anteil"=aufgabe34mergedf$zahl / aufgabe34mergedf$x)
aufgabe34mergedf

#Fall 5: Umzug innerhalb des Stadtteils (zei_id: 50 und tab_id: 10):

aufgabe35df <- subset(bewegungdfNeu, bewegungdfNeu$zei_id == 50 & bewegungdfNeu$tab_id == 10)
aufgabe35df


#entfernen der unwichtigen spalten:
Vars <- colnames(aufgabe35df)
Vars
Namen <- c("tab_id", "raum_id", "zei_id")
aufgabe35df <- aufgabe35df[, !(Vars %in% Namen)]
aufgabe35df

#mergen mit dataframe aus aufgabe2 für gesamteinwohner:

aufgabe35mergedf <- merge(aufgabe35df, haushaltedfAgg2, by.x=c("raum", "jahr"), by.y=c("ID", "Group.2"))
aufgabe35mergedf

#neue spalte mit Aussenfortzuege pro Einwohner des Stadtteils im jeweiligen Jahr:
#(diese Spalte ist gleichzeitig die Lösung)

aufgabe35mergedf <- cbind(aufgabe35mergedf, "Anteil"=aufgabe35mergedf$zahl / aufgabe35mergedf$x)
aufgabe35mergedf

#Fall 6: Sterbefaelle (zei_id: 10 und tab_id: 20):

aufgabe36df <- subset(bewegungdfNeu, bewegungdfNeu$zei_id == 10 & bewegungdfNeu$tab_id == 20)
aufgabe36df


#entfernen der unwichtigen spalten:
Vars <- colnames(aufgabe36df)
Vars
Namen <- c("tab_id", "raum_id", "zei_id")
aufgabe36df <- aufgabe36df[, !(Vars %in% Namen)]
aufgabe36df

#mergen mit dataframe aus aufgabe2 für gesamteinwohner:

aufgabe36mergedf <- merge(aufgabe36df, haushaltedfAgg2, by.x=c("raum", "jahr"), by.y=c("ID", "Group.2"))
aufgabe36mergedf

#neue spalte mit Aussenfortzuege pro Einwohner des Stadtteils im jeweiligen Jahr:
#(diese Spalte ist gleichzeitig die Lösung)

aufgabe36mergedf <- cbind(aufgabe36mergedf, "Anteil"=aufgabe36mergedf$zahl / aufgabe36mergedf$x)
aufgabe36mergedf

#Fall 7: Geburten (zei_id: 20 und tab_id: 20):

aufgabe37df <- subset(bewegungdfNeu, bewegungdfNeu$zei_id == 20 & bewegungdfNeu$tab_id == 20)
aufgabe37df


#entfernen der unwichtigen spalten:
Vars <- colnames(aufgabe37df)
Vars
Namen <- c("tab_id", "raum_id", "zei_id")
aufgabe37df <- aufgabe37df[, !(Vars %in% Namen)]
aufgabe37df

#mergen mit dataframe aus aufgabe2 für gesamteinwohner:

aufgabe37mergedf <- merge(aufgabe37df, haushaltedfAgg2, by.x=c("raum", "jahr"), by.y=c("ID", "Group.2"))
aufgabe37mergedf

#neue spalte mit Aussenfortzuege pro Einwohner des Stadtteils im jeweiligen Jahr:
#(diese Spalte ist gleichzeitig die Lösung)

aufgabe37mergedf <- cbind(aufgabe37mergedf, "Anteil"=aufgabe37mergedf$zahl / aufgabe37mergedf$x)
aufgabe37mergedf



# Die Spalte "Anteil gibt jeweils das Verhältnis des jeweiligen Falls (1-7) zur gesamten Einwohnerzahl des Jahres an
# (Bspw.: Anteil von 0.01 entspricht 1%)




############################################################################

#Aufgabe 4:
