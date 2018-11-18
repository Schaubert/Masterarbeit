##### Erste Datensatzqualitätsanalysen ####

## source global.r
source("./Code/global.r")

#### Daten einlesen ####
data <- readRDS("./Data/data.rds") %>% 
  setDT

setkeyv(data, cols = c("Names", "Saisons"))

data[, Spielminuten_lagg := c(0,Spielminuten)] %>% 
  .[, Pass_lagg := c(0,Pass)] %>% 
  .[Spielminuten == Spielminuten_lagg & 
      Spielminuten != 0 &
      Pass == Pass_lagg, 
    Problem := "Datenduplikat"]

## [[[[]]]] Erstmal checken, ob die ganzen Zahlen in der Doku noch stimmen, wegen neuem Duplikatsfilter

## Check, wie viele Zeilen nach Datenduplikaten noch übrig bleiben
sum(is.na(data[,Problem]))

## Datensatz anpassen
data <- data[is.na(Problem)] %>% 
  .[, Pass_lagg:= NULL] %>% 
  .[, Spielminuten_lagg := NULL]

#### Datenqualitätsanalysen ####

## Überprüfen, wie viele Zeilen mindestens 4 Spiele = 360 Minuten haben
nrow(data[Spielminuten >= 360])
# 2045
## Erst mal vernünftige Datengrundlage

## Histogram für Verteilung der Spielminuten
hist(data[, Spielminuten], breaks = 40)
# vernünftig gleichverteilt (0 als Ausreißer)

## Summary v.a. der numerischen Variablen
summary(data)
# Werte klingen auf ersten Eindruck plausibel

## Beispiel
data[which.max(Gegentore)]
# Timo Horn

## Anzahl an Spielern
data[, Names] %>% unique %>% length

## Wie viele Leistungsdaten pro Zeile?
table(data[, Anzahl_Leistungsdaten])

#### Filter nach Daten mit mindestens 4 Spielen Spielzeit ####

data_f <- data[Spielminuten >= 360]

## Wie viele Leistungsdaten pro Zeile?
table(data_f[, Anzahl_Leistungsdaten])
# 71 Datenzeilen mit 7 oder weniger Leistungsdaten (dann Sprung auf mind. 10) 
## --> genauer betrachten

test <- data_f[, Anzahl_Leistungsdaten := as.numeric(Anzahl_Leistungsdaten)] %>% 
  .[Anzahl_Leistungsdaten < 10]

## [[[[[]]]]] Baumgartlinger nachscrapen!

## Meiste wegen Jugend Bundesliga falsch oder Datenduplikat aus der Zeit mit
## wenigen Daten oder aus Österreichischer Bundesliga mit weniger Daten; 
## 
## Ausnahmen:
## Julian Baumgartlinger 2011/2012 --> per Hand nachscrapen

## Histogram für Verteilung der Spielminuten
hist(data_f[, Spielminuten], breaks = 30)
# Ziemlich gleichverteilt

summary(data_f[, .(Spielminuten, Eigentore, Assists, Schuss, Schussvorlagen, 
                   Ballkontakte, Pass, Pass_angekommen, Fehlpass, Passprozente,
                   Zweikampf, Zweikampfprozente, Fouls, Gefoult, Abseits, 
                   Laufweite, Sprints, Geschwindigkeit, Tore, Fusstore, 
                   Kopftore, Elfmetertore, Elfmeterverschossen, Gegentore, 
                   Gehalten, Gehaltenelfer)])

## Erkenntnis: gleich viele NAs bei Gegentore und Gehalten, Gehaltenelfer etwas
## mehr; spricht für Torhüter. Rest wird mit NAs belassen! Bei Analysen sollte
## sowieso nach Torhütern für diese Daten gefiltert werden

