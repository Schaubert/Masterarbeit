##### Erste Datensatzqualitätsanalysen ####

## source global.r
source("./Code/global.r")

#### Daten einlesen ####
data <- readRDS("./Data/data.rds")

#### Datenqualitätsanalysen ####

## Überprüfen, wie viele Zeilen mindestens 4 Spiele = 360 Minuten haben
nrow(data[Spielminuten >= 360])
# 2147
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
