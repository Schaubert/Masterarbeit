##### Erste Datensatzqualitätsanalysen ####

## source global.r
source("./Code/global.r")

#### Daten einlesen ####
data <- readRDS("./Data/data.rds") %>% 
  setDT %>% 
  .[, Anzahl_Leistungsdaten := as.numeric(Anzahl_Leistungsdaten)] %>% 
  .[, Groesse := as.numeric(Groesse)] %>% 
  .[, Gewicht := as.numeric(Gewicht)]

setkeyv(data, cols = c("Names", "Saisons"))

data[, Spielminuten_lagg := c(0,Spielminuten)] %>% 
  .[, Pass_lagg := c(0,Pass)] %>% 
  .[Spielminuten == Spielminuten_lagg & 
      Spielminuten != 0 &
      Pass == Pass_lagg, 
    Problem := "Datenduplikat"]

## Check, wie viele Zeilen nach Datenduplikaten noch übrig bleiben
sum(is.na(data[,Problem]))

## Datensatz anpassen
data <- data[is.na(Problem)] %>% 
  .[, Pass_lagg:= NULL] %>% 
  .[, Spielminuten_lagg := NULL]

#### Datenqualitätsanalysen ####

## Überprüfen, wie viele Zeilen mindestens 4 Spiele = 360 Minuten haben
nrow(data[Spielminuten >= 360])
# 2052
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
# 449

## Wie viele Leistungsdaten pro Zeile?
table(data[, Anzahl_Leistungsdaten])

#### Filter nach Daten mit mindestens 4 Spielen Spielzeit ####

data_f <- data[Spielminuten >= 360]

## Wie viele Leistungsdaten pro Zeile?
table(data_f[, Anzahl_Leistungsdaten])
# 70 Datenzeilen mit 7 oder weniger Leistungsdaten (dann Sprung auf mind. 10) 
## --> genauer betrachten

test <- data_f[Anzahl_Leistungsdaten < 10]

## Meiste wegen Jugend Bundesliga falsch oder Datenduplikat aus der Zeit mit
## wenigen Daten oder aus Österreichischer Bundesliga mit weniger Daten; 
## 
## Ausnahmen:
## Julian Baumgartlinger 2011/2012 --> per Hand nachscrapen --> erledigt

data_f <- data_f[Anzahl_Leistungsdaten > 9]
# 1982

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

#### NAs überprüfen ###
#### > Laufleistung, Sprints, Geschwindigkeit ####

test <- data_f[is.na(Laufweite)]
## Nur aus Saisons 2009 bis 2011 (wohl noch nicht bei allen getrackt, 
## als NA belassen!). Genauso Sprints und Geschwindigkeit. 2 mehr NAs bei
## Sprints, eventuell 0er, aber als NA ist man sicher

#### > Zweikampf, zwiekampfprozente ####

test <- data_f[is.na(Zweikampf)]
## Nur Torhüter
## 100% gewonnene Zweikämpfe bei NA Zweikämpfen (ziemlich sicher 0), ist nicht
## plausibel; mit 0en ersetzen!

data_f[is.na(Zweikampf), Zweikampfprozente := 0] %>% 
  .[is.na(Zweikampf), Zweikampf := 0]

data_f[is.na(Zweikampfprozente)]
## Mag sein, dass Roman Weidenfeller beide Zweikämpfe verloren hat, aber lieber 
## NA lassen!

#### > Fouls, Gefoult ####

test <- data_f[is.na(Fouls)]
## Nur Torhüter und Feldspieler mit wenigen Spielminuten. 0en klingen plausibel!
test <- data_f[is.na(Gefoult)]
## Nur Torhüter und Feldspieler mit wenigen Spielminuten. 0en klingen plausibel!

data_f[is.na(Fouls), Fouls := 0] %>% 
  .[is.na(Gefoult), Gefoult := 0]

#### > Abseits ####

test <- data_f[is.na(Abseits)]
## 698 von 1982 ohne Abseitsdaten. Torhüter und Abwehrspieler oft Saisons ohne
## Abseits. Zahl klingt plausibel. Kleine Kontrolle via Hand, bestätigt Theorie
## von Torhütern und Abwehrspielern. Bei Positionsmatching evtl. nochmal
## überprüfen!

data_f[is.na(Abseits), Abseits := 0]

#### > Tore, Fusstore, Kopftore, Elfmetertore, Elfmeterverschossen ####

## Die Tore sind die detailliertesten Daten von allen, selbst von 
## Nicht-Bundesliga-Daten, die NA Zahlen passen auch mit Torhütern und 
## Nicht-Torschützen in etwa zusammen; wird alles 0 gesetzt

data_f[is.na(Tore), Tore := 0] %>% 
  .[is.na(Fusstore), Fusstore := 0] %>% 
  .[is.na(Kopftore), Kopftore := 0] %>% 
  .[is.na(Elfmetertore), Elfmetertore := 0] %>% 
  .[is.na(Elfmeterverschossen), Elfmeterverschossen := 0]

#### > Gegentore, Gehalten, Gehaltenelfer ####

## 1834 NAs bei Gegentore und Gehalten spricht für Torhüterfilter; Gehaltenelfer
## etwas mehr NAs, auch plausibel. Also mit 0en auffüllen

data_f[is.na(Gegentore), Gegentore := 0] %>% 
  .[is.na(Gehalten), Gehalten := 0] %>% 
  .[is.na(Gehaltenelfer), Gehaltenelfer := 0]

#### > Eigentore ####

## 1879 Zeilen ohne Eigentore; klingt plausibel

data_f[is.na(Eigentore), Eigentore := 0]

#### > Assists, Schüsse, Schussvorlagen ####

test <- data_f[is.na(Assists)]
## Torhüter, Verteidiger oder wenige Spielminuten. Klingt plausibel. Mit 0en 
## auffüllen

test <- data_f[is.na(Schuss)]
## Torhüter, manche Verteidiger, ab und zu Spieler mit wenig Spielminuten.
## Klingt plausibel, mit 0en füllen

test <- data_f[is.na(Schussvorlagen)]
## Torhüter, manche Verteidiger, ab und zu Spieler mit wenig Spielminuten.
## Klingt plausibel, mit 0en füllen

data_f[is.na(Abseits), Abseits := 0] %>% 
  .[is.na(Schuss), Schuss := 0] %>% 
  .[is.na(Schussvorlagen), Schussvorlagen := 0]

# saveRDS(data_f, file = "./Data/cleaned_data_after_scrape.rds")
