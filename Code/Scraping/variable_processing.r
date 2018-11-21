##### Variablen hinzufügen und umwandeln ####

## source global.r
source("./Code/global.r")

#### Daten laden ####
data <- readRDS("./Data/cleaned_leistungsdaten_positionen.rds") %>% 
  setDT

#### Beispiel rpart ####

rpart(Hauptposition ~ Eigentore + Assists + Schuss + Schussvorlagen + 
        Ballkontakte + Pass +Passprozente + Zweikampf + Zweikampfprozente + 
        Fouls + Gefoult + Abseits + Laufweite + Sprints + Geschwindigkeit + Tore
      + Kopftore + Gegentore + Gehalten + Gehaltenelfer, data = data) %>% 
  rpart.plot

## Interessant die Aufteilung zwischen Rechter und Linker Verteidiger anhand
## der Anzahl der Schüsse

#### Hinzufügen einer Variable zu aggregierten Positionen ####

data %>% 
  .[, Hauptposition_adj := Hauptposition] %>% 
  .[Hauptposition %in% c("Linker Verteidiger", "Rechter Verteidiger"),
    Hauptposition_adj := "Außenverteidiger"] %>% 
  .[Hauptposition == "Hängende Spitze", 
    Hauptposition_adj := "Offensives Mittelfeld"] %>% 
  .[Hauptposition %in% c("Linkes Mittelfeld", "Rechtes Mittelfeld"),
    Hauptposition_adj := "Mittelfeld Außen"] %>% 
  .[Hauptposition %in% c("Linksaußen", "Rechtsaußen"),
    Hauptposition_adj := "Flügelspieler"] %>% 
  .[, Zweitposition_adj := Zweitposition] %>% 
  .[Zweitposition %in% c("Linker Verteidiger", "Rechter Verteidiger"),
    Zweitposition_adj := "Außenverteidiger"] %>% 
  .[Zweitposition == "Hängende Spitze", 
    Zweitposition_adj := "Offensives Mittelfeld"] %>% 
  .[Zweitposition %in% c("Linkes Mittelfeld", "Rechtes Mittelfeld"),
    Zweitposition_adj := "Mittelfeld außen"] %>% 
  .[Zweitposition %in% c("Linksaußen", "Rechtsaußen"),
    Zweitposition_adj := "Flügelstürmer"]

# Libero als Defensives Mittelfeld oder Innenverteidiger?
# Eventuell durch Clustern bestimmen!

rpart(Hauptposition_adj ~ Eigentore + Assists + Schuss + Schussvorlagen + 
        Ballkontakte + Pass +Passprozente + Zweikampf + Zweikampfprozente + 
        Fouls + Gefoult + Abseits + Laufweite + Sprints + Geschwindigkeit + Tore
      + Kopftore + Gegentore + Gehalten + Gehaltenelfer, data = data) %>% 
  rpart.plot(box.palette = list("green3", "mistyrose2", "red", "gold2", "blue", "yellow", 
                                "orange", "gray", "pink", "brown"))


#### Augefüllte 0en bei Abseits überprüfen ####
data[Abseits == 0, Hauptposition_adj] %>% table

## [[[[]]]] Hier geht es weiter!