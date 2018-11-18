##### Merge der erst gescrapeten Daten und der nach-gescrapeten Daten #####

## source global.r
source("./Code/global.r")

#### Datensätze einlesen ####

data_first <- readRDS("./Data/data_cleaned.rds") %>% 
  .[, Spielminuten_lagg := NULL]
data_second <- readRDS("./Data/nach_gescraped_data_cleaned.rds")

#### > Merge ####

data <- rbind(data_first, data_second, use.names = TRUE)

#### >> Doppelten Julian Baumgartlinger raus ####
to_deleted_row <- which(data[, Names] == "Julian Baumgartlinger")[1]
data <- data[c(1:(to_deleted_row-1), (to_deleted_row+1):nrow(data))]

#### Daten auffüllen ####

## Fülle Zeilen mit 0en für die Spieler, die kein Bundesligaspiel in der Saison
## bestritten haben

data %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Spielminuten := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Eigentore := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Assists := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Schuss := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Schussvorlagen := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Ballkontakte := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Pass := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Pass_angekommen := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Fehlpass := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Passprozente := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Zweikampf := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Zweikampfprozente := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Fouls := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Gefoult := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Abseits := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Laufweite := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Sprints := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Geschwindigkeit := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Tore := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Fusstore := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Kopftore := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Elfmetertore := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Elfmeterverschossen := 0,] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Gegentore := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Gehalten := 0] %>% 
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", Gehaltenelfer := 0]

# saveRDS(data, "./Data/data.rds")
