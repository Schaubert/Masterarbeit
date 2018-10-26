##### Scraping für Spielerlisten #####

## source global.r
source("./Code/global.r")

#### > Durchlaufe Webpages für Spielerlisten ####

## Stamm-URL
page_url <- "https://www.weltfussball.de/spielerliste/bundesliga-"

## Saisons, die interessieren (und noch ein paar mehr)
saisons <- seq(2009,2018)

## Zusammengesetzte URLS
urls <- vapply(saisons, 
               function(X){paste0(page_url, X, "-", X+1, "/nach-name/")}, 
               FUN.VALUE = "character")

#### > Beispiel von Scrapen für 1 Saison ####

## Scrape Data von URL
webdata <- xml2::read_html(paste0(urls[1], 13))

## Verarbeite Data von URL
data_processed <- html_nodes(webdata, ".standard_tabelle") %>% 
  html_text %>% 
  .[1] ## Nur Tabelle, 2. Element wäre "Aktuelle Meldungen"

## Säubere Daten
data_cleaned <- data_processed %>% 
  str_remove("Spieler\r\n\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\tMannschaft\r\n\t\t\t\t\t\t\t\tGeboren\r\n\t\t\t\t\t\t\t\tGröße\r\n\t\t\t\t\t\t\t\tPosition\r\n\t\t\t\t\t\t\t") %>% 
  str_replace_all("\r\n\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t", ",") %>% 
  str_replace_all("\r\n\t\t\t\t\t\t\t\t", ",") %>% #Formatierungen weg, Kommas als Replacement
  str_replace_all("\r\n\t\t\t\t\t\t\t", ",") %>% 
  str_split(",") %>% #Aufbrechen
  unlist(.) %>% 
  .[-length(.)] #letzter leerer String muss weg

objects <- length(data_cleaned)
objects %% 5 == 0 ## Soll immer TRUE sein!

## Fülle Daten in ein Data.Table
names <- data_cleaned[seq(1, objects, 5)]
club <- data_cleaned[seq(2, objects, 5)]
birthday <- data_cleaned[seq(3, objects, 5)]
height <- data_cleaned[seq(4, objects, 5)] %>% str_remove_all(" cm")
position <- data_cleaned[seq(5, objects, 5)]

dt <- data.table(names = names, 
                 club = club, 
                 birthday = birthday, 
                 height = height, 
                 position = position)

## Format gefällt mir! Funktion drum herum schreiben!

#### > Funktion fürs Scrapen ####

scrape_spielerliste <- function(saison = 2009){
  
  ## Stamm-URL
  page_url <- "https://www.weltfussball.de/spielerliste/bundesliga-"
  
  ## Zusammengesetzte finale URL
  url_final <- paste0(page_url, saison, "-", saison+1, "/nach-name/")
  
  ## Initialisiere ausgegebenen Data.Table
  dt <- data.table()
  
  for(i in 1:20){
    ## Scrape Data von URL
    webdata <- xml2::read_html(paste0(url_final, i))
    
    ## Verarbeite Data von URL
    data_processed <- html_nodes(webdata, ".standard_tabelle") %>% 
      html_text %>% 
      .[1] ## Nur Tabelle, 2. Element wäre "Aktuelle Meldungen"
    
    ## Die Tabellen sind in 50er Packs aufgeteilt, falls in einer Tabelle keine
    ## Spieler mehr sind, wird das Folgende angezeigt:
    false_string <- "Spieler\r\n\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\tMannschaft\r\n\t\t\t\t\t\t\t\tGeboren\r\n\t\t\t\t\t\t\t\tGröße\r\n\t\t\t\t\t\t\t\tPosition\r\n\t\t\t\t\t\t\tEs sind noch keine Daten vorhanden.\r\n\t\t\t\t\t\t\t"
    
    if(data_processed != false_string){
      
      ## Säubere Daten
      data_cleaned <- data_processed %>% 
        str_remove("Spieler\r\n\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\tMannschaft\r\n\t\t\t\t\t\t\t\tGeboren\r\n\t\t\t\t\t\t\t\tGröße\r\n\t\t\t\t\t\t\t\tPosition\r\n\t\t\t\t\t\t\t") %>% 
        str_replace_all("\r\n\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\r\n\t\t\t\t\t\t\t\t", ",") %>% 
        str_replace_all("\r\n\t\t\t\t\t\t\t\t", ",") %>% #Formatierungen weg, Kommas als Replacement
        str_replace_all("\r\n\t\t\t\t\t\t\t", ",") %>% 
        str_split(",") %>% #Aufbrechen
        unlist(.) %>% 
        .[-length(.)] #letzter leerer String muss weg
      
      objects <- length(data_cleaned)
      stopifnot(objects %% 5 == 0) ## Soll immer TRUE sein!
      
      ## Fülle Daten in ein Data.Table
      names <- data_cleaned[seq(1, objects, 5)]
      club <- data_cleaned[seq(2, objects, 5)]
      birthday <- data_cleaned[seq(3, objects, 5)]
      height <- data_cleaned[seq(4, objects, 5)] %>% str_remove_all(" cm")
      position <- data_cleaned[seq(5, objects, 5)]
      
      dt <- rbind(dt,
                  data.table(names = names, 
                       club = club, 
                       birthday = birthday, 
                       height = height, 
                       position = position),
                  use.names = TRUE)
    }
  }
  
  ## Gib dt aus
  dt
}

#### >> Testen der Funktion ####
spielerliste2009 <- scrape_spielerliste(saison = 2009)

## Crosschecks auf Vollständigkeit

## > Länge
nrow(spielerliste2009) == 564 # Es sind 564 Spieler aufgelistet
# TRUE

## > Inhalt
spielerliste2009[251, names] == "Juri Judt" # Der 251. aufgelistete Spieler ist Juri Judt
# TRUE

summary(spielerliste2009[, as.numeric(height)]) # Größen alle in glaubhaftem Bereich
# 166 bis 199 cm und 183.6 als Mittelwert -> TRUE

#### > Scrape Liste ####
for(i in seq_along(saisons)){
  
  spielerliste <- scrape_spielerliste(saison = saisons[i])
  
  saveRDS(spielerliste, file = paste0("./Data/Spielerlisten/spielerliste", saisons[i], ".rds"))
  
}


##### Wichtige Notizen #####
# Es kann vorkommen, dass bei Spielern die Größe nicht bekannt ist. Diese ist
# dann mit "???" angegeben. Beim Umwandeln in numerics entstehen dadurch
# natürlich NAs. (Bsp. Jonathan Meier -> ??? in der Saison 2017/2018).
# Spieler, die den Verein gewechselt haben, findet man unter dem Verein, unter
# dem sie in die Saison gestartet sind. (Bsp. Terrode -> Köln in der Saison
# 2017/2018).