##### Scraping für Spielerlisten #####

#### > Durchlaufe Webpages für Spielerlisten ####

## Stamm-URL
page_url <- "https://www.weltfussball.de/spielerliste/bundesliga-"

## Saisons, die interessieren (und noch ein paar mehr)
saisons <- seq(2009,2017)

## Zusammengesetzte URLS
urls <- vapply(saisons, 
               function(X){paste0(page_url, X, "-", X+1, "/nach-name/")}, 
               FUN.VALUE = "character")

#### > Beispiel von Scrapen für 1 Saison ####

## Scrape Data von URL
webdata <- xml2::read_html(paste0(urls[1], 1))

## Verarbeite Data von URL
data_processed <- html_nodes(webdata, ".standard_tabelle") %>% 
  html_text %>% .[1] ## Nur Tabelle, 2. Element wäre "Aktuelle Meldungen"

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