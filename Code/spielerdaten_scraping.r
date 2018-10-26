##### Scrapen von Spielerdaten auf sport1.de #####

## source global.r
source("./Code/global.r")

#### > URLs definieren ####

# Viele und vor allem ältere Spieler sind auf der sport1.de Seite durch ihren
# Namen innerhalb der URL leicht zu finden. Andere dagegen müssen durch eine
# fortlaufende Zahl gefunden werden. Bisher gibt es eine Liste mit Namen, die
# in den Bundesligasaisons gespielt haben und ihren Geburtstagen (um 
# gleichnamige Spieler zu unterscheiden). Insgesamt sind 577273 Spieler mit
# einer fortlaufenden Nummer anstatt des Namens in der URL eingetragen (Stand 
# 26.10.2018 um 17:15 Uhr). Da es keine Logik hinter dieser Nummerierung gibt
# und bereits bei frühen Zahlen, wie auch späten Zahlen Spieler auftauchen,
# die für unsere Analysen benötigt werden, werden hier einmal alle Spieler, die
# in der Datenbank sind mit ihrer Nummerierung aufgelistet, um danach alle 
# benötigten Daten aus einer gemeinsamen Quelle zu ziehen.

#### >> Scrape eine bestimmte URL um Name und Geburtstag zu erhalten ####

## Stamm-URL
page_url <- "https://www.sport1.de/person/"

## Die Spieler (erstmal ein Teil)
spielernummern <- seq(1, 100000)
# Bei allen Spielernummern wird der Vektor über 50 Mb groß

## Zusammengesetzte URLs
urls <- vapply(spielernummern, 
               function(X){paste0(page_url, X)}, 
               FUN.VALUE = "character")

#### >> Beispiel von Scrapen für 1 Spieler ####

## Scrape Data von URL
webdata <- xml2::read_html(urls[1])

## Verarbeite Data von URL
data_processed <- html_nodes(webdata, ".s1-column-double") %>% 
  html_text

## Säubere Daten
data_cleaned <- data_processed %>% 
  str_replace_all("Name", ",") %>% 
  str_replace_all("Nation", ",") %>% 
  str_replace_all("Geboren am", ",") %>% 
  str_replace_all("in\n", ",") %>% 
  str_split(",") %>% 
  unlist %>% 
  .[c(2,5)]

#### >> Schreibe Funktion für alle Spieler
scrape_for_url <- function(number_start = 1, number_end = 100, check_steps = 0){
  
  stopifnot(number_start <= number_end)
  
  dt <- data.table()
  
  for(number in number_start:number_end){
    page_url <- paste0("https://www.sport1.de/person/", number)
    
    ## Scrape Data von URL
    webdata <- try(xml2::read_html(page_url))
    
    if(class(webdata) != "try-error"){
    ## Verarbeite Data von URL
    data_processed <- html_nodes(webdata, ".s1-column-double") %>% 
      html_text
    
    ## Säubere Daten
    data_cleaned <- data_processed %>% 
      str_replace_all("Name", ",") %>% 
      str_replace_all("Nation", ",") %>% 
      str_replace_all("Geboren am", ",") %>% 
      str_replace_all("in\n", ",") %>% 
      str_split(",") %>% 
      unlist %>% 
      .[c(2,5)]
    
    ## Merge Data.Table
    
    dt <- rbind(dt,
                data.table(name = data_cleaned[1],
                           birthday = data_cleaned[2],
                           id = number),
                use.names = TRUE)
    }else{
      dt <- rbind(dt,
                  data.table(name = NA,
                             birthday = NA,
                             id = number),
                  use.names = TRUE)
    }
    if(check_steps != 0 && number %% check_steps == 0){
      print(number)
    }
    
  }
  
  dt
}

#### >> Testen der Funktion
erste_nummern <- scrape_for_url(number_start = 1, number_end = 100, check_steps = 25)

# Kurz Abspeichern, wegen Zeitgründen; es gab Probleme beim Geburtstag: Doppelchecken!
saveRDS(erste_nummern, file = "./Data/erste_nummern_sport1.rds")
 