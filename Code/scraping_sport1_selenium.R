##### Scrapen von Spielerdaten auf sport1.de #####

## source global.r
source("./Code/global.r")

#### > Test mit Thomas Müller ####

url <- "https://www.sport1.de/person/thomas-muller"

rD <- rsDriver(port = 2365L, browser = "chrome")

remDr <- rD[["client"]]
remDr$navigate(url)

Sys.sleep(3)

## Get Steckbrief
option <- remDr$findElement(using = 'css selector', ".s1-column-double")
steckbrief <- option$getElementText() %>% unlist

## Klicke auf Saisonbox
option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-title")
option$clickElement()

## Offene Box -> Saison auswählen
option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-menu li:nth-child(2) a")
option$clickElement()

option <- remDr$findElement(using = "css selector", ".s1-person-performance-data")
performance_data <- option$getElementText() %>% unlist

remDr$close()
rm("option", "rD", "remDr")

#### > Test mit nichtexistenter Url ####

url <- "https://www.sport1.de/person/thomas-muller"

rD <- rsDriver(port = 2365L, browser = "chrome")

remDr <- rD[["client"]]
remDr$navigate(url)

option <- try(remDr$findElement(using = "css selector", ".s1-dashboard-headline"))

if(class(option) != "try-error"){
  
  ## Get Steckbrief
  option <- remDr$findElement(using = 'css selector', ".s1-column-double")
  steckbrief <- option$getElementText() %>% unlist
  
  ## Klicke auf Saisonbox
  option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-title")
  option$clickElement()
  
  ## Offene Box -> Saison auswählen
  option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-menu li:nth-child(2) a")
  option$clickElement()
  
  Sys.sleep(3)
  
  ## Scrape data from box
  option <- remDr$findElement(using = "css selector", ".s1-person-performance-data")
  performance_data <- option$getElementText() %>% unlist
  
}

remDr$close()
rm("option", "rD", "remDr")


#### > Funktion zum Scrapen ####

scraper <- function(urls = "none", saisons = 2018){
  
  length_all <- length(urls)
  
  steckbrief <- vector(length = length(urls), mode = "character")
  performance_data <- vector(length = length(urls), mode = "character")
  
  for(i in seq_along(urls)){
    
    ## open Server
    rD <- rsDriver(port = as.integer(i), browser = "chrome")
    
    remDr <- rD[["client"]]
    remDr$navigate(urls[i])
    
    # Sys.sleep(3)
    
    ## Check, ob Spieler existent
    option <- try(remDr$findElement(using = "css selector", ".s1-dashboard-headline"))
    
    Sys.sleep(1)
    
    if(class(option) != "try-error"){
      
      ## Get Steckbrief
      option <- remDr$findElement(using = 'css selector', ".s1-column-double")
      steckbrief[i] <- option$getElementText() %>% unlist
      
      ## Klicke auf Saisonbox
      option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-title")
      option$clickElement()
      
      ## Offene Box -> Saison auswählen
      season <- 2018-saisons[i] + 1
      option <- remDr$findElement(using = "css selector", paste0(".s1-person-performance-data .s1-popup-button-menu li:nth-child(", season, ") a"))
      option$clickElement()
      
      Sys.sleep(3)
      
      ## Scrape data from box
      option <- remDr$findElement(using = "css selector", ".s1-person-performance-data")
      performance_data[i] <- option$getElementText() %>% unlist
      
    }else{
      steckbrief[i] <- NA
      performance_data[i] <- NA
    }
    
    remDr$close()
    rm("option", "rD", "remDr")
  }
  
  data.table(steckbrief = steckbrief, urls = urls, performance_data = performance_data, saisons = saisons)
}

#### >> Funktions Test ####
data_test <- scraper(urls = c("https://www.sport1.de/person/thomas-muller", "https://www.sport1.de/person/manuel-neuer"), saisons = c(2018, 2018))
# Klappt

##### > Render URLs #####
## Lade Spielerliste
gesamtliste <- readRDS("./Data/Spielerlisten/gesamtliste.rds") %>% 
  setDT

## Bringe Namen auf URL Form und extrahiere Saison für Scrape-Funktion
gesamtliste %>% 
  .[, name_for_url := str_replace_all(names, " ", "-")] %>% 
  .[, saison_for_url := vapply(saison, function(X){str_split(X, "/") %>% unlist %>% .[1]}, FUN.VALUE = "numeric") %>% as.numeric] %>% 
  .[, name_for_url := str_replace_all(name_for_url, "Ð", "d") %>% 
      str_replace_all("Ž", "z") %>% 
      str_replace_all("ë", "e") %>% 
      str_replace_all("Ç", "c") %>% 
      str_replace_all("ð", "d") %>% 
      ifelse(substr(., 1, 1) == "-", substr(., 2, 100), .)] %>% 
  .[, url := paste0("https://www.sport1.de/person/", name_for_url)]

#### > Funktionstest mit gerenderten URLs ####

scraped_data <- scraper(urls = gesamtliste[1:50, url], saisons = gesamtliste[1:50, saison_for_url])
saveRDS(scraped_data, file = "./Data/erste_leistungsdaten.rds")

scraped_data2 <- scraper(urls = gesamtliste[51:150, url], saisons = gesamtliste[51:150, saison_for_url])
