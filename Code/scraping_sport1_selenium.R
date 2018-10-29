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
    rD <- rsDriver(port = 2365L, browser = "chrome")
    
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

##### Render URLs #####
gesamtliste <- readRDS("./Data/Spielerlisten/gesamtliste.rds")