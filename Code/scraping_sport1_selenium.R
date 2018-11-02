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

scraper <- function(urls = "none", saisons = 2018, file = "./Data/daten.rds"){
  
  dt <- readRDS(file) %>% 
    setDT
  
  length_all <- length(urls)
  
  for(i in seq_along(urls)){
    
    ## open Server
    rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")
    
    remDr <- rD[["client"]]
    remDr$navigate(urls[i])
    
    # Sys.sleep(3)
    
    ## Check, ob Spieler existent
    option <- try(remDr$findElement(using = "css selector", ".s1-dashboard-headline"))
    
    Sys.sleep(1)
    
    if(class(option) != "try-error"){
      
      ## Get Steckbrief
      option <- remDr$findElement(using = 'css selector', ".s1-column-double")
      steckbrief <- option$getElementText() %>% unlist
      
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
      performance_data <- option$getElementText() %>% unlist
      
    }else{
      steckbrief <- NA
      performance_data <- NA
    }
    
    dt <- rbind(dt,
                data.table(steckbrief = steckbrief, 
                           urls = urls[i], 
                           performance_data = performance_data, 
                           saisons = saisons[i]))
    saveRDS(dt, file = file)
    remDr$close()
    rm("option", "rD", "remDr", "steckbrief", "performance_data")
  }
  
  dt
}

#### >> Funktions Test ####
data_test <- scraper(urls = c("https://www.sport1.de/person/thomas-muller", "https://www.sport1.de/person/manuel-neuer"), saisons = c(2018, 2018))
# Klappt

##### >> Render URLs #####
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
  .[, url := paste0("https://www.sport1.de/person/", name_for_url)] %>% 
  .[, anzahl := .N, by = names]

subset_list <- gesamtliste %>% 
  .[order(names)] %>% 
  .[anzahl > 2]

name_list <- subset_list[, names] %>% unique

#### >> Funktionstest mit gerenderten URLs ####

#scraped_data <- scraper(urls = gesamtliste[1:50, url], saisons = gesamtliste[1:50, saison_for_url])
saveRDS(scraped_data, file = "./Data/erste_leistungsdaten.rds")

scraped_data2 <- scraper(urls = gesamtliste[91:150, url], saisons = gesamtliste[91:150, saison_for_url], file = "./Data/zweite_leistungsdaten.rds")

#### > Upgedatete Funktion ####

scraper2 <- function(.data = dt(), .names = "none", file = "./Data/daten.rds"){
  
  dt <- readRDS("./Data/scrape2_test.rds") %>%
    setDT
  
  # dt <- data.table()
  
  for(i in seq_along(.names)){
    
    ## Get Saisons
    n <- nrow(.data[names == .names[i]])
    
    ## open Server
    rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")
    
    remDr <- rD[["client"]]
    remDr$navigate(.data[names == .names[i], url %>% head(1)])
    
    # Sys.sleep(3)
    
    ## Check, ob Spieler existent
    option <- try(remDr$findElement(using = "css selector", ".s1-dashboard-headline"))
    
    Sys.sleep(1)
    
    if(class(option) != "try-error"){
      
      ## Get Steckbrief
      option <- remDr$findElement(using = 'css selector', ".s1-column-double")
      steckbrief <- option$getElementText() %>% unlist
      
      ## Initialisiere Leistungsdatenvektor
      ldv <- vector(length = n, mode = "character")
      
      ## Ermittle Saisons
      saisons <- .data[names == .names[i], saison_for_url]
      
      for(j in 1:n){
        ## Klicke auf Saisonbox
        option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-title")
        option$clickElement()
        
        ## Offene Box -> Saison auswählen
        option <- remDr$findElement(using = "css selector", paste0(".s1-person-performance-data .s1-popup-button-menu li:nth-child(", 
                                                                   2018-saisons[j] + 1, 
                                                                   ") a"))
        if(option$getElementText() == paste0(saisons[j], " / ", saisons[j]+1)){
          option$clickElement()
          
          Sys.sleep(3)
          
          ## Scrape data from box
          option <- remDr$findElement(using = "css selector", ".s1-person-performance-data")
          ldv[j] <- option$getElementText() %>% unlist
          
        }else{
          ldv[j] <- "falsche Saison"
        }
        
        Sys.sleep(1)
      }
      
    }else{
      steckbrief <- NA
      ldv <- NA
      saisons <- NA
    }
    
    dt <- rbind(dt,
                data.table(name = .names[i],
                           steckbrief = steckbrief,
                           performance_data = list(ldv), 
                           saisons = list(saisons)))
    
    remDr$close()
    rm("option", "rD", "remDr", "steckbrief", "ldv", "saisons")
    saveRDS(dt, file = file)
    print(i)
  }
  
  dt
}

#### >> Testen der neuen scrape-Funktion ####
try(testit <- scraper2(.data = subset_list, .names = name_list[179:742], file = "./Data/scrape2_test.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))