##### Positionen Scrapen ####

## source global.r
source("./Code/global.r")

#### Daten laden ####
data <- readRDS(file = "./Data/cleaned_data_after_scrape.rds")

#### Liste der zu scrapenden Daten erstellen
position_list <- data.table(Name = data[, Names], Saison = data[, Saisons]) %>% 
  .[, .(Saison = list(Saison)),
    by = Name] %>% 
  .[, Name := vapply(Name, 
                     function(X){
                       temp <- str_split(X, " ") %>% 
                         unlist
                       
                       n <- length(temp)
                       
                       ifelse(temp[1] == "", 
                              paste(temp[2:n], collapse = " "), 
                              paste(temp, collapse = " "))
                       
  },
  FUN.VALUE = "character"
  )]

#### Beispiel für 1. Beobachtung ####

name <- position_list[1, Name]
saison <- position_list[, Saison] %>% unlist %>% .[1]

## open Server
rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")

remDr <- rD[["client"]]
remDr$navigate("https://www.transfermarkt.de")

## Wähle Box aus
option <- remDr$findElement(using = "css selector", "#schnellsuche input")
option$clickElement()

## Gib Spielernamen ein und drücke Enter
option$sendKeysToElement(list(name, "\uE007"))

Sys.sleep(2)

## Wähle Spieler in Spielerliste aus
option <- remDr$findElement(using = "css selector", ".tooltipstered")
option$clickElement()

Sys.sleep(2)

## Gehe zu kompletten Leistungsdaten
option <- remDr$findElement(using = "css selector", ".table-footer a")
option$clickElement()

Sys.sleep(2)

## Nehme aktuelle URL und navigiere zu Saison
curr_URL <- remDr$getCurrentUrl() %>% unlist
remDr$navigate(paste0(curr_URL, "/plus/0?saison=", saison))

Sys.sleep(2)

## Scrape Text
option <- remDr$findElement(using = "css selector", ".large-4 table")
positionen <- option$getElementText() %>% unlist

remDr$close()
rm("option", "rD", "remDr", "saison")

## Funktioniert!

#### Funktion zum Scrapen scheiben ####

position_scrape <- function(.data = dt()){
  
  dt <- readRDS( file = "./Data/position_list.rds") %>% 
    setDT
  
  # dt <- data.table()
  
  for(i in 1:nrow(.data)){
    
    rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")
    
    remDr <- rD[["client"]]
    remDr$navigate("https://www.transfermarkt.de")
    
    ## Wähle Box aus
    option <- remDr$findElement(using = "css selector", "#schnellsuche input")
    option$clickElement()
    
    ## Gib Spielernamen ein und drücke Enter
    option$sendKeysToElement(list(.data[i, Name], "\uE007"))
    
    Sys.sleep(4)
    
    ## Wähle Spieler in Spielerliste aus
    option <- remDr$findElement(using = "css selector", ".tooltipstered")
    scraped_name <- option$getElementText() %>% unlist
    
    .saisons <- unlist(.data[i, Saison])
    n <- length(.saisons)
    
    if(scraped_name == .data[i, Name]){
      option$clickElement()
      
      Sys.sleep(4)
      
      ## Gehe zu kompletten Leistungsdaten
      option <- remDr$findElement(using = "css selector", ".table-footer a")
      option$clickElement()
      
      Sys.sleep(4)
      
      ## Nehme aktuelle URL und navigiere zu Saison
      curr_URL <- remDr$getCurrentUrl() %>% unlist
      
      positionen <- vector(length = n, mode = "character")
      
      ## Für jede Saison einmal Daten abrufen
      for(j in 1:n){
        
        remDr$navigate(paste0(curr_URL, "/plus/0?saison=", .saisons[j]))
        
        Sys.sleep(2)
        
        ## Scrape Text
        option <- remDr$findElement(using = "css selector", ".large-4 table")
        positionen[j] <- option$getElementText() %>% unlist
        
      }
      
      dt <- rbind(dt,
                  data.table(Name = .data[i, Name],
                             Position = list(positionen),
                             Saison = list(.saisons)))
      
    }else{
      dt <- rbind(dt, 
                  data.table(Name = .data[i, Name],
                             Position = "wurde nicht gefunden",
                             Saison = list(.saisons)))
    }
    
    remDr$close()
    rm("option", "rD", "remDr", ".saisons", "n")
    
    saveRDS(dt, file = "./Data/position_list.rds")
    
  }
  
  dt
  
}

#### Testen der Funktion ####

rows <- nrow(readRDS( file = "./Data/position_list.rds"))
try(test <- position_scrape(.data = position_list[rows+1:nrow(position_list)]))

Sys.sleep(100)

# position_list[Name == "Klaas Jan Huntelaar", Name := "Klaas-Jan Huntelaar"]
position_list[Name == "Per Skjelbred",  Name := "Per Ciljan Skjelbred"]
rows <- nrow(readRDS( file = "./Data/position_list.rds"))
try(test <- position_scrape(.data = position_list[rows+1:nrow(position_list)]))

Sys.sleep(1000)

rows <- nrow(readRDS( file = "./Data/position_list.rds"))
try(test <- position_scrape(.data = position_list[rows+1:nrow(position_list)]))

Sys.sleep(1000)

rows <- nrow(readRDS( file = "./Data/position_list.rds"))
try(test <- position_scrape(.data = position_list[rows+1:nrow(position_list)]))

Sys.sleep(1000)

rows <- nrow(readRDS( file = "./Data/position_list.rds"))
try(test <- position_scrape(.data = position_list[rows+1:nrow(position_list)]))

## Wie viele Namen wurden nicht gefunden?
sum(test$Position == "wurde nicht gefunden")
# 48
# kann man per Hand eingeben!