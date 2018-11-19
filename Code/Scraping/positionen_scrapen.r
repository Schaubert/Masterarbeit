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

# name <- position_list[1, Name]
# saison <- position_list[, Saison] %>% unlist %>% .[1]
# 
# ## open Server
# rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")
# 
# remDr <- rD[["client"]]
# remDr$navigate("https://www.transfermarkt.de")
# 
# ## Wähle Box aus
# option <- remDr$findElement(using = "css selector", "#schnellsuche input")
# option$clickElement()
# 
# ## Gib Spielernamen ein und drücke Enter
# option$sendKeysToElement(list(name, "\uE007"))
# 
# Sys.sleep(2)
# 
# ## Wähle Spieler in Spielerliste aus
# option <- remDr$findElement(using = "css selector", ".tooltipstered")
# option$clickElement()
# 
# Sys.sleep(2)
# 
# ## Gehe zu kompletten Leistungsdaten
# option <- remDr$findElement(using = "css selector", ".table-footer a")
# option$clickElement()
# 
# Sys.sleep(2)
# 
# ## Nehme aktuelle URL und navigiere zu Saison
# curr_URL <- remDr$getCurrentUrl() %>% unlist
# remDr$navigate(paste0(curr_URL, "/plus/0?saison=", saison))
# 
# Sys.sleep(2)
# 
# ## Scrape Text
# option <- remDr$findElement(using = "css selector", ".large-4 table")
# positionen <- option$getElementText() %>% unlist
# 
# remDr$close()
# rm("option", "rD", "remDr", "saison")
# 
# ## Funktioniert!

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

#### Daten einlesen und für nachscrapen aufbereiten ####

data_firstscrape <- readRDS("./Data/position_list.rds") %>% 
  setDT

# falscher Rafinha (von Barca gescrapet statt von Schalke/Bayern)

data_firstscrape[Name == "Rafinha", Position := list("wurde nicht gefunden")]

not_found <- data_firstscrape[!str_detect(Position, "Spiele als ...")] %>% 
  .[, url := c("https://www.transfermarkt.de/alvaro-dominguez/profil/spieler/62915",
               "https://www.transfermarkt.de/felipe/profil/spieler/68815",
               "https://www.transfermarkt.de/jairo-samperio/profil/spieler/171167",
               "https://www.transfermarkt.de/naldo/profil/spieler/32213",
               "https://www.transfermarkt.de/rafinha/profil/spieler/33947",
               "https://www.transfermarkt.de/ronny/profil/spieler/43847",
               NA,
               "https://www.transfermarkt.de/adam-hlousek/profil/spieler/62800",
               NA,NA,NA,
               "https://www.transfermarkt.de/andre-hoffmann/profil/spieler/85164",
               "https://www.transfermarkt.de/artjoms-rudnevs/profil/spieler/103547",
               rep(NA,8),
               "https://www.transfermarkt.de/ermin-bicakcic/profil/spieler/51676",
               "https://www.transfermarkt.de/fallou-diagne/profil/spieler/82389",
               NA,
               "https://www.transfermarkt.de/filip-kostic/profil/spieler/161011",
               "https://www.transfermarkt.de/gojko-kacar/profil/spieler/28683",
               "https://www.transfermarkt.de/gotoku-sakai/profil/spieler/103310",
               "https://www.transfermarkt.de/hakan-calhanoglu/profil/spieler/126414",
               NA,
               "https://www.transfermarkt.de/haris-seferovic/profil/spieler/109256",
               NA,
               "https://www.transfermarkt.de/havard-nordtveit/profil/spieler/42234",
               "https://www.transfermarkt.de/ivan-perisic/profil/spieler/42460",
               "https://www.transfermarkt.de/ivica-olic/profil/spieler/7427",
               "https://www.transfermarkt.de/ivo-ilicevic/profil/spieler/30308",
               "https://www.transfermarkt.de/jan-polak/profil/spieler/3530",
               NA, NA,
               "https://www.transfermarkt.de/jaroslav-drobny/profil/spieler/12864",
               "https://www.transfermarkt.de/ju-ho-park/profil/spieler/111900",
               "https://www.transfermarkt.de/josip-drmic/profil/spieler/140579",
               NA,NA,NA,
               "https://www.transfermarkt.de/levent-aycicek/profil/spieler/117476",
               NA, NA,
               "https://www.transfermarkt.de/mario-gomez/profil/spieler/6288",
               "https://www.transfermarkt.de/mario-mandzukic/profil/spieler/34572",
               "https://www.transfermarkt.de/mario-vrancic/profil/spieler/39372",
               NA, NA,
               "https://www.transfermarkt.de/matija-nastasic/profil/spieler/143559",
               "https://www.transfermarkt.de/mensur-mujdza/profil/spieler/27900",
               "https://www.transfermarkt.de/mergim-mavraj/profil/spieler/38267",
               NA,
               "https://www.transfermarkt.de/milos-jojic/profil/spieler/160285",
               "https://www.transfermarkt.de/miso-brecko/profil/spieler/15300",
               NA,
               "https://www.transfermarkt.de/neven-subotic/profil/spieler/40995",
               "https://www.transfermarkt.de/nikolce-noveski/profil/spieler/1139",
               "https://www.transfermarkt.de/nuri-sahin/profil/spieler/31095",
               NA,NA,NA,
               "https://www.transfermarkt.de/pavel-krmas/profil/spieler/9697",
               "https://www.transfermarkt.de/peter-pekarik/profil/spieler/51100",
               "https://www.transfermarkt.de/petr-jiracek/profil/spieler/63737",
               rep(NA, 6),
               "https://www.transfermarkt.de/sead-kolasinac/profil/spieler/94005",
               NA,
               "https://www.transfermarkt.de/sejad-salihovic/profil/spieler/9354",
               NA,
               "https://www.transfermarkt.de/slobodan-medojevic/profil/spieler/48164",
               "https://www.transfermarkt.de/slobodan-rajkovic/profil/spieler/35934",
               "https://www.transfermarkt.de/sokratis/profil/spieler/34322",
               NA,NA,
               "https://www.transfermarkt.de/tolga-cigerci/profil/spieler/94199",
               NA,NA,
               "https://www.transfermarkt.de/vedad-ibisevic/profil/spieler/21175",
               "https://www.transfermarkt.de/vladimir-darida/profil/spieler/179643",
               "https://www.transfermarkt.de/yunus-malli/profil/spieler/85352",
               "https://www.transfermarkt.de/zlatko-junuzovic/profil/spieler/31007")]

# not_found <- data_firstscrape[Position == "wurde nicht gefunden"]
# data_found <- data_firstscrape[!Position == "wurde nicht gefunden"] %>% 
#   .[, length := Saison %>% unlist %>% length, by = Name]
# 
# names <- vector(mode = "character", length = sum(data_found[, length]))
# 
# count <- 1
# for(i in 1:nrow(data_found)){
#   
#   names[count:(count+data_found[i, length]-1)] <- data_found[i, Name]
#   count <- count+data_found[i, length]
#   
# }
# 
# 
# data_found_split <- data.table(Name = names, 
#                    Position = data_found[, Position %>% unlist], 
#                    Saison = data_found[, Saison %>% unlist])

