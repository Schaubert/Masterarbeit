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
          
          Sys.sleep(4)
          
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
try(testit <- scraper2(.data = subset_list, .names = name_list, file = "./Data/scrape2_test.rds"))

# Sys.sleep(1800)
# 
# file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
# try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))
# 
# Sys.sleep(1800)
# 
# file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
# try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))
# 
# Sys.sleep(1800)
# 
# file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
# try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))
# 
# Sys.sleep(1800)
# 
# file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
# try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))
# 
# Sys.sleep(1800)
# 
# file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
# try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))
# 
# Sys.sleep(1800)
# 
# file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
# try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))
# 
# Sys.sleep(1800)
# 
# file_length <- nrow(readRDS("./Data/scrape2_test.rds"))
# try(testit <- scraper2(.data = subset_list, .names = name_list[(file_length+1):742], file = "./Data/scrape2_test.rds"))

#### > Datenbereinigung ####

dataset <- readRDS("./Data/scrape2_test.rds")

## Checke, wie viele Spieler gescrapet werden konnten
nrow(dataset[!is.na(steckbrief)]) 
# 450 von 742 - 60.6%

## Checke ob Längen der Leistungsdaten und der Saisons übereinstimmen
dataset[, length(saisons %>% unlist)] == dataset[, length(performance_data %>% unlist)]
# TRUE

## Checke Anzahl an gescrapeter Saisons
dataset[!is.na(steckbrief), length(saisons %>% unlist)] 
# 2682
dataset <- dataset[!is.na(steckbrief)] 

## Erstelle Vektor mit Anzahl der Saisons
dataset[, lengths := performance_data %>% unlist %>% length, by = .(name)]

steck <- vector(mode = "character", length = sum(dataset[, lengths]))
names <- vector(mode = "character", length = sum(dataset[, lengths]))

count <- 1
for(i in 1:nrow(dataset)){
  
  steck[count:(count+dataset[i, lengths]-1)] <- dataset[i, steckbrief]
  names[count:(count+dataset[i, lengths]-1)] <- dataset[i, name]
  count <- count+dataset[i, lengths]
  
}

  
data <- data.table(Names = names,
                   Steckbrief = steck, 
                   Leistungsdaten = dataset[, performance_data %>% unlist], 
                   Saisons = dataset[, saisons %>% unlist]) %>% 
  setDT %>% 
  .[, Leistungsdaten := str_replace_all(Leistungsdaten, 
                                        "LEISTUNGSDATEN\n", 
                                        "") %>% 
      str_split(., "\n")] %>% 
  # Zähle Leistungsdaten, falls 1, dann hat Scrape nicht funktioniert
  .[, Anzahl_Leistungsdaten := vapply(X = Leistungsdaten, 
                                      FUN = function(X){
                                        X %>% unlist %>% length %>% as.character
                                        }, 
                                      FUN.VALUE = "character")] %>% 
  # Checke die Liga, der Leistungsdaten ab
  .[, Liga := vapply(X = Leistungsdaten,
                     FUN = function(X){
                       X %>% 
                         unlist %>% 
                         {.[2]} %>% 
                         str_split(., " ") %>% 
                         unlist %>% 
                         .[1]
                       
                     },
                     "character")]

## Check, wie viele Daten aus der Bundesliga stammen
nrow(data[Liga == "Bundesliga"])
# 1767

data %>% 
  .[, Spielminuten := vapply(X = Leistungsdaten,
                             FUN = function(X){
                               X %>% 
                                 unlist %>% 
                                 .[3] %>% 
                                 str_replace(., 
                                             "Gespielte Minuten ", 
                                             "")
                             },
                             FUN.VALUE = "character") %>% as.numeric]

## Datenprobleme - falls Spieler nicht gespielt hat, oft die Daten von der vorherigen Saison übernommen
# data[Names == " Naldo" & Saisons == 2010, Problem := "Hat in dieser Saison nicht gespielt"]
# data[Names == " Bamba Anderson" & Saisons == 2015, Problem := "Hat in dieser Saison nicht gespielt"]
# data[Names == " Raffael" & Saisons == 2012, Problem := "Falsche Daten"]
# data[Names == "Aleksandar Ignjovski" & Saisons == 2012, Problem := "Hat in dieser Saison nicht gespielt"]

data %>% 
  .[, Spielminuten_lagg := c(0, Spielminuten)] %>% 
  .[Spielminuten == Spielminuten_lagg, Problem := "Datenduplikat"]

## Wie viele Daten bleiben übrig?
nrow(data[Liga == "Bundesliga" & is.na(Problem)])
# 1661


#### > Filtere Daten ####
## Der Filter ist vorerst, da manche Daten noch nachgescrapet werden müssen
data <- data[Liga == "Bundesliga" & is.na(Problem)]
# saveRDS(data, "./Data/filtered_data.rds")
data <- readRDS("./Data/filtered_data.rds") %>% 
  setDT

## Ermittle Datentypen (Tore, Pässe, etc.)
data %>% 
  .[, Datentypen := vapply(X = Leistungsdaten,
                           FUN = function(X){
                             le <- 
                               X %>% 
                               unlist %>% 
                               length
                             
                             char <- X %>% 
                               unlist
                             
                             vec <- vector(length = le - 2, mode = "character")
                             for(i in 3:le){
                               vec[i-2] <- char[i] %>% 
                                 str_split(., " ") %>% 
                                 unlist %>% 
                                 .[1:(length(.)-1)] %>% 
                                 paste(collapse = " ")
                             }
                             list(vec)
                           },
                           FUN.VALUE = list("list"))]

## Ermittle Datentypen
datentypen <- data[, Datentypen] %>% 
  unlist %>% 
  unique #%>% 
  # str_replace(., " ", "_") %>% 
  # unlist


#### Erstelle Variablen für neue Daten ####
## Eigentore
eigen <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Eigentore" %in% .}
                },
                TRUE)

## Assists
assist <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Assists" %in% .}
                },
                TRUE)

## Torschüsse
schuss <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Torschüsse" %in% .}
                },
                TRUE)

## Torschussvorlagen
vorl <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Torschussvorlagen" %in% .}
                },
                TRUE)

## Ballkontakte
kont <- vapply(data[, Datentypen], 
               function(X){
                 X %>% 
                   unlist %>% 
                   {"Ballkontakte" %in% .}
               },
               TRUE)

## Gespielte Pässe
pass <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Gespielte Pässe" %in% .}
                },
                TRUE)

## Angekommene Pässe
angpass <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Angekommene Pässe" %in% .}
                },
                TRUE)

## Fehlpässe
fehl <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Fehlpässe" %in% .}
                },
                TRUE)

## Angekommene Pässe %
passpro <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Angekommene Pässe (%)" %in% .}
                },
                TRUE)

## Zweikämpfe
zweik <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Zweikämpfe" %in% .}
                },
                TRUE)

## Gewonnene Zweikämpfe %
zweikpro <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Gewonnene Zweikämpfe (%)" %in% .}
                },
                TRUE)

## Fouls
fouls <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Fouls" %in% .}
                },
                TRUE)

## Gefoult worden
fouled <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Gefoult worden" %in% .}
                },
                TRUE)

## Abseits
abseits <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Abseits" %in% .}
                },
                TRUE)

## Laufweite
lauf <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Laufweite (km)" %in% .}
                },
                TRUE)

## Sprints
sprint <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Sprints" %in% .}
                },
                TRUE)

## Speed
speed <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Maximale Geschwindigkeit (km/h)" %in% .}
                },
                TRUE)

## Tore
tore <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Tore" %in% .}
                },
                TRUE)

## Tore per Fuß
fuss <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Tore per Fuß" %in% .}
                },
                TRUE)

## Tore per Kopf
kopf <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Tore per Kopf" %in% .}
                },
                TRUE)

## Elfmetertore
elfer <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Elfmetertore" %in% .}
                },
                TRUE)

## Elfmeter verschossen
elfermiss <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Elfmeter verschossen" %in% .}
                },
                TRUE)

## Gegentore
gegen <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Gegentore" %in% .}
                },
                TRUE)

## Gehaltene Schüsse
gehalten <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Gehaltene Schüsse" %in% .}
                },
                TRUE)

## Gehaltene Elfmeter
gehaltenelfer <- vapply(data[, Datentypen], 
                function(X){
                  X %>% 
                    unlist %>% 
                    {"Gehaltene Elfmeter" %in% .}
                },
                TRUE)

data %>%
  .[eigen, Eigentore := vapply(X = Leistungsdaten,
                                        FUN = function(X){
                                          X %>% 
                                            unlist %>% 
                                            .[str_detect(., "Eigentore")] %>% 
                                            str_replace(., 
                                                        "Eigentore ", 
                                                        "")
                                        },
                                        FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[assist, Assists := vapply(X = Leistungsdaten,
                               FUN = function(X){
                                 X %>% 
                                   unlist %>% 
                                   .[str_detect(., "Assists")] %>% 
                                   str_replace(., 
                                               "Assists ", 
                                               "")
                               },
                               FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[schuss, Schuss := vapply(X = Leistungsdaten,
                               FUN = function(X){
                                 X %>% 
                                   unlist %>% 
                                   .[str_detect(., "Torschüsse")] %>% 
                                   str_replace(., 
                                               "Torschüsse ", 
                                               "")
                               },
                               FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[vorl, Schussvorlagen := vapply(X = Leistungsdaten,
                               FUN = function(X){
                                 X %>% 
                                   unlist %>% 
                                   .[str_detect(., "Torschussvorlagen")] %>% 
                                   str_replace(., 
                                               "Torschussvorlagen ", 
                                               "")
                               },
                               FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[kont, Ballkontakte := vapply(X = Leistungsdaten,
                               FUN = function(X){
                                 X %>% 
                                   unlist %>% 
                                   .[str_detect(., "Ballkontakte")] %>% 
                                   str_replace(., 
                                               "Ballkontakte ", 
                                               "")
                               },
                               FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[pass, Pass := vapply(X = Leistungsdaten,
                               FUN = function(X){
                                 X %>% 
                                   unlist %>% 
                                   .[str_detect(., "Gespielte Pässe")] %>% 
                                   str_replace(., 
                                               "Gespielte Pässe ", 
                                               "")
                               },
                               FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[angpass, Pass_angekommen := vapply(X = Leistungsdaten,
                               FUN = function(X){
                                 te <- X %>% 
                                   unlist %>% 
                                   str_replace(., 
                                               "Angekommene Pässe \\(%\\)", 
                                               "")
                                 te %>% 
                                   unlist %>% 
                                   .[str_detect(., "Angekommene Pässe")] %>% 
                                   str_replace(., 
                                               "Angekommene Pässe ", 
                                               "")
                               },
                               FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[fehl, Fehlpass := vapply(X = Leistungsdaten,
                         FUN = function(X){
                           X %>% 
                             unlist %>% 
                             .[str_detect(., "Fehlpässe")] %>% 
                             str_replace(., 
                                         "Fehlpässe ", 
                                         "")
                         },
                         FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[passpro, Passprozente := vapply(X = Leistungsdaten,
                         FUN = function(X){
                           X %>% 
                             unlist %>% 
                             .[str_detect(., "Angekommene Pässe \\(%\\)")] %>% 
                             str_replace(., 
                                         "Angekommene Pässe \\(%\\) ", 
                                         "")
                         },
                         FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[zweik, Zweikampf := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      te <- X %>% 
                                        unlist %>% 
                                        str_replace(., 
                                                    "Gewonnene Zweikämpfe", 
                                                    "")
                                      te %>% 
                                        unlist %>% 
                                        .[str_detect(., "Zweikämpfe")] %>% 
                                        str_replace(., 
                                                    "Zweikämpfe ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[zweikpro, Zweikampfprozente := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      X %>% 
                                        unlist %>% 
                                        .[str_detect(., "Gewonnene Zweikämpfe \\(%\\)")] %>% 
                                        str_replace(., 
                                                    "Gewonnene Zweikämpfe \\(%\\) ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[fouls, Fouls := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      X %>% 
                                        unlist %>% 
                                        .[str_detect(., "Fouls")] %>% 
                                        str_replace(., 
                                                    "Fouls ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[fouled, Gefoult := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      X %>% 
                                        unlist %>% 
                                        .[str_detect(., "Gefoult worden")] %>% 
                                        str_replace(., 
                                                    "Gefoult worden ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[abseits, Abseits := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      X %>% 
                                        unlist %>% 
                                        .[str_detect(., "Abseits")] %>% 
                                        str_replace(., 
                                                    "Abseits ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[lauf, Laufweite := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      X %>% 
                                        unlist %>% 
                                        .[str_detect(., "Laufweite \\(km\\)")] %>% 
                                        str_replace(., 
                                                    "Laufweite \\(km\\) ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[sprint, Sprints := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      X %>% 
                                        unlist %>% 
                                        .[str_detect(., "Sprints")] %>% 
                                        str_replace(., 
                                                    "Sprints ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[speed, Geschwindigkeit := vapply(X = Leistungsdaten,
                                    FUN = function(X){
                                      X %>% 
                                        unlist %>% 
                                        .[str_detect(., "Maximale Geschwindigkeit \\(km/h\\)")] %>% 
                                        str_replace(., 
                                                    "Maximale Geschwindigkeit \\(km/h\\) ", 
                                                    "")
                                    },
                                    FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[tore, Tore := vapply(X = Leistungsdaten,
                               FUN = function(X){
                                 te <- X %>% 
                                   unlist %>% 
                                   str_replace(., 
                                               "Tore per", 
                                               "")
                                 te %>% 
                                   unlist %>% 
                                   .[str_detect(., "Tore")] %>% 
                                   str_replace(., 
                                               "Tore ", 
                                               "")
                               },
                               FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[fuss, Fusstore := vapply(X = Leistungsdaten,
                                     FUN = function(X){
                                       X %>% 
                                         unlist %>% 
                                         .[str_detect(., "Tore per Fuß")] %>% 
                                         str_replace(., 
                                                     "Tore per Fuß ", 
                                                     "")
                                     },
                                     FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[kopf, Kopftore := vapply(X = Leistungsdaten,
                             FUN = function(X){
                               X %>% 
                                 unlist %>% 
                                 .[str_detect(., "Tore per Kopf")] %>% 
                                 str_replace(., 
                                             "Tore per Kopf ", 
                                             "")
                             },
                             FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[elfer, Elfmetertore := vapply(X = Leistungsdaten,
                             FUN = function(X){
                               X %>% 
                                 unlist %>% 
                                 .[str_detect(., "Elfmetertore")] %>% 
                                 str_replace(., 
                                             "Elfmetertore ", 
                                             "")
                             },
                             FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[elfermiss, Elfmeterverschossen := vapply(X = Leistungsdaten,
                             FUN = function(X){
                               X %>% 
                                 unlist %>% 
                                 .[str_detect(., "Elfmeter verschossen")] %>% 
                                 str_replace(., 
                                             "Elfmeter verschossen ", 
                                             "")
                             },
                             FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[gegen, Gegentore := vapply(X = Leistungsdaten,
                             FUN = function(X){
                               X %>% 
                                 unlist %>% 
                                 .[str_detect(., "Gegentore")] %>% 
                                 str_replace(., 
                                             "Gegentore ", 
                                             "")
                             },
                             FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[gehalten, Gehalten := vapply(X = Leistungsdaten,
                             FUN = function(X){
                               X %>% 
                                 unlist %>% 
                                 .[str_detect(., "Gehaltene Schüsse")] %>% 
                                 str_replace(., 
                                             "Gehaltene Schüsse ", 
                                             "")
                             },
                             FUN.VALUE = "character") %>% as.numeric]

data %>%
  .[gehaltenelfer, Gehaltenelfer := vapply(X = Leistungsdaten,
                             FUN = function(X){
                               X %>% 
                                 unlist %>% 
                                 .[str_detect(., "Gehaltene Elfmeter")] %>% 
                                 str_replace(., 
                                             "Gehaltene Elfmeter ", 
                                             "")
                             },
                             FUN.VALUE = "character") %>% as.numeric]



# saveRDS(data, "./Data/data_cleaned.rds")

