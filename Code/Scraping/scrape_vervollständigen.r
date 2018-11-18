##### Nachscrapen von misslungenen Scrapes ####

## source global.r
source("./Code/global.r")

##### Check auf fehlende Scrapes ####
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

# name_list <- subset_list[, names] %>% unique

data_scraped <- readRDS("./Data/scraped_data_with_missings.rds") %>%
  setDT

## 3 Fälle an Fehlern:
## > 1. nicht gescrapet
## > 2. falsche Saison
## > 3. falsche Liga

to_scrape_subs_list <- data_scraped[Anzahl_Leistungsdaten == 1 |
                                  !is.na(Problem) |
                                    Liga != "Bundesliga"]
to_scrape_subs_list[,
                    rescrape := ifelse(Anzahl_Leistungsdaten == 1 & 
                                         Leistungsdaten != "falsche Saison", 
                                       TRUE, 
                                       FALSE)] %>% 
  .[,
    saison_new := ifelse(Leistungsdaten == "falsche Saison", 
                         TRUE, 
                         FALSE)] %>% 
  .[, 
    switch_liga := ifelse(!is.na(Liga) & Liga != "Bundesliga",
                          TRUE,
                          FALSE)]

## Merge mit den Daten fürs Scrapen

data_all <- merge(x = to_scrape_subs_list,  by.x = c("Names", "Saisons"),
                  y = subset_list, by.y = c("names", "saison_for_url"),
                  all.x = TRUE
                  ) %>% 
  .[Names != " Felipe" | club == "Hannover 96"]



## Eine Reihe Verlust, durch doppelten Namen, crosscheck passt aber. Die Zeile war falsch (entstand durch falsches Scrapen)

## Per Hand eingefügte Saison-Adjuster
data_all[, saison_adj := 0]
data_all[Names == " Domínguez", saison_adj := 1]
data_all[Names == "Christian Pander", saison_adj := 2]
data_all[Names == "Daniel Van Buyten", saison_adj := 3]
data_all[Names == "Jan Schlaudraff", saison_adj := 2]
data_all[Names == "Kai Herdling", saison_adj := 1]
data_all[Names == "Leon Andreasen", saison_adj := 1]
data_all[Names == "Martin Lanig", saison_adj := 2]
## Probleme bei Buchstaben in Namen Nikolce Noveski
## Mit Vorischt behandeln!!!
#data_all[712:717, saison_adj := 2] 
data_all[Names == "Pavel Krmaš", saison_adj := 2]
data_all[Names == "Per Nilsson", saison_adj := 1]
data_all[Names == "Piotr Trochowski", saison_adj := 1]
data_all[Names == "Roel Brouwers", saison_adj := 1]
data_all[Names == "Sebastian Kehl", saison_adj := 2]
data_all[Names == "Simon Rolfes", saison_adj := 2]
data_all[Names == "Stefan Reinartz", saison_adj := 1]
data_all[Names == "Thorben Marx", saison_adj := 2]

name_list <- data_all[, Names] %>% unique


#### > Upgedatete Funktion ####

nach_scraper <- function(.data = dt(), .names = "none", file = "./Data/daten.rds"){

  dt <- readRDS(file) %>%
    setDT
  
  # dt <- data.table()
  
  for(i in seq_along(.names)){
    
    ## Get Saisons
    n <- nrow(.data[Names == .names[i]])
    
    ## open Server
    rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")
    
    remDr <- rD[["client"]]
    remDr$navigate(.data[Names == .names[i], url %>% head(1)])
    
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
      saisons <- .data[Names == .names[i], Saisons]
      saison_adj <- .data[Names == .names[i], saison_adj]
      
      for(j in 1:n){
        ## Klicke auf Saisonbox
        option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-title")
        option$clickElement()
        
        ## Offene Box -> Saison auswählen
        option <- remDr$findElement(using = "css selector", paste0(".s1-person-performance-data .s1-popup-button-menu li:nth-child(", 
                                                                   2018-saisons[j] + 1 - saison_adj[j], 
                                                                   ") a"))
        if(option$getElementText() == paste0(saisons[j], " / ", saisons[j]+1)){
          option$clickElement()
          
          Sys.sleep(6)
          
          option <- try(remDr$findElement(using = "css selector", ".s1-person-performance-competition.active"))
          competitions <- try(option$getElementText() %>% unlist)
          
          ## Falls keine Daten vorliegen
          if(class(option) == "try-error"){
            ldv[j] <- "gar keine Daten für diese Saison verfügbar"
          }else{
            
            ## Check, ob richtige Liga, wenn nicht dann Klick auf den nächsten Wettbewerb
            if(competitions == "Bundesliga"){
              
              ## Scrape data from box
              option <- remDr$findElement(using = "css selector", ".s1-person-performance-data")
              ldv[j] <- option$getElementText() %>% unlist
            }else{
              
              ## Check, ob Bundesligadaten verfügbar
              comp.option <- "character"
              while(class(comp.option) != "try-error" & competitions != "Bundesliga"){
                comp.option <- try(remDr$findElement(using = "css selector", ".active+ .s1-person-performance-competition"))
                try(comp.option$clickElement())
                competitions <- try(comp.option$getElementText() %>% unlist)
                
              }
              
              if(class(comp.option) == "try-error"){
                
                ldv[j] <- "keine Bundesligadaten für diese Saison verfügbar"
                
              }else{
                
                
                Sys.sleep(2)
                
                ## Scrape data from box
                option <- remDr$findElement(using = "css selector", ".s1-person-performance-data")
                ldv[j] <- option$getElementText() %>% unlist
              }
            }
          }
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

#### >> Testen der neuen nach_scrape-Funktion ####
try(testit <- nach_scraper(.data = data_all, .names = name_list, file = "./Data/nach_scrapen.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/nach_scrapen.rds"))
try(testit <- nach_scraper(.data = data_all, .names = name_list[(file_length+1):411], file = "./Data/nach_scrapen.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/nach_scrapen.rds"))
try(testit <- nach_scraper(.data = data_all, .names = name_list[(file_length+1):411], file = "./Data/nach_scrapen.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/nach_scrapen.rds"))
try(testit <- nach_scraper(.data = data_all, .names = name_list[(file_length+1):411], file = "./Data/nach_scrapen.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/nach_scrapen.rds"))
try(testit <- nach_scraper(.data = data_all, .names = name_list[(file_length+1):411], file = "./Data/nach_scrapen.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/nach_scrapen.rds"))
try(testit <- nach_scraper(.data = data_all, .names = name_list[(file_length+1):411], file = "./Data/nach_scrapen.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/nach_scrapen.rds"))
try(testit <- nach_scraper(.data = data_all, .names = name_list[(file_length+1):411], file = "./Data/nach_scrapen.rds"))

Sys.sleep(1800)

file_length <- nrow(readRDS("./Data/nach_scrapen.rds"))
try(testit <- nach_scraper(.data = data_all, .names = name_list[(file_length+1):411], file = "./Data/nach_scrapen.rds"))

##### >> nach scrapen Julian Baumgartlinger ####
# dt <- readRDS("./Data/nach_scrapen.rds") %>%
#   setDT
# 
# # Saisons
# n <- 1
# 
# ## open Server
# rD <- rsDriver(port = floor(runif(1,1,9999)) %>% as.integer, browser = "firefox")
# 
# remDr <- rD[["client"]]
# remDr$navigate("https://www.sport1.de/fussball/person/julian-baumgartlinger")
# 
# ## Get Steckbrief
# option <- remDr$findElement(using = 'css selector', ".s1-column-double")
# steckbrief <- option$getElementText() %>% unlist
# 
# ## Initialisiere Leistungsdatenvektor
# ldv <- vector(length = n, mode = "character")
# 
# ## Get Saisons
# option <- remDr$findElement(using = "css selector", ".s1-person-performance-data .s1-popup-button-title")
# option$clickElement()
# 
# ## Wähle 2011/2012
# option <- remDr$findElement(using = "css selector", paste0(".s1-person-performance-data .s1-popup-button-menu li:nth-child(", 
#                                                            8, 
#                                                            ") a"))
# option$clickElement()
# 
# ## Wähle Bundesliga
# comp.option <- remDr$findElement(using = "css selector", ".active+ .s1-person-performance-competition")
# comp.option$clickElement()
# 
# ## Scrape data
# option <- remDr$findElement(using = "css selector", ".s1-person-performance-data")
# ldv[1] <- option$getElementText() %>% unlist
# 
# dt <- rbind(dt,
#             data.table(name = "Julian Baumgartlinger",
#                        steckbrief = steckbrief,
#                        performance_data = list(ldv), 
#                        saisons = list(2011)))
# 
# remDr$close()
# rm("option", "rD", "remDr", "steckbrief", "ldv")
# saveRDS(dt, file = "./Data/nach_scrapen.rds")


##### > Datenbereinigung ####

falsche_saison <- readRDS(file = "./Data/nach_scrapen_falsche_saison.rds")
nach_scrapen <- readRDS(file = "./Data/nach_scrapen.rds")
dataset <- rbind(falsche_saison, nach_scrapen)

## Checke ob Längen der Leistungsdaten und der Saisons übereinstimmen
dataset[, length(saisons %>% unlist)] == dataset[, length(performance_data %>% unlist)]
# TRUE

## Checke Anzahl an nachgescrapeter Saisons
dataset[, length(saisons %>% unlist)] 
# 1119

## Erstelle Vektor mit Anzahl der Saisons
dataset[, lengths := performance_data %>% unlist %>% length, by = .(name)]
## Aktuell noch doppelte wegen nachscrapen der falschen Saisons, dort wird Länge 
## falsch kalkuliert, muss also noch halbiert werden
names_double <- dataset[1:18, name]
dataset[name %in% names_double, lengths := lengths/2]
dataset[name == "Julian Baumgartlinger", lengths := c(3,1)]

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
                     "character")] %>% 
  # Beim Scrapen immer auf Bundesliga geschaltet, falls möglich, also ersetzen
  .[!is.na(Liga), Liga := "Bundesliga"] %>% 
  # Falls keine Bundesligaleistungsdaten verfügbar waren, trotzdem Bundesliga 
  # einschreiben, da keine Spielzeit auch eine Information ist
  .[Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar", 
    Liga := "Bundesliga"]

## Check, wie viele Daten aus der Bundesliga stammen
nrow(data[Liga == "Bundesliga"])
# 992

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

## noch nicht rausfiltern wegen Saisons dazwischen
# data %>% 
#   .[, Spielminuten_lagg := c(0, Spielminuten)] %>% 
#   .[Spielminuten == Spielminuten_lagg, Problem := "Datenduplikat"]

data[Leistungsdaten == "falsche Saison", Problem := "falsche Saison"]

saveRDS(data, file = "./Data/nach_gescraped_data.rds")

## Wie viele Daten bleiben übrig?
nrow(data[Liga == "Bundesliga" & is.na(Problem)])
# 992

#### > Filtere Daten ####

## Der Filter ist nur für die nachgescrapeten Daten
data <- data[Liga == "Bundesliga" & is.na(Problem) |
               Leistungsdaten == "keine Bundesligadaten für diese Saison verfügbar"]
# saveRDS(data, "./Data/nach_gescraped_filtered_data.rds")
data <- readRDS("./Data/nach_gescraped_filtered_data.rds") %>% 
  setDT

## Ermittle Datentypen (Tore, Pässe, etc.)
data %>% 
  .[Anzahl_Leistungsdaten > 1, Datentypen := vapply(X = Leistungsdaten,
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



# saveRDS(data, "./Data/nach_gescraped_data_cleaned.rds")
