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
data_all[Names == "Nikolče Noveski", saison_adj := 2]
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
                                                                   2018-saisons[j] + 1 + saison_adj[j], 
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
try(testit <- nach_scraper(.data = data_all, .names = name_list[1:411], file = "./Data/nach_scrapen.rds"))

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
