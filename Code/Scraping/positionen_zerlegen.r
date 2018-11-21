##### Positionsstrings Zerlegen ####

## source global.r
source("./Code/global.r")

#### Daten laden ####
positions_raw <- readRDS("./Data/all_positions_raw.rds") %>% 
  setDT

#### Strings zerlegen ####
positions_raw[, length := Saison %>% unlist %>% length, 
              by = Name]

names <- vector(mode = "character", length = sum(positions_raw[, length]))

count <- 1
for(i in 1:nrow(positions_raw)){
  
  names[count:(count+positions_raw[i, length]-1)] <- positions_raw[i, Name]
  count <- count+positions_raw[i, length]
  
}


data <- data.table(Name = names,
                   Position = positions_raw[, Position %>% unlist], 
                   Saison = positions_raw[, Saison %>% unlist]) %>% 
  setDT %>% 
  .[, Position := str_replace_all(Position, "Spiele als ...      \n", "") %>%
      str_replace_all(., "Spiele als ...  \n", "") %>% 
      str_replace_all(., "\n", ";") %>% 
      str_split(., ";")] %>% 
  .[, Position := vapply(Position, function(X){
    
    vec <- vector(length = length(unlist(X)), mode = "character")
    
    for(i in 1:length(unlist(X))){
      string <- str_split(X %>% unlist %>% .[i], " ") %>% unlist
    
      n <- length(string)
      
      # Tore und Vorlagen raus
      if(!"Torwart" %in% string){
        string <- string[1:(n-2)]
      }
      
      vec[i] <- paste(string, collapse = " ")
    }
    
    list(vec)
    
    
  },
  FUN.VALUE = list("something"))] %>% 
  .[, Anzahl.Verschiedene.Positionen := Position %>% unlist %>% length, by = .(Name, Saison)] %>% 
  # Hauptposition und Anzahl Spiele bestimmen
  .[, Hauptposition := Position %>% 
      unlist %>% 
      .[1] %>% 
      str_split(., " ") %>%
      unlist %>% 
      .[1:{length(.)-1}] %>% 
      paste(., collapse = " "),
    by = .(Name, Saison)] %>% 
  .[, Anzahl.Spiele.Hauptposition := Position %>% 
      unlist %>% 
      .[1] %>% 
      str_split(., " ") %>%
      unlist %>% 
      .[{length(.)}],
    by = .(Name, Saison)] %>% 
  # Zweitposition bestimmen und Anzahl Spiele berechnen
  .[Anzahl.Verschiedene.Positionen > 1, Zweitposition := Position %>% 
      unlist %>% 
      .[2] %>% 
      str_split(., " ") %>%
      unlist %>% 
      .[1:{length(.)-1}] %>% 
      paste(., collapse = " "),
    by = .(Name, Saison)] %>% 
  .[Anzahl.Verschiedene.Positionen > 1, Anzahl.Spiele.Zweitposition := Position %>% 
      unlist %>% 
      .[2] %>% 
      str_split(., " ") %>%
      unlist %>% 
      .[{length(.)}],
    by = .(Name, Saison)] %>% 
  # Gesamtspiele berechnen
  .[, Anzahl.Spiele.Gesamt := Position %>% 
      unlist %>% 
      vapply(., function(X){
        
        games <- X %>% 
            str_split(., " ") %>% 
          unlist %>% 
            .[{length(.)}] %>% 
            as.numeric
    
  },
  FUN.VALUE = as.numeric(10)) %>% 
    sum,
  by = .(Name, Saison)]
  

## Sieht gut aus!

# saveRDS(data, "./Data/positionen_clean.rds")


#### Qualitätsüberprüfung der Positionen ####
## Absolute Anzahl der Beobachtungen auf Hauptpositionen

data[, Hauptposition] %>% table
## Bei 2 Spielern steht als Hauptposition "Verein" drin --> überprüft, das sind
## Datenfehler vom Scrapen von Sport1 gewesen; nachträglich entfernen!
##
## Eine Person mit Hauptposition "Libero" --> Julian Schuster 2017/2018
## Ansonsten recht ausgeglichene Verteilung der Positionen. Rechtes und Linkes
## Mittelfeld eventuell unterbesetzt, zusammenaddiert trotzdem 94!
## Häufigstes Auftreten bei Innenverteidigern (366)
