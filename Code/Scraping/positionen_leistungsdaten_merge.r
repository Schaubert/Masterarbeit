##### Positionen und Leistungsdaten mergen ####

## source global.r
source("./Code/global.r")

#### Daten laden ####
positionen <- readRDS("./Data/positionen_clean.rds") %>% 
  setDT

leistungsdaten <- readRDS("./Data/cleaned_data_after_scrape.rds") %>% 
  setDT %>% 
  .[, Names := vapply(Names, 
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

#### Mergen der Daten ####
# data <- merge(positionen, 
#               leistungsdaten, 
#               by.x = c("Name", "Saison"), 
#               by.y = c("Names", "Saisons"))
# 
# # Hat nicht bei allen geklappt -> Namensänderungen
# leistungsdaten[!Names %in% positionen[, Name], Names ] %>% unique
# positionen[!Name %in% leistungsdaten[, Names], Name ] %>% unique

data <- merge(positionen,
              leistungsdaten %>% 
                .[Names == "Klaas Jan Huntelaar", 
                  Names := "Klaas-Jan Huntelaar"] %>% 
                .[Names == "Per Skjelbred", 
                  Names := "Per Ciljan Skjelbred"],
              by.x = c("Name", "Saison"),
              by.y = c("Names", "Saisons")) %>% 
  # Falsche Daten rausschmeißen (haben da nicht gespielt, Sport1 Datenfehler)
  .[Hauptposition != "Verein   "]

# saveRDS(data, "./Data/cleaned_leistungsdaten_positionen.rds")