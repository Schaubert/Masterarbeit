##### Steckbrief zerlegen ####

## source global.r
source("./Code/global.r")

#### Daten einlesen ####
data <- readRDS("./Data/data.rds")

#### Steckbrief zerlegen ####

test_steckbrief <- data[1, Steckbrief]

test_steckbrief %>% 
  str_split(., "\n") %>% 
  unlist %>% 
  .[which({. == "Nation"})+1]

data[, Nation := Steckbrief %>% 
       str_split(., "\n") %>% 
       unlist %>% 
       .[which({. == "Nation"})+1]] %>% 
  .[, Groesse := Steckbrief %>% 
      str_split(., "\n") %>% 
      unlist %>% 
      .[which({. == "Größe in cm"})+1]] %>% 
  .[, Geburtstag := Steckbrief %>% 
      str_split(., "\n") %>% 
      unlist %>% 
      .[which({. == "Geboren am"})+1]] %>% 
  .[, Gewicht := Steckbrief %>% 
      str_split(., "\n") %>% 
      unlist %>% 
      .[which({. == "Gewicht in kg"})+1]] %>% 
  .[, StarkerFuss := Steckbrief %>% 
      str_split(., "\n") %>% 
      unlist %>% 
      .[which({. == "Starker Fuss"})+1]]

#### > Tests ####

sum(is.na(data[, StarkerFuss]))
# 0

sum(is.na(data[, Gewicht]))
# 0

sum(is.na(data[, Groesse]))
# 0

sum(is.na(data[, Geburtstag]))
# 0

sum(is.na(data[, Nation]))
# 0

## Hat wohl super geklappt
## Kommentar an mich selbst: So hätte die Leistungsdatenzerlegung auch viel
## schneller geklappt^^

# saveRDS(data, "./Data/data.rds")
