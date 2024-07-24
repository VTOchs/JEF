# Verteilung pro Schüler --------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)

dhondt <- function (parties, votes, n_seats){
  divisors <- 1:n_seats
  votes <- tibble(PARTY = as.character(parties), VOTES = votes)
  quotiens <- as_tibble(expand.grid(PARTY = parties, DIVISOR = divisors)) %>% 
    mutate(PARTY = as.character(PARTY)) %>% left_join(votes, 
                                                      by = "PARTY") %>% mutate(QUOTIENTS = VOTES/DIVISOR) %>% 
    mutate(ORDER = rank(-QUOTIENTS, ties.method = "max"))
  seats <- quotiens %>% arrange(ORDER) %>% filter(ORDER <= 
                                                    length(divisors)) %>% group_by(PARTY) %>% summarise(SEATS = n())
  
  
  undisputed <- quotiens %>% arrange(ORDER) %>% filter(ORDER <= length(divisors))
  
  candidates <- quotiens %>% filter(ORDER > length(divisors)) %>% 
    mutate(TIES_ORDER = rank(ORDER, ties.method = "min")) %>%
    filter(TIES_ORDER == 1) |> 
    arrange(desc(VOTES)) |> 
    head(n_seats - undisputed$ORDER |> max())
  
  seats <- seats |> mutate(SEATS = case_when(PARTY %in% candidates$PARTY ~ SEATS + 1,
                                             !PARTY %in% candidates$PARTY ~ SEATS))
  if (seats$SEATS |> sum() == n_seats) {
    seats |> select(PARTY, SEATS)
  } else {
    print("Sitzaufteilungsfehler")
  }
}

dhondt(parties = c("EVP", "S&D", "Renew", "Grüne", "ID"),
       votes = c(30, 30, 20, 20, 20),
       n_seats = 87)



# LaTeX Integration -------------------------------------------------------

library(tinytex)
latexmk("LaTeX/How-To.tex", engine = "pdflatex")



# tex-Dateien in neuer Ordnerstruktur zum Laufen bringen
# verschiedene Parteien/Städte/Themen von außen anwählbar machen
# Checken dass alle gecrawlten Dateien eingelesen werden
# Über latexmk kompilieren
# Länderverteilung nach Anzahl zuordnen lassen
# Stadtspezifische Dinge (Redner etc.) als Shiny-Input
# In richtiger Reihenfolge je nach dhondt-Ergebnis in ein pdf fügen