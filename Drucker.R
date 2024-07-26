# Verteilung pro Schüler --------------------------------------------------
rm(list = ls())
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

countries <- c("Österreich", "Belgien", "Kroatien", "Dänemark", "Estland",
               "Finnland", "Frankreich", "Deutschland", "Griechenland",
               "Ungarn", "Irland", "Italien", "Polen", "Rumänien", "Spanien")
numSuS <- 87

{partyDist <- dhondt(parties = c("EVP", "S&D", "Renew", "Grüne", "PfE"),
                    votes = c(30, 30, 20, 20, 20),
                    n_seats = numSuS)

countDist <- dhondt(parties = countries,
                    votes = rep(1, length(countries)),
                    n_seats = numSuS)

listDist <- vector(mode = "list", length = 5)
names(listDist) <- c("EVP", "S&D", "Renew", "Grüne", "PfE")

while (countDist$SEATS |> sum() > 0) {
  party <- partyDist[which.max(partyDist$SEATS),] |> pull(PARTY)
  partyDist[partyDist$PARTY == party, "SEATS"] <- partyDist[partyDist$PARTY == party, "SEATS"] - 1
  countryCandid <- countDist |> filter(SEATS == max(countDist$SEATS)) |> pull(PARTY)
  country <- sample(countryCandid, 1)
  listDist[[party]][[length(listDist[[party]]) + 1]] <- country
  countDist[countDist$PARTY == country, "SEATS"] <- countDist[countDist$PARTY == country, "SEATS"] - 1
}

listDist <- lapply(listDist, unlist)}
listDist
stop()


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