# Verteilung pro Schüler --------------------------------------------------
rm(list = ls())
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tinytex)

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

countries <- c("Österreich", "Belgien", "Bulgarien", "Kroatien", "Zypern", "Tschechien", "Tschechien", "Dänemark",
               "Estland", "Finnland", "Frankreich", "Deutschland", "Griechenland", "Ungarn",
               "Irland", "Italien", "Lettland", "Litauen", "Luxemburg", "Malta", "Niederlande",
               "Polen", "Portugal", "Rumänien", "Slowakei", "Slowenien", "Spanien", "Schweden")

header <- dashboardHeader(title = "Unterlagendrucker")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Eingabe", tabName = "input_tab"),
    menuItem("Ergebnis", tabName = "output_tab")
  )
)





numSuS <- 87
source("Folien.R")
source("Länderpapiere.R")
{partyDist <- dhondt(parties = c("EVP", "S&D", "Renew", "Grüne", "PfE"),
                    votes = c(188, 136, 77, 53, 84),
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



# LaTeX Integration -------------------------------------------------------


# latexmk("LaTeX/How-To.tex", engine = "pdflatex")

dfTexInput <- data.frame(index = "a",
                      group = "SD", # EVP/SD/RE/Green/ID | LEER,
                      topic = "Green Deal", # Green Deal/Migration/Armee 
                      com = "AFET", # AFET, AGRI, BUDG, DROI, ITRE, LIBE, SEDE, TRAN
                      city = "Bayreuth",
                      timeEinf = "09:00-09:45",
                      timeFrakOne = "09:45-11:15",
                      timeAuss = "11:30-12:45",
                      timeMittag = "12:45-13:15",
                      timeFrakTwo = "13:15-13:45",
                      timePlenar = "14:00-15:00",
                      date = "17. Mai 2024",
                      politiker = "Thomas Hacker",
                      politikerOffice = "Mitglied des Bundestages",
                      politikerPic = "Bilder/hacker.png",
                      stadtvertreter = "Janina Kiekebusch",
                      stadtvertreterOffice = "IHK-Referentin für europäischen Handel und EU-Politik",
                      stadtvertreterPic = "Bilder/kikebusch.png",
                      iso = "SWE",
                      evpLeader = "TBD",
                      evpRoom = "TBD",
                      sdLeader = "TBD",
                      sdRoom = "TBD",
                      reLeader = "TBD",
                      reRoom = "TBD",
                      greenLeader = "TBD",
                      greenRoom = "TBD",
                      idLeader = "TBD",
                      idRoom = "TBD")

{sink("LaTeX/Meta/var.tex")
paste0("\\newcommand\\Fraktion{", dfTexInput$group, "}\n") |> cat()
paste0("\\newcommand\\Thema{", dfTexInput$topic, "}\n") |> cat()
paste0("\\newcommand\\Committee{", dfTexInput$com, "}\n") |> cat()
paste0("\\newcommand\\city{", dfTexInput$city, "}\n") |> cat()
paste0("\\newcommand\\timeEinf{", dfTexInput$timeEinf, "}\n") |> cat()
paste0("\\newcommand\\timeFrakOne{", dfTexInput$timeFrakOne, "}\n") |> cat()
paste0("\\newcommand\\timeAuss{", dfTexInput$timeAuss, "}\n") |> cat()
paste0("\\newcommand\\timeMittag{", dfTexInput$timeMittag, "}\n") |> cat()
paste0("\\newcommand\\timeFrakTwo{", dfTexInput$timeFrakTwo, "}\n") |> cat()
paste0("\\newcommand\\timePlenar{", dfTexInput$timePlenar, "}\n") |> cat()
paste0("\\newcommand\\politiker{", dfTexInput$politiker, "}\n") |> cat()
paste0("\\newcommand\\politikerOffice{", dfTexInput$politikerOffice, "}\n") |> cat()
paste0("\\newcommand\\politikerPic{", dfTexInput$politikerPic, "}\n") |> cat()
paste0("\\newcommand\\stadtvertreter{", dfTexInput$stadtvertreter, "}\n") |> cat()
paste0("\\newcommand\\stadtvertreterOffice{", dfTexInput$stadtvertreterOffice, "}\n") |> cat()
paste0("\\newcommand\\stadtvertreterPic{", dfTexInput$stadtvertreterPic, "}\n") |> cat()
paste0("\\newcommand\\kurzel{", dfTexInput$iso, "}\n") |> cat()
paste0("\\newcommand\\evpLeader{", dfTexInput$evpLeader, "}\n") |> cat()
paste0("\\newcommand\\evpRoom{", dfTexInput$evpRoom, "}\n") |> cat()
paste0("\\newcommand\\sdLeader{", dfTexInput$sdLeader, "}\n") |> cat()
paste0("\\newcommand\\sdRoom{", dfTexInput$sdRoom, "}\n") |> cat()
paste0("\\newcommand\\reLeader{", dfTexInput$reLeader, "}\n") |> cat()
paste0("\\newcommand\\reRoom{", dfTexInput$reRoom, "}\n") |> cat()
paste0("\\newcommand\\greenLeader{", dfTexInput$greenLeader, "}\n") |> cat()
paste0("\\newcommand\\greenRoom{", dfTexInput$greenRoom, "}\n") |> cat()
paste0("\\newcommand\\idLeader{", dfTexInput$idLeader, "}\n") |> cat()
paste0("\\newcommand\\idRoom{", dfTexInput$idRoom, "}\n") |> cat()
sink()}
#



# Über latexmk kompilieren
# Länderverteilung nach Anzahl zuordnen lassen
# Stadtspezifische Dinge (Redner etc.) als Shiny-Input
# In richtiger Reihenfolge je nach dhondt-Ergebnis in ein pdf fügen
  # qpdf::pdf_combine(input = c("file.pdf", "file2.pdf", "file3.pdf"),
  #                   output = "output.pdf")