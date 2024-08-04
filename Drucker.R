# Libraries ---------------------------------------------------------------

rm(list = ls())
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tinytex)
library(DT)

# Funktionen --------------------------------------------------------------

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

get_sus_dist <- function(numSuS, landDist = T){
  
  df_caucus <- read.csv("Daten/caucus_data.csv")
  df_caucus$party <- df_caucus$party |> sapply(translate_group)
  
  partyDist <- dhondt(parties = df_caucus$party,
                      votes = df_caucus$total,
                      n_seats = numSuS)
  if (landDist) {
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
    
    listDist <- lapply(listDist, unlist)
    listDist
  } else {
    partyDist
  }
}

translation_data_group <- data.frame(
  en = c("EPP", "S&D", "Renew", "G / EFA", "PfE", "ECR", "The Left", "ESN", "Verts/ALE", "PPE", "Greens/EFA"),
  de = c("EVP", "S&D", "Renew", "Grüne", "PfE", "EKR", "Die Linke", "ESN", "Grüne", "EVP", "Grüne")
)

translate_group <- function(group){
  translation_data_group[translation_data_group$en == group, "de"]
}


# UI ----------------------------------------------------------------------

header <- dashboardHeader(title = "Unterlagendrucker")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Allgemein", tabName = "all_tab"),
    menuItem("Folien", tabName = "folien_tab"),
    menuItem("Sonstiges", tabName = "sonst_tab")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "all_tab",
      tabBox(
        title = "Allgemein",
        width = 12,
        selectInput("topic", "Thema:",
                     choices = c("Green Deal", "Migration", "Armee"),
                     selected = "Green Deal"),
        textInput("city", "Stadt:"),
        dateInput("date", "Datum:", format = "dd.mm.yyyy", language = "de", weekstart = 1),
        selectInput("docs", "Dokumente:",
                     choices = c("Repository", "Unterlagen SuS", "TN-Zertifikate", "SuS-Verteilung"),
                     selected = "Unterlagen SuS"),
        selectInput("reload", "Daten aktualisieren?",
                    choices = c("Länderpapiere", "Fraktionen & Ausschüsse", "Alle", "Keine"),
                    selected = "Keine")
      )
    ),
    
    tabItem(
      tabName = "folien_tab",
      fluidRow(
        box(
          width = 4,
          textInput("pol", "Politiker:"),
          textInput("pol_office", "Politiker (Amt):", value = "Mitglied des Europäischen Parlaments"),
          textInput("stadtvert", "Stadtvertreter:"),
          textInput("stadtvert_office", "Stadtvertreter (Amt):")
        ),
        box(
          width = 4,
          textInput("leit_evp", "Leitung EVP:", value = "TBD"),
          textInput("leit_sd", "Leitung S&D:", value = "TBD"),
          textInput("leit_renew", "Leitung Renew:", value = "TBD"),
          textInput("leit_green", "Leitung Grüne:", value = "TBD"),
          textInput("leit_pfe", "Leitung PfE:", value = "TBD")
        ),
        box(
          width = 4,
          textInput("room_evp", "Raum EVP:", value = "TBD"),
          textInput("room_sd", "Raum S&D:", value = "TBD"),
          textInput("room_renew", "Raum Renew:", value = "TBD"),
          textInput("room_green", "Raum Grüne:", value = "TBD"),
          textInput("room_pfe", "Raum PfE:", value = "TBD")
        )
      )
    ),
    
    tabItem(
      tabName = "sonst_tab",
      fluidRow(
        box(
          width = 4,
          numericInput("numSuS", "Anzahl SuS:", 0),
          textInput("tnListPath", "Dateiname TN-Excel:"),
          textInput("resPath", "Zielordner:", "Results"),
          actionButton("print", "Drucken")
        ),
        box(
          width = 4,
          textInput("timeEinf", "Uhrzeit Briefing:", value = "09:00-09:45"),
          textInput("timeFrakOne", "Uhrzeit 1. Fraktionssitzung:", value = "09:45-11:15"),
          textInput("timeAuss", "Uhrzeit Ausschusssitzung:", value = "11:30-12:45")
        ),
        box(
          width = 4,
          textInput("timeMittag", "Uhrzeit Mittagspause:", value = "12:45-13:15"),
          textInput("timeFrakTwo", "Uhrzeit 2. Fraktionssitzung:", value = "13:15-13:45"),
          textInput("timePlenar", "Uhrzeit Plenardebatte:", value = "14:00-15:00")
        )
      ),
      fluidRow(
        DT::dataTableOutput("susVert")
      )
    )
  )
)

ui <- dashboardPage(skin = "green", header, sidebar, body)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  countries <- c("Österreich", "Belgien", "Bulgarien", "Kroatien", "Zypern", "Tschechien", "Dänemark",
                 "Estland", "Finnland", "Frankreich", "Deutschland", "Griechenland", "Ungarn",
                 "Irland", "Italien", "Lettland", "Litauen", "Luxemburg", "Malta", "Niederlande",
                 "Polen", "Portugal", "Rumänien", "Slowakei", "Slowenien", "Spanien", "Schweden")
  
  groupsEP <- c("EVP", "S&D", "Renew", "Grüne", "PfE")
  
  observeEvent(input$print, 
     # Fix LaTex-Variables into tex-File
     {dfTexInput <- data.frame(index = "a",
                               topic = input$topic, # Green Deal/Migration/Armee 
                               city = input$city,
                               timeEinf = input$timeEinf,
                               timeFrakOne = input$timeFrakOne,
                               timeAuss = input$timeAuss,
                               timeMittag = input$timeMittag,
                               timeFrakTwo = input$timeFrakTwo,
                               timePlenar = input$timePlenar,
                               date = input$date |> as.character(),
                               politiker = input$pol,
                               politikerOffice = input$pol_office,
                               stadtvertreter = input$stadtvert,
                               stadtvertreterOffice = input$stadtvert_office,
                               evpLeader = input$leit_evp,
                               evpRoom = input$room_evp,
                               sdLeader = input$leit_sd,
                               sdRoom = input$room_sd,
                               reLeader = input$leit_renew,
                               reRoom = input$room_renew,
                               greenLeader = input$leit_green,
                               greenRoom = input$room_green,
                               pfeLeader = input$leit_pfe,
                               pfeRoom = input$room_pfe)
     sink("LaTeX/Meta/shinyin.tex")
     paste0("\\newcommand\\Thema{", dfTexInput$topic, "}\n") |> cat()
     paste0("\\newcommand\\city{", dfTexInput$city, "}\n") |> cat()
     paste0("\\newcommand\\timeEinf{", dfTexInput$timeEinf, "}\n") |> cat()
     paste0("\\newcommand\\timeFrakOne{", dfTexInput$timeFrakOne, "}\n") |> cat()
     paste0("\\newcommand\\timeAuss{", dfTexInput$timeAuss, "}\n") |> cat()
     paste0("\\newcommand\\timeMittag{", dfTexInput$timeMittag, "}\n") |> cat()
     paste0("\\newcommand\\timeFrakTwo{", dfTexInput$timeFrakTwo, "}\n") |> cat()
     paste0("\\newcommand\\timePlenar{", dfTexInput$timePlenar, "}\n") |> cat()
     paste0("\\newcommand\\politiker{", dfTexInput$politiker, "}\n") |> cat()
     paste0("\\newcommand\\politikerOffice{", dfTexInput$politikerOffice, "}\n") |> cat()
     paste0("\\newcommand\\stadtvertreter{", dfTexInput$stadtvertreter, "}\n") |> cat()
     paste0("\\newcommand\\stadtvertreterOffice{", dfTexInput$stadtvertreterOffice, "}\n") |> cat()
     paste0("\\newcommand\\evpLeader{", dfTexInput$evpLeader, "}\n") |> cat()
     paste0("\\newcommand\\evpRoom{", dfTexInput$evpRoom, "}\n") |> cat()
     paste0("\\newcommand\\sdLeader{", dfTexInput$sdLeader, "}\n") |> cat()
     paste0("\\newcommand\\sdRoom{", dfTexInput$sdRoom, "}\n") |> cat()
     paste0("\\newcommand\\reLeader{", dfTexInput$reLeader, "}\n") |> cat()
     paste0("\\newcommand\\reRoom{", dfTexInput$reRoom, "}\n") |> cat()
     paste0("\\newcommand\\greenLeader{", dfTexInput$greenLeader, "}\n") |> cat()
     paste0("\\newcommand\\greenRoom{", dfTexInput$greenRoom, "}\n") |> cat()
     paste0("\\newcommand\\pfeLeader{", dfTexInput$pfeLeader, "}\n") |> cat()
     paste0("\\newcommand\\pfeRoom{", dfTexInput$pfeRoom, "}\n") |> cat()
     sink()})        

  observeEvent(input$print, 
    if (input$reload == "Länderpapiere") {
      source("Länderpapiere.R")
    } else if (input$reload == "Fraktionen & Ausschüsse") {
      source("Folien.R")
    } else if (input$reload == "Alle") {
      source("Länderpapiere.R")
      source("Folien.R")
    })
  
  
  # verschiedene Druckoptionen durchspielen
  #         Fraktionen
  #           1. Fraktionssitzung
  #           2. Fraktionssitzung
  #           Fraktionspapier
  #         Ausschüsse
  #           Ausschusssitzung
  #         Sonstiges
  #           Briefing
  #           Plenarsitzung
  #           (TN-Zertifikate)
  #           Länderpapiere
  observeEvent(input$print, 
    if (input$docs == "Repository") {
      # Involvierte Ausschüsse festlegen
      if (input$topic == "Green Deal") {
        committees <- c("AGRI", "BUDG", "ITRE", "TRAN")
      } else if (input$topic == "Migration") {
        committees <- c("AFET", "DROI", "LIBE")
      } else if (input$topic == "Armee") {
        committees <- c("BUDG", "LIBE", "SEDE")
      }
      
      # Zielordner erstellen, falls nötig
      dir.create(file.path(input$resPath), showWarnings = F)
      ## Fraktionen
      dir.create(file.path(input$resPath, "Fraktionen"), showWarnings = F)
      for (group in groups) {
        dir.create(file.path(input$resPath, "Fraktionen", group), showWarnings = F)
      }
      ## Ausschüsse
      dir.create(file.path(input$resPath, "Ausschüsse"), showWarnings = F)
      ## Sonstiges
      dir.create(file.path(input$resPath, "Sonstiges"), showWarnings = F)
      
      # Compile pdfs
      ## Fraktionen
      for (group in groupsEP) {
        {dfTextVar <- data.frame(index = "a",
                                 group = group)
        sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\Fraktion{", dfTexInput$group, "}\n") |> cat()
        sink()}
        latexmk("LaTeX/Folien/1. Fraktionssitzung.tex", engine = "pdflatex",
                pdf_file = paste0(input$resPath, "Fraktionen/", group, "/1. Fraktionssitzung.pdf"))
        latexmk("LaTeX/Folien/2. Fraktionssitzung.tex", engine = "pdflatex",
                pdf_file = paste0(input$resPath, "Fraktionen/", group, "/2. Fraktionssitzung.pdf"))
        latexmk("LaTeX/Fraktionspapier.tex", engine = "pdflatex",
                pdf_file = paste0(input$resPath, "Fraktionen/", group, "/Fraktionspapier.pdf"))
      }
      ## Ausschüsse
      for (committee in committees) {
        {dfTextVar <- data.frame(index = "a",
                                 com = committee)
        sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\kurzel{", dfTexInput$iso, "}\n") |> cat()
        sink()}
        latexmk("LaTeX/Folien/Ausschusssitzung.tex", engine = "pdflatex",
                pdf_file = paste0(input$resPath, "Ausschüsse/", committee, ".pdf"))
      }
      ## Sonstiges
      latexmk("LaTeX/Folien/Plenarsitzung.tex", engine = "pdflatex",
              pdf_file = paste0(input$resPath, "Sonstiges/Plenarsitzung.pdf"))
      latexmk("LaTeX/Folien/Briefing.tex", engine = "pdflatex",
              pdf_file = paste0(input$resPath, "Sonstiges/Briefing.pdf"))
      ### ZUM LAUFEN BRINGEN & HIER NOCH TN-ZERTIFIKATE REIN ###
      
    } else if (input$docs == "Unterlagen SuS") {
      abc
    } else if (input$docs == "TN-Zertifikate") {
      abc
    } else if (input$docs == "SuS-Verteilung") {
      ### ZUM LAUFEN BRINGEN ###
      resSuS <- get_sus_dist(input$numSuS, landDist = F)
      output$susVert <- renderDataTable({datatable(resSuS)})
    }
  )
}
shinyApp(ui = ui, server = server)

stop()

# paste0("\\newcommand\\Fraktion{", dfTexInput$group, "}\n") |> cat()
# iso = "SWE",
# paste0("\\newcommand\\kurzel{", dfTexInput$iso, "}\n") |> cat()
# com = "AFET", # AFET, AGRI, BUDG, DROI, ITRE, LIBE, SEDE, TRAN
# paste0("\\newcommand\\Committee{", dfTexInput$com, "}\n") |> cat()

# Input
#   Allgemein
#     City
#     Date
#     Topic
#     Dokumente-Auswahl
#       a) Repository
#         Fraktionen
#           1. Fraktionssitzung
#           2. Fraktionssitzung
#           Fraktionspapier
#         Ausschüsse
#           Ausschusssitzung
#         Sonstiges
#           Briefing
#           Plenarsitzung
#           TN-Zertifikate
#       b) Unterlagen
#         Gesamt-PDF
#           Fraktionspapier
#           Gesetzesentwurf
#           Länderpapier
#       c) TN-Zertifikate (mit spezieller xlsx)
#       d) SuS-Verteilung

#   Folien
#     Politiker
#     Politiker (Amt)
#     Stadtvertreter
#     Stadtvertreter (Amt)
#     Fraktionsleiter
#     Fraktionsräume

#   Sonstiges
#     Anzahl SuS
#     TN xlsx
#     timeEinf = "09:00-09:45",
#     timeFrakOne = "09:45-11:15",
#     timeAuss = "11:30-12:45",
#     timeMittag = "12:45-13:15",
#     timeFrakTwo = "13:15-13:45",
#     timePlenar = "14:00-15:00"



stop()






# LaTeX Integration -------------------------------------------------------


# latexmk("LaTeX/How-To.tex", engine = "pdflatex", pdf_file = xxx)

# Über latexmk kompilieren
# Länderverteilung nach Anzahl zuordnen lassen
# In richtiger Reihenfolge je nach dhondt-Ergebnis in ein pdf fügen
  # qpdf::pdf_combine(input = c("file.pdf", "file2.pdf", "file3.pdf"),
  #                   output = "output.pdf")