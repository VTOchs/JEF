# Libraries ---------------------------------------------------------------

rm(list = ls())
library(DT)
library(qpdf)
library(readxl)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tinytex)

# Funktionen --------------------------------------------------------------

groupsEP <- c("EVP", "SD", "RE", "Green", "PfE")

countries <-  c("AUT", "BEL", "BGR", "HRV",
                "CYP", "CZE", "DNK", "EST",
                "FIN", "FRA", "DEU", "GRC",
                "HUN", "IRL", "ITA", "LVA",
                "LTU", "LUX", "MLT", "NLD",
                "POL", "PRT", "ROU", "SVK",
                "SVN", "ESP", "SWE")


# für die LaTeX-Dokumente
translation_data_latex <- data.frame(
  en = c("EPP", "S&D", "Renew", "G / EFA", "PfE", "ECR", "The Left", "ESN", "Verts/ALE", "PPE", "Greens/EFA"),
  de = c("EVP", "SD", "RE", "Green", "PfE", "EKR", "Die Linke", "ESN", "Green", "EVP", "Green")
)

translate_latex <- function(group){
  translation_data_latex[translation_data_latex$en == group, "de"]
}

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
  df_caucus$party <- df_caucus$party |> sapply(translate_latex)
  
  partyDist <- dhondt(parties = df_caucus$party,
                      votes = df_caucus$total,
                      n_seats = numSuS)
  if (landDist) {
    countDist <- dhondt(parties = countries,
                        votes = rep(1, length(countries)),
                        n_seats = numSuS)
    
    listDist <- vector(mode = "list", length = 5)
    names(listDist) <- groupsEP
    
    while (countDist$SEATS |> sum() > 0) {
      party <- partyDist[which.max(partyDist$SEATS),] |> pull(PARTY)
      partyDist[partyDist$PARTY == party, "SEATS"] <- partyDist[partyDist$PARTY == party, "SEATS"] - 1
      countryCandid <- countDist |> filter(SEATS == max(countDist$SEATS)) |> pull(PARTY)
      country <- sample(countryCandid, 1)
      listDist[[party]][[length(listDist[[party]]) + 1]] <- country
      countDist[countDist$PARTY == country, "SEATS"] <- countDist[countDist$PARTY == country, "SEATS"] - 1
    }
    
    listDist <- lapply(listDist, unlist)
    listDist <- listDist[order(sapply(listDist, length), decreasing = T)]
    listDist
  } else {
    partyDist |> arrange(desc(SEATS)) |> rename(SuS = SEATS,
                                                Fraktion = PARTY)
  }
}

# UI ----------------------------------------------------------------------

header <- dashboardHeader(title = "Unterlagendrucker")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Allgemein", tabName = "all_tab"),
    menuItem("R/U/TN", tabName = "r_u_tn_tab"),
    menuItem("R/TN", tabName = "r_tn_tab"),
    menuItem("R/U", tabName = "r_u_tab"),
    menuItem("R", tabName = "r_tab"),
    menuItem("TN", tabName = "tn_tab")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "all_tab",
      tabBox(
        title = "Allgemein",
        fluidRow(
          box(
            width = 12,
            selectInput("docs", "Dokumente:",
                        choices = c("Repository", "Unterlagen SuS", "TN-Zertifikate", "SuS-Verteilung"),
                        selected = "SuS-Verteilung"),
            numericInput("numSuS", "Anzahl SuS:", 0),
            actionButton("print", "Drucken")  
          )
        ),
        fluidRow(
          box(
            width = 12,
            tableOutput("susVert")  
          )
        )
      )
    ),
    
    tabItem(
      tabName = "r_u_tn_tab",
      fluidRow(
        box(
          width = 12,
          selectInput("topic", "Thema:",
                      choices = c("Green Deal", "Migration", "Armee"),
                      selected = "Green Deal"),
          textInput("city", "Stadt:"),
          dateInput("date", "Datum:", format = "dd.mm.yyyy", language = "de", weekstart = 1),
          textInput("resPath", "Zielordner:", "Results")
        )
      )
    ),
    
    tabItem(
      tabName = "r_tn_tab",
      fluidRow(
        box(
          width = 12,
          textInput("localSup", "Lokale Unterstützung:"),
          textInput("sponsor", "Sponsor:"),
          textInput("jefvorsitz", "Vorsitz JEF Bayern:", value = "Farras Fathi"),
          selectInput("gender", "Geschlecht Vorsitz JEF Bayern", choices = c("M", "W"), selected = "M")
        )
      )
    ),
    
    tabItem(
      tabName = "r_u_tab",
      fluidRow(
        box(
          width = 12,
          selectInput("reload", "Daten aktualisieren?",
                      choices = c("Länderpapiere", "Fraktionen & Ausschüsse", "Alle", "Keine"),
                      selected = "Keine"),
          textInput("timeEinf", "Uhrzeit Briefing:", value = "09:00-09:45"),
          textInput("timeFrakOne", "Uhrzeit 1. Fraktionssitzung:", value = "09:45-11:15"),
          textInput("timeAuss", "Uhrzeit Ausschusssitzung:", value = "11:30-12:45"),
          textInput("timeMittag", "Uhrzeit Mittagspause:", value = "12:45-13:15"),
          textInput("timeFrakTwo", "Uhrzeit 2. Fraktionssitzung:", value = "13:15-13:45"),
          textInput("timePlenar", "Uhrzeit Plenardebatte:", value = "14:00-15:00")
        )
      )
    ),
    
    tabItem(
      tabName = "r_tab",
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
      tabName = "tn_tab",
      fluidRow(
        box(
          width = 12,
          textInput("tnListPath", "Dateiname TN-Excel:")
        )
      )
    )
  )
)

ui <- dashboardPage(skin = "green", header, sidebar, body)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  
  observeEvent(input$print, 
     # Fix LaTex-Variables into tex-File
     {sink("LaTeX/Meta/shinyin.tex")
     paste0("\\newcommand\\Thema{", input$topic, "}\n") |> cat()
     paste0("\\newcommand\\city{", input$city, "}\n") |> cat()
     paste0("\\newcommand\\datum{", format(as.Date(input$date), "%d.%m.%Y"), "}\n") |> cat()
     paste0("\\newcommand\\timeEinf{", input$timeEinf, "}\n") |> cat()
     paste0("\\newcommand\\timeFrakOne{", input$timeFrakOne, "}\n") |> cat()
     paste0("\\newcommand\\timeAuss{", input$timeAuss, "}\n") |> cat()
     paste0("\\newcommand\\timeMittag{", input$timeMittag, "}\n") |> cat()
     paste0("\\newcommand\\timeFrakTwo{", input$timeFrakTwo, "}\n") |> cat()
     paste0("\\newcommand\\timePlenar{", input$timePlenar, "}\n") |> cat()
     paste0("\\newcommand\\politiker{", input$pol, "}\n") |> cat()
     paste0("\\newcommand\\politikerOffice{", input$pol_office, "}\n") |> cat()
     paste0("\\newcommand\\stadtvertreter{", input$stadtvert, "}\n") |> cat()
     paste0("\\newcommand\\stadtvertreterOffice{", input$stadtvert_office, "}\n") |> cat()
     paste0("\\newcommand\\localSupport{", input$localSup, "}\n") |> cat()
     paste0("\\newcommand\\sponsor{", input$sponsor, "}\n") |> cat()
     paste0("\\newcommand\\jefvorsitz{", input$jefvorsitz, "}\n") |> cat()
     paste0("\\newcommand\\gendervorsitz{", ifelse(input$gender == "M", "Landesvorsitzender", "Landesvorsitzende"), "}\n") |> cat()
     paste0("\\newcommand\\evpLeader{", input$leit_evp, "}\n") |> cat()
     paste0("\\newcommand\\evpRoom{", input$room_evp, "}\n") |> cat()
     paste0("\\newcommand\\sdLeader{", input$leit_sd, "}\n") |> cat()
     paste0("\\newcommand\\sdRoom{", input$room_sd, "}\n") |> cat()
     paste0("\\newcommand\\reLeader{", input$leit_renew, "}\n") |> cat()
     paste0("\\newcommand\\reRoom{", input$room_renew, "}\n") |> cat()
     paste0("\\newcommand\\greenLeader{", input$leit_green, "}\n") |> cat()
     paste0("\\newcommand\\greenRoom{", input$room_green, "}\n") |> cat()
     paste0("\\newcommand\\pfeLeader{", input$leit_pfe, "}\n") |> cat()
     paste0("\\newcommand\\pfeRoom{", input$room_pfe, "}\n") |> cat()
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
      for (group in groupsEP) {
        dir.create(file.path(input$resPath, "Fraktionen", group), showWarnings = F)
      }
      ## Ausschüsse
      dir.create(file.path(input$resPath, "Ausschüsse"), showWarnings = F)
      ## Sonstiges
      dir.create(file.path(input$resPath, "Sonstiges"), showWarnings = F)
      ### TN-Zertifikate
      dir.create(file.path(input$resPath, "Sonstiges", "TN-Zertifikate"), showWarnings = F)
      
      # Compile pdfs
      ## Fraktionen
      for (group in groupsEP) {
        {sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\Fraktion{", group, "}\n") |> cat()
        sink()}
        
        tools::texi2pdf("LaTeX/Folien/1. Fraktionssitzung.tex", clean = T)
        file.rename("1. Fraktionssitzung.pdf", paste0(input$resPath, "/Fraktionen/", group, "/1. Fraktionssitzung_", group, ".pdf"))
        
        tools::texi2pdf("LaTeX/Folien/2. Fraktionssitzung.tex", clean = T)
        file.rename("2. Fraktionssitzung.pdf", paste0(input$resPath, "/Fraktionen/", group, "/2. Fraktionssitzung_", group, ".pdf"))
        
        tools::texi2pdf("LaTeX/Fraktionspapier.tex", clean = T)
        file.rename("Fraktionspapier.pdf", paste0(input$resPath, "/Fraktionen/", group, "/Fraktionspapier_", group, ".pdf"))
      }
      
      ## Ausschüsse
      for (committee in committees) {
        {sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\Committee{", committee, "}\n") |> cat()
        paste0("\\newcommand\\Fraktion{LEER}\n") |> cat()
        sink()}
        tools::texi2pdf("LaTeX/Folien/Ausschusssitzung.tex", clean = T)
        file.rename("Ausschusssitzung.pdf", paste0(input$resPath, "/Ausschüsse/", committee, ".pdf"))
      }
      
      ## Sonstiges
      tools::texi2pdf("LaTeX/Folien/Plenarsitzung.tex", clean = T)
      file.rename("Plenarsitzung.pdf", paste0(input$resPath, "/Sonstiges/Plenarsitzung.pdf"))
      
      tools::texi2pdf("LaTeX/Folien/Briefing.tex", clean = T)
      file.rename("Briefing.pdf", paste0(input$resPath, "/Sonstiges/Briefing.pdf"))
      
      ## TN-Zertifikate
      
      for (excel in list.files("Daten/SuS")) {
        xlPath <- paste0("Daten/SuS/", excel)
        for (sheet in excel_sheets(xlPath)) {
          df_xlsx <- read_excel(xlPath, sheet = sheet)
          write.csv(df_xlsx, paste0("Daten/SuS/", sheet, ".csv"))
          {sink("LaTeX/Meta/var.tex")
            paste0("\\newcommand\\klasse{", sheet, "}\n") |> cat()
            sink()}
          tools::texi2pdf("LaTeX/TN-Zertifikat.tex", clean = T)
          file.rename("TN-Zertifikat.pdf", paste0(input$resPath, "/Sonstiges/TN-Zertifikate", sheet, ".pdf"))
        }
      }
      
    } else if (input$docs == "Unterlagen SuS") {
      
      dir.create(file.path(input$resPath), showWarnings = F)
      dir.create(file.path(input$resPath, "Einzeldokumente"), showWarnings = F)
      
      for (group in groupsEP) {
        {sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\Fraktion{", group, "}\n") |> cat()
        sink()}
        
        tools::texi2pdf("LaTeX/Fraktionspapier.tex", clean = T)
        file.rename("Fraktionspapier.pdf", paste0(input$resPath, "/Einzeldokumente/Fraktionspapier_", group,".pdf"))
      }
      
      for (member in countries) {
        {sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\kurzel{", member, "}\n") |> cat()
        sink()}
        
        tools::texi2pdf("LaTeX/Länderpapier.tex", clean = T)
        file.rename("Länderpapier.pdf", paste0(input$resPath, "/Einzeldokumente/Länderpapier_", member,".pdf"))
        
      }
      
      susFrakLand <- get_sus_dist(input$numSuS)
      pdf_order <- c()
      for (group in susFrakLand |> names()) {
        for (member in susFrakLand[[group]]) {
          pdf_order <- append(pdf_order, paste0(input$resPath, "/Einzeldokumente/Fraktionspapier_", group,".pdf"))
          pdf_order <- append(pdf_order, paste0("LaTeX/Gesetzesentwürfe/Entwurf_", input$topic, ".pdf"))
          pdf_order <- append(pdf_order, paste0(input$resPath, "/Einzeldokumente/Länderpapier_", member,".pdf"))
        }
      }
      pdf_combine(input = pdf_order,
                        output = paste0(input$resPath, "/Schülerunterlagen_SimEP.pdf"))

    } else if (input$docs == "TN-Zertifikate") {
      
      xlPath <- paste0("Daten/SuS/", input$tnListPath, ".xlsx")
      for (sheet in excel_sheets(xlPath)) {
        df_xlsx <- read_excel(xlPath, sheet = sheet)
        write.csv(df_xlsx, paste0("Daten/SuS/", sheet, ".csv"), row.names = FALSE)
        {sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\klasse{", sheet, "}\n") |> cat()
        sink()}
        
        tools::texi2pdf("LaTeX/TN-Zertifikat.tex", clean = T)
        file.rename("TN-Zertifikat.pdf", paste0(input$resPath, "/", sheet, ".pdf"))
        
      }
    } else if (input$docs == "SuS-Verteilung") {
      resSuS <- get_sus_dist(input$numSuS, landDist = T)
      output$susVert <- renderTable({resSuS})
    }
  )
}
shinyApp(ui = ui, server = server)

stop()

# tools::texi2pdf("LaTeX/TN-Zertifikat.tex", clean = T)
# \begin{comment}
# Anleitung für Erstellung richtiger csv-Dateien:
#   1. Entsprechende Excel-Tabelle öffnen (Spalten müssen "Vorname" und "Nachname" heißen)
# 2. Alt + F11
# 3. Einfügen > Modul
# 4. Folgendes reinkopieren:
#   
# Sub ExcelToCSV()
# Dim sh As Worksheet
# Dim file_path As String
# Application.ScreenUpdating = False
# file_path = ActiveWorkbook.Path & "\" & _
# Left(ActiveWorkbook.Name, InStr(ActiveWorkbook.Name, ".") - 1)
# For Each sh In Worksheets
# sh.Copy
# ActiveWorkbook.SaveAs Filename:=file_path & "-" & sh.Name & ".csv", _
# FileFormat:=xlCSVUTF8, CreateBackup:=False
# ActiveWorkbook.Close False
# Next
# Application.ScreenUpdating = True
# End Sub
# 
# 5. F5 drücken
# 6. kurz warten & entstandene .csv-Dateien in "Daten"-Ordner laden
# 
# \end{comment}