# Libraries ---------------------------------------------------------------

rm(list = ls())
library(qpdf)
library(readxl)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tinytex)

source("Scripts/drucker_helper.R")


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
                        choices = c("Repository", "Unterlagen SuS (min. 27)", "TN-Zertifikate", "SuS-Verteilung"),
                        selected = "SuS-Verteilung"),
            numericInput("numSuS", "Anzahl SuS:", 27),
            actionButton("reload", "Daten aktualisieren"),
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
                      choices = c("Green Deal", "Asyl", "Armee"),
                      selected = "Asyl"),
          selectInput("city", "Stadt:",
                      choices = c("München", "Nürnberg"),
                      selected = "München"),
          # textInput("city", "Stadt:", value = "München"),
          dateInput("date", "Datum:", format = "dd.mm.yyyy", language = "de", weekstart = 1),
          selectInput("resPath", "Zielordner:",
                      choices = c("München", "Nürnberg"),
                      selected = "München")
          # textInput("resPath", "Zielordner:", "Results")
        )
      )
    ),
    
    tabItem(
      tabName = "r_tn_tab",
      fluidRow(
        box(
          width = 12,
          selectInput("localSup", "Lokale Unterstützung:",
                      choices = c("das Europe Direct München", "das Europe Direct Nürnberg"),
                      selected = "das Europe Direct München"),
          # textInput("localSup", "Lokale Unterstützung:", "das Europe Direct München"),
          selectInput("sponsor", "Sponsor:",
                      choices = c("die Stadt München", "die Stadt Nürnberg"),
                      selected = "die Stadt München"),
          # textInput("sponsor", "Sponsor:", "die Stadt München"),
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
          textInput("timeVorb", "Uhrzeit Vorbereitung:", value = "07:45-09:00"),
          textInput("timeEinf", "Uhrzeit Briefing:", value = "09:00-09:45"),
          textInput("timeFrakOne", "Uhrzeit 1. Fraktionssitzung:", value = "09:45-11:15"),
          textInput("timePauseOne", "Uhrzeit 1. Zwischenpause:", value = "11:15-11:30"),
          textInput("timeAuss", "Uhrzeit Ausschusssitzung:", value = "11:30-12:45"),
          textInput("timeMittag", "Uhrzeit Mittagspause:", value = "12:45-13:15"),
          textInput("timeFrakTwo", "Uhrzeit 2. Fraktionssitzung:", value = "13:15-13:45"),
          textInput("timePauseTwo", "Uhrzeit 2. Zwischenpause:", value = "13:45-14:00"),
          textInput("timePlenar", "Uhrzeit Plenardebatte:", value = "14:00-15:00")
        )
      )
    ),
    
    tabItem(
      tabName = "r_tab",
      fluidRow(
        box(
          width = 4,
          selectInput("pol", "Politiker:",
                      choices = c("Maria Noichl", "Monika Hohlmeier"),
                      selected = "Maria Noichl"),
          # textInput("pol", "Politiker:", value = "Maria Noichl"),
          textInput("pol_office", "Politiker (Amt):", value = "Mitglied des Europäischen Parlaments"),
          selectInput("stadtvert", "Stadtvertreter:",
                      choices = c("Florian Kraus", "Dr. Andrea Heilmaier"),
                      selected = "Florian Kraus"),
          # textInput("stadtvert", "Stadtvertreter:", value = "Florian Kraus"),
          selectInput("stadtvert_office", "Stadtvertreter (Amt):",
                      choices = c("Stadtschulrat", "Wirtschafts- und Wissenschaftsreferentin"),
                      selected = "Stadtschulrat"),
          # textInput("stadtvert_office", "Stadtvertreter (Amt):", value = "Stadtschulrat"),
          selectInput("location", "Veranstaltungsort:",
                      choices = c("im Bayerischen Landtag", "im Nürnberger Rathaus"),
                      selected = "im Bayerischen Landtag")
        ),
        box(
          width = 4,
          textInput("leit_evp", "Leitung EVP:", value = "Linus"),
          textInput("leit_sd", "Leitung S&D:", value = "Christoph"),
          textInput("leit_renew", "Leitung Renew:", value = "Marco"),
          textInput("leit_green", "Leitung Grüne:", value = "Katharina"),
          textInput("leit_pfe", "Leitung PfE:", value = "Franz")
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

  observeEvent(input$reload,
                {source("Scripts/Länderpapiere.R")
                source("Scripts/Folien.R")
                print("New data downloaded!")}
               )
  
  observeEvent(input$print, 
     
       # Involvierte Ausschüsse festlegen
       {if (input$topic == "Green Deal") {
         committees <- c("AGRI", "BUDG", "ITRE", "TRAN")
       } else if (input$topic == "Asyl") {
         committees <- c("BUDG", "DROI", "EMPL", "LIBE")
       } else if (input$topic == "Armee") {
         committees <- c("BUDG", "LIBE", "SEDE")
       }
               
        # Fix LaTex-Variables into tex-File
        sink("LaTeX/Meta/shinyin.tex")
        cat(paste0("\\newcommand\\Thema{", input$topic, "}\n"))
        cat(paste0("\\newcommand\\city{", input$city, "}\n"))
        cat(paste0("\\newcommand\\datum{", format(as.Date(input$date), "%d.%m.%Y"), "}\n"))
        cat(paste0("\\newcommand\\timeVorb{", input$timeVorb, "}\n"))
        cat(paste0("\\newcommand\\timeEinf{", input$timeEinf, "}\n"))
        cat(paste0("\\newcommand\\timeFrakOne{", input$timeFrakOne, "}\n"))
        cat(paste0("\\newcommand\\timePauseOne{", input$timePauseOne, "}\n"))
        cat(paste0("\\newcommand\\timeAuss{", input$timeAuss, "}\n"))
        cat(paste0("\\newcommand\\timeMittag{", input$timeMittag, "}\n"))
        cat(paste0("\\newcommand\\timeFrakTwo{", input$timeFrakTwo, "}\n"))
        cat(paste0("\\newcommand\\timePauseTwo{", input$timePauseTwo, "}\n"))
        cat(paste0("\\newcommand\\timePlenar{", input$timePlenar, "}\n"))
        cat(paste0("\\newcommand\\politiker{", input$pol, "}\n"))
        cat(paste0("\\newcommand\\politikerOffice{", input$pol_office, "}\n"))
        cat(paste0("\\newcommand\\stadtvertreter{", input$stadtvert, "}\n"))
        cat(paste0("\\newcommand\\stadtvertreterOffice{", input$stadtvert_office, "}\n"))
        cat(paste0("\\newcommand\\localSupport{", input$localSup, "}\n"))
        cat(paste0("\\newcommand\\sponsor{", input$sponsor, "}\n"))
        cat(paste0("\\newcommand\\jefvorsitz{", input$jefvorsitz, "}\n"))
        cat(paste0("\\newcommand\\gendervorsitz{", ifelse(input$gender == "M", "Landesvorsitzender", "Landesvorsitzende"), "}\n"))
        cat(paste0("\\newcommand\\evpLeader{", input$leit_evp, "}\n"))
        cat(paste0("\\newcommand\\evpRoom{", input$room_evp, "}\n"))
        cat(paste0("\\newcommand\\sdLeader{", input$leit_sd, "}\n"))
        cat(paste0("\\newcommand\\sdRoom{", input$room_sd, "}\n"))
        cat(paste0("\\newcommand\\reLeader{", input$leit_renew, "}\n"))
        cat(paste0("\\newcommand\\reRoom{", input$room_renew, "}\n"))
        cat(paste0("\\newcommand\\greenLeader{", input$leit_green, "}\n"))
        cat(paste0("\\newcommand\\greenRoom{", input$room_green, "}\n"))
        cat(paste0("\\newcommand\\pfeLeader{", input$leit_pfe, "}\n"))
        cat(paste0("\\newcommand\\pfeRoom{", input$room_pfe, "}\n"))
        cat(paste0("\\newcommand\\numSuS{", input$numSuS, "}\n"))
        cat(paste0("\\newcommand\\location{", input$location, "}\n"))
        cat(paste0("\\newcommand\\anzahlcomm{", length(committees), "}\n"))
        sink()}
      ) 

  observeEvent(input$print, 
    
    if (input$docs == "Repository") {
      
      if (input$topic == "Green Deal") {
        committees <- c("AGRI", "BUDG", "ITRE", "TRAN")
      } else if (input$topic == "Asyl") {
        committees <- c("BUDG", "DROI", "EMPL", "LIBE")
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
      pdf_order <- c()
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
        
        pdf_order <- append(pdf_order, paste0("LaTeX/Sonstiges/Raum ", group, ".pdf"))
      }
      
      ## Ausschüsse
      for (committee in committees) {
        {sink("LaTeX/Meta/var.tex")
        paste0("\\newcommand\\Committee{", committee, "}\n") |> cat()
        paste0("\\newcommand\\Fraktion{LEER}\n") |> cat()
        sink()}
        tools::texi2pdf("LaTeX/Folien/Ausschusssitzung.tex", clean = T)
        file.rename("Ausschusssitzung.pdf", paste0(input$resPath, "/Ausschüsse/", committee, ".pdf"))
        pdf_order <- append(pdf_order, paste0("LaTeX/Sonstiges/", committee, "_Schilder/", list.files(paste0("LaTeX/Sonstiges/", committee, "_Schilder"))))
      }
      
      # print("Ausschüsse fertig!")
      
      ## Sonstiges
      
      tools::texi2pdf("LaTeX/Folien/Plenarsitzung.tex", clean = T)
      file.rename("Plenarsitzung.pdf", paste0(input$resPath, "/Sonstiges/Plenarsitzung.pdf"))
      
      tools::texi2pdf("LaTeX/Folien/Briefing.tex", clean = T)
      file.rename("Briefing.pdf", paste0(input$resPath, "/Sonstiges/Briefing.pdf"))
      
      tools::texi2pdf("LaTeX/How-To.tex", clean = T)
      file.rename("How-To.pdf", paste0(input$resPath, "/Sonstiges/How-To.pdf"))
      
      tools::texi2pdf("LaTeX/PM.tex", clean = T)
      file.rename("PM.pdf", paste0(input$resPath, "/Sonstiges/Pressemitteilung.pdf"))
      
      tools::texi2pdf("LaTeX/Datenschutzvereinbarung.tex", clean = T)
      file.rename("Datenschutzvereinbarung.pdf", paste0(input$resPath, "/Sonstiges/Datenschutzvereinbarung.pdf"))
      
      file.copy(paste0("LaTeX/Gesetzesentwürfe/Entwurf_", input$topic, ".pdf"), paste0(input$resPath, "/Sonstiges/Entwurf_", input$topic, ".pdf"))
      
      pdf_order <- append(pdf_order, "LaTeX/Sonstiges/Namen Leitung.pdf")
      pdf_order <- append(pdf_order, "LaTeX/Sonstiges/Namen Vorstand.pdf")
      
      pdf_combine(input = pdf_order,
                  output = paste0(input$resPath, "/Sonstiges/Schilder.pdf"))
      
      ## TN-Zertifikate
      
      for (excel in list.files("Daten/SuS", pattern='xlsx')) {
        xlPath <- paste0("Daten/SuS/", excel)
        for (sheet in excel_sheets(xlPath)) {
          df_xlsx <- read_excel(xlPath, sheet = sheet)
          write.csv(df_xlsx, paste0("Daten/SuS/", sheet, ".csv"), row.names = FALSE)
          {sink("LaTeX/Meta/var.tex")
            paste0("\\newcommand\\klasse{", sheet, "}\n") |> cat()
            sink()}
          tools::texi2pdf("LaTeX/TN-Zertifikat.tex", clean = T)
          file.rename("TN-Zertifikat.pdf", paste0(input$resPath, "/Sonstiges/TN-Zertifikate/", sheet, ".pdf"))
        }
      }
      
      for (suffix in c("aux", "log", "out", "nav", "toc", "gz", "snm")) {
        move_temp_files("temp", suffix)
        move_temp_files(target_dir = "temp", file_ext = suffix, source_dir = "LaTeX")
        move_temp_files(target_dir = "temp", file_ext = suffix, source_dir = "LaTeX/Folien")
      }
      
      print(paste0("Repo-Druck (", input$city, ") fertig!"))
      
    } else if (input$docs == "Unterlagen SuS (min. 27)") {
      
      dir.create(file.path(input$resPath), showWarnings = F)
      dir.create(file.path(input$resPath, "Einzeldokumente"), showWarnings = F)
      
      for (group in groupsEP) {
        output_file <- paste0(input$resPath, "/Einzeldokumente/Fraktionspapier_", group, ".pdf")
        
        if (!file.exists(output_file)) {
          {sink("LaTeX/Meta/var.tex")
          paste0("\\newcommand\\Fraktion{", group, "}\n") |> cat()
          sink()}
          
          tools::texi2pdf("LaTeX/Fraktionspapier.tex", clean = T)
          file.rename("Fraktionspapier.pdf", paste0(input$resPath, "/Einzeldokumente/Fraktionspapier_", group,".pdf"))
        }
      }
      
      for (member in countries) {
        output_file <- paste0(input$resPath, "/Einzeldokumente/Länderpapier_", member, ".pdf")
        
        if (!file.exists(output_file)) {
          {sink("LaTeX/Meta/var.tex")
          paste0("\\newcommand\\kurzel{", member, "}\n") |> cat()
          sink()}
          
          tools::texi2pdf("LaTeX/Länderpapier.tex", clean = T)
          file.rename("Länderpapier.pdf", paste0(input$resPath, "/Einzeldokumente/Länderpapier_", member,".pdf"))
        }
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
      
      for (suffix in c("aux", "log", "out", "nav", "toc", "gz", "snm")) {
        move_temp_files("temp", suffix)
        move_temp_files(target_dir = "temp", file_ext = suffix, source_dir = "LaTeX")
        move_temp_files(target_dir = "temp", file_ext = suffix, source_dir = "LaTeX/Folien")
      }
      
      print("Unterlagen-Druck fertig!")

    } else if (input$docs == "TN-Zertifikate") {
      
      dir.create(file.path(input$resPath), showWarnings = F)
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
      
      for (suffix in c("aux", "log", "out", "nav", "toc", "gz", "snm")) {
        move_temp_files("temp", suffix)
        move_temp_files(target_dir = "temp", file_ext = suffix, source_dir = "LaTeX")
        move_temp_files(target_dir = "temp", file_ext = suffix, source_dir = "LaTeX/Folien")
      }
      
      print("Zertifikate-Druck fertig!")
      
    } else if (input$docs == "SuS-Verteilung") {
      resSuS <- get_sus_dist(input$numSuS, landDist = F)
      output$susVert <- renderTable({resSuS})
    }
  )
}
shinyApp(ui = ui, server = server)