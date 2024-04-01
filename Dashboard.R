library(diffobj)
library(ggplot2)
library(pkgmaker)
library(shiny)
library(shinydashboard)
library(stringr)

collapse_diff <- function(str_list){
  for (i in seq_along(str_list)) {
    if (names(str_list)[i] == "Diff") {
      str_list[i] <- paste0("<b>", str_list[i], "</b>")
    }
  }
  str_list |> unlist() |> paste(collapse = "")
}


# UI -------------------------------------------------------------

header <- dashboardHeader(title = "Plenardebatte")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Eingabe", tabName = "input_tab"),
    menuItem("Ergebnis", tabName = "output_tab")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "input_tab",
      tabBox(
        title = "Eingabe",
        width = 12,
        tabPanel("EVP",
                 textInput("evp_vize", "Fraktionsvize:"),
                 textInput("evp_section", "Abschnitt:"),
                 textInput("evp_old", "alter Text:"),
                 textInput("evp_new", "neuer Text:")),
        tabPanel("S&D",
                 textInput("sd_vize", "Fraktionsvize:"),
                 textInput("sd_section", "Abschnitt:"),
                 textInput("sd_old", "alter Text:"),
                 textInput("sd_new", "neuer Text:")),
        tabPanel("Renew",
                 textInput("renew_vize", "Fraktionsvize:"),
                 textInput("renew_section", "Abschnitt:"),
                 textInput("renew_old", "alter Text:"),
                 textInput("renew_new", "neuer Text:")),
        tabPanel("Grüne",
                 textInput("green_vize", "Fraktionsvize:"),
                 textInput("green_section", "Abschnitt:"),
                 textInput("green_old", "alter Text:"),
                 textInput("green_new", "neuer Text:")),
        tabPanel("ID",
                 textInput("id_vize", "Fraktionsvize:"),
                 textInput("id_section", "Abschnitt:"),
                 textInput("id_old", "alter Text:"),
                 textInput("id_new", "neuer Text:"))
      ),
    ),
    
    tabItem(
      tabName = "output_tab",
      tabBox(
        title = "Abstimmungsergebnis",
        width = 12,
        tabPanel("EVP",
                 fluidRow(
                   box(
                     width = 3,
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "EP_Logo.png",
                         width = 150
                       )),
                     uiOutput("evp_logo")
                   ),
                   box(
                     width = 9,
                     align = "left",
                     tags$head(tags$style("#evp_vize{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #evp_section_print{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #evp_old_print{color: black;
                                 font-size: 20px;
                                 }
                                          #evp_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("evp_vize"),
                     uiOutput("evp_section_print"),
                     uiOutput("evp_old_print"),
                     uiOutput("evp_new_print")
                     )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#evp_res_print{color: black;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                     numericInput("evp_yes", "Ja:", value = NA),
                     numericInput("evp_no", "Nein:", value = NA),
                     numericInput("evp_abst", "Enthaltung:", value = NA),
                     textOutput("evp_res_print"),
                     uiOutput("evp_res_img")
                   ),
                   box(
                     width = 9,
                     plotOutput("evp_chart")
                   )
                 )
        ),
        
        
        
        tabPanel("S&D",
                 fluidRow(
                   box(
                     width = 3,
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "EP_Logo.png",
                         width = 150
                       )),
                     uiOutput("sd_logo")
                   ),
                   box(
                     width = 9,
                     align = "left",
                     tags$head(tags$style("#sd_vize{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #sd_section_print{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #sd_old_print{color: black;
                                 font-size: 20px;
                                 }
                                          #sd_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("sd_vize"),
                     uiOutput("sd_section_print"),
                     uiOutput("sd_old_print"),
                     uiOutput("sd_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#sd_res_print{color: black;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                     numericInput("sd_yes", "Ja:", value = NA),
                     numericInput("sd_no", "Nein:", value = NA),
                     numericInput("sd_abst", "Enthaltung:", value = NA),
                     textOutput("sd_res_print"),
                     uiOutput("sd_res_img")
                   ),
                   box(
                     width = 9,
                     plotOutput("sd_chart")
                   )
                 )
        ),
        
        tabPanel("Renew",
                 fluidRow(
                   box(
                     width = 3,
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "EP_Logo.png",
                         width = 150
                       )),
                     uiOutput("renew_logo")
                   ),
                   box(
                     width = 9,
                     align = "left",
                     tags$head(tags$style("#renew_vize{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #renew_section_print{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #renew_old_print{color: black;
                                 font-size: 20px;
                                 }
                                          #renew_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("renew_vize"),
                     uiOutput("renew_section_print"),
                     uiOutput("renew_old_print"),
                     uiOutput("renew_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#renew_res_print{color: black;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                     numericInput("renew_yes", "Ja:", value = NA),
                     numericInput("renew_no", "Nein:", value = NA),
                     numericInput("renew_abst", "Enthaltung:", value = NA),
                     textOutput("renew_res_print"),
                     uiOutput("renew_res_img")
                   ),
                   box(
                     width = 9,
                     plotOutput("renew_chart")
                   )
                 )
        ),
        
        tabPanel("Grüne",
                 fluidRow(
                   box(
                     width = 3,
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "EP_Logo.png",
                         width = 150
                       )),
                     uiOutput("green_logo")
                   ),
                   box(
                     width = 9,
                     align = "left",
                     tags$head(tags$style("#green_vize{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #green_section_print{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #green_old_print{color: black;
                                 font-size: 20px;
                                 }
                                          #green_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("green_vize"),
                     uiOutput("green_section_print"),
                     uiOutput("green_old_print"),
                     uiOutput("green_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#green_res_print{color: black;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                     numericInput("green_yes", "Ja:", value = NA),
                     numericInput("green_no", "Nein:", value = NA),
                     numericInput("green_abst", "Enthaltung:", value = NA),
                     textOutput("green_res_print"),
                     uiOutput("green_res_img")
                   ),
                   box(
                     width = 9,
                     plotOutput("green_chart")
                   )
                 )
        ),
        
        tabPanel("ID",
                 fluidRow(
                   box(
                     width = 3,
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "EP_Logo.png",
                         width = 150
                       )),
                     uiOutput("id_logo")
                   ),
                   box(
                     width = 9,
                     align = "left",
                     tags$head(tags$style("#id_vize{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #id_section_print{color: black;
                                 font-size: 24px;
                                 font-style: bold;
                                 }
                                          #id_old_print{color: black;
                                 font-size: 20px;
                                 }
                                          #id_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("id_vize"),
                     uiOutput("id_section_print"),
                     uiOutput("id_old_print"),
                     uiOutput("id_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#id_res_print{color: black;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                     numericInput("id_yes", "Ja:", value = NA),
                     numericInput("id_no", "Nein:", value = NA),
                     numericInput("id_abst", "Enthaltung:", value = NA),
                     textOutput("id_res_print"),
                     uiOutput("id_res_img")
                   ),
                   box(
                     width = 9,
                     plotOutput("id_chart")
             )
           )
        ),
        
        tabPanel("Abschlussabstimmung",
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#tot_res_print{color: black;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                     numericInput("tot_yes", "Ja:", value = NA),
                     numericInput("tot_no", "Nein:", value = NA),
                     numericInput("tot_abst", "Enthaltung:", value = NA)
                   ),
                   box(
                     width = 6,
                     tags$figure(
                       textOutput("tot_res_print"),
                       uiOutput("tot_res_img")  
                     )
                   ),
                   box(
                     width = 3,
                     tags$figure(
                       class = "centerFigure",
                       tags$img(
                         src = "EP_Logo.png",
                         height = 142
                       )))),
                 fluidRow(
                     plotOutput("tot_chart")
          )
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  data <- reactive({
    data.frame(
      cat = factor(c('Ja', 'Nein', 'Enthaltung'), levels = c('Ja', 'Nein', 'Enthaltung')),
      evp = c(input$evp_yes, input$evp_no, input$evp_abst),
      sd = c(input$sd_yes, input$sd_no, input$sd_abst),
      renew = c(input$renew_yes, input$renew_no, input$renew_abst),
      green = c(input$green_yes, input$green_no, input$green_abst),
      id = c(input$id_yes, input$id_no, input$id_abst),
      tot = c(input$tot_yes, input$tot_no, input$tot_abst)
    )
  })
  

  # EVP
  output$evp_chart <- renderPlot({
    df <- data()
    
    barchart <- ggplot(df, aes(x = cat, y = evp, fill = cat)) +
      geom_col() +
      scale_fill_manual(values = c('#00A86B', '#D32F2F', '#FFD600')) +
      theme(axis.text.x = element_text(size = 15)) +
      guides(fill = "none") +
      ylab("Anzahl") +
      xlab("") +
      geom_text(aes(label = evp), position = position_stack(vjust = 0.5), size = 10) +
      theme_void()
    
    print(barchart)
  })
  output$evp_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$evp_vize))
  })
  output$evp_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$evp_section))
  })
  output$evp_old_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", input$evp_old))
  })
  
  output$evp_new_print <- renderUI({
    
    evp_old <- input$evp_old
    evp_new <- input$evp_new
    
    evp_equal_string <- diffChr(evp_old, evp_new, format = "ansi8")@cur.dat$eq |> trimws()
    evp_eq_mat <- str_locate(evp_new, strsplit(evp_equal_string, " ")[[1]]) # get positions where strings are the same
    evp_new_split <- str_split(evp_new, "")[[1]]
    
    evp_diff_list <- list()
    
    if (evp_eq_mat[1,1]!=1) { # if first part is not equal
      # get string up to first equal part
      evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[1:(evp_eq_mat[1,1]-1)] |> paste(collapse = "")
    }
    for (i in 1:nrow(evp_eq_mat)) {
      # get first equal part
      evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[evp_eq_mat[i,1]:evp_eq_mat[i,2]] |> paste(collapse = "")
      if (i < nrow(evp_eq_mat)) {# if not last equal part
        # get different part (between two equal parts)  
        evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[(evp_eq_mat[i,2]+1):(evp_eq_mat[(i+1),1]-1)] |> paste(collapse = "")
      }else{
        if (evp_eq_mat[i,2]!=length(evp_new_split)) { #if last part is not equal
          # get last (different) part
          evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[(evp_eq_mat[i,2]+1):length(evp_new_split)] |> paste(collapse = "")
        }
      }
    }
    
    if (evp_eq_mat[1,1]!=1) { # String B fängt anders an
      for (i in seq_along(evp_diff_list)) {
        if (i%%2==1) { # erster Part anders
          names(evp_diff_list)[i] <- "Diff"
        }else{
          names(evp_diff_list)[i] <- "Same"
        }
      }
    }else{
      for (i in seq_along(evp_diff_list)) {
        if (i%%2==1) { # erster Part gleich
          names(evp_diff_list)[i] <- "Same"
        }else{
          names(evp_diff_list)[i] <- "Diff"
        }
      }
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", collapse_diff(evp_diff_list)))
  })
  
  output$evp_logo <- renderUI({tags$img(src = "EPP.png", width = 150, height = 100)})
  evp_res <- reactive({
    if (is.na(input$evp_yes) | is.na(input$evp_no) | is.na(input$evp_abst)) {
      print("")
    } else if (input$evp_yes > input$evp_no) {
      print("Der Änderungsantrag ist angenommen!")
    } else {
      print("Der Änderungsantrag ist abgelehnt!")
    }
  })
  output$evp_res_print <- renderText(evp_res())
  
  output$evp_res_img <- renderUI({
    if (evp_res() == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (evp_res() == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
  
  
  
  # S&D
  output$sd_chart <- renderPlot({
    df <- data()
    
    barchart <- ggplot(df, aes(x = cat, y = sd, fill = cat)) +
      geom_col() +
      scale_fill_manual(values = c('#00A86B', '#D32F2F', '#FFD600')) +
      theme(axis.text.x = element_text(size = 15)) +
      guides(fill = "none") +
      ylab("Anzahl") +
      xlab("") +
      geom_text(aes(label = sd), position = position_stack(vjust = 0.5), size = 10) +
      theme_void()
    
    print(barchart)
  })
  output$sd_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$sd_vize))
  })
  output$sd_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$sd_section))
  })
  output$sd_old_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", input$sd_old))
  })
  
  output$sd_new_print <- renderUI({
    
    sd_old <- input$sd_old
    sd_new <- input$sd_new
    
    sd_equal_string <- diffChr(sd_old, sd_new, format = "ansi8")@cur.dat$eq |> trimws()
    sd_eq_mat <- str_locate(sd_new, strsplit(sd_equal_string, " ")[[1]]) # get positions where strings are the same
    sd_new_split <- str_split(sd_new, "")[[1]]
    
    sd_diff_list <- list()
    
    if (sd_eq_mat[1,1]!=1) { # if first part is not equal
      # get string up to first equal part
      sd_diff_list[length(sd_diff_list)+1] <- sd_new_split[1:(sd_eq_mat[1,1]-1)] |> paste(collapse = "")
    }
    for (i in 1:nrow(sd_eq_mat)) {
      # get first equal part
      sd_diff_list[length(sd_diff_list)+1] <- sd_new_split[sd_eq_mat[i,1]:sd_eq_mat[i,2]] |> paste(collapse = "")
      if (i < nrow(sd_eq_mat)) {# if not last equal part
        # get different part (between two equal parts)  
        sd_diff_list[length(sd_diff_list)+1] <- sd_new_split[(sd_eq_mat[i,2]+1):(sd_eq_mat[(i+1),1]-1)] |> paste(collapse = "")
      }else{
        if (sd_eq_mat[i,2]!=length(sd_new_split)) { #if last part is not equal
          # get last (different) part
          sd_diff_list[length(sd_diff_list)+1] <- sd_new_split[(sd_eq_mat[i,2]+1):length(sd_new_split)] |> paste(collapse = "")
        }
      }
    }
    
    if (sd_eq_mat[1,1]!=1) { # String B fängt anders an
      for (i in seq_along(sd_diff_list)) {
        if (i%%2==1) { # erster Part anders
          names(sd_diff_list)[i] <- "Diff"
        }else{
          names(sd_diff_list)[i] <- "Same"
        }
      }
    }else{
      for (i in seq_along(sd_diff_list)) {
        if (i%%2==1) { # erster Part gleich
          names(sd_diff_list)[i] <- "Same"
        }else{
          names(sd_diff_list)[i] <- "Diff"
        }
      }
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", collapse_diff(sd_diff_list)))
  })
  
  output$sd_logo <- renderUI({tags$img(src = "S&D.png", width = 150, height = 100)})
  sd_res <- reactive({
    if (is.na(input$sd_yes) | is.na(input$sd_no) | is.na(input$sd_abst)) {
      print("")
    } else if (input$sd_yes > input$sd_no) {
      print("Der Änderungsantrag ist angenommen!")
    } else {
      print("Der Änderungsantrag ist abgelehnt!")
    }
  })
  output$sd_res_print <- renderText(sd_res())  
  
  output$sd_res_img <- renderUI({
    if (sd_res() == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (sd_res() == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
  
  
  
  # Renew
  output$renew_chart <- renderPlot({
    df <- data()
    
    barchart <- ggplot(df, aes(x = cat, y = renew, fill = cat)) +
      geom_col() +
      scale_fill_manual(values = c('#00A86B', '#D32F2F', '#FFD600')) +
      theme(axis.text.x = element_text(size = 15)) +
      guides(fill = "none") +
      ylab("Anzahl") +
      xlab("") +
      geom_text(aes(label = renew), position = position_stack(vjust = 0.5), size = 10) +
      theme_void()
    
    print(barchart)  
  })
  output$renew_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$renew_vize))
  })
  output$renew_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$renew_section))
  })
  output$renew_old_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", input$renew_old))
  })
  
  output$renew_new_print <- renderUI({
    
    renew_old <- input$renew_old
    renew_new <- input$renew_new
    
    renew_equal_string <- diffChr(renew_old, renew_new, format = "ansi8")@cur.dat$eq |> trimws()
    renew_eq_mat <- str_locate(renew_new, strsplit(renew_equal_string, " ")[[1]]) # get positions where strings are the same
    renew_new_split <- str_split(renew_new, "")[[1]]
    
    renew_diff_list <- list()
    
    if (renew_eq_mat[1,1]!=1) { # if first part is not equal
      # get string up to first equal part
      renew_diff_list[length(renew_diff_list)+1] <- renew_new_split[1:(renew_eq_mat[1,1]-1)] |> paste(collapse = "")
    }
    for (i in 1:nrow(renew_eq_mat)) {
      # get first equal part
      renew_diff_list[length(renew_diff_list)+1] <- renew_new_split[renew_eq_mat[i,1]:renew_eq_mat[i,2]] |> paste(collapse = "")
      if (i < nrow(renew_eq_mat)) {# if not last equal part
        # get different part (between two equal parts)  
        renew_diff_list[length(renew_diff_list)+1] <- renew_new_split[(renew_eq_mat[i,2]+1):(renew_eq_mat[(i+1),1]-1)] |> paste(collapse = "")
      }else{
        if (renew_eq_mat[i,2]!=length(renew_new_split)) { #if last part is not equal
          # get last (different) part
          renew_diff_list[length(renew_diff_list)+1] <- renew_new_split[(renew_eq_mat[i,2]+1):length(renew_new_split)] |> paste(collapse = "")
        }
      }
    }
    
    if (renew_eq_mat[1,1]!=1) { # String B fängt anders an
      for (i in seq_along(renew_diff_list)) {
        if (i%%2==1) { # erster Part anders
          names(renew_diff_list)[i] <- "Diff"
        }else{
          names(renew_diff_list)[i] <- "Same"
        }
      }
    }else{
      for (i in seq_along(renew_diff_list)) {
        if (i%%2==1) { # erster Part gleich
          names(renew_diff_list)[i] <- "Same"
        }else{
          names(renew_diff_list)[i] <- "Diff"
        }
      }
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", collapse_diff(renew_diff_list)))
  })
  
  output$renew_logo <- renderUI({tags$img(src = "Renew.png", width = 150, height = 100)})
  renew_res <- reactive({
    if (is.na(input$renew_yes) | is.na(input$renew_no) | is.na(input$renew_abst)) {
      print("")
    } else if (input$renew_yes > input$renew_no) {
      print("Der Änderungsantrag ist angenommen!")
    } else {
      print("Der Änderungsantrag ist abgelehnt!")
    }
  })
  output$renew_res_print <- renderText(renew_res())  
  
  output$renew_res_img <- renderUI({
    if (renew_res() == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (renew_res() == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
  
  
  
  # Greens
  output$green_chart <- renderPlot({
    df <- data()
    
    barchart <- ggplot(df, aes(x = cat, y = green, fill = cat)) +
      geom_col() +
      scale_fill_manual(values = c('#00A86B', '#D32F2F', '#FFD600')) +
      theme(axis.text.x = element_text(size = 15)) +
      guides(fill = "none") +
      ylab("Anzahl") +
      xlab("") +
      geom_text(aes(label = green), position = position_stack(vjust = 0.5), size = 10) +
      theme_void()
    
    print(barchart)
  })
  
  output$green_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$green_vize))
  })
  output$green_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$green_section))
  })
  output$green_old_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", input$green_old))
  })
  
  output$green_new_print <- renderUI({
    
    green_old <- input$green_old
    green_new <- input$green_new
    
    green_equal_string <- diffChr(green_old, green_new, format = "ansi8")@cur.dat$eq |> trimws()
    green_eq_mat <- str_locate(green_new, strsplit(green_equal_string, " ")[[1]]) # get positions where strings are the same
    green_new_split <- str_split(green_new, "")[[1]]
    
    green_diff_list <- list()
    
    if (green_eq_mat[1,1]!=1) { # if first part is not equal
      # get string up to first equal part
      green_diff_list[length(green_diff_list)+1] <- green_new_split[1:(green_eq_mat[1,1]-1)] |> paste(collapse = "")
    }
    for (i in 1:nrow(green_eq_mat)) {
      # get first equal part
      green_diff_list[length(green_diff_list)+1] <- green_new_split[green_eq_mat[i,1]:green_eq_mat[i,2]] |> paste(collapse = "")
      if (i < nrow(green_eq_mat)) {# if not last equal part
        # get different part (between two equal parts)  
        green_diff_list[length(green_diff_list)+1] <- green_new_split[(green_eq_mat[i,2]+1):(green_eq_mat[(i+1),1]-1)] |> paste(collapse = "")
      }else{
        if (green_eq_mat[i,2]!=length(green_new_split)) { #if last part is not equal
          # get last (different) part
          green_diff_list[length(green_diff_list)+1] <- green_new_split[(green_eq_mat[i,2]+1):length(green_new_split)] |> paste(collapse = "")
        }
      }
    }
    
    if (green_eq_mat[1,1]!=1) { # String B fängt anders an
      for (i in seq_along(green_diff_list)) {
        if (i%%2==1) { # erster Part anders
          names(green_diff_list)[i] <- "Diff"
        }else{
          names(green_diff_list)[i] <- "Same"
        }
      }
    }else{
      for (i in seq_along(green_diff_list)) {
        if (i%%2==1) { # erster Part gleich
          names(green_diff_list)[i] <- "Same"
        }else{
          names(green_diff_list)[i] <- "Diff"
        }
      }
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", collapse_diff(green_diff_list)))
  })
  
  output$green_logo <- renderUI({tags$img(src = "Greens.png", width = 150, height = 100)})
  green_res <- reactive({
    if (is.na(input$green_yes) | is.na(input$green_no) | is.na(input$green_abst)) {
      print("")
    } else if (input$green_yes > input$green_no) {
      print("Der Änderungsantrag ist angenommen!")
    } else {
      print("Der Änderungsantrag ist abgelehnt!")
    }
  })
  output$green_res_print <- renderText(green_res())  
  
  output$green_res_img <- renderUI({
    if (green_res() == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (green_res() == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
  
  
  
  # ID
  output$id_chart <- renderPlot({
    df <- data()
    
    barchart <- ggplot(df, aes(x = cat, y = id, fill = cat)) +
      geom_col() +
      scale_fill_manual(values = c('#00A86B', '#D32F2F', '#FFD600')) +
      theme(axis.text.x = element_text(size = 15)) +
      guides(fill = "none") +
      ylab("Anzahl") +
      xlab("") +
      geom_text(aes(label = id), position = position_stack(vjust = 0.5), size = 10) +
      theme_void()
    
    print(barchart)
  })
  output$id_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$id_vize))
  })
  output$id_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$id_section))
  })
  output$id_old_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", input$id_old))
  })
  
  output$id_new_print <- renderUI({
    
    id_old <- input$id_old
    id_new <- input$id_new
    
    id_equal_string <- diffChr(id_old, id_new, format = "ansi8")@cur.dat$eq |> trimws()
    id_eq_mat <- str_locate(id_new, strsplit(id_equal_string, " ")[[1]]) # get positions where strings are the same
    id_new_split <- str_split(id_new, "")[[1]]
    
    id_diff_list <- list()
    
    if (id_eq_mat[1,1]!=1) { # if first part is not equal
      # get string up to first equal part
      id_diff_list[length(id_diff_list)+1] <- id_new_split[1:(id_eq_mat[1,1]-1)] |> paste(collapse = "")
    }
    for (i in 1:nrow(id_eq_mat)) {
      # get first equal part
      id_diff_list[length(id_diff_list)+1] <- id_new_split[id_eq_mat[i,1]:id_eq_mat[i,2]] |> paste(collapse = "")
      if (i < nrow(id_eq_mat)) {# if not last equal part
        # get different part (between two equal parts)  
        id_diff_list[length(id_diff_list)+1] <- id_new_split[(id_eq_mat[i,2]+1):(id_eq_mat[(i+1),1]-1)] |> paste(collapse = "")
      }else{
        if (id_eq_mat[i,2]!=length(id_new_split)) { #if last part is not equal
          # get last (different) part
          id_diff_list[length(id_diff_list)+1] <- id_new_split[(id_eq_mat[i,2]+1):length(id_new_split)] |> paste(collapse = "")
        }
      }
    }
    
    if (id_eq_mat[1,1]!=1) { # String B fängt anders an
      for (i in seq_along(id_diff_list)) {
        if (i%%2==1) { # erster Part anders
          names(id_diff_list)[i] <- "Diff"
        }else{
          names(id_diff_list)[i] <- "Same"
        }
      }
    }else{
      for (i in seq_along(id_diff_list)) {
        if (i%%2==1) { # erster Part gleich
          names(id_diff_list)[i] <- "Same"
        }else{
          names(id_diff_list)[i] <- "Diff"
        }
      }
    }
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", collapse_diff(id_diff_list)))
  })
  
  output$id_logo <- renderUI({tags$img(src = "ID.png", width = 150, height = 100)})
  id_res <- reactive({
    if (is.na(input$id_yes) | is.na(input$id_no) | is.na(input$id_abst)) {
      print("")
    } else if (input$id_yes > input$id_no) {
      print("Der Änderungsantrag ist angenommen!")
    } else {
      print("Der Änderungsantrag ist abgelehnt!")
    }
  })
  output$id_res_print <- renderText(id_res())
  
  output$id_res_img <- renderUI({
    if (id_res() == "Der Änderungsantrag ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (id_res() == "Der Änderungsantrag ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
  
  
  # Abschlussabstimmung
  output$tot_chart <- renderPlot({
    df <- data()
    
    barchart <- ggplot(df, aes(x = cat, y = tot, fill = cat)) +
      geom_col() +
      scale_fill_manual(values = c('#00A86B', '#D32F2F', '#FFD600')) +
      theme(axis.text.x = element_text(size = 15)) +
      guides(fill = "none") +
      ylab("Anzahl") +
      xlab("") +
      geom_text(aes(label = tot), position = position_stack(vjust = 0.5), size = 10) +
      theme_void()
    
    print(barchart)
  })

  tot_res <- reactive({
    if (is.na(input$tot_yes) | is.na(input$tot_no) | is.na(input$tot_abst)) {
      print("")
    } else if (input$tot_yes > input$tot_no) {
      print("Der Gesetzesentwurf ist angenommen!")
    } else {
      print("Der Gesetzesentwurf ist abgelehnt!")
    }
  })
  output$tot_res_print <- renderText(tot_res())
  
  output$tot_res_img <- renderUI({
    if (tot_res() == "Der Gesetzesentwurf ist angenommen!") {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if (tot_res() == "Der Gesetzesentwurf ist abgelehnt!") {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
  
}



shinyApp(ui = ui, server = server)