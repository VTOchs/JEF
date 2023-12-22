library(shiny)
library(shinydashboard)
library(ggplot2)

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
                 # textInput("evp_old", "alter Text:"),
                 textInput("evp_new", "Änderungsantrag:")),
        tabPanel("S&D",
                 textInput("sd_vize", "Fraktionsvize:"),
                 textInput("sd_section", "Abschnitt:"),
                 # textInput("sd_old", "alter Text:"),
                 textInput("sd_new", "Änderungsantrag:")),
        tabPanel("Renew",
                 textInput("renew_vize", "Fraktionsvize:"),
                 textInput("renew_section", "Abschnitt:"),
                 # textInput("renew_old", "alter Text:"),
                 textInput("renew_new", "Änderungsantrag:")),
        tabPanel("Grüne",
                 textInput("green_vize", "Fraktionsvize:"),
                 textInput("green_section", "Abschnitt:"),
                 # textInput("green_old", "alter Text:"),
                 textInput("green_new", "Änderungsantrag:")),
        tabPanel("ID",
                 textInput("id_vize", "Fraktionsvize:"),
                 textInput("id_section", "Abschnitt:"),
                 # textInput("id_old", "alter Text:"),
                 textInput("id_new", "Änderungsantrag:"))
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
                                          #evp_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("evp_vize"),
                     uiOutput("evp_section_print"),
                     textOutput("evp_new_print")
                     )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#evp_res_print{color: black;
                                 font-size: 40px;
                                 font-style: bold;
                                 }")),
                     numericInput("evp_yes", "Ja:", value = NA),
                     numericInput("evp_no", "Nein:", value = NA),
                     numericInput("evp_abst", "Enthaltung:", value = NA),
                     textOutput("evp_res_print")
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
                                          #sd_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("sd_vize"),
                     uiOutput("sd_section_print"),
                     textOutput("sd_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#sd_res_print{color: black;
                                 font-size: 40px;
                                 font-style: bold;
                                 }")),
                     numericInput("sd_yes", "Ja:", value = NA),
                     numericInput("sd_no", "Nein:", value = NA),
                     numericInput("sd_abst", "Enthaltung:", value = NA),
                     textOutput("sd_res_print")
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
                                          #renew_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("renew_vize"),
                     uiOutput("renew_section_print"),
                     textOutput("renew_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#renew_res_print{color: black;
                                 font-size: 40px;
                                 font-style: bold;
                                 }")),
                     numericInput("renew_yes", "Ja:", value = NA),
                     numericInput("renew_no", "Nein:", value = NA),
                     numericInput("renew_abst", "Enthaltung:", value = NA),
                     textOutput("renew_res_print")
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
                                          #green_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("green_vize"),
                     uiOutput("green_section_print"),
                     textOutput("green_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#green_res_print{color: black;
                                 font-size: 40px;
                                 font-style: bold;
                                 }")),
                     numericInput("green_yes", "Ja:", value = NA),
                     numericInput("green_no", "Nein:", value = NA),
                     numericInput("green_abst", "Enthaltung:", value = NA),
                     textOutput("green_res_print")
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
                                          #id_new_print{color: black;
                                 font-size: 20px;
                                 }")),
                     uiOutput("id_vize"),
                     uiOutput("id_section_print"),
                     textOutput("id_new_print")
                   )),
                 fluidRow(
                   box(
                     width = 3,
                     tags$head(tags$style("#id_res_print{color: black;
                                 font-size: 40px;
                                 font-style: bold;
                                 }")),
                     numericInput("id_yes", "Ja:", value = NA),
                     numericInput("id_no", "Nein:", value = NA),
                     numericInput("id_abst", "Enthaltung:", value = NA),
                     textOutput("id_res_print")
                   ),
                   box(
                     width = 9,
                     plotOutput("id_chart")
                   )
                 )
        )
      )
    )
  ))

ui <- dashboardPage(header, sidebar, body)




server <- function(input, output, session) {
  
  data <- reactive({
    data.frame(
      cat = factor(c('Ja', 'Nein', 'Enthaltung'), levels = c('Ja', 'Nein', 'Enthaltung')),
      evp = c(input$evp_yes, input$evp_no, input$evp_abst),
      sd = c(input$sd_yes, input$sd_no, input$sd_abst),
      renew = c(input$renew_yes, input$renew_no, input$renew_abst),
      green = c(input$green_yes, input$green_no, input$green_abst),
      id = c(input$id_yes, input$id_no, input$id_abst)
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
  output$evp_new_print <- renderText(input$evp_new)
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
  output$sd_new_print <- renderText(input$sd_new)
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
  output$renew_new_print <- renderText(input$renew_new)
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
  output$green_new_print <- renderText(input$green_new)
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
  output$id_new_print <- renderText(input$id_new)
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
}


shinyApp(ui = ui, server = server)