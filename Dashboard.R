library(diffobj)
library(ggparliament)
library(ggplot2)
library(grid)
library(pkgmaker)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

collapse_diff <- function(str_list){
  for (i in seq_along(str_list)) {
    if (names(str_list)[i] == "Diff") {
      str_list[i] <- paste0("<b>", str_list[i], "</b>")
    }
  }
  str_list |> unlist() |> paste(collapse = "")
}

plot_result_circle <- function(df, party){
  
  hsize <- 3
  df_plot <- df %>%
    mutate(x = hsize) |> 
    filter(cat == "Ja" | cat == "Nein")
  
  party_wo <- enquo(party) # needed for the ggplot syntax
  
  abst <- df %>% 
    filter(cat == "Enthaltung") %>% 
    pull(!!party_wo)
  
  main_plot <- ggplot(df_plot, aes(x = hsize, y = !!party_wo, fill = cat)) +
    geom_col() +
    coord_polar(theta = "y", start = pi, direction = -1) +
    xlim(c(0.2, hsize + 0.5)) +
    theme_void() +
    scale_fill_manual(values = c('#00A86B', '#D32F2F')) +
    labs(fill='') +
    theme(plot.title = element_text(hjust = 0.5, size = 16), legend.position = "none") +
    annotate("text", x = 0.3, y = 0, 
             label = paste("Enthaltungen: \n", abst), 
             hjust = 0.5, vjust = 0.5, size = 5)
  
  # Display the plot with the line
  grid.newpage()
  print(main_plot)
  grid.lines(x = c(0.5, 0.5), y = c(0.79, 0.89), gp = gpar(col = "black", lwd = 4, lty = "solid", alpha = 0.8))
  
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
                 textInput("id_new", "neuer Text:")),
        tabPanel("Abschlussabstimmung",
                 radioButtons("topic", "Thema:",
                              choices = c("Green Deal/Migration", "Armee"),
                              selected = "Green Deal/Migration")  
        )
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

ui <- dashboardPage(skin = "green", header, sidebar, body)



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
    plot_result_circle(df, evp)
  })
  output$evp_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$evp_vize))
  })
  output$evp_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$evp_section))
  })
  
  output$evp_old_print <- renderUI({
    diffOldNew_evp <- as.character(diffChr(input$evp_old, input$evp_new, pager="off"))[1]
    splitOldNew_evp <- strsplit(diffOldNew_evp, "")[[1]]
    splitMat_evp <- str_locate_all(diffOldNew_evp, "<span class='diffobj-trim'></span>")[[1]]

    oldBold_evp <- splitOldNew_evp[(splitMat_evp[1,2]+1):(splitMat_evp[2,1]-1)]
    oldBold_evp <- gsub("<span class='diffobj-word delete'>", "<b>", paste(oldBold_evp, collapse = ""))
    oldBold_evp <- gsub("</span>", "</b>", paste(oldBold_evp, collapse = ""))
    oldBold_evp
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", oldBold_evp))
  })
  
  output$evp_new_print <- renderUI({
    diffOldNew_evp <- as.character(diffChr(input$evp_old, input$evp_new, pager="off"))[1]
    splitOldNew_evp <- strsplit(diffOldNew_evp, "")[[1]]
    splitMat_evp <- str_locate_all(diffOldNew_evp, "<span class='diffobj-trim'></span>")[[1]]
    
    newBold_evp <- splitOldNew_evp[(splitMat_evp[3,2]+1):(splitMat_evp[4,1]-1)]
    newBold_evp <- gsub("<span class='diffobj-word insert'>", "<b>", paste(newBold_evp, collapse = ""))
    newBold_evp <- gsub("</span>", "</b>", paste(newBold_evp, collapse = ""))
    newBold_evp
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", newBold_evp))
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
    plot_result_circle(df, sd)
  })
  output$sd_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$sd_vize))
  })
  output$sd_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$sd_section))
  })
  
  output$sd_old_print <- renderUI({
    diffOldNew_sd <- as.character(diffChr(input$sd_old, input$sd_new, pager="off"))[1]
    splitOldNew_sd <- strsplit(diffOldNew_sd, "")[[1]]
    splitMat_sd <- str_locate_all(diffOldNew_sd, "<span class='diffobj-trim'></span>")[[1]]
    
    oldBold_sd <- splitOldNew_sd[(splitMat_sd[1,2]+1):(splitMat_sd[2,1]-1)]
    oldBold_sd <- gsub("<span class='diffobj-word delete'>", "<b>", paste(oldBold_sd, collapse = ""))
    oldBold_sd <- gsub("</span>", "</b>", paste(oldBold_sd, collapse = ""))
    oldBold_sd
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", oldBold_sd))
  })
  
  output$sd_new_print <- renderUI({
    diffOldNew_sd <- as.character(diffChr(input$sd_old, input$sd_new, pager="off"))[1]
    splitOldNew_sd <- strsplit(diffOldNew_sd, "")[[1]]
    splitMat_sd <- str_locate_all(diffOldNew_sd, "<span class='diffobj-trim'></span>")[[1]]
    
    newBold_sd <- splitOldNew_sd[(splitMat_sd[3,2]+1):(splitMat_sd[4,1]-1)]
    newBold_sd <- gsub("<span class='diffobj-word insert'>", "<b>", paste(newBold_sd, collapse = ""))
    newBold_sd <- gsub("</span>", "</b>", paste(newBold_sd, collapse = ""))
    newBold_sd
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", newBold_sd))
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
    plot_result_circle(df, renew) 
  })
  output$renew_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$renew_vize))
  })
  output$renew_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$renew_section))
  })
  
  output$renew_old_print <- renderUI({
    diffOldNew_renew <- as.character(diffChr(input$renew_old, input$renew_new, pager="off"))[1]
    splitOldNew_renew <- strsplit(diffOldNew_renew, "")[[1]]
    splitMat_renew <- str_locate_all(diffOldNew_renew, "<span class='diffobj-trim'></span>")[[1]]
    
    oldBold_renew <- splitOldNew_renew[(splitMat_renew[1,2]+1):(splitMat_renew[2,1]-1)]
    oldBold_renew <- gsub("<span class='diffobj-word delete'>", "<b>", paste(oldBold_renew, collapse = ""))
    oldBold_renew <- gsub("</span>", "</b>", paste(oldBold_renew, collapse = ""))
    oldBold_renew
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", oldBold_renew))
  })
  
  output$renew_new_print <- renderUI({
    diffOldNew_renew <- as.character(diffChr(input$renew_old, input$renew_new, pager="off"))[1]
    splitOldNew_renew <- strsplit(diffOldNew_renew, "")[[1]]
    splitMat_renew <- str_locate_all(diffOldNew_renew, "<span class='diffobj-trim'></span>")[[1]]
    
    newBold_renew <- splitOldNew_renew[(splitMat_renew[3,2]+1):(splitMat_renew[4,1]-1)]
    newBold_renew <- gsub("<span class='diffobj-word insert'>", "<b>", paste(newBold_renew, collapse = ""))
    newBold_renew <- gsub("</span>", "</b>", paste(newBold_renew, collapse = ""))
    newBold_renew
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", newBold_renew))
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
    plot_result_circle(df, green)
  })
  
  output$green_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$green_vize))
  })
  output$green_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$green_section))
  })
  
  output$green_old_print <- renderUI({
    diffOldNew_green <- as.character(diffChr(input$green_old, input$green_new, pager="off"))[1]
    splitOldNew_green <- strsplit(diffOldNew_green, "")[[1]]
    splitMat_green <- str_locate_all(diffOldNew_green, "<span class='diffobj-trim'></span>")[[1]]
    
    oldBold_green <- splitOldNew_green[(splitMat_green[1,2]+1):(splitMat_green[2,1]-1)]
    oldBold_green <- gsub("<span class='diffobj-word delete'>", "<b>", paste(oldBold_green, collapse = ""))
    oldBold_green <- gsub("</span>", "</b>", paste(oldBold_green, collapse = ""))
    oldBold_green
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", oldBold_green))
  })
  
  output$green_new_print <- renderUI({
    diffOldNew_green <- as.character(diffChr(input$green_old, input$green_new, pager="off"))[1]
    splitOldNew_green <- strsplit(diffOldNew_green, "")[[1]]
    splitMat_green <- str_locate_all(diffOldNew_green, "<span class='diffobj-trim'></span>")[[1]]
    
    newBold_green <- splitOldNew_green[(splitMat_green[3,2]+1):(splitMat_green[4,1]-1)]
    newBold_green <- gsub("<span class='diffobj-word insert'>", "<b>", paste(newBold_green, collapse = ""))
    newBold_green <- gsub("</span>", "</b>", paste(newBold_green, collapse = ""))
    newBold_green
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", newBold_green))
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
    plot_result_circle(df, id)
  })
  output$id_vize <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Fraktionsvize:</div> ", input$id_vize))
  })
  output$id_section_print <- renderUI({
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Abschnitt:</div> ", input$id_section))
  })
  
  output$id_old_print <- renderUI({
    diffOldNew_id <- as.character(diffChr(input$id_old, input$id_new, pager="off"))[1]
    splitOldNew_id <- strsplit(diffOldNew_id, "")[[1]]
    splitMat_id <- str_locate_all(diffOldNew_id, "<span class='diffobj-trim'></span>")[[1]]
    
    oldBold_id <- splitOldNew_id[(splitMat_id[1,2]+1):(splitMat_id[2,1]-1)]
    oldBold_id <- gsub("<span class='diffobj-word delete'>", "<b>", paste(oldBold_id, collapse = ""))
    oldBold_id <- gsub("</span>", "</b>", paste(oldBold_id, collapse = ""))
    oldBold_id
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Alter Text:</div> ", oldBold_id))
  })
  
  output$id_new_print <- renderUI({
    diffOldNew_id <- as.character(diffChr(input$id_old, input$id_new, pager="off"))[1]
    splitOldNew_id <- strsplit(diffOldNew_id, "")[[1]]
    splitMat_id <- str_locate_all(diffOldNew_id, "<span class='diffobj-trim'></span>")[[1]]
    
    newBold_id <- splitOldNew_id[(splitMat_id[3,2]+1):(splitMat_id[4,1]-1)]
    newBold_id <- gsub("<span class='diffobj-word insert'>", "<b>", paste(newBold_id, collapse = ""))
    newBold_id <- gsub("</span>", "</b>", paste(newBold_id, collapse = ""))
    newBold_id
    
    HTML(paste0("<div style='font-weight: bold; display: inline-block;'>Neuer Text:</div> ", newBold_id))
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
        if (input$topic == "Green Deal/Migration") {
          print("Der Gesetzesentwurf ist angenommen!")
        } else{
          print("Die Entschließung ist angenommen!")  
        }
    } else {
      if (input$topic == "Green Deal/Migration") {
        print("Der Gesetzesentwurf ist angenommen!")
      } else{
        print("Die Entschließung ist angenommen!")  
      }
    }
  })
  output$tot_res_print <- renderText(tot_res())
  
  output$tot_res_img <- renderUI({
    if ((tot_res() == "Die Entschließung ist angenommen!") | (tot_res() == "Der Gesetzesentwurf ist angenommen!")) {
      img(src = "angenommen.png", height = "100px", width = "100px")
    } else if ((tot_res() == "Die Entschließung ist abgelehnt!") | (tot_res() == "Der Gesetzesentwurf ist abgelehnt!")) {
      img(src = "abgelehnt.png", height = "100px", width = "100px")
    }
  })
  
}



shinyApp(ui = ui, server = server)