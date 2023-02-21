#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(tidyverse)
library(openxlsx)
library(readxl)
library(stringr)
library(janitor)
# library(XLConnect)
# library(gtools) #usikker på om denne gjer noko

# importer SB_prepare-funksjonar og variabelkoding
source("base/Studentsvar_prepare_tools.R", encoding = "UTF-8")
# importer utskriftskode
source("base/Studentsvar_report.R", encoding = "UTF-8")

# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Studentundersøking"),
  
  tabsetPanel(selected = "Fritekst",
              # Utskrift av gjennomsnitt, med samanlikning mellom to år
              # Mal etter Studiebarometerrapport
              tabPanel("Snitt over år",
                       sidebarLayout(
                         sidebarPanel(
                           textInput("instnr", "Institusjonsnummer DBH"),
                           textInput("surveyname", "Namn på undersøking"),
                           selectInput(
                             "levelfilter",
                             "Filtrer på nivå",
                             list("Bachelor", "Master", "Annet", "Alle"),
                             multiple = FALSE,
                             selectize = TRUE,
                             width = NULL,
                             size = NULL
                           ),
                           fileInput("malfil", "Malfil med overskrifter, spørsmålskategoriar og variabelnamn"),
                           fileInput("nyastedata", "Rådatafil nyaste undersøking"),
                           textInput("nyastear", "Årstall for nyaste undersøking"),
                           fileInput("forrigedata", "Rådatafil forrige undersøking"),
                           textInput("forrigear", "Årstall for forrige undersøking"),
                           
                           downloadButton("downloadData", "Lagre fil")      
                         ),
                         
                         mainPanel(
                           p("Konverterer rådata frå studentundersøking (t.d. Studiebarometeret) til 
               XLSX-filer med gjennomsnitt for spørsmål og samanlikning med eit tidlegare år.
               Ein treng filer med rådata frå to år, og ei XLSX-fil som inneheld overskrifter, 
               spørsmålstekstar og variabelnamn."),
                           htmlOutput("feedbacktext"),
                           textOutput("instnr"),
                           textOutput("malfil"),
                           textOutput("nyastedata"),
                           textOutput("forrigedata"),
                         ),
                       )
              ), # End snitt-panel
              
              # Utskrift av filer med fritekstsvar per institutt
              tabPanel("Fritekst",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput("fritekstfil", "Datafil med fritekstsvar"),
                           textInput("surveynamn", "Namn på undersøking"),
                           textInput("data_ar", "Årstall for undersøkinga"),
                           varSelectInput(
                             inputId = "programvariabel",
                             label = "Velg studieprogramkodevariabel for å hente data frå DBH",
                             data = NULL,
                             selected = NULL,
                             multiple = FALSE,
                             selectize = TRUE,
                             width = NULL,
                             size = NULL
                           ),
                           actionButton("getDBHdata", "Hent DBH-data"),
                           varSelectInput(
                             inputId = "grupperingsvariabel",
                             label = "Velg variabel å gruppere etter",
                             data = NULL,
                             selected = NULL,
                             multiple = FALSE,
                             selectize = TRUE,
                             width = NULL,
                             size = NULL
                           ),
                           varSelectInput(
                             inputId = "fritekstvariabler",
                             label = "Velg variablar å ta med",
                             data = NULL,
                             selected = NULL,
                             multiple = TRUE,
                             selectize = TRUE,
                             width = NULL,
                             size = NULL
                           ),
                           
                           # downloadButton("downloadOpenAnswers", "Lagre filer")      
                           actionButton("downloadOpenAnswers", "Lagre filer")      
                         ),
                         
                         mainPanel(
                           p("Deler opp filer med fritekst frå studentundersøking (t.d. Studiebarometeret) til 
               XLSX-filer for kvart institutt."),
                           # htmlOutput("feedbacktext"),
                           # textOutput("malfil"),
                           textOutput("fritekstfil"),
                           uiOutput("valgteVariabler")
                         ),
                       )
              ), # End fritekst-panel
  )
)

# Define server logic
server <- function(input, output) {
  
  # Lastar ned fil med snitt per år
  observeEvent(input$downloadData, {
    feedbacktext <- HTML(paste0("Brukar DBH-data for institusjonnummer ", input$instnr, "<br/>",
                                "Brukar malfila ", input$malfil$name, "<br/>",
                                "Brukar rådata frå ", input$nyastedata$name, " (", input$nyastear, ")",  "<br/>",
                                "Brukar rådata frå ", input$forrigedata$name, " (", input$forrigear, ")")
                         
    )
    output$feedbacktext <- renderText({feedbacktext})
  })
  
  # TODO: putt dbh_add_programdata i ein observeevent der ein sjekkar om det er 
  # valt ein variabel å fylle ut programdata på 
  # Oppdaterer selectbox for å velje variablar
  fritekstdata <- reactiveVal(NULL)
  
  observeEvent(input$fritekstfil, ignoreInit = T, {
    fritekstdata(read_excel(input$fritekstfil$datapath, col_types = "text"))
  })
  
  # observeEvent(input$fritekstfil, {
  # observeEvent(fritekstdata(), {
  observeEvent(fritekstdata(), {
    # fritekstdata <- read_excel(input$fritekstfil$datapath)
    updateVarSelectInput(inputId = "fritekstvariabler", data = fritekstdata())
    updateVarSelectInput(inputId = "programvariabel", data = fritekstdata())
    updateVarSelectInput(inputId = "grupperingsvariabel", data = fritekstdata())
  })
  
  observeEvent(input$getDBHdata, {
    # fritekstdata <- eventReactive(input$getDBHdata, {
    # print(fritekstdata() %>% names)
    # print(input$programvariabel)
    # fritekstdata_dbh <- 
    fritekstdata(fritekstdata() %>% dbh_add_programdata(., as.character(input$programvariabel), 1175))
    updateVarSelectInput(inputId = "fritekstvariabler", data = fritekstdata())
    updateVarSelectInput(inputId = "programvariabel", data = fritekstdata())
    updateVarSelectInput(inputId = "grupperingsvariabel", data = fritekstdata())
  })
  
  observeEvent(input$fritekstvariabler, {
    variablerTekst <- paste(input$fritekstvariabler, sep = "", collapse = "<br/>")
    output$valgteVariabler <- renderUI({HTML(variablerTekst)})
  })
  
  # TODO sjå til downloadData / downloadHandler lenger nede, 
  # slik at utskrift blir skilt frå å lage arbeidsbok
  observeEvent(input$downloadOpenAnswers, {
    # output$downloadOpenAnswers <- downloadHandler({
  
    sdf <- OM_fritekst_xlsx_2022(sdf = fritekstdata(),
                                 fritekstvariabler = input$fritekstvariabler,
                                 grupperingsvariabel = input$grupperingsvariabel,
                                 surveynamn = input$surveynamn,
                                 data_ar = input$data_ar)
    # print(sdf %>% head)
    
  })
  
  # Lagar arbeidsbokobjekt
  workbook <- reactive({
    if (is.null(input$nyastedata)) return()
    if (!is.null(input$malfil)) {
      templatepath <- input$malfil$datapath
    } else {
      templatepath <- "malfiler/Studiebarometeret_2023_vars.xlsx"
    }
    surveyname <- input$surveyname
    levelfilter <- input$levelfilter
    print(paste(surveyname, levelfilter, sep = " - " ))
    
    # TODO: legg inn case for å sjekke om ein skal bruke SA_prepare, SB_prepare eller OM_prepare
    df <- SB_prepare_2023(input$nyastedata$datapath, input$nyastear, input$instnr)
    print("Eventuelle programkoder utan Fakultetstilknytning: ") 
    print(df %>% filter(is.na(FAKNAVN)) %>% select(Studieprogramkode) %>% 
            unique)
    df_previous <- SB_prepare_2023(input$forrigedata$datapath, input$forrigear, input$instnr)
    
    workbook <- OM_print_2023(survey = surveyname, source_df = df, source_df_forrige = df_previous,
                              malfil = templatepath, nivå = levelfilter)
    
    # TODO les inn forklaringsark, legg til
    # print(typeof(workbook))
    return(workbook)
  })
  
  # Skriv ut arbeidsbok til fil
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$surveyname, "_", 
             input$levelfilter, "_",
             input$nyastear, "_",
             "rshiny",
             ".xlsx")
    },
    content = function(file) {
      saveWorkbook(workbook(), file = file, overwrite = TRUE)
      # write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # output$instnr <- renderText({paste0("Brukar DBH-data for institusjonnummer ", input$instnr, ".")})
  # output$malfil <- renderText({paste0("Brukar malfila ", input$malfil$name)})
  # output$nyastedata <- renderText({paste0("Brukar rådata frå ", input$nyastedata$name, ".")})
  # output$forrigedata <- renderText({paste0("Brukar rådata frå ", input$forrigedata$name, ".")})
  # output$nyastedata <- renderText({hentnyastear()})
  # output$forrigedata <- renderText({hentforrigear()})
  # output$datasett <- renderText({lagdatanyaste()})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
