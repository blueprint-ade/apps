#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rhandsontable)
library(shiny)
library(lubridate)
library(RPostgreSQL)
library(DBI)
library(dbx)
library(dplyr)


library(RPostgres)
drv <- DBI::dbDriver("PostgreSQL")

con <- dbxConnect(
   adapter = "postgres", dbname = "postgres",
   host = "tmcmanus.c2z3qgvuwrq7.ca-central-1.rds.amazonaws.com",
   port = "5432",
   user = "tmcmanus", 
   password = "tamer3(>closure")

df_tracker <- dbxSelect(con, "select * from game_events_demo") %>% 
   mutate(event = as.factor(event))

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
   titlePanel("Game events tracker"),
   sidebarLayout(
      sidebarPanel(
         helpText(
            "Shiny app designed to allow data collectors to",
            "add and update records in the database "),
        
         wellPanel(
            h3("Controls"),
            actionButton("addRow", "Add row"), br(),
            actionButton("upsert", "Upsert table"), br(), 
            fileInput("fileUpload", "Upload tracker spreadsheet",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")), br(),
            downloadButton("save", "Write CSV")
         )  
      ),
      mainPanel(
         tabsetPanel(id = "tabs",
            # In-game tracker
            tabPanel("In-game Tracker", value = "tracker",
               rHandsontableOutput("hot_tracker")),
            tabPanel("Spreadsheet upload", value = "upload",
               rHandsontableOutput("hot_upload"))
         )

      )
      
   )
))
  

  server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
    ## Handsontable
    observe({
      
      if (!is.null(input$hot_tracker)) {
        df_tracker <- hot_to_r(input$hot_tracker)
      } else {
        if (is.null(values[["df_tracker"]]))
          df_tracker <- df_tracker
        else
          df_tracker <- values$df_tracker
      }
      values$df_tracker <- df_tracker

      if (!is.null(input$hot_upload)) {
        df_upload = hot_to_r(input$hot_upload)
      } else {
        if (is.null(values$df_upload))
          df_upload <- df_tracker %>% filter(FALSE)
        else
          df_upload <- values$df_upload
      }
      values$df_upload <- df_upload
    })
    
    output$hot_tracker <- renderRHandsontable({
      
      if (!is.null(values$df_tracker)) {
         rhandsontable(values$df_tracker, 
            stretchH = "all")
      } else {

         rhandsontable(df_tracker, 
            useTypes = as.logical(input$useType), 
            stretchH = "all")

      }
    })

    output$hot_upload <- renderRHandsontable({
      if (!is.null(values$df_upload)) {
         rhandsontable(values$df_upload,
            strechH = "all")
      }
    })
    
    observeEvent(input$addRow, {
      values$df_tracker <- add_row(values$DF, 
         event_id = stringi::stri_rand_strings(1, 10), 
         date = Sys.Date(), 
         time = as.character(Sys.time()))
    })

    observeEvent(input$fileUpload, {
      df_upload <- read.csv(input$fileUpload$datapath)
      values$df_upload <- df_upload %>% 
        mutate(event_id = stringi::stri_rand_strings(nrow(df_upload), 10))
    })
    
    ## Save to disk
    output$save <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(values$df_tracker, file, row.names = FALSE)
      }
    )

    ## Upsert into database
    observeEvent(input$upsert, {

      active_tab <- input$tabs

      if(active_tab == "tracker") {

        values$df_tracker <- dbxUpsert(con, 
           "game_events_demo", 
           values$df_tracker, 
           where_cols = c("event_id"))

        showNotification("Upsert successful")
      } else {

        values$df_upload <- dbxUpsert(con,
          "game_events_demo", 
          values$df_upload,
          where_cols = c("event_id"))

      }
    })
    
  })
  
  
  # PART 4 - Run the application 
  shinyApp(ui = ui, server = server)