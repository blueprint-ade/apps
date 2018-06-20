#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(sf)
library(tidyverse)

df_dots <- data.table::fread(here::here("census-map-data.csv") %>% 
  mutate(pop_colour = fct_rev(fct_infreq(lang)))

df_dots[df_dots == ""] <- NA_character_

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    style = "opacity: 0.8; z-index:9000",
    fixed = TRUE,
    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
    width = 330, height = "auto",
    checkboxInput("pop", "Show population statistics", value = FALSE),
    uiOutput("popSelector"),
    uiOutput("valSelector"),
    plotOutput("popBar"),
    downloadButton("downloadData", "Download underlying data"),
    sliderInput("sampleFrac", label = "Sample fraction of population", min = 0, max = 1, value = 0.05)
  )
  # conditionalPanel(
  #   condition = "input.providers", 
  #   fixed = TRUE, 
  #   draggable = TRUE, 
  #   checkboxGroupInput("which_providers", "Provider Networks", 
  #                      choices = unique(df_provdiders$network))
  # ),
)

server <- function(input, output, session) {
  
   # Reactive expression for the data subsetted to what the user selected
   popStats <- reactive({
     
     bounds <- input$map_bounds
     
     fd <- df_dots %>% 
       sample_frac(input$sampleFrac)
     
     if(!is.null(bounds)) {
       fd <- fd %>% filter(
         X >= bounds$west, X <= bounds$east, 
         Y >= bounds$south, Y <= bounds$north)
     }
     
     
     if(input$pop & !is.null(input$pop_colour)) {
       
       
       fd %>% 
         mutate(pop_colour = NULL) %>% 
         mutate(pop_colour = fct_rev(fct_infreq(fd[[input$pop_colour]]))) %>% 
         filter(pop_colour %in% input$val_selector)
       
     } else {
       
       fd %>% filter(FALSE)
       
     }
     
   })
   
   
   
   # This reactive expression represents the palette function,
   # which changes as the user makes selections in UI.
   pal <- reactive({
     
     levels <- popStats() %>% 
       filter(!is.na(pop_colour)) %>% 
       arrange(as.numeric(pop_colour)) %>% 
       pull(pop_colour)
     n <- length(unique(levels))
     colorFactor(
       RColorBrewer::brewer.pal(n, "Dark2"), 
       levels, 
       na.color = "grey")
     
   })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(df_dots) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(~min(X), ~min(Y), ~max(X), ~max(Y)) 
  })
  
  output$popBar <- renderPlot({
    
    p <- popStats() %>% 
      filter(!is.na(pop_colour)) %>% 
      ggplot(aes(pop_colour, fill = pop_colour)) + 
      geom_bar() + 
      coord_flip() +
      theme_minimal() + 
      labs(x = NULL, y = NULL) +
      scale_fill_manual(
        values = RColorBrewer::brewer.pal(n = length(unique(popStats()$pop_colour)), "Dark2"),
        guide = FALSE)
    
    p
    
  }, bg = "transparent", height = 250)
  
  output$popSelector <- renderUI({
    
    if(input$pop) {
      
      selectInput("pop_colour", "Colour clients by", 
                  c("Employment status" = "employed", 
                    "Marital status" = "marstat", 
                    "Languages" = "lang"), selected = "employed")
      
    }
    
  })
  
  output$valSelector <- renderUI({
    
    checkboxGroupInput("val_selector", "Show / hide groups", 
                         choices = unique(df_dots[[input$pop_colour]]),
                         selected = unique(popStats()$pop_colour))
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), "_", input$sampleFrac, "_", "census-data-dots.csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(popStats(), file, row.names = FALSE)
    }
  )
  
  # 
  # # 
  # # output$dlmDensity <- renderPlot({
  # #   
  # #   if(nrow(easClients()) == 0)
  # #     return(NULL)
  # #   
  # #   if(input$pop_colour == "dlmcat") {
  # #     
  # #     p <- easClients() %>% ggplot(aes(dlm100)) +
  # #       geom_histogram(aes(fill = dlmcat), alpha = 0.9) +
  # #       scale_x_continuous(limits = c(0,100), breaks = c(0, 100))
  # #     
  # #   } else {
  # #     
  # #     p <- easClients() %>% ggplot(aes_string(input$pop_colour)) +
  # #       geom_bar(aes_string(fill = input$pop_colour), alpha = 0.9) + 
  # #       coord_flip()
  # #     
  # #   }
  # #   
  # #   p + 
  # #     theme_minimal(base_family = "Roboto Condensed") +
  # #     labs(x = NULL, y = NULL) + 
  # #     theme(axis.text.y = element_blank()) + 
  # #     scale_fill_brewer(palette = "Dark2", guide = FALSE) 
  # #   
  # #   
  # # }, bg = "transparent")
  # 
  # 
  # # Incremental changes to the map (in this case, replacing the
  # # circles when a new color is chosen) should be performed in
  # # an observer. Each independent set of things that can change
  # # should be managed in its own observer.
  # 
  # 
  observe({
    pal <- pal()
    
    leafletProxy("map", data = popStats()) %>%
      clearShapes() %>%
      addCircles(lng = ~X, lat = ~Y, color = ~pal(pop_colour),
                 radius = 20, weight = 0.1, fillOpacity = 0.9)  #%>% 
      # addCircles(lng = df_providers$lng, lat = df_providers$lat,
      #            radius = 150, weight = 4, fillOpacity = 0.6,
      #            color = "red", 
      #            popup = df_providers$popup)
  })
    
   observe({
     proxy <- leafletProxy("map", data = df_dots)
     
     proxy %>% clearControls()
     pal <- pal()
     proxy %>% addLegend(
       position = "bottomright", 
       pal = pal, values = fct_rev(popStats()$pop_colour))
     
     
   })
  
  
  #### DEBUG
  
  observe({
    
    print(input$map_bounds)
    print(input$val_selector)
    
  })
  
  
  
}

shinyApp(ui, server)
