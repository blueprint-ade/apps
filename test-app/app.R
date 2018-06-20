


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


file_list <- list.files()
df_dots <- data.table::fread("census-map-data.csv")


ui <- bootstrapPage(
p(paste(file_list, collapse = "<br>"))

)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
