#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This was useful https://shiny.rstudio.com/articles/debugging.html

library(shiny)
library(shinydashboard)
library(googlesheets4)
library(tidyverse)
library(dplyr)
library(tidyr)

#read in data ----
url <- "1KzLoJSakrpr3nY2KiFMOd9jn1DEwKKNbw2QibnW_aXg"
gs4_deauth()
full_table <-read_sheet(url, col_types = "c") %>%
    rename("Genre"=4, "Location(s)"=5)


genres <- c("Scifi","Fantasy","Romance","Biography","Horror/Thriller","Crime","Comedy","Non-fiction","Poetry","Education","Graphic Novel","Fiction","Classics")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(title="Book recs"),
  sidebar = dashboardSidebar(
    checkboxGroupInput(inputId = "genre",
                        label = "Genre",
                        choices = genres,
                        selected = genres)
  ),
  body = dashboardBody(
    checkboxGroupInput(inputId="visible_cols",
                       label=NULL,
                       choices = colnames(full_table),
                       selected=colnames(full_table)),
    tableOutput(outputId = "book_table")
  ),
  title="Book recs",
  skin = "yellow"
)
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  display_table <- reactiveVal(full_table)

  #update visible columns in the display table ----
  observeEvent(input$visible_cols, {
    full_table %>%
      select(input$visible_cols) %>%
      display_table()
  })
  
  # book_table ----
  output$book_table<- renderTable({
    display_table()
    
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
