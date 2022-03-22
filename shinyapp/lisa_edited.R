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
library(stringr)

#read in data ----

full_table <- read.csv("comments_and_demogs.csv")
#full_table$anything_else <- str_replace_all(full_table$anything_else, "[^[:alnum:]]", " ")
full_table$anything_else <- str_replace_all(full_table$anything_else, "(<|>)", "")

full_table <- full_table$anything_else

#find themes 
All <- full_table

Government <- full_table[(grepl("govern",full_table, ignore.case = TRUE, useBytes = TRUE))]

Trust <- full_table[(grepl("trust",full_table, ignore.case = TRUE, useBytes = TRUE))]

Science <- full_table[(grepl("scien",full_table, ignore.case = TRUE, useBytes = TRUE))]

Safe <- full_table[(grepl("safe|risk",full_table, ignore.case = TRUE, useBytes = TRUE))]

# Random_selection <- xxxxxx

themes <- c(Government, Trust, Science, Safe)
theme_labels <- c("All","Government","Trust","Science","Safe","Random Selection")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(title="See themes:"),
  sidebar = dashboardSidebar(
    checkboxGroupInput(inputId="chosen_themes",
                       label= NULL,
                       choices = theme_labels,
                       selected= NULL)
    
  ),
  body = dashboardBody(
    tableOutput(outputId = "comm_table")
    
  ),
  title="Vaccine Study Participant Comments",
  skin = "yellow"
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  display_table <- reactiveVal(full_table)
  
  #update visible columns in the display table ----
  observeEvent(input$chosen_themes, {
      themes %in% input$chosen_themes %>%
      display_table()
  })
  
  # comm_table ----
  output$comm_table<- renderTable({
    display_table()
    
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
