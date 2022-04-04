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
library(tidyverse)
library(dplyr)
library(tidyr)
library(DT)

# this code runs once, when the app is launched
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

#load comments etc from file
full_table <- read.csv('cut_comments_and_demogs.csv')
full_table$Comments <- str_replace_all(full_table$Comments, "(<|>)", "")
#full_table <- full_table$anything_else
#count 'em
# n_items <- nrow(full_table)
# random_sample <- function(n_items){
#   numbers <- seq(1:n_items)
#   return(sample(numbers, 5))
#   
# } 

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(title="Participant Comments"),
  
  # Sidebar with a slider input for number of bins 
  sidebar = dashboardSidebar(
      h5("Use these buttons to see comments including the following words, or to see all comments:"),
      actionButton("findGov", "Government"),
      actionButton("findTrust", "Trust"),
      actionButton("findPharma", "Big Pharma"),
      actionButton("findSci", "Science"),
      actionButton("findHealth", "Health"),
      actionButton("findSafe", "Safety"),
      actionButton("findAll", "See All Comments")
      # thanks https://www.rdataguy.com/2019/11/lesson-9-random-number-generator-part-2.html
    ),
    
    # Show a plot of the generated distribution
    body = dashboardBody(
      #plotOutput("distPlot")
      #textOutput("text"),
      tableOutput("book_table")
    ),
  title="Comments from vaccine hesitant participants after taking part in our study",
  skin="yellow"
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  display_table <- reactiveVal(full_table$Comments)
  # Use an action button as an event to generate the list of random numbers
  observeEvent(input$findGov, {  
    # Randomly sample values from the specified range
    full_table$Comments[(grepl("govern",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findHealth, {  
    # Randomly sample values from the specified range
    full_table$Comments[(grepl("health",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findTrust, {  
    # Randomly sample values from the specified range
    full_table$Comments[(grepl("trust",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findSafe, {  
    # Randomly sample values from the specified range
    full_table$Comments[(grepl("safe",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findSci, {  
    # Randomly sample values from the specified range
    full_table$Comments[(grepl("scien",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findPharma, {  
    # Randomly sample values from the specified range
    full_table$Comments[(grepl("pharma",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findAll, {  
    # Randomly sample values from the specified range
    full_table$Comments %>%
      display_table()
  })
  
  # Output the list of random numbers only AFTER the "Generate!" button is pressed
  output$book_table <- renderTable({
    display_table()
  
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)