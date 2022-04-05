#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This was useful https://shiny.rstudio.com/articles/debugging.html

# none of the below would be possible without Lisa DeBruine's coding club: https://psyteachr.github.io/mms/coding_club.html 

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
# attempt to sort out the funny characters
full_table$Comments <- str_replace_all(full_table$Comments, "(<|>)", "")
#full_table <- full_table$Comments
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
  
  # Sidebar with loadsa buttons:
  sidebar = dashboardSidebar(
      h5("Use these buttons to see comments including the following words, or to see all comments:"),
      actionButton("findGov", "Government"),
      actionButton("findMedia", "Media"),
      actionButton("findTrust", "Trust"),
      actionButton("findPharma", "Big Pharma"),
      actionButton("findSci", "Science"),
      actionButton("findRes","Research"),
      actionButton("findTime","Time"),
      actionButton("findHealth", "Health"),
      actionButton("findSafe", "Safety"),
      actionButton("findAll", "See All Comments")
      # thanks https://www.rdataguy.com/2019/11/lesson-9-random-number-generator-part-2.html
    ),
    
    # what's in the body of the app?
    body = dashboardBody(
      #plotOutput("you could put plots here"),
      #textOutput("you could have text output"),
      tableOutput("comms_table")
    ),
  title="Comments from vaccine hesitant participants after taking part in our study",
  skin="yellow"
  )


# Define server logic below. This does the stuff behind the scenes, connecting the user (ui) to the content
server <- function(input, output, session) {
  #create a display table made up of the comments column, that can react to events
  display_table <- reactiveVal(full_table$Comments)
  
  # when findGov button is pressed - do the following:
  observeEvent(input$findGov, {  
    # find all instances of governm and pass to display table
    full_table$Comments[(grepl("govern",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findHealth, {  
    # find all instances of health and pass to display table
    full_table$Comments[(grepl("health",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findTrust, {  
    # find all instances of trust and pass to display table
    full_table$Comments[(grepl("trust",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findRes, {  
    # find all instances of research and pass to display table
    full_table$Comments[(grepl("research",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findMedia, {  
    # find all instances of media and pass to display table
    full_table$Comments[(grepl("media",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findTime, {  
    # find all instances of time and pass to display table
    full_table$Comments[(grepl("time",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findSafe, {  
    # find all instances of safe/ty and pass to display table
    full_table$Comments[(grepl("safe",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findSci, {  
    # find all instances of science/tists and pass to display table
    full_table$Comments[(grepl("scien",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findPharma, {  
    # find all instances of phara and pass to display table
    full_table$Comments[(grepl("pharma",full_table$Comments, ignore.case = TRUE, useBytes = TRUE))] %>%
      display_table()
  })
  
  observeEvent(input$findAll, {  
    # pass full column to display table
    full_table$Comments %>%
      display_table()
  })
  
  # The output is the comms table which is made up of the latest display_table
  output$comms_table <- renderTable({
    display_table()
  
  }, 
  colnames = TRUE
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)