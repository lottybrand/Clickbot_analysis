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
#library(shinyWidgets)

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
  header = dashboardHeader(titleWidth = "calc(100% - 44px)",title="Comments from vaccine hesitant participants after taking part in our study"),
  # Sidebar with loadsa buttons:
  sidebar = dashboardSidebar(
      p("Use these buttons to see comments including the following words, or to see all comments:", style="padding:1em;"),
      actionButton("findGov", "Government"),
      actionButton("findMedia", "Media"),
      actionButton("findTrust", "Trust"),
      actionButton("findPharma", "Big Pharma"),
      actionButton("findSci", "Science"),
      actionButton("findRes","Research"),
      actionButton("findTime","Time"),
      actionButton("findHealth", "Health"),
      actionButton("findSafe", "Safety"),
      actionButton("findThank", "Thanks"),
      actionButton("findAll", "See All Comments")
    ),
    
    # what's in the body of the app?
    body = dashboardBody(
      #plotOutput("you could put plots here"),
      #textOutput("you could have text output"),
      DTOutput("comms_table")
    ),
  skin="yellow"
  )


# Define server logic below. This does the stuff behind the scenes, connecting the user (ui) to the content
server <- function(input, output, session) {
  #create a display table made up of the comments column, that can react to events
  display_table <- reactiveVal(full_table%>%
                                 select(Comments))
  
  # function for filtering the table for certain words
  filter_table = function (word) {
    full_table %>% 
      filter(grepl(word, Comments, ignore.case = TRUE, useBytes = TRUE)) %>% 
      select(Comments) %>%
      display_table()
  }
  
  # when findGov button is pressed - do the following:
  observeEvent(input$findGov, {  
    # find all instances of governm and pass to display table
    filter_table("govern")
  })
  
  observeEvent(input$findHealth, {  
    # find all instances of health and pass to display table
    filter_table("health")
  })
  
  observeEvent(input$findTrust, {  
    # find all instances of trust and pass to display table
    filter_table("trust")
  })
  
  observeEvent(input$findRes, {  
    # find all instances of research and pass to display table
    filter_table("research")
  })
  
  observeEvent(input$findMedia, {  
    # find all instances of media and pass to display table
    filter_table("media")
  })
  
  observeEvent(input$findTime, {  
    # find all instances of time and pass to display table
    filter_table("time")
  })
  
  observeEvent(input$findSafe, {  
    # find all instances of safe/ty and pass to display table
    filter_table("safe")
  })
  
  observeEvent(input$findSci, {  
    # find all instances of science/tists and pass to display table
    filter_table("scien")
  })
  
  observeEvent(input$findPharma, {  
    # find all instances of phara and pass to display table
    filter_table("pharma")
  })
  
  observeEvent(input$findThank, {  
    # find all instances of trust and pass to display table
    filter_table ("thank")
  })
  
  observeEvent(input$findAll, {  
    # pass full column to display table
    full_table %>%
      select(Comments) %>%
      display_table()
  })
  
  # The output is the comms table which is made up of the latest display_table
  output$comms_table <- renderDT({
    display_table()
  
  }, 
  rownames=FALSE,
  options= list(paging=FALSE,searching=TRUE,info=TRUE,ordering=FALSE)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

