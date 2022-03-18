# trying edits 

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

# this code runs once, when the app is launched
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

#load comments etc from file
df <- read.csv('comments_and_demogs.csv')

#count 'em
n_items <- nrow(df)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("free text responses from vaccine hesitant participants after taking part in our study. They were asked: 'If you have any other feedback about this study, please include it here: ' "),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h5("Free text responses from participants, all reporting as either 'against' or 'neutral' towards Covid-19 vaccines as reported in Prolific pre-screening. The buttons below display all comments containing that word. Or you can view all comments"),
      actionButton("government", "Government"), # thanks https://www.rdataguy.com/2019/11/lesson-9-random-number-generator-part-2.html
      actionButton("trust", "Trust"),
      actionButton("all", "All")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("government"),
      tableOutput("trust"),
      tableOutput("all")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Use an action button as an event to generate the list of random numbers
  find_gov <- eventReactive(input$government, {  
    
    # Randomly sample values from the specified range
    
  })
  
  # Use an action button as an event to generate the list of random numbers
  find_trust <- eventReactive(input$trust, {  
    
    # Randomly sample values from the specified range
    
    
  })
  
  # Use an action button as an event to generate the list of random numbers
  all_data <- eventReactive(input$all, {  
    
    # Randomly sample values from the specified range
    numbers <- seq(1:n_items)
    sample(numbers, 2)
    
  })
  
  # Output the list of random numbers only AFTER the "Generate!" button is pressed
  output$findgov <- renderTable({
    df[(grepl("govern",df$anything_else, ignore.case = TRUE)),]
  }, rownames = FALSE, colnames = FALSE)
  
  output$findtrust <- renderTable({
    df[(grepl("trust",df$anything_else, ignore.case = TRUE)),]
  }, rownames = FALSE, colnames = FALSE)
  
  output$all_data <- renderTable({
    df$anything_else
  }, rownames = FALSE, colnames = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
