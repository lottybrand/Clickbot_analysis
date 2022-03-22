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

# this code runs once, when the app is launched
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

#load comments etc from file
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


#count 'em
n_items <- nrow(df)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Comments from vaccine hesitant participants after taking part in our study. They were asked: 'If you have any other feedback about this study, please include it here: "),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h5("Use the buttons to see comments including that theme"),
            actionButton("showGov", "Government"),
            actionButton("showAll", "All Comments")
        ),
        
        
        mainPanel(
            tableOutput("govComms"),
            tableOutput("allcomms")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Use an action button as an event to display the Gov comments
    govern_data <- eventReactive(input$showGov, {  
        
        #find all instances of govern
        Government
    
    })
    
    all_data <- eventReactive(input$showAll, {
      full_table
    })
    
    # Output the list of random numbers only AFTER the "Generate!" button is pressed
    output$govComms <- renderTable({
        Government
    }, rownames = FALSE, colnames = FALSE)
    
    output$allComms <- renderTable({
      full_table
    }, rownames = FALSE, colnames = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
