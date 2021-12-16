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
    titlePanel("Proof of concept : free text comments from participants"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h5("Free text responses from participants, all self defining as against or unsure about vaccination. Click 'Generate' to see a random selection"),
            actionButton("do", "Generate!") # thanks https://www.rdataguy.com/2019/11/lesson-9-random-number-generator-part-2.html
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot")
            textOutput("text"),
            tableOutput("randNumbers")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Use an action button as an event to generate the list of random numbers
    random_data <- eventReactive(input$do, {  
        
        # Randomly sample values from the specified range
        numbers <- seq(1:n_items)
        sample(numbers, 5)
        
    })
    
    # Output the list of random numbers only AFTER the "Generate!" button is pressed
    output$randNumbers <- renderTable({
        df$anything_else[random_data()]
    }, rownames = FALSE, colnames = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
