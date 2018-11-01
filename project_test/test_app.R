#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)

lowell_game <- read_rds(path = "saved_data")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Harvard Men's Lacrosse vs. UMass Lowell"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
        sidebarPanel(
          selectInput("name", "Player:", 
                      choices=lowell_game$name)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      lowell_game %>% 
      ggplot(aes(x = number, y = sh, fill = g)) + 
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Shots for each Harvard Player in their Game against UMass-Lowell", 
           subtile = "Color mapped onto Goals",
           x = "Player Number",
           y = "Shots",
           fill = "Goals")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

