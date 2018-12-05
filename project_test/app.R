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

# reads in saved statistical data
brown_app <- read_rds(path = "brown")

# Image of the restraining (offensive) area of a lacrosse field being made adjustable so that
# I can use it as the background for a ggplot graph.

field_Img <- "page_lacrosse_mens.png"
field <- rasterGrob(readPNG(field_Img),
                    width = unit(1, "npc"), height = unit(1, "npc"))

# Labels to be used for facet_wrap() in the upcoming graph
labels <- c(`T` = "Shots that satisfied the new 60-second shot clock rule", 
            `F` = "Shots that failed the new 60-second shot clock rule")

# Creating a list, shot_options, that contains options for the whole team, each position, 
# and each player, so that the user can choose to look at shots from each of these options

players <-  unique(brown_game$player)
positions <- unique(brown_game$position)
all <- "Whole Team"
shot_options <- c(all, positions, players)

# Linking specific shot results with specific colors

MyPalette <- c(Goal = "#00FF00", Miss = "black", Save = "red")

# Define UI for application that draws bar graph
ui <- fluidPage(
   
   # Application title
   titlePanel("Harvard Men's Lacrosse 2018"),
   
   # Sidebar to select whole team, position, or specific players 
   
   sidebarLayout(
     sidebarPanel(
       selectInput(inputId = "shooter",
                   label = "Select Whole Team, Position Group, or Individual Player",
                   choices = shot_options,
                   selected = shot_options[0]), width = 3),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("All Shots", plotOutput("all_shots")), 
          tabPanel("Heat Maps", plotOutput("heat"),
          # Sidebar with a drop-down menu to select a shot result
                         selectInput("result", "Shot Result:", 
                                     choices=brown_app$result)
            ),
          tabPanel("Table", tableOutput("table"))
      )
   )
)
)



# Define server logic 

server <- function(input, output) {
   
   output$heat <- renderPlot({
     
     # Plotting shots onto the lacrosse field, filtering for shot result (goal, save miss), 
     # then creating a density heat map based on shot location was quite tough 
     # (a lot of trial and error), but I finally figured it out. For the heat density map, 
     # the center, red area represents where the most shots of that type are being taken.
     
     heat_map <- brown_app %>% 
       filter(result == input$result) %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Harvard Men's Lacrosse Shot Density vs. Brown",
            x = "Offensive Area of Field", 
            color = "Shot Result") +
       geom_point(stat = "identity", position = "dodge") +
       xlim(-250, 250) +
       ylim(-50, 420) +
       #scale_color_manual(labels = c("Goal", "Miss", "Save"), 
       #                   values = c("#00FF00", "black", "red")) +
       # Hiding numbers on the axes
       theme(axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_blank()) +
       stat_density2d(aes(fill=..level..), geom="polygon", alpha = .1) + 
       scale_fill_distiller(palette = "Spectral")
     
     heat_map
   }, height = 400, width = 650, units = "px")
   
   output$all_shots <- renderPlot({
     
     # Plotting all shots onto the lacrosse field, with shot result mapped onto color 
     # (green for goal, black for miss, red for save). 
     
     if (input$shooter != "Whole Team") {
       if (input$shooter %in% unique(brown_app$player)) {
         brown_app <- brown_app %>% 
           filter(player == input$shooter)
       } 
       else if (input$shooter %in% unique(brown_app$position)) {
         brown_app <- brown_app %>% 
           filter(position == input$shooter)
       }
     }
     
     shot_map <- brown_app %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Harvard Men's Lacrosse Shot Accuracy vs. Brown",
            x = "Offensive Area of Field", 
            color = "Shot Result") +
       geom_point(stat = "identity", position = "dodge") +
       xlim(-250, 250) +
       ylim(-50, 420) +
       scale_colour_manual(values = MyPalette) +
     # Hiding numbers on the axes
     theme(axis.title.y = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           axis.ticks.x = element_blank(),
           axis.text.x = element_blank()) 
     
     shot_map
   }, height = 500, width = 700, units = "px")
}

# Run the application 
shinyApp(ui = ui, server = server)


