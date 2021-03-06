# Required libraries

library(shiny)
library(grid)
library(jpeg)
library(png)
library(readxl)
library(janitor)
library(scales)
library(tidyverse)

# Reading in saved shot data

shots <- read_rds(path = "shot_data")
  
# Ordering the factor levels for result so that "Goal" appears selected first

shots$result <- factor(x = shots$result, 
                           levels = c("Goal",
                                      "Miss",
                                      "Save"), 
                           ordered = TRUE)

# Image of one end of a lacrosse field being made adjustable so that I can use it as the 
# background for a ggplot graph.

field_Img <- "page_lacrosse_mens.png"
field <- rasterGrob(readPNG(field_Img),
                    width = unit(1, "npc"), 
                    height = unit(1, "npc"))

# Labels to be used in the 'Shot Clock Abidance' tab showing differences
# in shots based on those that satisfy the new NCAA 80-second shot clock rule
# vs. those shots that would have violated the new shot clock rule.

labels <- c(`T` = "Shots that satisfied the new 80-second shot clock rule", 
            `F` = "Shots that failed the new 80-second shot clock rule")

# Creating a list, 'shot_options', that contains options for the whole team, each
# position, and each player, so that the user can choose to look at shots from
# any single one of these options

players <-  unique(shots$player)
positions <- unique(shots$position)
all <- "Whole Team"
shot_options <- c(all, positions, players)

# Linking specific shot results with specific colors

MyPalette <- c(Goal = "#00FF00", Miss = "black", Save = "red")

# Define UI for application that draws plot

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
      
      # Tab panels for the Shiny App with specified Input bars
     
      mainPanel(
        tabsetPanel(
          tabPanel("About This App", htmlOutput("about")),
          tabPanel("All Shots", plotOutput("all_shots"), htmlOutput("stats")), 
          tabPanel("Heat Maps", plotOutput("heat"),
                   selectInput("result", "Shot Result:", 
                               choices = levels(factor(shots$result)),
                               selected = shots$result[0])
            ),
          tabPanel("Retrospective Shot Clock", plotOutput("clock"),
                   selectInput("clock_sat", "New 80-second Shot Clock:", 
                               choices = c("Satisfied", "Violated"),
                               selected = "Satisfied"))
      )
   )
)
)



# Define server logic 

server <- function(input, output) {
   
   output$heat <- renderPlot({
     
     # Finding if the user selected an individual player or position to look at
     # and then filtering if so.
     
     if (input$shooter != "Whole Team") {
       if (input$shooter %in% unique(shots$player)) {
         shots <- shots %>% 
           filter(player == input$shooter)
       } 
       else if (input$shooter %in% unique(shots$position)) {
         shots <- shots %>% 
           filter(position == input$shooter)
       }
     }
     
     # Plotting shots onto the lacrosse field, filtering for shot result (goal,
     # save, or miss), then creating a density heat map based on shot location was
     # quite tough (a lot of trial and error), but I finally figured it out. For
     # the heat density map, the center areas represent where the most shots of
     # that type are being taken.
     
     heat_map <- shots %>% 
       filter(result == input$result) %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Shot Density",
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
             axis.text.x = element_blank(),
             legend.position="none") +
       stat_density2d(aes(fill=..level..), geom="polygon", alpha = .13) + 
       scale_fill_distiller(palette = "Spectral")
     
     heat_map
   }, height = 410, width = 687.5, units = "px")
   
   output$all_shots <- renderPlot({
     
     # Finding if the user selected an individual player or position to look at
     # and then filtering if so.
     
     if (input$shooter != "Whole Team") {
       if (input$shooter %in% unique(shots$player)) {
         shots <- shots %>% 
           filter(player == input$shooter)
       } 
       else if (input$shooter %in% unique(shots$position)) {
         shots <- shots %>% 
           filter(position == input$shooter)
       }
     }
     
     # Finding shot percentage to be used in subtitle of plot
     
     sh_percent <- shots %>% 
       mutate(shot_n = n()) %>% 
       filter(result == "Goal") %>% 
       mutate(sh_per = n() / shot_n,
              sh_per = percent(sh_per)) %>% 
       select(sh_per) %>% 
       head(1) 
     
     my_subtitle <- paste("Shot Percentage: ", sh_percent$sh_per)
     
     # Plotting all shots onto the lacrosse field, with shot result mapped onto color 
     # (green for goal, black for miss, red for save). 
     
     shot_map <- shots %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result, size = 2, alpha = 0.7)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Shot Accuracy",
            x = "Offensive Area of Field", 
            color = "Shot Result",
            subtitle = my_subtitle) +
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
     
     # Does not show alpha or size in the legend
     
     shot_map + guides(alpha = FALSE, size = FALSE)
     
   }, height = 492, width = 825, units = "px")
   
   output$clock <- renderPlot({
     
     # Finding if the user selected an individual player or position to look at
     # and then filtering if so.
     
     if (input$shooter != "Whole Team") {
       if (input$shooter %in% unique(shots$player)) {
         shots <- shots %>% 
           filter(player == input$shooter)
       } 
       else if (input$shooter %in% unique(shots$position)) {
         shots <- shots %>% 
           filter(position == input$shooter)
       }
     }
     
     # Selecting which shots to show based on the user's input on whether to see
     # shots that satisfy or violate the new shot clock rule.
     
     if (input$clock_sat == "Satisfied") {
       shots <- shots %>% 
         filter(sh_clock == "T")
     }
     else if (input$clock_sat == "Violated") {
       shots <- shots %>% 
         filter(sh_clock == "F")
     }
     
     # Finding shot percentage to be used in subtitle of plot
     
     sh_percent <- shots %>% 
       mutate(shot_n = n()) %>% 
       filter(result == "Goal") %>% 
       mutate(sh_per = n() / shot_n,
              sh_per = percent(sh_per)) %>% 
       select(sh_per) %>% 
       head(1) 
     
     my_subtitle_2 <- paste("Shot Percentage: ", sh_percent$sh_per)
     
     # Plotting shots onto the lacrosse field based on whether the shot would
     # satisfy the new 80-second shot clock rule in lacrosse. I am looking at
     # this to see how much our offense will need to change going forward, as
     # well as to see if shots later in possessions (after the new shot clock
     # would have expired) have notably different results.
     
     clock_map <- shots %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result, size = 2, alpha = 0.7)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Shot Clock Abidance",
            x = "Offensive Area of Field", 
            color = "Shot Result",
            subtitle = my_subtitle_2) +
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
     
     # Does not show alpha or size in the legend
     
     clock_map + guides(alpha = FALSE, size = FALSE)
     
   }, height = 410, width = 687.5, units = "px")
   
   output$about <- renderUI({
     
     # Summary of application
     
     str1 <- paste("Goal")
     str2 <- paste("The Harvard Men's Lacrosse Team looks to identify and gain every advantage that 
                   it can possibly have over its opponents, and I am looking into the data behind 
                   where and when we shoot the ball to find a new edge for the upcoming season.")
     str3 <- paste("Summary")
     str4 <- paste("This application gives the user various ways to analyze the shooting performance
                   of the Harvard Men's Lacrosse Team in the 2018 season. The user has the ability to
                   choose whether or not to look at shots from the whole team, from specific position
                   groups, or from individual players. The user can then observe the specified shots 
                   on an actual lacrosse field where they occurred, with shooting percentage and shot
                   result provided. The user can also look at density heat maps for shots based on result.
                   Finally, this app also serves a forward-looking purpose for assessing how our offense
                   will fare under the new 80-second shot clock being introduced in 2019, as the third 
                   tab allows the user to toggle between shots that would satisy or violate the shot clock.")
     HTML(paste(h3(str1), p(str2), h3(str3), p(str4)))})  
}

# Run the application 
shinyApp(ui = ui, server = server)


