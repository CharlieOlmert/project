#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(grid)
library(jpeg)
library(png)
library(readxl)
library(janitor)
library(scales)
library(tidyverse)

# Reading in saved shot data

brown_app <- read_rds(path = "brown")
  
# Ordering the factor levels for result so that "Goal" appears selected first

brown_app$result <- factor(x = brown_app$result, 
                           levels = c("Goal",
                                        "Miss",
                                        "Save"), 
                           ordered = TRUE)

# Image of the restraining (offensive) area of a lacrosse field being made
# adjustable so that I can use it as the background for a ggplot graph.

field_Img <- "page_lacrosse_mens.png"
field <- rasterGrob(readPNG(field_Img),
                    width = unit(1, "npc"), 
                    height = unit(1, "npc"))

# Labels to be used for facet_wrap() in the upcoming graph showing differences
# in shots based on those that satisfy the new NCAA 80-second shot clock rule
# vs. those shots that would have violated the new shot clock rule

labels <- c(`T` = "Shots that satisfied the new 80-second shot clock rule", 
            `F` = "Shots that failed the new 80-second shot clock rule")

# Creating a list, shot_options, that contains options for the whole team, each
# position, and each player, so that the user can choose to look at shots from
# each of these options

players <-  unique(brown_app$player)
positions <- unique(brown_app$position)
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
      
      # Tab panels for the Shiny App. The Heat Maps tab allows the user to
      # select a shot result (Goal, Miss, or Save) to inspect.
     
      mainPanel(
        tabsetPanel(
          tabPanel("About this app", htmlOutput("about")),
          tabPanel("All Shots", plotOutput("all_shots"), htmlOutput("stats")), 
          tabPanel("Heat Maps", plotOutput("heat"),
                   selectInput("result", "Shot Result:", 
                               choices = levels(factor(brown_app$result)),
                               selected = brown_app$result[0])
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
       if (input$shooter %in% unique(brown_app$player)) {
         brown_app <- brown_app %>% 
           filter(player == input$shooter)
       } 
       else if (input$shooter %in% unique(brown_app$position)) {
         brown_app <- brown_app %>% 
           filter(position == input$shooter)
       }
     }
     
     # Plotting shots onto the lacrosse field, filtering for shot result (goal,
     # save miss), then creating a density heat map based on shot location was
     # quite tough (a lot of trial and error), but I finally figured it out. For
     # the heat density map, the center area represents where the most shots of
     # that type are being taken.
     
     heat_map <- brown_app %>% 
       filter(result == input$result) %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Shot Density vs. Brown",
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
   }, height = 400, width = 650, units = "px")
   
   output$all_shots <- renderPlot({
     
     # Finding if the user selected an individual player or position to look at
     # and then filtering if so.
     
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
     
     # Finding shot percentage to be used in subtitle of plot
     
     sh_percent <- brown_app %>% 
       mutate(shots = n()) %>% 
       filter(result == "Goal") %>% 
       mutate(sh_per = n() / shots,
              sh_per = percent(sh_per)) %>% 
       select(sh_per) %>% 
       head(1) 
     
     my_subtitle <- paste("Shot Percentage: ", sh_percent$sh_per)
     
     
     # Changing numeric(0) to NA then to 0.
     
     #Xsh_percent <- lapply(sh_percent, function(x) if(identical(x, character(0))) NA_character_ else x) 
     
     # Plotting all shots onto the lacrosse field, with shot result mapped onto color 
     # (green for goal, black for miss, red for save). 
     
     shot_map <- brown_app %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Shot Accuracy vs. Brown",
            x = "Offensive Area of Field", 
            color = "Shot Result",
            subtitle = my_subtitle) +
       geom_point(stat = "identity", position = "dodge", size = 2, alpha = 0.7) +
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
   
   output$clock <- renderPlot({
     
     # Finding if the user selected an individual player or position to look at
     # and then filtering if so.
     
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
     
     # Selecting which shots to show based on the user's input on whether to see
     # shots that satisfy or violate the new shot clock rule.
     
     if (input$clock_sat == "Satisfied") {
       brown_app <- brown_app %>% 
         filter(sh_clock == "T")
     }
     else if (input$clock_sat == "Violated") {
       brown_app <- brown_app %>% 
         filter(sh_clock == "F")
     }
     
     # Finding shot percentage to be used in subtitle of plot
     
     sh_percent <- brown_app %>% 
       mutate(shots = n()) %>% 
       filter(result == "Goal") %>% 
       mutate(sh_per = n() / shots,
              sh_per = percent(sh_per)) %>% 
       select(sh_per) %>% 
       head(1) 
     
     my_subtitle_2 <- paste("Shot Percentage: ", sh_percent$sh_per)
     
     # Plotting shots onto the lacrosse field based on whether the shot would
     # satisfy the new 80-second shot clock rule in lacrosse. I am looking at
     # this to see how much our offense will need to change going forward, as
     # well as to see if shots later in possessions (after the new shot clock
     # would have expired) have noticeably different results.
     
     clock_map <- brown_app %>% 
       ggplot(aes(x = sh_x_pxls, y = sh_y_pxls, color = result)) + 
       annotation_custom(field, -250, 250, -50, 420) +
       labs(title = "Shot Clock Abidance vs. Brown",
            x = "Offensive Area of Field", 
            color = "Shot Result",
            subtitle = my_subtitle_2) +
       geom_point(stat = "identity", position = "dodge", size = 2, alpha = 0.7) +
       xlim(-250, 250) +
       ylim(-50, 420) +
       scale_colour_manual(values = MyPalette) +
       # Hiding numbers on the axes
       theme(axis.title.y = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_blank()) 
     
     clock_map
   }, height = 400, width = 650, units = "px")
   
   output$about <- renderUI({
     
     # Summary of application
     
     str1 <- paste("Summary")
     str2 <- paste("This application gives the user various ways to analyze the shooting performance
                   of the Harvard Men's Lacrosse Team in the 2018 season. The user has the ability to
                   choose whether or not to look at shots from the whole team, from specific position
                   groups, or from individual players. The user can then observe the specified shots 
                   on an actual lacrosse field where they occurred, with shooting percentage and shot
                   result provided. The user can also look at heat maps for shots based on result.
                   Finally, this app also serves a forward-looking purpose for assessing how our offense
                   will fare under the new 80-second shot clock being introduced in 2019, as the third 
                   tab allows the user to toggle between shots that would satisy or violate the shot clock.")
   
     HTML(paste(h3(str1), p(str2)))})     
}



# Run the application 
shinyApp(ui = ui, server = server)


