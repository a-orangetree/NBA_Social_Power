library(shiny)
library(tidyverse)
library(plotly)

# load data

# Define all UI Stuff here -- see https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
ui <- fluidPage(
  titlePanel("Data Science II Final Project: NBA Social Power Data Analaysis"),
  
  mainPanel(
    h3("Adrian Naranjo and Julia Wilkins"),
    br(),
    p(strong("Introduction: "), "This is just like a bold <p> tag in HTML - maybe add introduction to project here")
  ), 
  sidebarPanel(
    h4("We can also do stuff in a sidebar panel like this if we want")
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  # This is where we tell the app how to get the information from the UI such as if a person
  # changes a slider or button
  
}

# Run the app ----
shinyApp(ui = ui, server = server)