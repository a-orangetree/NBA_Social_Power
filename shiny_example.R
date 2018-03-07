library(shiny)
library(tidyverse)
# library(ggplot2)
library(plotly)

kenpom = read.csv("kenpom2.csv")
b10 = filter(kenpom, Conf == "B10")
# print(b10)

# Define UI ----
ui <- fluidPage(
  titlePanel('JuliaPom'),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select which two variables you would like to compare."),
      
      selectInput("xax", 
                  label = "Choose an X-axis variable:",
                  choices = list("Rank", 
                                 "Wins",
                                 "Losses", 
                                 "AdjEM",
                                 "AdjO",
                                 "AdjD",
                                 "AdjT",
                                 "Luck",
                                 "Strength of Schedule: AdjEM",
                                 "Strength of Schedule: OppO",
                                 "Strength of Schedule: OppD",
                                 "NCSOS: AdjEM"
                                 ),
                  selected = "Rank"),
      
      selectInput("yax", 
                  label = "Choose a Y-axis variable:",
                  choices = list("Rank", 
                                 "Wins",
                                 "Losses", 
                                 "AdjEM",
                                 "AdjO",
                                 "AdjD",
                                 "AdjT",
                                 "Luck",
                                 "Strength of Schedule: AdjEM",
                                 "Strength of Schedule: OppO",
                                 "Strength of Schedule: OppD",
                                 "NCSOS: AdjEM"),
                  selected = "AdjO"),
      
      sliderInput("rank_slider", label = "Select range of team ranks to view:", min = 1, 
                  max = 351, value = c(1, 25))
    ),
    
    mainPanel(
      textOutput("selected_x"),
      textOutput("selected_y"),
      plotOutput("first_g"),
      textOutput("range")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # output$selected_x <- renderText({
  #    paste("You have selected x: ", input$xax)
  #  })
  # 
  #  output$selected_y <- renderText({
  #    paste("You have selected y: ", input$yax)
  #  })
  
  output$range <- renderText({ 
    paste(input$rank_slider) 
  })
  
  
  output$first_g <- renderPlot({
    
    start_range <- c(input$rank_slider)[1]
    end_range <- c(input$rank_slider)[2]
    
    # print(start_range)
    # print(end_range)
    # 
    
    final_x <- switch(input$xax,
                      "Rank" = "Rank",
                      "Wins" = "Wins",
                      "Losses" = "Losses",
                      "AdjO" = "AdjO",
                      "AdjD" = "AdjD",
                      "AdjT" = "Adjt",
                      "Luck" = "Luck",
                      "Strength of Schedule: AdjEM" = "Strength of Schedule: AdjEM",
                      "Strength of Schedule: OppO" = "Strength of Schedule: OppO",
                      "Strength of Schedule: OppD" = "Strength of Schedule: OppD",
                      "NCSOS: AdjEM" = "NCSOS: AdjEM",
                      "AdjEM" = "AdjEM")

    final_y <- switch(input$yax,
                       "Rank" = "Rank",
                       "Wins" = "Wins",
                       "Losses" = "Losses",
                       "AdjO" = "AdjO",
                       "AdjD" = "AdjD",
                       "AdjT" = "Adjt",
                       "Luck" = "Luck",
                       "Strength of Schedule: AdjEM" = "Strength of Schedule: AdjEM",
                       "Strength of Schedule: OppO" = "Strength of Schedule: OppO",
                       "Strength of Schedule: OppD" = "Strength of Schedule: OppD",
                       "NCSOS: AdjEM" = "NCSOS: AdjEM",
                       "AdjEM" = "AdjEM")
    
    p <- ggplot(data = filter(kenpom, Rank >= start_range & Rank <= end_range)) + 
      geom_point(mapping = aes_string(x = final_x, y = final_y, color = "Conf", size = "Rank"))
    print(p)
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)