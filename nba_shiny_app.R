library(shiny)
library(tidyverse)
library(plotly)

# load data
players_with_salary <- read_csv('raw_data/nba_2017_nba_players_with_salary.csv') # 342 x 39 (PS)


# Define all UI Stuff here -- see https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
ui <- fluidPage(
  # titlePanel("Data Science II Final Project: NBA Data Statistical Analysis"),
  
  mainPanel(
    br(),
    h1("Data Science II Final Project: NBA Data Statistical Analysis"),
    h4("Adrian Naranjo and Julia Wilkins"), br(),
    h3("Introduction"),
    p("In this report, we will first walk through an Exploratory Data Analysis (EDA) using the NBA Social Power Dataset
      before using this data to explore the following questions: "),
    tags$ol(tags$li("Can we predict salary from performance statistics?"), 
            tags$li("Can we predict team valuations from individual salaries?"),
            tags$li("Can we predict salary based on social media stats or vice versa?")), br(),
   
    # interactive EDA graphs
    h3("Exploratory Data Analysis (EDA)"), br(),
    h4("Salary vs. Individual Metrics Graph"),
    helpText("Select which individual metric you would like to compare with salary:"),
    
    selectInput("ind_metrics_x", 
                label = "Choose an X-axis Metric:",
                choices = list("Points",
                               "Pace"),
                selected = "POINTS"),
    plotOutput("individual_eda_graph"), br(), br(),
    
    # HEADERS FOR OTHER QUESTIONS
    h3("Q1: Can we predict salary from performance statistics?")
    # h3("Q2: Can we predict team valuations from individual salaries?"),
    # h3("Q3: Can we predict salary based on social media stats or vice versa?")
    
  ),  
  br(),br(),
  sidebarPanel(
    # for some reason you have to put this image in a folder called www, so that's where the image is
    img(src="i.png", width=200),
    h4("We can also do stuff in a sidebar panel like this if we want")
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  # This is where we tell the app how to get the information from the UI such as if a person
  # changes a slider or button
  
  output$individual_eda_graph <- renderPlot({
    
    ind_metrics_out_x <- switch(input$ind_metrics_x,
                      "Points" = "POINTS",
                      "Pace" = "PACE")
    
    ind_plot <- ggplot(data = players_with_salary) + 
      geom_point(mapping = aes_string(x = ind_metrics_out_x, y = "SALARY_MILLIONS")) +
      ggtitle("Salary vs. Individual Metrics") + 
      ylab("Salary") + 
      xlab(ind_metrics_out_x)
    print(ind_plot)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)