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
library(ggplot2)
football = read_csv(file = "~/Desktop/football/data/football.csv")
#football$OpponentRank <- as.factor(football$OpponentRank)
view(football)
# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "Illini Football",
  tabPanel(
    title = "Visualization",
    titlePanel(title = "Illini Football Data 1892-2022"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("year", "Year Range:",
                    min = min(football$Season), max = max(football$Season),
                    value = c(1940,1990)
                    ),
        selectInput("location", "Location",
                    choices = sort(unique(football$Location)), selected = 'vs.'),
        selectInput("opponent", "Opponent",
                    choices = sort(unique(football$Opponent)), selected = 'Purdue')
      ),
      mainPanel(plotOutput("plot"))
    )
    ),
  tabPanel(title = "Table", dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("about.Rmd"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  year_range = reactive({
    football %>% 
      filter(Season >= input$year[1] & Season <= input$year[2])
  })  
  
  observeEvent(
    eventExpr = input$year,
    handlerExpr = {
      updateSelectInput(inputId = "location", choices = unique(year_range()$Location), selected = min(unique(year_range()$Location)))
    }
  )
  loc = reactive({
    year_range() %>% 
      filter(Location == input$location)
  })  
  
  observeEvent(
    eventExpr = input$location,
    handlerExpr = {
      updateSelectInput(inputId = "opponent", choices = unique(loc()$Opponent), selected = min(unique(loc()$Opponent)))
    }
  )
  output$plot = renderPlot(
    football %>% 
      filter(Season >= input$year[1] & Season <= input$year[2]) %>% 
      filter(Location == input$location) %>% 
      filter(Opponent == input$opponent) %>% 
      mutate(pointDifferiential = IlliniScore - OpponentScore) %>% 
      ggplot(aes(x=Season, y = pointDifferiential, color = Opponent)) + 
      geom_point(size = 2) +
      geom_smooth(method = "loess") +
      theme_minimal()
  )
  
  output$table = renderDataTable(
    loc() %>% 
      filter(Opponent == input$opponent) %>% 
      calc_ptdf()
  )

}


# Run the application 
shinyApp(ui = ui, server = server)
