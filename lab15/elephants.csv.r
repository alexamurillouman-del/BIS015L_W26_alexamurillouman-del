## Load the libraries
library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title="Elephants Range by Sex"),
  
  dashboardSidebar(selectInput("y",
                "Select Variable",
                choices = c("age", "height"),
                selected="age")
  ),
  
  dashboardBody( 
    
    plotOutput("plot", width="500px", height="400px")
    
  )
)

server <- function(input, output, session) {
  
  
  output$plot <- renderPlot({
    
    elephants %>%
      filter(!is.na(sex), sex != "NA") %>%
      ggplot(aes_string(x="sex", y=input$y, fill="sex")) +
      geom_boxplot(alpha=0.75, color="turquoise4") +
      labs(title="Elephants Range by Sex",
           x="Sex", y=input$y, fill="Sex") +
      theme_minimal()
  })
}

shinyApp(ui, server)
