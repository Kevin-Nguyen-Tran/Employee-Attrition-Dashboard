rm(list = ls())
library(shiny)
library(tidyverse)
data <- read.csv("C:/Users/Kevin/Desktop/Employee-Attrition-Dashboard/WA_Fn-UseC_-HR-Employee-Attrition.csv")
employee_data <- as.tibble(data)

employee_data <- rename(employee_data, Age = ï..Age)

vars <- setdiff(names(employee_data), "Attrition")


ui <- fluidPage(
  selectInput(inputId = "varix", label = "Choose X Variable", vars),
  selectInput(inputId = "variy", label = "Choose Y Variable", vars),
  plotOutput(outputId = "scatter")
)

server <- function(input, output){
  output$scatter <- renderPlot({ggplot(data = employee_data, aes(x = input$varix, y = input$variy)) +
      geom_point()})
  
}

shinyApp(ui = ui, server = server)
