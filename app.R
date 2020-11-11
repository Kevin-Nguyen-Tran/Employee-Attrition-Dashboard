rm(list = ls())
library(shiny)
library(tidyverse)
data <- read.csv("C:/Users/Kevin/Desktop/Employee-Attrition-Dashboard/WA_Fn-UseC_-HR-Employee-Attrition.csv")
employee_data <- as.tibble(data)

employee_data <- rename(employee_data, Age = ï..Age)

employee_data2 <- employee_data %>%
  select(Age, HourlyRate)


#ui <- fluidPage(
  #selectInput(inputId = "varix", label = "Choose X Variable", "Distance" = "employee_data[ ,4]", "Age" = "employee_data[ ,1]"),
  #selectInput(inputId = "variy", label = "Choose Y Variable", "Distance" = "employee_data[ ,4]", "Age" = "employee_data[ ,1]"),
 # plotOutput(outputId = "scatter")
#)

#server <- function(input, output){
 # output$scatter <- renderPlot({ggplot(data = employee_data, aes(x = input$varix, y = input$variy)) +
  #    geom_point()})
  
#}

#shinyApp(ui = ui, server = server)

ui <- fluidPage(
    radioButtons(inputId = "plotType", label = "Plot Type:",choices = c("Scatter" = "p", "Line" = "l")),
    plotOutput(outputId = "plot")
  )  

server <- function(input, output){
  output$plot <- renderPlot({
    plot(employee_data2, type = input$plotType)
  })
  
}

shinyApp(ui = ui, server = server)
















